(in-package #:mfiano.lib.base.pkg)

(define-package #:mfiano.lib.gfx.image-pipeline
  (:local-nicknames
   (#:bt #:bordeaux-threads-2)
   (#:lp #:lparallel)
   (#:pb #:mfiano.lib.gfx.pixel-buffer)
   (#:pf #:mfiano.lib.gfx.pixel-format))
  (:use #:mfiano.lib.base.util)
  (:export
   #:cancel-plan
   #:make-plan
   #:map-image
   #:map-image-pipeline
   #:with-plan))

(in-package #:mfiano.lib.gfx.image-pipeline)

(defstruct (plan
            (:constructor %make-plan)
            (:predicate nil)
            (:copier nil))
  (width 1 :type u16+)
  (height 1 :type u16+)
  (threads 1 :type u8+)
  (kernel (lp:make-kernel 1) :type lp:kernel)
  (workspace (make-f64-array 0) :type pb:buffer)
  (encoder (constantly nil) :type function)
  (decoder (constantly nil) :type function)
  (storage :u32 :type pf:storage)
  (pixels-length 0 :type fixnum)
  (fused t :type boolean)
  (canceled nil :type boolean))

(define-constant +span-length+ 2048)

(defun map-spans (count span-length function)
  (declare (optimize (speed 3))
           (type (uint 40) count)
           (type (integer 1 #.(expt 2 20)) span-length)
           (type function function))
  (let ((workers (lp:kernel-worker-count))
        (next (bt:make-atomic-integer))
        (channel (lp:make-channel)))
    (declare (type u8+ workers))
    (flet ((claim-spans ()
             (loop :for claim :of-type (integer 1 #.(expt 2 40)) := (bt:atomic-integer-incf next)
                   :for start :of-type (uint 60) := (* (1- claim) span-length)
                   :while (< start count)
                   :do (funcall function start (min count (+ start span-length))))))
      (dotimes (worker workers)
        (lp:submit-task channel #'claim-spans))
      (dotimes (worker workers)
        (lp:receive-result channel)))
    (values)))

(defun prefault-workspace (workspace kernel)
  (let ((lp:*kernel* kernel))
    (map-spans (length workspace)
               (* +span-length+ 4)
               (lambda (start end)
                 (fill workspace 0d0 :start start :end end)))))

(defun make-decoder (pixel-format filler width)
  (declare (type u16+ width))
  (if filler
      (lambda (pixels pixel-index workspace workspace-index count)
        (declare (optimize (speed 3))
                 (ignore pixels)
                 (type function filler)
                 (type pb:index pixel-index workspace-index)
                 (type fixnum count))
        (while (plusp count)
          (multiple-value-bind (y x) (floor pixel-index width)
            (let ((run (min count (- width x))))
              (funcall filler workspace workspace-index x y run)
              (incf pixel-index run)
              (incf workspace-index (* run 4))
              (decf count run))))
        (values))
      (pf:decoder pixel-format)))

(defun pixel-array-length (pixel-format width height)
  (* width height (if (eq (pf:layout pixel-format) :packed) 1 (pf:channel-count pixel-format))))

(defun make-plan (&key (pixel-format 'pf:rgba8888/u32) width height (threads 4) (fused t) filler)
  (check-type pixel-format pf:pixel-format)
  (check-type width u16+)
  (check-type height u16+)
  (check-type threads u8+)
  (check-type fused boolean)
  (check-type filler (or null function))
  (let* ((kernel (lp:make-kernel threads :name "Pixel plan"))
         (workspace (make-f64-array (if fused
                                        (* threads +span-length+ 4)
                                        (* (max (* width height) (* threads +span-length+)) 4))))
         (encoder (pf:encoder pixel-format))
         (decoder (make-decoder pixel-format filler width)))
    (prefault-workspace workspace kernel)
    (%make-plan :encoder encoder
                :decoder decoder
                :width width
                :height height
                :threads threads
                :kernel kernel
                :workspace workspace
                :storage (pf:storage pixel-format)
                :pixels-length (pixel-array-length pixel-format width height)
                :fused fused)))

(defun cancel-plan (plan)
  (unless (plan-canceled plan)
    (let ((lp:*kernel* (plan-kernel plan)))
      (mapc #'bordeaux-threads:join-thread (lp:end-kernel :wait t))
      (setf (plan-workspace plan) (make-f64-array 0)
            (plan-canceled plan) t)))
  (values))

(defmacro with-plan ((plan &rest args) &body body)
  `(let ((,plan (make-plan ,@args)))
     (unwind-protect (progn ,@body)
       (cancel-plan ,plan))))

(defun check-plan (plan)
  (when (plan-canceled plan)
    (error "plan was canceled"))
  (values))

(define-ftype check-rect (u16+ u16+ u16 u16 (or u16 null) (or u16 null)) (values))
(declaim (inline check-rect))
(defun check-rect (width height x1 y1 x2 y2)
  (declare (optimize (speed 3)))
  (unless (and (<= 0 x1 x2 width) (<= 0 y1 y2 height) (< x1 x2) (< y1 y2))
    (error "bad rect: (~d ~d ~d ~d) for image ~dx~d" x1 y1 x2 y2 width height)))

(define-ftype make-rect (plan u16 u16 (or u16 null) (or u16 null)) (values u16 u16 u16 u16))
(declaim (inline make-rect))
(defun make-rect (plan x1 y1 x2 y2)
  (declare (optimize (speed 3)))
  (let* ((width (plan-width plan))
         (height (plan-height plan))
         (x2 (or x2 width))
         (y2 (or y2 height)))
    (check-rect width height x1 y1 x2 y2)
    (values (- x2 x1) (- y2 y1) x1 y1)))

(defun check-pixels (plan pixels)
  (let ((storage (plan-storage plan))
        (needed (plan-pixels-length plan)))
    (unless (ecase storage
              (:u8 (typep pixels '(u8-array (*))))
              (:u16 (typep pixels '(u16-array (*))))
              (:u32 (typep pixels '(u32-array (*))))
              (:f32 (typep pixels '(f32-array (*))))
              (:f64 (typep pixels '(f64-array (*)))))
      (error "the plan's pixel format needs a ~s array for pixels" storage))
    (when (< (length pixels) needed)
      (error "pixels has ~d elements, but the plan's image needs ~d" (length pixels) needed))
    (values)))

(defmacro span-runs ((pixel-index workspace-index count) per-worker &body body)
  (with-gensyms (start end position worker first-row first-column row column)
    `(lambda (,start ,end)
       (declare (type u32 ,start ,end))
       (let ,(when per-worker
               `((,worker (or (lp:kernel-worker-index) 0))))
         ,@(when per-worker
             `((declare (type u8 ,worker))))
         (multiple-value-bind (,first-row ,first-column) (floor ,start rect-width)
           (loop :with ,position :of-type u32 := ,start
                 :for ,row :of-type u32 :from ,first-row
                 :for ,column :of-type u16 := ,first-column :then 0
                 :while (< ,position ,end)
                 :do (let* ((,count (if (= rect-width image-width)
                                        (- ,end ,position)
                                        (min (- ,end ,position) (- rect-width ,column))))
                            (,pixel-index (+ (* (+ rect-y ,row) image-width) rect-x ,column))
                            (,workspace-index
                              ,(if per-worker
                                   `(* (+ (* ,worker +span-length+) (- ,position ,start))
                                       workspace-lanes)
                                   `(* ,pixel-index workspace-lanes))))
                       (declare (type fixnum ,count)
                                (type pb:index ,pixel-index ,workspace-index))
                       ,@body
                       (incf ,position ,count))))))))

(defun map-image (plan pixels op &key (x1 0) (y1 0) x2 y2)
  (declare (optimize (speed 3)))
  (declare (type (or null function) op))
  (check-plan plan)
  (check-pixels plan pixels)
  (multiple-value-bind (rect-width rect-height rect-x rect-y) (make-rect plan x1 y1 x2 y2)
    (let* ((image-width (plan-width plan))
           (workspace (plan-workspace plan))
           (workspace-lanes 4)
           (encoder (plan-encoder plan))
           (decoder (plan-decoder plan))
           (lp:*kernel* (plan-kernel plan)))
      (map-spans
       (* rect-width rect-height)
       +span-length+
       (span-runs (pixel-index workspace-index count) t
         (funcall decoder pixels pixel-index workspace workspace-index count)
         (when op
           (loop :with end :of-type fixnum := (+ workspace-index (* count workspace-lanes))
                 :for lane :of-type fixnum :from workspace-index :below end :by workspace-lanes
                 :do (funcall op workspace lane)))
         (funcall encoder workspace workspace-index pixels pixel-index count)))
      (values))))

(defmacro do-pipeline-phases ((&rest phases) &key per-worker)
  (flet ((phase-loop (op)
           `(loop :with end :of-type fixnum := (+ workspace-index (* count workspace-lanes))
                  :for lane :of-type fixnum :from workspace-index :below end :by workspace-lanes
                  :do (funcall ,op workspace lane))))
    `(map-spans
      (* rect-width rect-height)
      +span-length+
      (span-runs (pixel-index workspace-index count) ,per-worker
        ,@(when (find :pre phases)
            `((funcall decoder pixels pixel-index workspace workspace-index count)))
        ,@(loop :for (phase op) :in '((:pre pre) (:work work) (:post post))
                :when (find phase phases)
                  :collect (phase-loop op))
        ,@(when (find :post phases)
            `((funcall encoder workspace workspace-index pixels pixel-index count)))))))

(defun map-image-pipeline (plan pixels &key pre work post (x1 0) (y1 0) x2 y2)
  (declare (optimize (speed 3)))
  (declare (type function pre work post))
  (check-plan plan)
  (check-pixels plan pixels)
  (multiple-value-bind (rect-width rect-height rect-x rect-y) (make-rect plan x1 y1 x2 y2)
    (let* ((image-width (plan-width plan))
           (workspace (plan-workspace plan))
           (workspace-lanes 4)
           (encoder (plan-encoder plan))
           (decoder (plan-decoder plan))
           (lp:*kernel* (plan-kernel plan)))
      (if (plan-fused plan)
          (do-pipeline-phases (:pre :work :post) :per-worker t)
          (progn
            (do-pipeline-phases (:pre))
            (do-pipeline-phases (:work))
            (do-pipeline-phases (:post))))
      (values))))
