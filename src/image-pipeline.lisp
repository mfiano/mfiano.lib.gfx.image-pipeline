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
  (back (make-f64-array 0) :type pb:buffer)
  (reach 1 :type u8)
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

(defun make-plan (&key
                    (pixel-format 'pf:rgba8888/u32)
                    width
                    height
                    (threads 4)
                    (fused t)
                    (reach 1)
                    filler)
  (check-type pixel-format pf:pixel-format)
  (check-type width u16+)
  (check-type height u16+)
  (check-type threads u8+)
  (check-type fused boolean)
  (check-type reach u8)
  (check-type filler (or null function))
  (let* ((kernel (lp:make-kernel threads :name "Pixel plan"))
         (padded (* (+ width reach reach) (+ height reach reach)))
         (workspace (make-f64-array (if fused
                                        (* threads +span-length+ 4)
                                        (* (max padded (* threads +span-length+)) 4))))
         (back (make-f64-array (if fused 0 (* padded 4))))
         (encoder (pf:encoder pixel-format))
         (decoder (make-decoder pixel-format filler width)))
    (prefault-workspace workspace kernel)
    (prefault-workspace back kernel)
    (%make-plan :encoder encoder
                :decoder decoder
                :back back
                :reach reach
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
            (plan-back plan) (make-f64-array 0)
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

(defmacro span-runs ((pixel-index workspace-index count &optional (x (gensym)) (y (gensym)))
                     per-worker
                     &body body)
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
                 :do (let* ((,count ,(if per-worker
                                         `(if (= rect-width image-width)
                                              (- ,end ,position)
                                              (min (- ,end ,position) (- rect-width ,column)))
                                         `(min (- ,end ,position) (- rect-width ,column))))
                            (,x (+ rect-x ,column))
                            (,y (+ rect-y ,row))
                            (,pixel-index (+ (* ,y image-width) ,x))
                            (,workspace-index
                              ,(if per-worker
                                   `(* (+ (* ,worker +span-length+) (- ,position ,start))
                                       workspace-lanes)
                                   `(* (+ (* (+ ,y reach) padded-width) ,x reach)
                                       workspace-lanes))))
                       (declare (type fixnum ,count)
                                (type u16 ,x ,y)
                                (type pb:index ,pixel-index ,workspace-index)
                                (ignorable ,x ,y ,pixel-index))
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
           `(when ,op
              (loop :with end :of-type fixnum := (+ workspace-index (* count workspace-lanes))
                    :for lane :of-type fixnum :from workspace-index :below end :by workspace-lanes
                    :do (funcall ,op workspace lane)))))
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

(define-ftype grow-rect (u16 u16 u16 u16 u16+ u16+ u8 boolean) (values u16 u16 u16 u16))
(defun grow-rect (x y width height image-width image-height reach wrap)
  (flet ((grow (start size limit)
           (let ((low (- start reach))
                 (high (+ start size reach)))
             (if (and wrap (or (minusp low) (> high limit)))
                 (values 0 limit)
                 (values (max 0 low) (min limit high))))))
    (multiple-value-bind (x1 x2) (grow x width image-width)
      (multiple-value-bind (y1 y2) (grow y height image-height)
        (values x1 y1 x2 y2)))))

(define-ftype padded-lane (fixnum fixnum u16+ u8) pb:index)
(declaim (inline padded-lane))
(defun padded-lane (x y width reach)
  (* (+ (* (+ y reach) (+ width reach reach)) x reach) 4))

(defun copy-ring (source destination width reach x1 y1 x2 y2 rect-x rect-y rect-x2 rect-y2)
  (declare (optimize (speed 3))
           (type pb:buffer source destination)
           (type u16+ width)
           (type u8 reach)
           (type u16 x1 y1 x2 y2 rect-x rect-y rect-x2 rect-y2))
  (flet ((copy (from-x to-x y)
           (when (< from-x to-x)
             (replace destination
                      source
                      :start1 (padded-lane from-x y width reach)
                      :end1 (padded-lane to-x y width reach)
                      :start2 (padded-lane from-x y width reach)))))
    (loop :for y :from y1 :below y2
          :do (if (< (1- rect-y) y rect-y2)
                  (progn
                    (copy x1 rect-x y)
                    (copy rect-x2 x2 y))
                  (copy x1 x2 y))))
  (values))

(defun fill-border (buffer width height reach edge)
  (declare (optimize (speed 3))
           (type pb:buffer buffer)
           (type u16+ width height)
           (type u8 reach))
  (flet ((fill-pixel (x y)
           (let ((lane (padded-lane x y width reach)))
             (if (listp edge)
                 (loop :for value :of-type f64 :in edge
                       :for channel :of-type fixnum :from lane
                       :do (setf (aref buffer channel) value))
                 (let ((from (if (eq edge :wrap)
                                 (padded-lane (mod x width) (mod y height) width reach)
                                 (padded-lane (min (max x 0) (1- width))
                                              (min (max y 0) (1- height))
                                              width
                                              reach))))
                   (replace buffer buffer :start1 lane :end1 (+ lane 4) :start2 from))))))
    (loop :for y :of-type fixnum :from (- reach) :below (+ height reach)
          :do (if (< -1 y height)
                  (dotimes (i reach)
                    (fill-pixel (- -1 i) y)
                    (fill-pixel (+ width i) y))
                  (loop :for x :of-type fixnum :from (- reach) :below (+ width reach)
                        :do (fill-pixel x y)))))
  (values))

(defun check-edge (edge)
  (unless (or (member edge '(:clamp :wrap))
              (and (listp edge)
                   (= (length edge) 4)
                   (every #'realp edge)))
    (error "edge must be :clamp, :wrap or a list of four channel values, not ~s" edge))
  (if (listp edge)
      (mapcar #'f64 edge)
      edge))

(defun map-image-pipeline (plan
                           pixels
                           &key
                             pre
                             work
                             post
                             (passes 1)
                             (edge :clamp)
                             (x1 0)
                             (y1 0)
                             x2
                             y2)
  (declare (optimize (speed 3)))
  (declare (type (or null function) pre post)
           (type function work))
  (check-plan plan)
  (check-pixels plan pixels)
  (check-type passes u16+)
  (let ((edge (check-edge edge)))
    (multiple-value-bind (rect-width rect-height rect-x rect-y) (make-rect plan x1 y1 x2 y2)
      (let* ((image-width (plan-width plan))
             (image-height (plan-height plan))
             (workspace (plan-workspace plan))
             (workspace-lanes 4)
             (encoder (plan-encoder plan))
             (decoder (plan-decoder plan))
             (lp:*kernel* (plan-kernel plan)))
        (if (plan-fused plan)
            (progn
              (unless (= passes 1)
                (error "a fused plan runs one pass, not ~d" passes))
              (do-pipeline-phases (:pre :work :post) :per-worker t))
            (let* ((reach (plan-reach plan))
                   (padded-width (+ image-width reach reach))
                   (stride (* padded-width workspace-lanes))
                   (source workspace)
                   (destination (plan-back plan)))
              (declare (type pb:buffer source destination))
              (multiple-value-bind (grown-x1 grown-y1 grown-x2 grown-y2)
                  (grow-rect rect-x
                             rect-y
                             rect-width
                             rect-height
                             image-width
                             image-height
                             reach
                             (eq edge :wrap))
                (let ((rect-x grown-x1)
                      (rect-y grown-y1)
                      (rect-width (- grown-x2 grown-x1))
                      (rect-height (- grown-y2 grown-y1)))
                  (do-pipeline-phases (:pre)))
                (when (> passes 1)
                  (copy-ring source
                             destination
                             image-width
                             reach
                             grown-x1
                             grown-y1
                             grown-x2
                             grown-y2
                             rect-x
                             rect-y
                             (+ rect-x rect-width)
                             (+ rect-y rect-height))))
              (dotimes (pass passes)
                (fill-border source image-width image-height reach edge)
                (let ((last (= pass (1- passes))))
                  (map-spans
                   (* rect-width rect-height)
                   +span-length+
                   (span-runs (pixel-index lane count x y) nil
                     (loop :with end :of-type fixnum := (+ lane (* count workspace-lanes))
                           :for at :of-type fixnum :from lane :below end :by workspace-lanes
                           :for column :of-type fixnum :from x
                           :do (funcall work source destination at stride column y))
                     (when last
                       (when post
                         (loop :with end :of-type fixnum := (+ lane (* count workspace-lanes))
                               :for at :of-type fixnum :from lane :below end :by workspace-lanes
                               :do (funcall post destination at)))
                       (funcall encoder destination lane pixels pixel-index count)))))
                (rotatef source destination))))
        (values)))))
