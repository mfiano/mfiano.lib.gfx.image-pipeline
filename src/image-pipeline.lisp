(in-package #:mfiano.lib.base.pkg)

(define-package #:mfiano.lib.gfx.image-pipeline
  (:local-nicknames
   (#:bt #:bordeaux-threads-2)
   (#:lp #:lparallel)
   (#:pb #:mfiano.lib.gfx.pixel-buffer)
   (#:pf #:mfiano.lib.gfx.pixel-format))
  (:use #:mfiano.lib.base.util)
  (:export
   #:make-plan
   #:map-image
   #:map-image-pipeline))

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
  (pixels-length 0 :type fixnum))

(defun map-rows (count function)
  (declare (optimize (speed 3))
           (type u16 count)
           (type function function))
  (let ((workers (lp:kernel-worker-count))
        (next (bt:make-atomic-integer))
        (channel (lp:make-channel)))
    (declare (type u8+ workers))
    (flet ((claim-rows ()
             (loop :for row :of-type fixnum := (1- (bt:atomic-integer-incf next))
                   :while (< row count)
                   :do (funcall function row))))
      (dotimes (worker workers)
        (lp:submit-task channel #'claim-rows))
      (dotimes (worker workers)
        (lp:receive-result channel)))
    (values)))

(defun prefault-workspace (workspace kernel row-length)
  (let ((lp:*kernel* kernel))
    (map-rows
     (floor (length workspace) row-length)
     (lambda (row)
       (let ((start (* row row-length)))
         (fill workspace 0d0 :start start :end (+ start row-length)))))))

(defun make-decoder (pixel-format filler width)
  (declare (type u16+ width))
  (if filler
      (lambda (pixels pixel-index workspace workspace-index count)
        (declare (optimize (speed 3))
                 (ignore pixels)
                 (type function filler)
                 (type pb:index pixel-index))
        (multiple-value-bind (y x) (floor pixel-index width)
          (funcall filler workspace workspace-index x y count))
        (values))
      (pf:decoder pixel-format)))

(defun pixel-array-length (pixel-format width height)
  (* width height (if (eq (pf:layout pixel-format) :packed) 1 (pf:channel-count pixel-format))))

(defun make-plan (&key (pixel-format 'pf:rgba8888/u32) width height (threads 4) filler)
  (check-type width u16+)
  (check-type height u16+)
  (check-type threads u8+)
  (check-type pixel-format pf:pixel-format)
  (check-type filler (or null function))
  (let* ((kernel (lp:make-kernel threads :name "Pixel plan"))
         (row-length (* width 4))
         (workspace (make-f64-array (* row-length (max threads height))))
         (encoder (pf:encoder pixel-format))
         (decoder (make-decoder pixel-format filler width)))
    (prefault-workspace workspace kernel row-length)
    (%make-plan :encoder encoder
                :decoder decoder
                :width width
                :height height
                :threads threads
                :kernel kernel
                :workspace workspace
                :storage (pf:storage pixel-format)
                :pixels-length (pixel-array-length pixel-format width height))))

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

(defun map-image (plan pixels op &key (x1 0) (y1 0) x2 y2)
  (declare (optimize (speed 3)))
  (declare (type (or null function) op))
  (check-pixels plan pixels)
  (multiple-value-bind (rect-width rect-height rect-x rect-y) (make-rect plan x1 y1 x2 y2)
    (let* ((image-width (plan-width plan))
           (workspace (plan-workspace plan))
           (workspace-lanes 4)
           (workspace-row-width (* image-width workspace-lanes))
           (encoder (plan-encoder plan))
           (decoder (plan-decoder plan))
           (lp:*kernel* (plan-kernel plan)))
      (map-rows
       rect-height
       (lambda (row-offset)
         (declare (type u16 row-offset))
         (let* ((image-y (+ rect-y row-offset))
                (image-row-index (* image-y image-width))
                (pixel-index (+ image-row-index rect-x))
                (worker (or (lp:kernel-worker-index) 0))
                (workspace-index (* worker workspace-row-width)))
           (declare (type pb:index workspace-index))
           (funcall decoder pixels pixel-index workspace workspace-index rect-width)
           (when op
             (loop :with end :of-type fixnum := (+ workspace-index (* rect-width workspace-lanes))
                   :for lane :of-type fixnum :from workspace-index :below end :by workspace-lanes
                   :do (funcall op workspace lane)))
           (funcall encoder workspace workspace-index pixels pixel-index rect-width))))
      (values))))

(defmacro do-pipeline-phases ((&rest phases) &key worker-rows)
  (flet ((phase-loop (op)
           `(loop :for lane :of-type fixnum :from workspace-index :below end :by workspace-lanes
                  :do (funcall ,op workspace lane))))
    `(map-rows
      rect-height
      (lambda (rect-row-index)
        (declare (type pb:index rect-row-index))
        (let* ((image-y (+ rect-y rect-row-index))
               (image-row-index (* image-y image-width))
               (pixel-index (+ image-row-index rect-x))
               (workspace-index ,(if worker-rows
                                     '(* (or (lp:kernel-worker-index) 0) workspace-row-width)
                                     '(* pixel-index workspace-lanes)))
               (end (+ workspace-index (* rect-width workspace-lanes))))
          (declare (type pb:index workspace-index)
                   (type fixnum end))
          ,@(when (find :pre phases)
              `((funcall decoder pixels pixel-index workspace workspace-index rect-width)))
          ,@(loop :for (phase op) :in '((:pre pre) (:work work) (:post post))
                  :when (find phase phases)
                    :collect (phase-loop op))
          ,@(when (find :post phases)
              `((funcall encoder workspace workspace-index pixels pixel-index rect-width))))))))

(defun map-image-pipeline (plan pixels &key pre work post fused (x1 0) (y1 0) x2 y2)
  (declare (optimize (speed 3)))
  (declare (type function pre work post))
  (check-pixels plan pixels)
  (multiple-value-bind (rect-width rect-height rect-x rect-y) (make-rect plan x1 y1 x2 y2)
    (let* ((image-width (plan-width plan))
           (workspace (plan-workspace plan))
           (workspace-lanes 4)
           (workspace-row-width (* image-width workspace-lanes))
           (encoder (plan-encoder plan))
           (decoder (plan-decoder plan))
           (lp:*kernel* (plan-kernel plan)))
      (if fused
          (do-pipeline-phases (:pre :work :post) :worker-rows t)
          (progn
            (do-pipeline-phases (:pre))
            (do-pipeline-phases (:work))
            (do-pipeline-phases (:post))))
      (values))))
