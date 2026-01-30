(in-package #:mfiano.lib.base.pkg)

(define-package #:mfiano.lib.gfx.image-pipeline
  (:local-nicknames
   (#:lp #:lparallel)
   (#:pb #:mfiano.lib.gfx.pixel-buffer)
   (#:pf #:mfiano.lib.gfx.pixel-format))
  (:use #:mfiano.lib.base.util)
  (:export
   #:make-plan
   #:map-image
   #:map-image-pipeline))

(in-package #:mfiano.lib.gfx.image-pipeline)

(deftype image () '(or (u8-array (*)) (u32-array (*))))

(defstruct (plan
            (:constructor %make-plan)
            (:predicate nil)
            (:copier nil))
  (width 1 :type u16+)
  (height 1 :type u16+)
  (threads 1 :type u8+)
  (kernel (lp:make-kernel 1) :type lparallel:kernel)
  (workspace (make-f64-array 0) :type pb:buffer)
  (encoder (constantly nil) :type function)
  (decoder (constantly nil) :type function))

(defun make-plan (&key (pixel-format 'pf:rgba8888/u32) width height (threads 4))
  (check-type width fixnum+)
  (check-type height fixnum+)
  (check-type threads u8+)
  (check-type pixel-format pf:pixel-format)
  (let* ((kernel (lp:make-kernel threads :name "Pixel plan"))
         (row-length (* width 4))
         (workspace (make-f64-array (* row-length (max threads height))))
         (encoder (pf:get-encoder pixel-format))
         (decoder (pf:get-decoder pixel-format)))
    (check-type encoder function)
    (check-type decoder function)
    (%make-plan :encoder encoder
                :decoder decoder
                :width width
                :height height
                :threads threads
                :kernel kernel
                :workspace workspace)))

(define-ftype make-rect (plan u16 u16 (or u16 null) (or u16 null)) (values u16 u16 u16 u16))
(declaim (inline make-rect))
(defun make-rect (plan x1 y1 x2 y2)
  (let ((width (plan-width plan))
        (height (plan-height plan)))
    (values x1 y1 (or x2 width) (or y2 height))))

(define-ftype check-rect (plan u16 u16 (or u16 null) (or u16 null)) (values))
(declaim (inline check-rect))
(defun check-rect (plan x1 y1 x2 y2)
  (declare (optimize speed))
  (let ((width (plan-width plan))
        (height (plan-height plan)))
    (unless (and (<= 0 x1 x2 width) (<= 0 y1 y2 height) (< x1 x2) (< y1 y2))
      (error "bad rect: (~d ~d ~d ~d) for image ~dx~d" x1 y1 x2 y2 width height))))

(defun map-image (plan pixels op &key (x1 0) (y1 0) x2 y2)
  (declare (optimize (speed 3)))
  (declare (type function op))
  (multiple-value-bind (x1 y1 x2 y2) (make-rect plan x1 y1 x2 y2)
    (let* ((image-width (plan-width plan))
           (rect-width (- x2 x1))
           (rect-height (- y2 y1))
           (workspace (plan-workspace plan))
           (workspace-lanes 4)
           (workspace-row-width (* image-width workspace-lanes))
           (encoder (plan-encoder plan))
           (decoder (plan-decoder plan))
           (lp:*kernel* (plan-kernel plan)))
      (check-rect plan x1 y1 x2 y2)
      (lp:pdotimes (row-offset rect-height)
        (declare (type pb:index row-offset))
        (let* ((image-y (+ y1 row-offset))
               (image-row-index (* image-y image-width))
               (pixel-index (+ image-row-index x1))
               (worker (or (lp:kernel-worker-index) 0))
               (workspace-index (* worker workspace-row-width)))
          (declare (type pb:index workspace-index))
          (funcall decoder pixels pixel-index workspace workspace-index rect-width)
          (loop :for x :below rect-width
                :for lane-index :of-type pb:index :from workspace-index :by workspace-lanes
                :do (funcall op workspace lane-index))
          (funcall encoder workspace workspace-index pixels pixel-index rect-width))))
    (values)))

(defmacro do-pipeline-phases (&rest phases)
  `(lp:pdotimes (rect-row-index rect-height)
     (declare (type pb:index rect-row-index))
     (let* ((image-y (+ y1 rect-row-index))
            (image-row-index (* image-y image-width))
            (pixel-index (+ image-row-index x1))
            (workspace-index (* pixel-index workspace-lanes)))
       ,@(when (find :pre phases)
           `((funcall decoder pixels pixel-index workspace workspace-index rect-width)))
       (loop :for x :below rect-width
             :for lane-index :of-type pb:index :from workspace-index :by workspace-lanes
             ,@(when (find :pre phases)
                 `(:do (funcall pre workspace lane-index)))
             ,@(when (find :work phases)
                 `(:do (funcall work workspace lane-index)))
             ,@(when (find :post phases)
                 `(:do (funcall post workspace lane-index))))
       ,@(when (find :post phases)
           `((funcall encoder workspace workspace-index pixels pixel-index rect-width))))))

(defun map-image-pipeline (plan pixels &key pre work post fused (x1 0) (y1 0) x2 y2)
  (declare (optimize (speed 3)))
  (declare (type function pre work post))
  (multiple-value-bind (x1 y1 x2 y2) (make-rect plan x1 y1 x2 y2)
    (let* ((image-width (plan-width plan))
           (rect-width (- x2 x1))
           (rect-height (- y2 y1))
           (workspace (plan-workspace plan))
           (workspace-lanes 4)
           (encoder (plan-encoder plan))
           (decoder (plan-decoder plan))
           (lp:*kernel* (plan-kernel plan)))
      (check-rect plan x1 y1 x2 y2)
      (if fused
          (do-pipeline-phases :pre :work :post)
          (progn
            (do-pipeline-phases :pre)
            (do-pipeline-phases :work)
            (do-pipeline-phases :post))))
    (values)))
