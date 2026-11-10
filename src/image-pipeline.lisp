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

(define-constant +span-length+ 2048)

(deftype lane () '(unsigned-byte 36))

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

(declaim (inline %make-rect))
(defstruct (rect
            (:constructor %make-rect)
            (:predicate nil)
            (:copier nil))
  (width 1 :type u16+)
  (height 1 :type u16+)
  (x 0 :type u16)
  (y 0 :type u16))

(defun claim-spans (counter count function)
  (declare (optimize (speed 3))
           (type (uint 40) count)
           (type function function))
  (loop :for claim :of-type (uint 40) := (bt:atomic-integer-incf counter)
        :for start := (* (1- claim) +span-length+)
        :while (< start count)
        :do (funcall function start (min count (+ start +span-length+))))
  (values))

(defun run-workers (function)
  (let ((channel (lp:make-channel))
        (workers (lp:kernel-worker-count)))
    (dotimes (worker workers)
      (lp:submit-task channel function))
    (dotimes (worker workers)
      (lp:receive-result channel)))
  (values))

(defun map-spans (count function)
  (let ((counter (bt:make-atomic-integer)))
    (run-workers (lambda () (claim-spans counter count function)))))

(defun make-barrier (parties)
  (let ((lock (bt:make-lock :name "Pixel plan barrier"))
        (condition (bt:make-condition-variable))
        (arrived 0)
        (generation 0)
        (broken nil))
    (values
     (lambda (action)
       (bt:with-lock-held (lock)
         (let ((seen generation))
           (cond
             (broken nil)
             ((= (incf arrived) parties)
              (funcall action)
              (setf arrived 0)
              (incf generation)
              (bt:condition-broadcast condition)
              t)
             (t
              (until (or broken (/= generation seen))
                (bt:condition-wait condition lock))
              (not broken))))))
     (lambda ()
       (bt:with-lock-held (lock)
         (setf broken t)
         (bt:condition-broadcast condition))))))

(defun run-phases (phases)
  (multiple-value-bind (arrive break) (make-barrier (lp:kernel-worker-count))
    (let ((counters (loop :repeat (length phases) :collect (bt:make-atomic-integer))))
      (run-workers
       (lambda ()
         (let ((finished nil))
           (unwind-protect
                (progn
                  (loop :for (action count function) :in phases
                        :for counter :in counters
                        :while (or (null action) (funcall arrive action))
                        :do (claim-spans counter count function))
                  (setf finished t))
             (unless finished
               (funcall break)))))))))

(defun prefault-workspace (workspace kernel)
  (let ((lp:*kernel* kernel))
    (map-spans (length workspace)
               (lambda (start end) (fill workspace 0d0 :start start :end end)))))

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

(defun check-rect (width height x1 y1 x2 y2)
  (unless (and (<= 0 x1 x2 width) (<= 0 y1 y2 height) (< x1 x2) (< y1 y2))
    (error "bad rect: (~d ~d ~d ~d) for image ~dx~d" x1 y1 x2 y2 width height)))

(declaim (inline make-rect))
(defun make-rect (plan x1 y1 x2 y2)
  (let* ((width (plan-width plan))
         (height (plan-height plan))
         (x2 (or x2 width))
         (y2 (or y2 height)))
    (check-rect width height x1 y1 x2 y2)
    (%make-rect :width (- x2 x1) :height (- y2 y1) :x x1 :y y1)))

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

(define-ftype span-runs (u32 u32 rect boolean function) (values))
(defun span-runs (start end rect merge function)
  (declare (optimize (speed 3)))
  (let ((rect-width (rect-width rect))
        (rect-x (rect-x rect)))
    (multiple-value-bind (row column) (floor start rect-width)
      (loop :with position :of-type u32 := start
            :for y :of-type u16 :from (+ (rect-y rect) row)
            :for x := (+ rect-x column) :then rect-x
            :while (< position end)
            :do (let ((count (if merge
                                 (- end position)
                                 (min (- end position) (- (+ rect-x rect-width) x)))))
                  (funcall function x y count (- position start))
                  (incf position count)))))
  (values))

(define-ftype map-lanes ((or null function) pb:buffer lane u16) (values))
(declaim (inline map-lanes))
(defun map-lanes (op buffer lane count)
  (declare (optimize (speed 3)))
  (when op
    (loop :for at :from lane :below (+ lane (* count 4)) :by 4
          :do (funcall op buffer at)))
  (values))

(define-ftype worker-lane () lane)
(defun worker-lane ()
  (* (or (lp:kernel-worker-index) 0) +span-length+ 4))

(defun fused-span (plan pixels rect &rest ops)
  (let ((workspace (plan-workspace plan))
        (decoder (plan-decoder plan))
        (encoder (plan-encoder plan))
        (image-width (plan-width plan)))
    (lambda (start end)
      (let ((base (worker-lane)))
        (flet ((run (x y count offset)
                 (declare (optimize (speed 3))
                          (type u16 x y offset))
                 (let ((pixel-index (+ (* y image-width) x))
                       (lane (+ base (* offset 4))))
                   (funcall decoder pixels pixel-index workspace lane count)
                   (dolist (op ops)
                     (map-lanes op workspace lane count))
                   (funcall encoder workspace lane pixels pixel-index count))))
          (declare (dynamic-extent #'run))
          (span-runs start end rect (= (rect-width rect) image-width) #'run))))))

(defun map-image (plan pixels op &key (x1 0) (y1 0) x2 y2)
  (check-plan plan)
  (check-pixels plan pixels)
  (let ((lp:*kernel* (plan-kernel plan))
        (rect (make-rect plan x1 y1 x2 y2)))
    (map-spans (* (rect-width rect) (rect-height rect))
               (fused-span plan pixels rect op)))
  (values))

(define-ftype grow-rect (rect u16+ u16+ u8 boolean) rect)
(defun grow-rect (rect image-width image-height reach wrap)
  (flet ((grow (start size limit)
           (let ((low (- start reach))
                 (high (+ start size reach)))
             (if (and wrap (or (minusp low) (> high limit)))
                 (values 0 limit)
                 (values (max 0 low) (min limit high))))))
    (multiple-value-bind (x1 x2) (grow (rect-x rect) (rect-width rect) image-width)
      (multiple-value-bind (y1 y2) (grow (rect-y rect) (rect-height rect) image-height)
        (%make-rect :width (- x2 x1) :height (- y2 y1) :x x1 :y y1)))))

(declaim (inline padded-lane))
(defun padded-lane (x y width reach)
  (* (+ (* (+ y reach) (+ width reach reach)) x reach) 4))

(defun fill-border (buffer width height reach edge)
  (declare (optimize (speed 3))
           (type pb:buffer buffer)
           (type u16+ width height)
           (type u8 reach))
  (flet ((fill-pixel (x y)
           (let ((lane (padded-lane x y width reach)))
             (if (listp edge)
                 (loop :for value :in edge
                       :for channel :from lane
                       :do (setf (aref buffer channel) value))
                 (let ((from (if (eq edge :wrap)
                                 (padded-lane (mod x width) (mod y height) width reach)
                                 (padded-lane (min (max x 0) (1- width))
                                              (min (max y 0) (1- height))
                                              width
                                              reach))))
                   (replace buffer buffer :start1 lane :end1 (+ lane 4) :start2 from))))))
    (loop :for y :from (- reach) :below (+ height reach)
          :do (if (< -1 y height)
                  (dotimes (i reach)
                    (fill-pixel (- -1 i) y)
                    (fill-pixel (+ width i) y))
                  (loop :for x :from (- reach) :below (+ width reach)
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

(defun prepare-span (plan pixels pre copy rect)
  (declare (type (or null pb:buffer) copy))
  (let ((buffer (plan-workspace plan))
        (decoder (plan-decoder plan))
        (image-width (plan-width plan))
        (reach (plan-reach plan)))
    (lambda (start end)
      (flet ((run (x y count offset)
               (declare (optimize (speed 3))
                        (type u16 x y)
                        (ignore offset))
               (let ((lane (padded-lane x y image-width reach)))
                 (funcall decoder pixels (+ (* y image-width) x) buffer lane count)
                 (map-lanes pre buffer lane count)
                 (when copy
                   (replace copy buffer :start1 lane :end1 (+ lane (* count 4)) :start2 lane)))))
        (declare (dynamic-extent #'run))
        (span-runs start end rect nil #'run)))))

(defun pass-span (plan pixels work post source destination rect final)
  (declare (type function work))
  (let* ((encoder (plan-encoder plan))
         (image-width (plan-width plan))
         (reach (plan-reach plan))
         (stride (* (+ image-width reach reach) 4)))
    (lambda (start end)
      (flet ((run (x y count offset)
               (declare (optimize (speed 3))
                        (type u16 x y count)
                        (ignore offset))
               (let ((lane (padded-lane x y image-width reach)))
                 (dotimes (i count)
                   (funcall work source destination (+ lane (* i 4)) stride (+ x i) y))
                 (when final
                   (map-lanes post destination lane count)
                   (funcall encoder destination lane pixels (+ (* y image-width) x) count)))))
        (declare (dynamic-extent #'run))
        (span-runs start end rect nil #'run)))))

(defun run-unfused (plan pixels pre work post passes edge rect)
  (let ((front (plan-workspace plan))
        (back (plan-back plan))
        (width (plan-width plan))
        (height (plan-height plan))
        (reach (plan-reach plan)))
    (let ((grown (grow-rect rect width height reach (eq edge :wrap))))
      (flet ((pass-phase (pass)
               (let ((source (if (evenp pass) front back))
                     (destination (if (evenp pass) back front))
                     (final (= pass (1- passes))))
                 (list (lambda () (fill-border source width height reach edge))
                       (* (rect-width rect) (rect-height rect))
                       (pass-span plan pixels work post source destination rect final)))))
        (run-phases
         (cons (list nil
                     (* (rect-width grown) (rect-height grown))
                     (prepare-span plan pixels pre (when (> passes 1) back) grown))
               (loop :for pass :below passes
                     :collect (pass-phase pass))))))))

(defun map-image-pipeline (plan pixels
                           &key pre work post (passes 1) (edge :clamp) (x1 0) (y1 0) x2 y2)
  (declare (type (or null function) pre post)
           (type function work)
           (type u16+ passes))
  (check-plan plan)
  (check-pixels plan pixels)
  (let ((lp:*kernel* (plan-kernel plan))
        (rect (make-rect plan x1 y1 x2 y2)))
    (cond
      ((not (plan-fused plan))
       (run-unfused plan pixels pre work post passes (check-edge edge) rect))
      ((= passes 1)
       (map-spans (* (rect-width rect) (rect-height rect))
                  (fused-span plan pixels rect pre work post)))
      (t
       (error "a fused plan runs one pass, not ~d" passes))))
  (values))
