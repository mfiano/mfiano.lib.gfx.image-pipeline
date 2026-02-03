(in-package #:mfiano.lib.base.pkg)

(define-package #:mfiano.lib.gfx.image-pipeline/test
  (:local-nicknames
   (#:ip #:mfiano.lib.gfx.image-pipeline)
   (#:pb #:mfiano.lib.gfx.pixel-buffer)
   (#:pf #:mfiano.lib.gfx.pixel-format)
   (#:lp #:lparallel))
  (:use #:mfiano.lib.base.util #:1am))

(in-package #:mfiano.lib.gfx.image-pipeline/test)

(defun make-canonical-buffer (width height)
  (let* ((pixel-count (* width height))
         (buffer (pb:make-buffer pixel-count))
         (pixel-index 0))
    (dotimes (y height)
      (dotimes (x width)
        (let ((byte-r (mod (+ (* x 3) (* y 5)) 256))
              (byte-g (mod (+ (* x 7) (* y 11)) 256))
              (byte-b (mod (+ (* x 13) (* y 17)) 256))
              (byte-a (mod (+ (* x 19) (* y 23)) 256))
              (lane-index (* pixel-index 4)))
          (pb:set-channels buffer
                           lane-index
                           (/ byte-r 255d0)
                           (/ byte-g 255d0)
                           (/ byte-b 255d0)
                           (/ byte-a 255d0))
          (incf pixel-index))))
    buffer))

(defun make-pattern-image (plan)
  (let* ((width (ip::plan-width plan))
         (height (ip::plan-height plan))
         (pixel-count (* width height))
         (buffer (make-f64-array (* pixel-count 4)))
         (pixels (make-u32-array pixel-count))
         (encoder (ip::plan-encoder plan)))
    (dotimes (pixel-number pixel-count)
      (let ((index (* pixel-number 4)))
        (pb:set-channels buffer index
                         (/ (mod (* pixel-number 13) 256) 255d0)
                         (/ (mod (* pixel-number 29) 256) 255d0)
                         (/ (mod (* pixel-number 47) 256) 255d0)
                         (/ (mod (* pixel-number 71) 256) 255d0))))
    (funcall encoder buffer 0 pixels 0 pixel-count)
    pixels))

(defun encode-u32-rgba (buffer width height)
  (let* ((pixel-count (* width height))
         (pixels (make-u32-array pixel-count)))
    (pf:encode-rgba8888/u32 buffer 0 pixels 0 pixel-count)
    pixels))

(defun decode-u32-rgba (pixels width height)
  (let* ((pixel-count (* width height))
         (buffer (make-f64-array (* pixel-count 4))))
    (pf:decode-rgba8888/u32 pixels 0 buffer 0 pixel-count)
    buffer))

(defun encode-u8-rgba (buffer width height)
  (let* ((pixel-count (* width height))
         (pixels (make-u8-array (* pixel-count 4))))
    (pf:encode-rgba8888/u8 buffer 0 pixels 0 pixel-count)
    pixels))

(defun decode-u8-rgba (pixels width height)
  (let* ((pixel-count (* width height))
         (buffer (make-f64-array (* pixel-count 4))))
    (pf:decode-rgba8888/u8 pixels 0 buffer 0 pixel-count)
    buffer))

(defun decode-image (plan pixels)
  (let* ((width (ip::plan-width plan))
         (height (ip::plan-height plan))
         (pixel-count (* width height))
         (buffer (make-f64-array (* pixel-count 4)))
         (decoder (ip::plan-decoder plan)))
    (funcall decoder pixels 0 buffer 0 pixel-count)
    buffer))

(defun encode-image (plan buffer)
  (let* ((width (ip::plan-width plan))
         (height (ip::plan-height plan))
         (pixel-count (* width height))
         (pixels (make-u32-array pixel-count))
         (encoder (ip::plan-encoder plan)))
    (funcall encoder buffer 0 pixels 0 pixel-count)
    pixels))

(defun apply-op-rect (buffer image-width x1 y1 x2 y2 op)
  (let ((rect-width (- x2 x1))
        (rect-height (- y2 y1)))
    (dotimes (row-offset rect-height)
      (let ((base-pixel-index (+ (* (+ y1 row-offset) image-width) x1)))
        (dotimes (column-offset rect-width)
          (let ((lane-index (* (+ base-pixel-index column-offset) 4)))
            (funcall op buffer lane-index)))))
    buffer))

(defun apply-op-sequential (plan pixels op &key (x1 0) (y1 0) x2 y2)
  (let* ((width (ip::plan-width plan))
         (height (ip::plan-height plan))
         (x2 (or x2 width))
         (y2 (or y2 height))
         (buffer (decode-image plan pixels)))
    (loop :for y :from y1 :below y2
          :do (loop :for x :from x1 :below x2
                    :for pixel-index := (+ x (* y width))
                    :for lane-index := (* pixel-index 4)
                    :do (funcall op buffer lane-index)))
    (encode-image plan buffer)))

(defun apply-pipeline-sequential (plan pixels pre work post &key (x1 0) (y1 0) x2 y2)
  (let* ((width (ip::plan-width plan))
         (height (ip::plan-height plan))
         (x2 (or x2 width))
         (y2 (or y2 height))
         (buffer (decode-image plan pixels)))
    (loop :for y :from y1 :below y2
          :do (loop :for x :from x1 :below x2
                    :for pixel-index := (+ x (* y width))
                    :for lane-index := (* pixel-index 4)
                    :do (funcall pre buffer lane-index)
                        (funcall work buffer lane-index)
                        (funcall post buffer lane-index)))
    (encode-image plan buffer)))

(defun op-swap-bump (buffer lane-index)
  (multiple-value-bind (red green blue alpha) (pb:get-channels buffer lane-index)
    (let ((new-green (min 1d0 (+ green (/ 1d0 255d0))))
          (new-alpha (max 0d0 (- alpha (/ 2d0 255d0)))))
      (pb:set-channels buffer lane-index blue new-green red new-alpha))))

(defun op-pre (buffer lane-index)
  (multiple-value-bind (red green blue alpha) (pb:get-channels buffer lane-index)
    (pb:set-channels buffer lane-index (- 1d0 red) green blue alpha)))

(defun op-work (buffer lane-index)
  (multiple-value-bind (red green blue alpha) (pb:get-channels buffer lane-index)
    (pb:set-channels buffer lane-index red blue green alpha)))

(defun op-post (buffer lane-index)
  (multiple-value-bind (red green blue alpha) (pb:get-channels buffer lane-index)
    (pb:set-channels buffer lane-index red (min 1d0 (+ green (/ 1d0 255d0))) blue alpha)))

(defun op-invert-rgb ()
  (lambda (buffer lane-index)
    (multiple-value-bind (r g b) (pb:get-primaries buffer lane-index)
      (pb:set-primaries buffer lane-index (- 1d0 r) (- 1d0 g) (- 1d0 b)))))

(defun op-identity ()
  (lambda (buffer lane-index)
    (declare (ignore buffer lane-index))
    (values)))

(defun op-oob-clamp ()
  (lambda (buffer lane-index)
    (setf (pb:channel0 buffer lane-index) 2d0)
    (setf (pb:channel1 buffer lane-index) -1d0)
    (setf (pb:channel2 buffer lane-index) 0.5d0)
    (setf (pb:channel3 buffer lane-index) 2d0)
    (values)))

(defun pre-add-red (amount)
  (lambda (buffer lane-index)
    (setf (pb:channel0 buffer lane-index) (+ (pb:channel0 buffer lane-index) amount))
    (values)))

(defun work-mul-green (amount)
  (lambda (buffer lane-index)
    (setf (pb:channel1 buffer lane-index) (* (pb:channel1 buffer lane-index) amount))
    (values)))

(defun post-set-alpha (amount)
  (lambda (buffer lane-index)
    (setf (pb:channel3 buffer lane-index) amount)
    (values)))

(test image-pipeline.make-plan
  (let* ((width 64)
         (height 128)
         (threads 16)
         (plan (ip:make-plan :pixel-format 'pf:rgba8888/u32
                             :width width
                             :height height
                             :threads threads)))
    (is (= (ip::plan-width plan) width))
    (is (= (ip::plan-height plan) height))
    (is (= (ip::plan-threads plan) threads))
    (is (functionp (ip::plan-encoder plan)))
    (is (functionp (ip::plan-decoder plan)))
    (is (= (length (ip::plan-workspace plan)) (* width 4 (max threads height))))))

(test image-pipeline.map-image.u32.full
  (let* ((width 64)
         (height 128)
         (threads 16)
         (plan (ip:make-plan :pixel-format 'pf:rgba8888/u32
                             :width width
                             :height height
                             :threads threads))
         (input-buffer (make-canonical-buffer width height))
         (input-pixels (encode-u32-rgba input-buffer width height))
         (expected-buffer (copy-seq input-buffer))
         (expected-pixels nil)
         (actual-pixels (copy-seq input-pixels)))
    (apply-op-rect expected-buffer width 0 0 width height #'op-swap-bump)
    (setf expected-pixels (encode-u32-rgba expected-buffer width height))
    (ip:map-image plan actual-pixels #'op-swap-bump)
    (is (equalp actual-pixels expected-pixels))))

(test image-pipeline.map-image.u32.subrect
  (let* ((width 64)
         (height 128)
         (threads 16)
         (plan (ip:make-plan :pixel-format 'pf:rgba8888/u32
                             :width width
                             :height height
                             :threads threads))
         (x1 7)
         (y1 9)
         (x2 27)
         (y2 33)
         (input-buffer (make-canonical-buffer width height))
         (input-pixels (encode-u32-rgba input-buffer width height))
         (expected-buffer (copy-seq input-buffer))
         (expected-pixels nil)
         (actual-pixels (copy-seq input-pixels)))
    (apply-op-rect expected-buffer width x1 y1 x2 y2 #'op-swap-bump)
    (setf expected-pixels (encode-u32-rgba expected-buffer width height))
    (ip:map-image plan actual-pixels #'op-swap-bump :x1 x1 :y1 y1 :x2 x2 :y2 y2)
    (is (equalp actual-pixels expected-pixels))))

(test image-pipeline.map-image.u8.full
  (let* ((width 64)
         (height 128)
         (threads 16)
         (plan (ip:make-plan :pixel-format 'pf:rgba8888/u8
                             :width width
                             :height height
                             :threads threads))
         (input-buffer (make-canonical-buffer width height))
         (input-pixels (encode-u8-rgba input-buffer width height))
         (expected-buffer (copy-seq input-buffer))
         (expected-pixels nil)
         (actual-pixels (copy-seq input-pixels)))
    (apply-op-rect expected-buffer width 0 0 width height #'op-swap-bump)
    (setf expected-pixels (encode-u8-rgba expected-buffer width height))
    (ip:map-image plan actual-pixels #'op-swap-bump)
    (is (equalp actual-pixels expected-pixels))))

(test image-pipeline.map-image-pipeline.u32.fused-vs-unfused
  (let* ((width 64)
         (height 128)
         (threads 16)
         (plan-fused (ip:make-plan :pixel-format 'pf:rgba8888/u32
                                   :width width
                                   :height height
                                   :threads threads))
         (plan-unfused (ip:make-plan :pixel-format 'pf:rgba8888/u32
                                     :width width
                                     :height height
                                     :threads threads))
         (input-buffer (make-canonical-buffer width height))
         (input-pixels (encode-u32-rgba input-buffer width height))
         (pixels-fused (copy-seq input-pixels))
         (pixels-unfused (copy-seq input-pixels))
         (expected-buffer (copy-seq input-buffer))
         (expected-pixels nil))
    (apply-op-rect expected-buffer width 0 0 width height #'op-pre)
    (apply-op-rect expected-buffer width 0 0 width height #'op-work)
    (apply-op-rect expected-buffer width 0 0 width height #'op-post)
    (setf expected-pixels (encode-u32-rgba expected-buffer width height))
    (ip:map-image-pipeline plan-fused
                           pixels-fused
                           :pre #'op-pre
                           :work #'op-work
                           :post #'op-post
                           :fused t)
    (ip:map-image-pipeline plan-unfused
                           pixels-unfused
                           :pre #'op-pre
                           :work #'op-work
                           :post #'op-post
                           :fused nil)
    (is (equalp pixels-fused pixels-unfused))
    (is (equalp pixels-fused expected-pixels))))

(test image-pipeline.map-image-pipeline.u8.fused-vs-unfused
  (let* ((width 64)
         (height 128)
         (threads 16)
         (plan-fused (ip:make-plan :pixel-format 'pf:rgba8888/u8
                                   :width width
                                   :height height
                                   :threads threads))
         (plan-unfused (ip:make-plan :pixel-format 'pf:rgba8888/u8
                                     :width width
                                     :height height
                                     :threads threads))
         (input-buffer (make-canonical-buffer width height))
         (input-pixels (encode-u8-rgba input-buffer width height))
         (pixels-fused (copy-seq input-pixels))
         (pixels-unfused (copy-seq input-pixels))
         (expected-buffer (copy-seq input-buffer))
         (expected-pixels nil))
    (apply-op-rect expected-buffer width 0 0 width height #'op-pre)
    (apply-op-rect expected-buffer width 0 0 width height #'op-work)
    (apply-op-rect expected-buffer width 0 0 width height #'op-post)
    (setf expected-pixels (encode-u8-rgba expected-buffer width height))
    (ip:map-image-pipeline plan-fused
                           pixels-fused
                           :pre #'op-pre
                           :work #'op-work
                           :post #'op-post
                           :fused t)
    (ip:map-image-pipeline plan-unfused
                           pixels-unfused
                           :pre #'op-pre
                           :work #'op-work
                           :post #'op-post
                           :fused nil)
    (is (equalp pixels-fused pixels-unfused))
    (is (equalp pixels-fused expected-pixels))))

(test image-pipeline.make-plan.properties
  (let* ((width 64)
         (height 128)
         (threads 8)
         (plan (ip:make-plan :width width
                             :height height
                             :threads threads
                             :pixel-format 'pf:rgba8888/u32)))
    (let ((workspace (ip::plan-workspace plan))
          (expected-length (* width 4 (max threads height))))
      (is (= (ip::plan-width plan) width))
      (is (= (ip::plan-height plan) height))
      (is (= (ip::plan-threads plan) threads))
      (is (= (length workspace) expected-length))
      (is (eq (ip::plan-encoder plan) (pf:encoder 'pf:rgba8888/u32)))
      (is (eq (ip::plan-decoder plan) (pf:decoder 'pf:rgba8888/u32))))))

(test image-pipeline.map-image.identity
  (let* ((plan (ip:make-plan :width 96 :height 96 :threads 8 :pixel-format 'pf:rgba8888/u32))
         (pixels (make-pattern-image plan))
         (expected (copy-seq pixels)))
    (ip:map-image plan pixels (op-identity))
    (is (equalp pixels expected))))

(test image-pipeline.map-image.rect-boundary
  (let* ((plan (ip:make-plan :width 64 :height 96 :threads 8 :pixel-format 'pf:rgba8888/u32))
         (pixels (make-pattern-image plan))
         (pixels-copy (copy-seq pixels))
         (op (op-invert-rgb))
         (width (ip::plan-width plan))
         (height (ip::plan-height plan)))
    (let ((expected-right (apply-op-sequential plan
                                               pixels-copy
                                               op
                                               :x1 (1- width)
                                               :y1 0
                                               :x2 width
                                               :y2 height)))
      (ip:map-image plan pixels op :x1 (1- width) :y1 0 :x2 width :y2 height)
      (is (equalp pixels expected-right)))))

(test image-pipeline.map-image.thin-strips
  (let* ((plan (ip:make-plan :width 80 :height 80 :threads 8 :pixel-format 'pf:rgba8888/u32))
         (op (op-invert-rgb))
         (width (ip::plan-width plan))
         (height (ip::plan-height plan))
         (pixels1 (make-pattern-image plan))
         (expected1 (apply-op-sequential plan
                                         (copy-seq pixels1)
                                         op
                                         :x1 0
                                         :y1 (1- height)
                                         :x2 width
                                         :y2 height)))
    (ip:map-image plan pixels1 op :x1 0 :y1 (1- height) :x2 width :y2 height)
    (is (equalp pixels1 expected1))))

(test image-pipeline.map-image.bad-rects
  (let* ((plan (ip:make-plan :width 32 :height 32 :threads 8 :pixel-format 'pf:rgba8888/u32))
         (pixels (make-pattern-image plan))
         (op (op-identity)))
    (signals error (ip:map-image plan pixels op :x1 0 :y1 0 :x2 0 :y2 1))
    (signals error (ip:map-image plan pixels op :x1 10 :y1 0 :x2 9 :y2 1))
    (signals error (ip:map-image plan pixels op :x1 0 :y1 0 :x2 33 :y2 1))
    (signals error (ip:map-image plan pixels op :x1 0 :y1 31 :x2 32 :y2 31))))

(test image-pipeline.map-image.clamp-via-pixel-format
  (let* ((plan (ip:make-plan :width 64 :height 64 :threads 8 :pixel-format 'pf:rgba8888/u32))
         (pixels (make-pattern-image plan))
         (op (op-oob-clamp))
         (expected (apply-op-sequential plan (copy-seq pixels) op)))
    (ip:map-image plan pixels op)
    (is (equalp pixels expected))))

(test image-pipeline.map-image-pipeline.fused-vs-unfused
  (let* ((plan (ip:make-plan :width 96 :height 128 :threads 8 :pixel-format 'pf:rgba8888/u32))
         (pixels-fused (make-pattern-image plan))
         (pixels-unfused (copy-seq pixels-fused))
         (pixels-map-image (copy-seq pixels-fused))
         (pre (pre-add-red 0.2d0))
         (work (work-mul-green 0.75d0))
         (post (post-set-alpha 2d0))
         (op (lambda (buffer lane-index)
               (funcall pre buffer lane-index)
               (funcall work buffer lane-index)
               (funcall post buffer lane-index)
               (values)))
         (expected (apply-pipeline-sequential plan (copy-seq pixels-fused) pre work post)))
    (ip:map-image-pipeline plan pixels-fused :pre pre :work work :post post :fused t)
    (ip:map-image-pipeline plan pixels-unfused :pre pre :work work :post post :fused nil)
    (ip:map-image plan pixels-map-image op)
    (is (equalp pixels-fused expected))
    (is (equalp pixels-unfused expected))
    (is (equalp pixels-map-image expected))))

(test image-pipeline.map-image-pipeline.height-smaller-than-threads
  (let* ((plan (ip:make-plan :width 64 :height 4 :threads 16 :pixel-format 'pf:rgba8888/u32))
         (pixels (make-pattern-image plan))
         (pre (pre-add-red 0.1d0))
         (work (work-mul-green 1.1d0))
         (post (post-set-alpha 1.5d0))
         (expected (apply-pipeline-sequential plan (copy-seq pixels) pre work post)))
    (ip:map-image-pipeline plan pixels :pre pre :work work :post post :fused t)
    (is (equalp pixels expected))))
