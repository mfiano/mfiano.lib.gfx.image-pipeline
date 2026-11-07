(in-package #:mfiano.lib.gfx.image-pipeline/test)

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
