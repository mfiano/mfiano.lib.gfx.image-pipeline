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
    (is (= (length (ip::plan-workspace plan)) (* (span-length) 4 threads)))))

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
                                     :threads threads
                                     :fused nil))
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
                           :post #'op-post)
    (ip:map-image-pipeline plan-unfused
                           pixels-unfused
                           :pre #'op-pre
                           :work (per-pixel-work #'op-work)
                           :post #'op-post)
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
                                     :threads threads
                                     :fused nil))
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
                           :post #'op-post)
    (ip:map-image-pipeline plan-unfused
                           pixels-unfused
                           :pre #'op-pre
                           :work (per-pixel-work #'op-work)
                           :post #'op-post)
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
          (expected-length (* (span-length) 4 threads)))
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

(test image-pipeline.map-image.without-op
  (let* ((plan (ip:make-plan :width 96 :height 96 :threads 8 :pixel-format 'pf:rgba8888/u32))
         (pixels (make-pattern-image plan))
         (expected (copy-seq pixels)))
    (ip:map-image plan pixels nil)
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
         (plan-unfused (ip:make-plan :width 96 :height 128 :threads 8 :fused nil))
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
    (ip:map-image-pipeline plan pixels-fused :pre pre :work work :post post)
    (ip:map-image-pipeline plan-unfused
                           pixels-unfused
                           :pre pre
                           :work (per-pixel-work work)
                           :post post)
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
    (ip:map-image-pipeline plan pixels :pre pre :work work :post post)
    (is (equalp pixels expected))))

(test image-pipeline.map-image-pipeline.subrect
  (let* ((plan (ip:make-plan :width 96 :height 128 :threads 8 :pixel-format 'pf:rgba8888/u32))
         (plan-unfused (ip:make-plan :width 96 :height 128 :threads 8 :fused nil))
         (pixels-fused (make-pattern-image plan))
         (pixels-unfused (copy-seq pixels-fused))
         (pre (pre-add-red 0.2d0))
         (work (work-mul-green 0.75d0))
         (post (post-set-alpha 0.5d0))
         (rect (list :x1 5 :y1 11 :x2 61 :y2 90))
         (args (list* :pre pre :work work :post post rect))
         (expected (apply #'apply-pipeline-sequential plan pixels-fused pre work post rect)))
    (apply #'ip:map-image-pipeline plan pixels-fused args)
    (apply #'ip:map-image-pipeline plan-unfused pixels-unfused :work (per-pixel-work work) args)
    (is (equalp pixels-fused expected))
    (is (equalp pixels-unfused expected))))

(test image-pipeline.make-plan.fill
  (let ((plan (ip:make-plan :width 16
                            :height 8
                            :threads 4
                            :pixel-format 'pf:rgba8888/u32
                            :filler #'fill-position)))
    (is (functionp (ip::plan-decoder plan)))
    (is (not (eq (ip::plan-decoder plan) (pf:decoder 'pf:rgba8888/u32))))
    (is (eq (ip::plan-encoder plan) (pf:encoder 'pf:rgba8888/u32)))
    (signals error (ip:make-plan :width 16 :height 8 :filler 42))))

(test image-pipeline.map-image.fill.full
  (let* ((plan (ip:make-plan :width 64
                             :height 128
                             :threads 16
                             :pixel-format 'pf:rgba8888/u32
                             :filler #'fill-position))
         (pixels (make-pattern-image plan))
         (expected (apply-fill-sequential 64 128 pixels (list #'op-swap-bump))))
    (ip:map-image plan pixels #'op-swap-bump)
    (is (equalp pixels expected))))

(test image-pipeline.map-image.fill.subrect
  (let* ((plan (ip:make-plan :width 64
                             :height 128
                             :threads 16
                             :pixel-format 'pf:rgba8888/u32
                             :filler #'fill-position))
         (pixels (make-pattern-image plan))
         (ops (list #'op-swap-bump))
         (expected (apply-fill-sequential 64 128 pixels ops :x1 7 :y1 9 :x2 27 :y2 33)))
    (ip:map-image plan pixels #'op-swap-bump :x1 7 :y1 9 :x2 27 :y2 33)
    (is (equalp pixels expected))))

(test image-pipeline.map-image.fill.without-op
  (let* ((plan (ip:make-plan :width 64
                             :height 128
                             :threads 16
                             :pixel-format 'pf:rgba8888/u32
                             :filler #'fill-position))
         (pixels (make-pattern-image plan))
         (expected (apply-fill-sequential 64 128 pixels nil :x1 7 :y1 9 :x2 27 :y2 33)))
    (ip:map-image plan pixels nil :x1 7 :y1 9 :x2 27 :y2 33)
    (is (equalp pixels expected))))

(test image-pipeline.map-image-pipeline.fill.fused-vs-unfused
  (let* ((plan (ip:make-plan :width 96
                             :height 128
                             :threads 8
                             :pixel-format 'pf:rgba8888/u32
                             :filler #'fill-position))
         (plan-unfused (ip:make-plan :width 96
                                     :height 128
                                     :threads 8
                                     :filler #'fill-position
                                     :fused nil))
         (pixels-fused (make-pattern-image plan))
         (pixels-unfused (copy-seq pixels-fused))
         (pre (pre-add-red 0.2d0))
         (work (work-mul-green 0.75d0))
         (post (post-set-alpha 0.5d0))
         (expected (apply-fill-sequential 96 128 pixels-fused (list pre work post))))
    (ip:map-image-pipeline plan pixels-fused :pre pre :work work :post post)
    (ip:map-image-pipeline plan-unfused
                           pixels-unfused
                           :pre pre
                           :work (per-pixel-work work)
                           :post post)
    (is (equalp pixels-fused expected))
    (is (equalp pixels-unfused expected))))

(test image-pipeline.map-image-pipeline.fill.subrect
  (let* ((plan (ip:make-plan :width 96
                             :height 128
                             :threads 8
                             :pixel-format 'pf:rgba8888/u32
                             :filler #'fill-position))
         (plan-unfused (ip:make-plan :width 96
                                     :height 128
                                     :threads 8
                                     :filler #'fill-position
                                     :fused nil))
         (pixels-fused (make-pattern-image plan))
         (pixels-unfused (copy-seq pixels-fused))
         (pre (pre-add-red 0.2d0))
         (work (work-mul-green 0.75d0))
         (post (post-set-alpha 0.5d0))
         (ops (list pre work post))
         (args (list :pre pre :work work :post post :x1 5 :y1 11 :x2 61 :y2 90))
         (expected (apply-fill-sequential 96 128 pixels-fused ops :x1 5 :y1 11 :x2 61 :y2 90)))
    (apply #'ip:map-image-pipeline plan pixels-fused args)
    (apply #'ip:map-image-pipeline plan-unfused pixels-unfused :work (per-pixel-work work) args)
    (is (equalp pixels-fused expected))
    (is (equalp pixels-unfused expected))))

(test image-pipeline.map-image.bad-pixels
  (let ((plan (ip:make-plan :width 8 :height 8 :threads 4 :pixel-format 'pf:rgba8888/u32)))
    (signals error (ip:map-image plan (make-array 64) nil))))

(test image-pipeline.map-image.wrong-storage
  (let ((plan (ip:make-plan :width 8 :height 8 :threads 4 :pixel-format 'pf:rgba8888/u32)))
    (signals error (ip:map-image plan (make-u8-array (* 8 8 4)) nil))))

(test image-pipeline.map-image.short-pixels
  (let ((plan (ip:make-plan :width 8 :height 8 :threads 4 :pixel-format 'pf:rgba8888/u8)))
    (signals error (ip:map-image plan (make-u8-array (* 8 8)) nil))))

(test image-pipeline.map-image-pipeline.bad-pixels
  (let ((plan (ip:make-plan :width 8 :height 8 :threads 4 :pixel-format 'pf:rgba8888/u32))
        (op (op-identity)))
    (signals error (ip:map-image-pipeline plan (make-array 64) :pre op :work op :post op))))

(test image-pipeline.map-image.f32-storage
  (let ((plan (ip:make-plan :width 8 :height 8 :threads 4 :pixel-format 'pf:rgba/f32))
        (pixels (make-f32-array (* 8 8 4))))
    (dotimes (i (length pixels))
      (setf (aref pixels i) (/ (mod i 7) 7f0)))
    (let ((expected (copy-seq pixels)))
      (ip:map-image plan pixels nil)
      (is (equalp pixels expected)))))

(test image-pipeline.make-plan.unfused-workspace
  (let ((plan (ip:make-plan :width 16 :height 64 :threads 4 :fused nil)))
    (is (= (length (ip::plan-workspace plan)) (* (max (* 16 64) (* 4 (span-length))) 4)))))

(test image-pipeline.cancel-plan.ends-threads
  (let* ((before (plan-thread-count))
         (plan (ip:make-plan :width 8 :height 8 :threads 4)))
    (is (= (plan-thread-count) (+ before 4)))
    (ip:cancel-plan plan)
    (is (= (plan-thread-count) before))
    (is (zerop (length (ip::plan-workspace plan))))))

(test image-pipeline.cancel-plan.twice
  (let ((plan (ip:make-plan :width 8 :height 8 :threads 2)))
    (ip:cancel-plan plan)
    (ip:cancel-plan plan)
    (is (ip::plan-canceled plan))))

(test image-pipeline.cancel-plan.refuses-mappers
  (let* ((plan (ip:make-plan :width 8 :height 8 :threads 2))
         (pixels (make-u32-array 64))
         (op (op-identity))
         (map-image (lambda () (ip:map-image plan pixels op)))
         (pipeline (lambda () (ip:map-image-pipeline plan pixels :pre op :work op :post op))))
    (ip:cancel-plan plan)
    (is (equal (error-message map-image) "plan was canceled"))
    (is (equal (error-message pipeline) "plan was canceled"))))

(test image-pipeline.with-plan.cancels-on-exit
  (let ((escaped nil))
    (block exit
      (ip:with-plan (plan :width 8 :height 8 :threads 2)
        (setf escaped plan)
        (return-from exit)))
    (is (ip::plan-canceled escaped))))

(test image-pipeline.spans.row-longer-than-a-span
  (destructuring-bind (expected &rest results) (pipeline-results (+ (* 2 (span-length)) 5) 2)
    (dolist (result results)
      (is (equalp result expected)))))

(test image-pipeline.spans.cross-rows
  (destructuring-bind (expected &rest results) (pipeline-results 3 (+ (span-length) 2))
    (dolist (result results)
      (is (equalp result expected)))))

(test image-pipeline.spans.narrow-subrect
  (let ((height (span-length)))
    (destructuring-bind (expected &rest results)
        (pipeline-results 7 height :x1 2 :y1 1 :x2 5 :y2 (1- height))
      (dolist (result results)
        (is (equalp result expected))))))

(test image-pipeline.spans.fill.row-longer-than-a-span
  (destructuring-bind (expected &rest results)
      (pipeline-results (+ (* 2 (span-length)) 5) 2 :filler #'fill-position)
    (dolist (result results)
      (is (equalp result expected)))))

(test image-pipeline.spans.fill.cross-rows
  (destructuring-bind (expected &rest results)
      (pipeline-results 3 (+ (span-length) 2) :filler #'fill-position)
    (dolist (result results)
      (is (equalp result expected)))))

(test image-pipeline.spans.fill.narrow-subrect
  (let ((height (span-length)))
    (destructuring-bind (expected &rest results)
        (pipeline-results 7 height :filler #'fill-position :x1 2 :y1 1 :x2 5 :y2 (1- height))
      (dolist (result results)
        (is (equalp result expected))))))

(test image-pipeline.neighbors.clamp
  (multiple-value-bind (pixels expected) (blur-results 37 23 :clamp 1)
    (is (equalp pixels expected))))

(test image-pipeline.neighbors.wrap
  (multiple-value-bind (pixels expected) (blur-results 37 23 :wrap 1)
    (is (equalp pixels expected))))

(test image-pipeline.neighbors.constant
  (multiple-value-bind (pixels expected) (blur-results 37 23 '(1 0 1/2 1) 1)
    (is (equalp pixels expected))))

(test image-pipeline.neighbors.passes
  (dolist (edge (list :clamp :wrap '(0 1 0 1)))
    (multiple-value-bind (pixels expected) (blur-results 37 23 edge 3)
      (is (equalp pixels expected)))))

(test image-pipeline.neighbors.subrect
  (dolist (rect '((5 4 30 19) (0 3 10 12) (20 0 37 9) (0 0 37 23)))
    (dolist (edge '(:clamp :wrap))
      (destructuring-bind (x1 y1 x2 y2) rect
        (multiple-value-bind (pixels expected)
            (blur-results 37 23 edge 2 :x1 x1 :y1 y1 :x2 x2 :y2 y2)
          (is (equalp pixels expected)))))))

(test image-pipeline.neighbors.spans-cross-rows
  (dolist (shape (list (list 3 (+ (span-length) 2)) (list (+ (* 2 (span-length)) 5) 2)))
    (multiple-value-bind (pixels expected) (apply #'blur-results (append shape '(:clamp 2)))
      (is (equalp pixels expected)))))

(test image-pipeline.neighbors.position
  (ip:with-plan (plan :width 40 :height 30 :threads 4 :fused nil)
    (let* ((pixels (make-pattern-image plan))
           (expected (apply-fill-sequential 40 30 pixels nil :x1 3 :y1 2 :x2 33 :y2 25)))
      (ip:map-image-pipeline plan
                             pixels
                             :work (lambda (source destination lane stride x y)
                                     (declare (ignore source stride))
                                     (set-position destination lane x y))
                             :x1 3
                             :y1 2
                             :x2 33
                             :y2 25)
      (is (equalp pixels expected)))))

(test image-pipeline.neighbors.filler
  (ip:with-plan (plan :width 37 :height 23 :threads 4 :fused nil :filler #'fill-position)
    (let* ((pixels (make-pattern-image plan))
           (filled (decode-u32-rgba (apply-fill-sequential 37 23 pixels nil) 37 23))
           (expected (encode-image plan (box-blur-buffer filled 37 23 :wrap 2))))
      (ip:map-image-pipeline plan pixels :work #'box-blur :passes 2 :edge :wrap)
      (is (equalp pixels expected)))))

(test image-pipeline.neighbors.reach
  (ip:with-plan (plan :width 16 :height 8 :threads 2 :fused nil :reach 3)
    (is (= (length (ip::plan-back plan)) (* 22 14 4))))
  (signals error (ip:make-plan :width 16 :height 8 :fused nil :reach -1))
  (signals error (ip:make-plan :width 16 :height 8 :fused nil :reach 256)))

(test image-pipeline.neighbors.bad-arguments
  (ip:with-plan (plan :width 16 :height 8 :threads 2)
    (let ((pixels (make-pattern-image plan)))
      (signals error (ip:map-image-pipeline plan pixels :work #'op-work :passes 2))))
  (ip:with-plan (plan :width 16 :height 8 :threads 2 :fused nil)
    (let ((pixels (make-pattern-image plan)))
      (signals error (ip:map-image-pipeline plan pixels :work #'box-blur :edge :mirror))
      (signals error (ip:map-image-pipeline plan pixels :work #'box-blur :edge '(1 0 0)))
      (signals error (ip:map-image-pipeline plan pixels :work #'box-blur :passes 0)))))

(test image-pipeline.neighbors.errors-release-the-other-workers
  (let ((debug-tasks lp:*debug-tasks-p*))
    (unwind-protect
         (ip:with-plan (plan :width 37 :height 23 :threads 4 :fused nil)
           (setf lp:*debug-tasks-p* nil)
           (let ((pixels (make-pattern-image plan))
                 (visits 0))
             (signals error
               (ip:map-image-pipeline plan
                                      pixels
                                      :work (lambda (source destination lane stride x y)
                                              (when (and (= x 5) (= y 5) (= (incf visits) 2))
                                                (error "failing on the second pass"))
                                              (box-blur source destination lane stride x y))
                                      :passes 3))
             (multiple-value-bind (pixels expected) (blur-results 37 23 :clamp 3)
               (is (equalp pixels expected)))))
      (setf lp:*debug-tasks-p* debug-tasks))))
