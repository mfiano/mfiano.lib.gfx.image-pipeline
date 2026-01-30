# mfiano.lib.gfx.image-pipeline : A small, fast pixel mapping layer for CPU-side image work

## About this project

You give it:

* an image buffer (packed pixels in some `mfiano.lib.gfx.pixel-format`),
* a `plan` (dimensions, pixel-format codec, threads, workspace),
* and an operation that mutates pixels via a `mfiano.lib.gfx.pixel-buffer` view (4 lanes of `double-float`s per pixel).

It handles:

* decoding packed pixels -> per-pixel `double-float` lanes,
* running your function on each pixel (optionally in parallel by rows),
* encoding lanes back to packed pixels,
* optionally doing a 3-phase "pipeline" (pre/work/post), fused or unfused.

## What it operates on

### Pixel storage

`image-pipeline` works on images stored as either:

* `(simple-array (unsigned-byte 8) (*))` (for formats like `mfiano.lib.gfx.pixel-format:rgba8888/u8`, `mfiano.lib.gfx.pixel-format:rgb888/u8`, etc)
* `(simple-array (unsigned-byte 32) (*))` (for formats like `mfiano.lib.gfx.pixel-format:rgba8888/u32`, `mfiano.lib.gfx.pixel-format:abgr8888/u32`, etc)

Which one you use depends on the pixel format you built the plan with.

### Workspace representation

Inside the pipeline, pixels are represented as a `(simple-array double-float (*))` with 4 lanes per pixel. That's exactly what `mfiano.lib.gfx.pixel-buffer` is: a "pixel workspace" you mutate with `channel0..3`, `get-channels`, `set-channels`, etc.

## Public API

### `make-plan`

```lisp
(mfiano.lib.gfx.image-pipeline:make-plan &key pixel-format width height threads) => plan
```

* `pixel-format` defaults to `mfiano.lib.gfx.pixel-format:rgba8888/u32`
* `width`/`height` are required
* `threads` controls row-parallelism via `lparallel`

### `map-image`

```lisp
(mfiano.lib.gfx.image-pipeline:map-image plan pixels op &key x1 y1 x2 y2) => (values)
```

* `op` is called as `(funcall op workspace lane-index)` for each pixel
* Rect is half-open: `[x1,x2) x [y1,y2)`
* Mutates `pixels` in place
* Signals an error condition for invalid rects

### `map-image-pipeline`

```lisp
(mfiano.lib.gfx.image-pipeline:map-image-pipeline plan pixels &key pre work post fused x1 y1 x2 y2) => (values)
```

* `pre`, `work`, `post` are each called like `op` above
* If `fused` is true, it does decode -> pre/work/post -> encode in one pass
* If `fused` is nil, it does three passes over the workspace: pre, then work, then post; slower but required for spatially dependent image work (convolution, etc.)

## Quick start

### 1) Make a plan

```lisp
(defparameter *plan*
  (mfiano.lib.gfx.image-pipeline:make-plan
   :pixel-format 'mfiano.lib.gfx.pixel-format:rgba8888/u32
   :width 2048
   :height 1024
   :threads 8))
```

### 2) Run an operation over the whole image

Example: invert RGB, preserve alpha.

```lisp
(defun invert-rgb-op ()
  (lambda (buffer lane-index)
    (multiple-value-bind (r g b a) (mfiano.lib.gfx.pixel-buffer:get-channels buffer lane-index)
      (mfiano.lib.gfx.pixel-buffer:set-channels buffer lane-index (- 1d0 r) (- 1d0 g) (- 1d0 b) a))))

(mfiano.lib.gfx.image-pipeline:map-image *plan* pixels (invert-rgb-op))
```

### 3) Run over a sub-rect

Example: only process the top-left 256x256.

```lisp
(mfiano.lib.gfx.image-pipeline:map-image *plan* pixels (invert-rgb-op) :x1 0 :y1 0 :x2 256 :y2 256)
```

## 3-phase pipeline example

A common pattern is: normalize -> do work -> clamp/finalize. That's what `map-image-pipeline` is for.

Example:

* `pre`: add a little red
* `work`: scale green
* `post`: force alpha (note: alpha is clamped by `mfiano.lib.gfx.pixel-buffer`'s `(setf channel3)`)

```lisp
(defun pre-add-red (amount)
  (lambda (buffer lane-index)
    (setf (mfiano.lib.gfx.pixel-buffer:channel0 buffer lane-index) (+ (mfiano.lib.gfx.pixel-buffer:channel0 buffer lane-index) amount))
    (values)))

(defun work-mul-green (amount)
  (lambda (buffer lane-index)
    (setf (mfiano.lib.gfx.pixel-buffer:channel1 buffer lane-index) (* (mfiano.lib.gfx.pixel-buffer:channel1 buffer lane-index) amount))
    (values)))

(defun post-set-alpha (amount)
  (lambda (buffer lane-index)
    (setf (mfiano.lib.gfx.pixel-buffer:channel3 buffer lane-index) amount)
    (values)))

(mfiano.lib.gfx.image-pipeline:map-image-pipeline
  *plan*
  pixels
  :pre (pre-add-red 0.2d0)
  :work (work-mul-green 0.75d0)
  :post (post-set-alpha 2d0)
  :fused t)
```

If you want the 'classic pipeline semantics' (three passes), set `:fused nil`.

## Advanced example

Another common workflow is to perform a color-grading operation that involves working in a specific color space. Naively, one would convert the image's pixels into a working pixel space (which may require intermediate color space conversions and chromatic adaptations), perform a modification over all the pixels, then do the inverse conversion back to the display color space (which, again, may require several color space conversions). We can simplify this process using `image-pipeline` along with `mfiano.lib.gfx.color`:

```lisp
;; pre operation: convert pixels from sRGB to Oklch:
;; sRGB (companded) -> linear light sRGB (uncompanded) -> Oklab -> Oklch
(declaim (inline pre-op))
(defun pre-op (buffer index)
  (declare (optimize (speed 3)))
  (mfiano.lib.gfx.color:srgb->oklch buffer index)
  (values))

;; Work operation in our intermediate Oklch color space: alter the hue of each pixel while perceptually retaining chroma and lightness properties.
(declaim (inline work-op))
(defun work-op (buffer index)
  (declare (optimize (speed 3)))
  (mfiano.lib.gfx.color:shift-hue :oklch buffer index 30d0)
  (values))

;; post operation: convert pixels from Oklch back to sRGB:
;; Oklch -> Oklab -> linear light sRGB (uncompanded) -> sRGB (companded)
(declaim (inline post-op))
(defun post-op (buffer index)
  (declare (optimize (speed 3)))
  (mfiano.lib.gfx.color:oklch->srgb buffer index)
  (values))

;; Set up and run a pipeline which:
;; - prepares a pixel array by decoding a 3250x3250 RGBA image (10.5M pixels, 42.25MiB of packed channels)
;; - creates a plan with an image-sized workspace and 24 threads
;; - run a fused pipeline over each of the ~10.5M pixels that runs pre, work, and post ops in sequence
;; - times the results
;; - writes the new image out to disk
(defun test ()
  (let* ((pixel-format 'mfiano.lib.gfx.pixel-format:rgba8888/u32)
         (pixel-storage (mfiano.lib.gfx.pixel-format:get-storage pixel-format))
         (image-path "~/test.qoi")
         ;; use `mfiano.lib.codec.qoi` to decode a QOI image to a (unsigned-byte 32) array (our pixel format's packed RGBA888 32bit integer representation)
         (image (qoi:decode-file image-path :output pixel-storage))
         (w (qoi:width image))
         (h (qoi:height image))
         (channels (qoi:channels image))
         (pixels (qoi:pixels image))
         ;; create a plan with a workspace the size of the image, using 24 threads (change for your hardware)
         (plan (ip:make-plan :pixel-format pixel-format :width w :height h :threads 24)))
    ;; (SBCL): force a GC before timing to reduce noise
    (sb-ext:gc :full t)
    ;; benchmark the resource usage
    (time
     (ip:map-image-pipeline
      plan
      pixels
      :pre #'pre-op
      :work #'work-op
      :post #'post-op
      :fused t))
    ;; write the modified image out to disk
    (qoi:encode-file pixels "~/out.qoi" :width w :height h :channels channels)
    (values)))

(test)
Evaluation took:
  0.116 seconds of real time
  2.327821 seconds of total run time (2.297376 user, 0.030445 system)
  2006.90% CPU
  441,334,584 processor cycles
  0 bytes consed
```

Notice how we did 2.3s of work in 0.1s, using ~83% of our available CPU, and consed zero bytes.

The results are more than acceptable for a live coding environment, considering the work involved: multiple color space conversions to our temporary pixels, math to change the hue, and multiple conversions to bring it back to our display color space, over 42.25M bytes, with intermediate decodes and encodes from/to a `pixel-buffer`.

## Notes and gotchas

### Rect semantics

Rects are half-open, and must satisfy:

* `0 <= x1 < x2 <= width`
* `0 <= y1 < y2 <= height`

Invalid rects signal an error (by design).

### Alpha clamping

`mfiano.lib.gfx.pixel-buffer:channel3` clamps to `[0,1]` on write. If you want unclamped alpha for some reason, that's a different buffer contract, not this one.

### Determinism

Row parallelism means your `op` should be:

* pure with respect to external state (or at least thread-safe),
* only mutate the pixel it's handed (the workspace slice),
* not depend on the order rows are processed.

If you want something order-dependent (dithering with an error buffer, convolution/CA neighborhoods, etc.), you probably want a different abstraction than 'map pixels independently', but see "Future work" below.

### Kernel lifetime

A `plan` owns an `lparallel` kernel. In long-running programs, you typically create a plan once and reuse it.

If you need to tear it down, you can end the kernel (internal accessor):

```lisp
(lp:end-kernel (mfiano.lib.gfx.image-pipeline::plan-kernel *plan*))
```

## Performance tips (the "don't accidentally make it slow" list)

* Reuse `plan`s. Creating kernels repeatedly is expensive compared to mapping.
* Keep `op` small and avoid allocations.
* Prefer using `mfiano.lib.gfx.pixel-buffer:*` accessors, not `multiple-value-list`, consy helpers, etc.
* Use a pixel format that matches your storage. If your images are naturally `(simple-array (unsigned-byte 32) (*))`, prefer a `mfiano.lib.gfx.pixel-format:.../u32` format.

## Dependencies

Runtime:

* `mfiano.lib.base`
* `mfiano.lib.gfx.pixel-buffer`
* `mfiano.lib.gfx.pixel-format`
* `lparallel`

Tests:

* `1am`

## Running tests

```lisp
(asdf:test-system :mfiano.lib.gfx.image-pipeline)
```

## Future work

* Support thread-safe double-buffering to allow for fast spatially-aware operations (image convolutions, cellular automata, etc).

## License

Licensed under the MIT License.
