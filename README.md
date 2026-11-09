# mfiano.lib.gfx.image-pipeline : A small, fast pixel mapping layer for CPU-side image work

## About this project

You give it:

* an image buffer (packed pixels in some `mfiano.lib.gfx.pixel-format`),
* a `plan` (dimensions, pixel-format codec, threads, pipeline mode, workspace),
* and an operation that mutates pixels via a `mfiano.lib.gfx.pixel-buffer` view (4 lanes of `double-float`s per pixel).

It handles:

* decoding packed pixels -> per-pixel `double-float` lanes,
* running your function on each pixel (optionally in parallel),
* encoding lanes back to packed pixels,
* optionally doing a 3-phase "pipeline" (pre/work/post), either fused into one pass or unfused, where the work phase can read neighboring pixels.

## What it operates on

### Pixel storage

`image-pipeline` works on images stored in the array type of the plan's pixel format, which follows from the format's storage:

* `:u8`: `(simple-array (unsigned-byte 8) (*))`, for formats like `mfiano.lib.gfx.pixel-format:rgba8888/u8` and `mfiano.lib.gfx.pixel-format:rgb888/u8`
* `:u16`: `(simple-array (unsigned-byte 16) (*))`, for formats like `mfiano.lib.gfx.pixel-format:ga1616/u16`
* `:u32`: `(simple-array (unsigned-byte 32) (*))`, for formats like `mfiano.lib.gfx.pixel-format:rgba8888/u32` and `mfiano.lib.gfx.pixel-format:abgr8888/u32`
* `:f32`: `(simple-array single-float (*))`, for formats like `mfiano.lib.gfx.pixel-format:rgba/f32`
* `:f64`: `(simple-array double-float (*))`, for formats like `mfiano.lib.gfx.pixel-format:rgba/f64`

### Workspace representation

Inside the pipeline, pixels are represented as a `(simple-array double-float (*))` with 4 lanes per pixel. That's exactly what `mfiano.lib.gfx.pixel-buffer` is: a "pixel workspace" you mutate with `channel0..3`, `get-channels`, `set-channels`, etc.

## Public API

### `make-plan`

```lisp
(mfiano.lib.gfx.image-pipeline:make-plan &key pixel-format width height threads filler fused reach) => plan
```

* `pixel-format` defaults to `mfiano.lib.gfx.pixel-format:rgba8888/u32`
* `width`/`height` are required
* `threads` sets how many `lparallel` workers share the work. Workers take the image 2048 pixels at a time, whatever its shape, so a single long row or column is shared among them like any other image
* `filler` makes the plan generate pixels instead of decoding them; see [Generating images](#generating-images)
* `fused` defaults to true and sets how `map-image-pipeline` runs with this plan:
  * True: each run of 2048 pixels is decoded, run through `pre`, `work` and `post`, and encoded in one pass. The workspace holds 2048 pixels per thread, 64 KB each.
  * Nil: `pre` runs over the rect and every pixel within `reach` of it, then `work` runs once per pass, then `post`. Each pass reads one copy of the image and writes the other, so `work` can read the pixels around the one it computes; see [Neighborhood operations](#neighborhood-operations). The plan holds two copies of the image at 32 bytes per pixel, each with a border `reach` pixels wide, and the first holds at least 2048 pixels per thread.
* `reach` defaults to 1 and only matters for unfused plans: it is how far from its own pixel a `work` op may read. A 3x3 neighborhood needs 1 and a 5x5 one needs 2
* Making a plan writes its whole workspace once, so the first map with it runs as fast as later ones

### `cancel-plan`

```lisp
(mfiano.lib.gfx.image-pipeline:cancel-plan plan) => (values)
```

* Ends the plan's threads and releases its workspace
* Canceling a canceled plan does nothing
* Mapping with a canceled plan signals an error condition; a canceled plan never starts again
* Canceling a plan while another thread maps with it is not supported

### `with-plan`

```lisp
(mfiano.lib.gfx.image-pipeline:with-plan (plan &rest make-plan-args) &body body)
```

* Makes a plan from `make-plan-args`, binds it to `plan` around `body`, and cancels it however `body` exits

### `map-image`

```lisp
(mfiano.lib.gfx.image-pipeline:map-image plan pixels op &key x1 y1 x2 y2) => (values)
```

* `op` is called as `(funcall op workspace lane-index)` for each pixel
* If `op` is nil, pixels are only decoded and encoded, or generated and encoded with a `filler`
* Rect is half-open: `[x1,x2) x [y1,y2)`
* Mutates `pixels` in place
* Signals an error condition for invalid rects
* Signals an error condition if `pixels` is not the array type of the plan's pixel format, or is too short for the plan's image

### `map-image-pipeline`

```lisp
(mfiano.lib.gfx.image-pipeline:map-image-pipeline plan pixels &key pre work post passes edge x1 y1 x2 y2) => (values)
```

* `pre` and `post` are called like `op` above, and either may be nil
* In a fused plan, `work` is called like `op` too. In an unfused plan it is called as `(funcall work source destination lane stride x y)`; see [Neighborhood operations](#neighborhood-operations)
* `passes` defaults to 1 and is how many times an unfused plan runs `work`. A fused plan signals an error condition for any other value
* `edge` defaults to `:clamp` and sets what an unfused `work` op reads past the image's edge: `:clamp` repeats the nearest edge pixel, `:wrap` reads from the opposite side, and a list of four channel values reads that constant color
* Runs fused or unfused, as the plan was made; see `make-plan`
* Signals the same error conditions as `map-image` for invalid rects and `pixels`, and an error condition for an invalid `passes` or `edge`

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
  :post (post-set-alpha 2d0))
```

In a plan made with `:fused nil`, `work` reads neighboring pixels instead; see [Neighborhood operations](#neighborhood-operations).

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
;; - creates a plan with 24 threads
;; - run a fused pipeline over each of the ~10.5M pixels that runs pre, work, and post ops in sequence
;; - times the results
;; - writes the new image out to disk
(defun test ()
  (let* ((pixel-format 'mfiano.lib.gfx.pixel-format:rgba8888/u32)
         (pixel-storage (mfiano.lib.gfx.pixel-format:storage pixel-format))
         (image-path "~/test.qoi")
         ;; use `mfiano.lib.codec.qoi` to decode a QOI image to a (unsigned-byte 32) array (our pixel format's packed RGBA888 32bit integer representation)
         (image (mfiano.lib.codec.qoi:decode-file image-path :output pixel-storage))
         (w (mfiano.lib.codec.qoi:width image))
         (h (mfiano.lib.codec.qoi:height image))
         (channels (mfiano.lib.codec.qoi:channels image))
         (pixels (mfiano.lib.codec.qoi:pixels image))
         ;; create a plan using 24 threads (change for your hardware)
         (plan (mfiano.lib.gfx.image-pipeline:make-plan
                :pixel-format pixel-format
                :width w
                :height h
                :threads 24)))
    ;; (SBCL): force a GC before timing to reduce noise
    (sb-ext:gc :full t)
    ;; benchmark the resource usage
    (time
     (mfiano.lib.gfx.image-pipeline:map-image-pipeline
      plan
      pixels
      :pre #'pre-op
      :work #'work-op
      :post #'post-op))
    ;; write the modified image out to disk
    (mfiano.lib.codec.qoi:encode-file pixels "~/out.qoi" :width w :height h :channels channels)
    (values)))

(test)
Evaluation took:
  0.087 seconds of real time
  2.045712 seconds of total run time (2.045044 user, 0.000668 system)
  2351.72% CPU
  329,750,282 processor cycles
  0 bytes consed
```

Notice how we did 2.0s of work in 0.087s, using ~98% of our available CPU, and consed zero bytes.

The results are more than acceptable for a live coding environment, considering the work involved: multiple color space conversions to our temporary pixels, math to change the hue, and multiple conversions to bring it back to our display color space, over 42.25M bytes, with intermediate decodes and encodes from/to a `pixel-buffer`.

## Generating images

Give `make-plan` a `:filler` function and the plan generates an image instead of processing one. Where it would decode `pixels`, it calls your function to fill those pixels instead. Your ops then run on the generated values in the same pass, and the result is encoded into `pixels`, which is only written to. A noise generator, for example, can have its samples colored and encoded without a second pass over the image.

The function is called as `(funcall filler buffer index x y count)` to fill `count` consecutive pixels of one row. A row can be filled by several calls, and one call never crosses into the next row. The first pixel's channels start at lane `index`, and each following pixel starts 4 lanes after the one before it. `x` and `y` are the first pixel's column and row in the image, so pixel `i` of the call is at column `(+ x i)`. The function must set all four channels of every pixel and must be safe to call from several threads at once, since pixels are generated in parallel.

This example generates a diagonal gray ramp and then inverts it with the op from the quick start:

```lisp
(defun diagonal-ramp (width height)
  (let ((scale (/ 1d0 (+ width height))))
    (lambda (buffer index x y count)
      (dotimes (i count)
        (let ((value (* (+ x i y) scale)))
          (mfiano.lib.gfx.pixel-buffer:set-channels buffer (+ index (* i 4)) value value value 1d0)))
      (values))))

(defparameter *ramp-plan*
  (mfiano.lib.gfx.image-pipeline:make-plan
   :pixel-format 'mfiano.lib.gfx.pixel-format:rgba8888/u32
   :width 2048
   :height 1024
   :threads 8
   :filler (diagonal-ramp 2048 1024)))

(mfiano.lib.gfx.image-pipeline:map-image *ramp-plan* pixels (invert-rgb-op))
```

To keep the ramp as generated, pass `nil` as the op:

```lisp
(mfiano.lib.gfx.image-pipeline:map-image *ramp-plan* pixels nil)
```

## Neighborhood operations

An unfused plan lets `work` read the pixels around the one it computes, which convolution filters and cellular automata need. The plan keeps two copies of the image. Each pass reads from one copy, `source`, and writes into the other, `destination`, and the copies swap before the next pass, so a pass never reads a pixel it has already written.

`work` is called as `(funcall work source destination lane stride x y)` for each pixel of the rect. `lane` is the pixel's first channel in both copies, `stride` is the number of lanes in one row, and `x` and `y` are the pixel's column and row in the image. The neighbor `dx` columns to the right and `dy` rows down starts at lane `(+ lane (* 4 dx) (* stride dy))`, for `dx` and `dy` up to the plan's `reach` in either direction. `work` must write all four channels of its pixel in `destination`, because nothing else writes them.

A neighbor past the image's edge comes from the `edge` argument of `map-image-pipeline`. A neighbor outside the rect but inside the image is the image's own pixel, run through `pre`. `pre` runs once before the first pass and `post` once after the last, so a pipeline can convert into a working color space, filter there, and convert back.

This example blurs an image three times with a 3x3 box filter:

```lisp
(defun box-blur (source destination lane stride x y)
  (declare (ignore x y))
  (dotimes (channel 3)
    (setf (aref destination (+ lane channel))
          (/ (loop :for dy :from -1 :to 1
                   :sum (loop :for dx :from -1 :to 1
                              :sum (aref source (+ lane channel (* dx 4) (* dy stride)))))
             9)))
  (setf (aref destination (+ lane 3)) (aref source (+ lane 3))))

(defparameter *blur-plan*
  (mfiano.lib.gfx.image-pipeline:make-plan
   :pixel-format 'mfiano.lib.gfx.pixel-format:rgba8888/u32
   :width 2048
   :height 1024
   :threads 8
   :fused nil))

(mfiano.lib.gfx.image-pipeline:map-image-pipeline *blur-plan* pixels :work #'box-blur :passes 3)
```

## Notes and gotchas

### Rect semantics

Rects are half-open, and must satisfy:

* `0 <= x1 < x2 <= width`
* `0 <= y1 < y2 <= height`

Invalid rects signal an error (by design).

### Alpha clamping

`mfiano.lib.gfx.pixel-buffer:channel3` clamps to `[0,1]` on write. If you want unclamped alpha for some reason, that's a different buffer contract, not this one.

### Determinism

Parallelism means your `op` should be:

* pure with respect to external state (or at least thread-safe),
* only mutate the pixel it's handed (the workspace slice),
* not depend on the order pixels are processed.

Convolution filters and cellular automata read their neighbors from the previous pass, so they fit an unfused plan; see [Neighborhood operations](#neighborhood-operations). Work that depends on the order pixels are processed in, such as dithering with an error buffer, needs a different abstraction than mapping pixels independently.

### Plan lifetime

A `plan` owns worker threads and its workspace. Make a plan once and map with it as often as you like, then cancel it with `cancel-plan` when you are done, or scope it with `with-plan`. A plan that becomes unreachable without being canceled has its workspace reclaimed by the garbage collector, but its threads keep running.

## Performance tips (the "don't accidentally make it slow" list)

* Reuse `plan`s. Creating kernels repeatedly is expensive compared to mapping.
* Keep `op` small and avoid allocations.
* Prefer using `mfiano.lib.gfx.pixel-buffer:*` accessors, not `multiple-value-list`, consy helpers, etc.
* Use a pixel format that matches your storage. If your images are naturally `(simple-array (unsigned-byte 32) (*))`, prefer a `mfiano.lib.gfx.pixel-format:.../u32` format.

## Dependencies

Runtime:

* `bordeaux-threads`
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

## License

Licensed under the MIT License.
