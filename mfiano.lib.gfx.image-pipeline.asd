(asdf:defsystem #:mfiano.lib.gfx.image-pipeline
  :description "A small, fast pixel mapping layer for CPU-side image work"
  :author "Michael Fiano <michael.fiano@gmail.com>"
  :license "MIT"
  :homepage "https://github.com/mfiano/mfiano.lib.gfx.image-pipeline"
  :encoding :utf-8
  :depends-on
  (#:mfiano.lib.base
   #:mfiano.lib.gfx.pixel-buffer
   #:mfiano.lib.gfx.pixel-format
   #:lparallel)
  :in-order-to ((test-op (test-op #:mfiano.lib.gfx.image-pipeline/test)))
  :pathname "src"
  :serial t
  :components
  ((:file "image-pipeline")))

(asdf:defsystem #:mfiano.lib.gfx.image-pipeline/test
  :description "Unit tests for mfiano.lib.gfx.image-pipeline"
  :author "Michael Fiano <michael.fiano@gmail.com>"
  :license "MIT"
  :homepage "https://github.com/mfiano/mfiano.lib.gfx.image-pipeline"
  :encoding :utf-8
  :depends-on
  (#:1am
   #:mfiano.lib.gfx.image-pipeline
   #:mfiano.lib.gfx.pixel-buffer
   #:mfiano.lib.gfx.pixel-format
   #:lparallel)
  :perform (test-op (o c) (symbol-call '#:1am '#:run))
  :pathname "test"
  :serial t
  :components
  ((:file "test")))
