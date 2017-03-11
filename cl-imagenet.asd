(in-package #:asdf)

(defsystem "cl-imagenet"
  :description "IMAGENET parsing and reading"
  :author "Eugene Zaikonnikov <eugene@funcall.org>"
  :version "0.2"
  :license "BSD"
  :depends-on (#:cl-jpeg #:cxml #:opticl #:bordeaux-threads #:cl-fad #:trivial-channels #:clx)
  :weakly-depends-on ( #:cffi)
  :components
  ((:file "package")
   (:file "imagenet-interface")))
