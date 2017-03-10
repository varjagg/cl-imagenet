(in-package #:asdf)

(defsystem "cl-imagenet"
  :description "IMAGENET parsing and reading"
  :author "Eugene Zaikonnikov <eugene@funcall.org>"
  :version "0.2"
  :license "BSD"
  :depends-on (#:alexandria #:cl-jpeg #:cxml #:opticl #:bordeaux-threads #:cffi #:cl-fad #:trivial-channels #:clx)
  :components
  ((:file "package")
   (:file "imagenet-interface")))
