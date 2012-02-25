;;;; cassowary.asd

(asdf:defsystem #:cassowary
  :serial t
  :depends-on (:cffi)
  :components ((:file "package")
               (:file "cassowary")))

