; vim: ft=lisp et
(in-package :asdf)
(defsystem :list-ext
  :depends-on
  (
   "type-ext" ; type extensions.
   )
  :components((:file "list-ext")))

(defmethod component-depends-on ((o test-op) (c (eql (find-system "list-ext"))))
  (append (call-next-method) '((test-op "list-ext.test"))))
