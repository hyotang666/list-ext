; vim: ft=lisp et
(in-package :asdf)
(defsystem :list-ext.test
  :depends-on
  (:jingoh "list-ext")
  :components
  ((:file "list-ext"))
  :perform
  (test-op (o c) (symbol-call :jingoh :examine :list-ext)))
