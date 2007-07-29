;;; -*- mode: lisp -*-
(defpackage :empty-system
  (:use :asdf :cl))
(in-package :empty-system)

(defclass closure-source-file (cl-source-file) ())

#+sbcl
(defmethod perform :around ((o compile-op) (s closure-source-file))
  ;; shut up already.  Correctness first.
  (handler-bind ((sb-ext:compiler-note #'muffle-warning))
    (let ((*compile-print* nil))
      (call-next-method))))

(defsystem :empty
    :default-component-class closure-source-file
    :serial t
    :components
    ((:file "package")
     (:file "empty"))
    :depends-on ())

;;; (defsystem :empty-test
;;;     :default-component-class closure-source-file
;;;     :serial t
;;;     :components
;;;     ((:file "test"))
;;;     :depends-on (:empty :rt))
