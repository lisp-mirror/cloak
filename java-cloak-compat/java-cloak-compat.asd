;;; -*- mode: lisp -*-
(defpackage :java-cloak-compat-system
  (:use :asdf :cl))
(in-package :java-cloak-compat-system)

(defclass closure-source-file (cl-source-file) ())

#+sbcl
(defmethod perform :around ((o compile-op) (s closure-source-file))
  ;; shut up already.  Correctness first.
  (handler-bind ((sb-ext:compiler-note #'muffle-warning))
    (let ((*compile-print* nil))
      (call-next-method))))

(defsystem :java-cloak-compat
    :default-component-class closure-source-file
    :serial t
    :components
    ((:file "package")
     (:file "java"))
    :depends-on (:cloak :split-sequence))
