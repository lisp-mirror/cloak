(in-package :cl-user)

(defpackage :cloak
  (:use :cl :cloak-system))

(defpackage :cloak-signatures
  (:use))

(defvar cloak::*fast* '(optimize (speed 3) (safety 0) (debug 0)))
