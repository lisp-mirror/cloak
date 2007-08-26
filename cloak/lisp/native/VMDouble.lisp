(in-package :cloak)

(defvar +double-float-positive-infinity+ (long-to-double 9218868437227405312))
(defvar +double-float-negative-infinity+ (long-to-double -4503599627370496))
(defvar +nan+ (long-to-double 9221120237041090560))

(defun |java/lang/VMDouble.doubleToRawLongBits(D)| (d)
  (double-to-long d))

(defun |java/lang/VMDouble.longBitsToDouble(J)| (l)
  (long-to-double l))

(defun exponent-sign-fixup (str)
  (let ((idx (search "E+" str :test 'char-equal)))
    (if idx
	(concatenate 'string (subseq str 0 (1+ idx)) (subseq str (+ idx 2)))
	str)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar +digit+ '(#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9)))

(defstatic |java/lang/VMDouble.doubleToLongBits(D)| (d)
  (if (nanp d)
      #x7ff8000000000000
      (|java/lang/VMDouble.doubleToRawLongBits(D)| d)))
