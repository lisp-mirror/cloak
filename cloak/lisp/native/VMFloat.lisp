(in-package :cloak)

(defstatic |java/lang/VMFloat.floatToIntBits(F)| (d)
  (if (nanp d)
      #x7fc00000
      (|java/lang/VMFloat.floatToRawIntBits(F)| d)))

(defun |java/lang/VMFloat.floatToRawIntBits(F)| (f)
  (float-to-int f))

(defun |java/lang/VMFloat.intBitsToFloat(I)| (i)
  (int-to-float i))
