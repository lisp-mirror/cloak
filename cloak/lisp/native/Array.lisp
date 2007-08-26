(in-package :cloak)

(defstatic |java/lang/reflect/Array.createObjectArray(Ljava/lang/Class;I)|
    (class-object length)
  (when (minusp length)
    (throw-exception "java/lang/NegativeArraySizeException"))
  (let ((element-type (class-vmdata class-object)))
    (assert (not (symbolp (cls.name element-type))))
    (make-cloak-array
	(find-array-class-for element-type)
      (safe-make-array length :initial-element +null+))))
