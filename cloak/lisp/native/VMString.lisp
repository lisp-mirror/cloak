(in-package :cloak)

(defstatic |java/lang/VMString.intern(Ljava/lang/String;)| (str)
  (intern-cloak-string (get-string-value str)))
