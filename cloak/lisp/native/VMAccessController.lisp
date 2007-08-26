(in-package :cloak)

(defstatic |java/security/VMAccessController.getStack()| ()
  (let ((classes
	 (list (find-cloak-class nil "java/security/VMAccessController")))
	(methods
	 (list (make-cloak-string "getStack"))))
    (do-stack-methods (method class)
      (push (class-object class) classes)
      (push (make-cloak-string (cm.name method)) methods))
    (make-cloak-array
	"[Ljava/lang/Object;"
      (vector
       (make-cloak-array "[Ljava/lang/Class;"
	 (make-array (length classes) :initial-contents (reverse classes)))
       (make-cloak-array "[Ljava/lang/String;"
	 (make-array (length methods) :initial-contents (reverse methods)))))))
