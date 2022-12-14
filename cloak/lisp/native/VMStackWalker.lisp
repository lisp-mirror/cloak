(in-package :cloak)

(defun ignored-context-method-p (method class)
  (or (and (equal (cls.name class) "gnu/classpath/VMStackWalker")
	   (equal (cm.name method) "getClassContext"))
      (and (equal (cls.name class) "java/lang/reflect/Method")
	   (equal (cm.name method) "invoke"))))

(defmacro do-stack-methods ((method class) &body body)
  (let ((pc (gensym))
	(c (gensym))
	(top (gensym)))
    `(let ((,top t))
       (do-stack (,pc ,c)
	 (multiple-value-bind (,method ,class)
	     (resolve-stackframe (sb-sys:sap-int ,pc) ,c)
	   (when (and method
		      (not
		       (and ,top (ignored-context-method-p ,method ,class))))
	     (setf ,top nil)
	     ,@body))))))

(defstatic |gnu/classpath/VMStackWalker.getClassContext()| ()
  (let ((results '()))
    (do-stack-methods (method class)
      (push (class-object class) results))
    (make-cloak-array "[Ljava/lang/Class;"
      (make-array (length results)
		  ;; skip VMStackWalker.getClassContext
		  :initial-contents (cdr (reverse results))))))

(defstatic |gnu/classpath/VMStackWalker.getCallingClass()| ()
  (let ((result +null+)
	(i 0))
    (do-stack-methods (method class)
      ;; 0 -- VMStackWalker.getCallingClassLoader
      ;; 1 -- the calling class, aka getClassContext[0]
      ;; 2 -- the one we're looking for, aka getClassContext[1]
      (when (eql i 2)
	(setf result (class-object class))
	(return))
      (incf i))
    result))

(defstatic |gnu/classpath/VMStackWalker.getCallingClassLoader()| ()
  (let ((result +null+)
	(i 0))
    (do-stack-methods (method class)
      ;; 0 -- VMStackWalker.getCallingClassLoader
      ;; 1 -- the calling class, aka getClassContext[0]
      ;; 2 -- the one we're looking for, aka getClassContext[1]
      (when (eql i 2)
	(whereas ((cl (cls.class-loader class)))
	  (setf result (cl.object cl)))
	(return))
      (incf i))
    result))

#+(or)
(defstatic |gnu/classpath/VMStackWalker.getClassContext()| ()
  (let ((results '()))
    (do-stack (pc c)
      (multiple-value-bind (method class)
	  (resolve-stackframe (sb-sys:sap-int pc) c)
	(when (and method
		   (or results (not (ignored-context-method-p method class))))
	  (push (class-object class) results))))
    (make-cloak-array "[Ljava/lang/Class;"
      (make-array (length results) :initial-contents (reverse results)))))
