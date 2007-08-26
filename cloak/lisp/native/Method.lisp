(in-package :cloak)

(defun method-vmdata (method)
  (let* ((class (method-declaring-class method))
	 (slot (getfield "slot" method)))
    (elt (cls.methods class) slot)))

(defun method-declaring-class (method-object)
  ;; das ist aber nicht schoen
  (if (cloak-type-p method-object
		    (find-cloak-class nil "java/lang/reflect/Constructor"))
      (class-vmdata (getfield "clazz" method-object))
      (class-vmdata (getfield "declaringClass" method-object))))

(defnative |java/lang/reflect/Method.getParameterTypes()| (method)
  (let ((declaring-class (method-declaring-class method)))
    (make-cloak-array "[Ljava/lang/Class;"
      (map 'vector
	(compose #'class-object (curry #'find-referenced-class declaring-class))
	(cm.argtype-descriptors (method-vmdata method))))))

(defnative |java/lang/reflect/Method.getReturnType()| (method)
  (class-object
   (find-referenced-class
    (method-declaring-class method)
    (cm.rtype-descriptor (method-vmdata method)))))

(defnative |java/lang/reflect/Method.getModifiersInternal()| (method)
  (cm.access-flags (method-vmdata method)))

(defnative |java/lang/reflect/Method.getExceptionTypes()| (method)
  (let ((declaring-class (method-declaring-class method)))
    (make-cloak-array "[Ljava/lang/Class;"
      (map 'vector (compose #'class-object (curry #'class-from-pool declaring-class))
	   (cm.exceptions (method-vmdata method))))))

(defun widen-and-box (value signature)
  (if (symbolp signature)
      (ecase signature
	(:byte (make-cloak-instance "java/lang/Byte" "<init>(B)" value))
	(:short (make-cloak-instance "java/lang/Short" "<init>(S)" value))
	(:int (make-cloak-instance "java/lang/Integer" "<init>(I)" value))
	(:long (make-cloak-instance "java/lang/Long" "<init>(J)" value))
	(:char (make-cloak-instance "java/lang/Character" "<init>(C)" value))
	(:float
	  (setf value (coerce value 'single-float))
	  (make-cloak-instance "java/lang/Float" "<init>(F)" value))
	(:double
	  (setf value (coerce value 'double-float))
	  (make-cloak-instance "java/lang/Double" "<init>(D)" value))
	(:boolean (make-cloak-instance "java/lang/Boolean" "<init>(Z)" value))
	(:void nil))
      value))

;;; why is this an instance method?  The slots are passed as arguments.
(defnative |java/lang/reflect/Method.invokeNative(Ljava/lang/Object;[Ljava/lang/Object;Ljava/lang/Class;I)|
    (this object args class-object method-id)
  ;; IllegalAccessException
  (let* ((class (class-vmdata class-object))
	 (method (elt (cls.methods class) method-id))
	 (signature (cm.signature method))
	 (type-signatures (cm.argtype-descriptors method))
	 (types (mapcar (curry #'find-referenced-class class) type-signatures)))
    (unless (eql (if (nullp args) 0 (length (co.data args))) (length types))
      (throw-exception "java/lang/IllegalArgumentException"
	  "method expected ~D arguments, got ~D"
	(length types)
	(length (co.data args))))
    (setf args
	  (with-collector ()
	    (unless (nullp args)
	      (for ((arg :across (co.data args))
		    (type :in types)
		    (type-signature :in type-signatures))
		(collect
		 (unbox-and-widen arg type type-signature))))))
    (cond
      ((cm.staticp method)
	(maybe-initialize-class class)	;for ExceptionInInitializerError
	(setf object class))
      (t
	(assert-nonnull object)
	(unless (cloak-type-p object class)
	  (throw-exception "java/lang/IllegalArgumentException"))))
    (fast-handler-case
	(widen-and-box
	 (apply-cloak-method signature object args)
	 (cm.rtype-descriptor method))
      (lambda (cause)
	(throw-exception
	    (make-cloak-instance "java/lang/reflect/InvocationTargetException"
		"<init>(Ljava/lang/Throwable;)"
	      cause))))))
