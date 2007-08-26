(in-package :cloak)

(defun primitive-code/signature (code)
  (ecase code
    (:byte 2)
    (:short 4)

    (:char 3)

    (:int (* 8 3))
    (:long (* 16 3))
    (:float (* 32 3))
    (:double (* 64 3))

    (:boolean 5)))

(defun primitive-code/class-name (name)
  (let ((keyword
	 ;; das koennte man in eine funktion verpacken, mir faellt nur
	 ;; kein guter name ein:
	 (cond
	   ((equal name "java/lang/Byte") :byte)
	   ((equal name "java/lang/Short") :short)
	   ((equal name "java/lang/Integer") :int)
	   ((equal name "java/lang/Long") :long)
	   ((equal name "java/lang/Double") :double)
	   ((equal name "java/lang/Float") :float)
	   ((equal name "java/lang/Boolean") :boolean)
	   ((equal name "java/lang/Character") :char)
	   (t nil))))
    (if keyword
	(primitive-code/signature keyword)
	nil)))

(defun primitive-code/class (class)
  (primitive-code/class-name (cls.name class)))

(defun unbox-and-widen (object type type-signature)
  (cond
    ((typep type 'reference-type)
      (unless (or (nullp object) (cloak-type-p object type))
	(throw-exception "java/lang/IllegalArgumentException"
	    "expected an instance of ~_~A, ~_but got ~A"
	  type
	  object))
      object)
    (t
      (let ((wanted (primitive-code/signature type-signature))
	    (actual (and (not (nullp object))
                         (primitive-code/class (%class object)))))
	(unless (and actual (zerop (mod wanted actual)))
	  (throw-exception "java/lang/IllegalArgumentException"
	      "expected a ~A, ~_but got ~A"
	    type-signature
	    object)))
      (let ((value (getfield "value" object)))
	(case type-signature
	  (:float (coerce value 'single-float))
	  (:double (coerce value 'double-float))
	  (t value))))))

;;; XXX why is this an instance method? The slots are passed as arguments.
;;;
(defnative |java/lang/reflect/Constructor.constructNative([Ljava/lang/Object;Ljava/lang/Class;I)|
    (this args class-object constructor-id)
  ;; XXX IllegalAccessException
  (let* ((class (class-vmdata class-object))
	 (method (elt (cls.methods class) constructor-id))
	 (signature (cm.signature method))
	 (type-signatures (cm.argtype-descriptors method))
	 (types (mapcar (curry #'find-referenced-class class)
			type-signatures)))
    (when (cls.abstractp class)
      (throw-exception "java/lang/InstantiationException"
	  "cannot instantiate abstract class: ~A"
	class-object))
    (unless (eql (if (nullp args) 0 (length (co.data args))) (length types))
      (throw-exception "java/lang/IllegalArgumentException"
	  "constructor expected ~D arguments, got ~D"
	(length types)
	(length args)))
    (setf args
	  (with-collector ()
	    (unless (nullp args)
	      (for ((arg :across (co.data args))
		    (type :in types)
		    (type-signature :in type-signatures))
		(collect
		 (unbox-and-widen arg type type-signature))))))
    (maybe-initialize-class class)	;for ExceptionInInitializerError
    (fast-handler-case
	(apply #'make-cloak-instance class signature args)
      (lambda (cause)
	(throw-exception
	    (make-cloak-instance "java/lang/reflect/InvocationTargetException"
		"<init>(Ljava/lang/Throwable;)"
	      cause))))))

(defun constructor-vmdata (constructor)
  (let* ((class (class-vmdata (getfield "clazz" constructor)))
	 (slot (getfield "slot" constructor)))
    (values (elt (cls.methods class) slot)
	    class)))

(defnative |java/lang/reflect/Constructor.getModifiersInternal()| (this)
  (cm.access-flags (constructor-vmdata this)))

(defnative |java/lang/reflect/Constructor.getParameterTypes()| (this)
  (multiple-value-bind (method class)
      (constructor-vmdata this)
    (make-cloak-array "[Ljava/lang/Class;"
      (map 'vector
	(lambda (foo)
	  (class-object (find-referenced-class class foo)))
	(cm.argtype-descriptors method)))))

(defnative |java/lang/reflect/Constructor.getExceptionTypes()| (this)
  (multiple-value-bind (method class)
      (constructor-vmdata this)
    (make-cloak-array "[Ljava/lang/Class;"
      (map 'vector
	(lambda (foo)
	  (class-object (find-referenced-class class foo)))
	(cm.exceptions method)))))
