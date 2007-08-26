(in-package :cloak)

(defun class-vmdata (class)
  (getfield "vmdata" class))

(defun class-object (class)
  (with-global-monitor ()
    (or (cls.object class)
	(let ((class-object
	       (make-cloak-instance
		   (find-cloak-class nil "java/lang/Class")
		   "<init>(Ljava/lang/Object;Ljava/security/ProtectionDomain;)"
		 class
		 (if (typep class 'reference-type)
		     (cls.protection-domain class)
		     +null+))))
	  (setf (cls.object class) class-object)
	  class-object))))

(defstatic |java/lang/VMClass.forName(Ljava/lang/String;ZLjava/lang/ClassLoader;)|
    (name init cl)
  (block nil
    (fast-handler-bind
        (lambda (o)
	  (let ((class
		 (find-cloak-class nil "java/lang/NoClassDefFoundError")))
	    (when (cloak-type-p o class)
	      ;; we throw ClassNotFoundException, not NoClassDefFoundError.
	      (throw-exception "java/lang/ClassNotFoundException"))))
      (let ((str (get-string-value name)))
	(when (find #\/ str)
	  (throw-exception "java/lang/ClassNotFoundException" "~A" str))
	(let ((c (find-cloak-class (and cl (classloader-vmdata cl))
				   (substitute #\/ #\. str))))
	  (when (eql init 1)
	    (initialize-class c))
	  (class-object c))))))

(defstatic |java/lang/VMClass.initialize(Ljava/lang/Class;)| (c)
  (maybe-initialize-class (class-vmdata c)))

(defstatic |java/lang/VMClass.isPrimitive(Ljava/lang/Class;)| (c)
  (if (typep (class-vmdata c) 'primitive-class) 1 0))

(defstatic |java/lang/VMClass.getName(Ljava/lang/Class;)| (c)
  (make-cloak-string
   (let ((class (class-vmdata c)))
     (etypecase class
       (primitive-class (string-downcase (symbol-name (cls.name class))))
       (cloak-array-class (substitute #\. #\/ (cls.jdi-signature class)))
       (reference-type (substitute #\. #\/ (cls.name class)))))))

(defstatic |java/lang/VMClass.getDeclaredConstructors(Ljava/lang/Class;Z)|
    (c publiconly)
  (make-cloak-array "[Ljava/lang/reflect/Constructor;"
    (coerce
     (let ((class (class-vmdata c))
	   (constructor-class
	    (find-cloak-class nil "java/lang/reflect/Constructor")))
       (etypecase class
	 (primitive-class '())
	 (cloak-array-class '())
	 (reference-type
	   (with-collector ()
	     (for* ((i :from 0)
		    (method :in (cls.methods class)))
	       (when (and (equalp (cm.name method) "<init>")
			  (or (eql publiconly 0)
			      (cm.publicp method)))
		 (collect
		  (make-cloak-instance
		      constructor-class "<init>(Ljava/lang/Class;I)"
		    (class-object class)
		    i))))))))
     'vector)))

(defstatic |java/lang/VMClass.getDeclaredFields(Ljava/lang/Class;Z)|
    (c publiconly)
  (make-cloak-array "[Ljava/lang/reflect/Field;"
    (coerce
     (let ((class (class-vmdata c))
	   (field-class
	    (find-cloak-class nil "java/lang/reflect/Field")))
       (etypecase class
	 (primitive-class '())
	 (cloak-array-class '())
	 (reference-type
	   (with-collector ()
	     (for* ((i :from 0)
		    (field :in (cls.fields class)))
	       (when (or (eql publiconly 0) (cm.publicp field))
		 (collect
		  (make-cloak-instance
		      field-class
		      "<init>(Ljava/lang/Class;Ljava/lang/String;I)"
		    (class-object class)
		    (make-cloak-string (cm.name field))
		    i))))))))
     'vector)))

(defstatic |java/lang/VMClass.getDeclaredMethods(Ljava/lang/Class;Z)|
    (c publiconly)
  (make-cloak-array "[Ljava/lang/reflect/Method;"
    (coerce
     (let ((class (class-vmdata c))
	   (method-class (find-cloak-class nil "java/lang/reflect/Method")))
       (etypecase class
	 (primitive-class '())
	 (cloak-array-class '())
	 (reference-type
	   (with-collector ()
	     (for* ((i :from 0)
		    (method :in (cls.methods class)))
	       (when (and (or (eql publiconly 0) (cm.publicp method))
			  (not (equalp (cm.name method) "<init>"))
			  (not (equalp (cm.name method) "<clinit>")))
		 (collect
		  (make-cloak-instance
		      method-class
		      "<init>(Ljava/lang/Class;Ljava/lang/String;I)"
		    (class-object class)
		    (make-cloak-string (cm.name method))
		    i))))))))
     'vector)))

(defstatic |java/lang/VMClass.getInterfaces(Ljava/lang/Class;)| (c)
  (make-cloak-array "[Ljava/lang/Class;"
    (coerce
     (let ((class (class-vmdata c)))
       (etypecase class
	 (primitive-class '())
	 (reference-type (mapcar #'class-object (cls.interfaces class)))))
     'vector)))

(defstatic |java/lang/VMClass.isInterface(Ljava/lang/Class;)| (c)
  (let ((class (class-vmdata c)))
    (etypecase class
      (primitive-class 0)
      (reference-type (if (cls.interfacep class) 1 0)))))

(defstatic |java/lang/VMClass.isArray(Ljava/lang/Class;)| (c)
  (let ((class (class-vmdata c)))
    (etypecase class
      (primitive-class 0)
      (cloak-array-class 1)
      (reference-type 0))))

(defstatic |java/lang/VMClass.getSuperclass(Ljava/lang/Class;)| (c)
  (let ((class (class-vmdata c)))
    (etypecase class
      (primitive-class +null+)
      (reference-type
	(if (and (cls.superclass class)
		 ;; XXX das ist doch pfuschig, warum steht im class file
		 ;; java/lang/Object wenn getSuperclass() dann null will?
		 (not (cls.interfacep class)))
	    (class-object (cls.superclass class))
	    +null+)))))

(defstatic |java/lang/VMClass.getComponentType(Ljava/lang/Class;)| (class-object)
  (let ((c (class-vmdata class-object)))
    (if (typep c 'cloak-array-class)
	(class-object (find-referenced-class c (cls.element-type c)))
	+null+)))

(defstatic |java/lang/VMClass.getClassLoader(Ljava/lang/Class;)| (class-object)
  (let ((cl (cls.class-loader (class-vmdata class-object))))
    (if cl
        (cl.object cl)
        +null+)))

(defstatic |java/lang/VMClass.getDeclaredClasses(Ljava/lang/Class;Z)| (class-object publiconly)
  (let ((class (class-vmdata class-object)))
    (make-cloak-array "[Ljava/lang/Class;"
      (coerce
       (with-collector (collect)
	 (dolist (i (cls.inner-class-table class))
	   (destructuring-bind (inner outer inner-name flags) i
	     (declare (ignore inner-name))
	     (when (and (eq outer class)
			(or (eql publiconly 0)
			    (logtest +acc_public+ flags)))
	       (collect (class-object inner))))))
       'vector))))

(defstatic |java/lang/VMClass.getDeclaringClass(Ljava/lang/Class;)| (class-object)
  (let ((class (class-vmdata class-object)))
    (etypecase class
      (primitive-class +null+)
      (cloak-array-class +null+)
      (reference-type
	(dolist (i (cls.inner-class-table class)
                  +null+)
	  (destructuring-bind (inner outer inner-name flags) i
	    (declare (ignore inner-name flags))
	    (when (eq inner class)
	      (return (class-object outer)))))))))

(defstatic |java/lang/VMClass.isInstance(Ljava/lang/Class;Ljava/lang/Object;)|
    (c o)
  (if (and (not (nullp o))
           (cloak-subclass-p (%class o) (class-vmdata c)))
      1
      0))

(defstatic |java/lang/VMClass.getModifiers(Ljava/lang/Class;Z)|
    (class ignoreinner)
  (let ((c (class-vmdata class)))
    (cond
      ((and (zerop ignoreinner)
	    (typep c 'reference-type)
	    (for (((inner nil nil flags) :in (cls.inner-class-table c)))
	      (when (eq inner c)
		(return flags)))))
      (t
	(cls.access-flags c)))))

(defstatic
    |java/lang/VMClass.isAssignableFrom(Ljava/lang/Class;Ljava/lang/Class;)|
    (c d)
  (assert-nonnull c)
  (assert-nonnull d)
  (if (let ((a (class-vmdata c))
	    (b (class-vmdata d)))
	(if (or (typep a 'primitive-class)
		(typep b 'primitive-class))
	    (eq a b)
	    (cloak-subclass-p b a)))
      1
      0))

(defstatic |java/lang/VMClass.loadArrayClass(Ljava/lang/String;Ljava/lang/ClassLoader;)|
    (name classloader)
  (cloak-handler-bind
      (("java/lang/NoClassDefFoundError"
	(lambda (c)
	  ;; the VM throws an unchecked exception,we turn it into a checked one
	  (throw-exception "java/lang/ClassNotFoundException" "~A" c))))
    (let ((str (get-string-value name)))
      (when (find #\/ str)
	(throw-exception "java/lang/ClassNotFoundException" "~A"
	  (get-string-value name)))
      (class-object
       (find-cloak-class
	(and classloader (classloader-vmdata classloader))
	(substitute #\/ #\. str))))))

(defstatic |java/lang/VMClass.throwException(Ljava/lang/Throwable;)|
    (throwable)
  (throw-exception throwable))
