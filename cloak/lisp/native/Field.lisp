(in-package :cloak)

(defun field-vmdata (field)
  (let* ((class (getfield "declaringClass" field))
	 (slot (getfield "slot" field)))
    (elt (cls.fields (class-vmdata class)) slot)))

(defun field-class-object (field)
  (getfield "declaringClass" field))

(defnative |java/lang/reflect/Field.getModifiersInternal()| (field)
  (cm.access-flags (field-vmdata field)))

(defun %find-field (field-object object direction requested-signature)
  (let ((class (class-vmdata (field-class-object field-object)))
	(field (field-vmdata field-object))
	(signature (cm.type-descriptor (field-vmdata field-object))))
    (when (and requested-signature
	       (or (not (symbolp signature))
		   (let ((requested-code
			  (primitive-code/signature requested-signature))
			 (actual-code
			  (primitive-code/signature signature)))
		     (not (zerop
			   (ecase direction
			     (:get (mod requested-code actual-code))
			     (:set (mod actual-code requested-code))))))))
      (throw-exception "java/lang/IllegalArgumentException"))
    (cond
      ((cm.staticp field)
	(maybe-initialize-class class)
	(setf object (cls.static-field-values class)))
      (t
	(assert-nonnull object)
	(unless (cloak-type-p object class)
	  (throw-exception "java/lang/IllegalArgumentException"))))
    (values field (vtable-index class field) object signature)))

(defun do-get (field-object object &optional requested-signature)
  (multiple-value-bind (field index actual-object)
      (%find-field field-object object :get requested-signature)
    (funcall (field-reffer (cm.type-descriptor field)) actual-object index)))

(defun (setf do-get) (newval field-object object &optional requested-signature)
  (multiple-value-bind (field index actual-object actual-signature)
      (%find-field field-object object :set requested-signature)
    (case actual-signature
      (:float (setf newval (coerce newval 'single-float)))
      (:double (setf newval (coerce newval 'double-float))))
    (funcall (field-setter (cm.type-descriptor field))
	     actual-object
	     index
	     newval)))

(defnative |java/lang/reflect/Field.get(Ljava/lang/Object;)| (field-object object)
  (widen-and-box (do-get field-object object)
		 (cm.type-descriptor (field-vmdata field-object))))

(defnative |java/lang/reflect/Field.getBoolean(Ljava/lang/Object;)|
    (field-object object)
  (do-get field-object object :boolean))

(defnative |java/lang/reflect/Field.getByte(Ljava/lang/Object;)|
    (field-object object)
  (do-get field-object object :byte))

(defnative |java/lang/reflect/Field.getChar(Ljava/lang/Object;)|
    (field-object object)
  (do-get field-object object :char))

(defnative |java/lang/reflect/Field.getDouble(Ljava/lang/Object;)|
    (field-object object)
  (coerce (do-get field-object object :double) 'double-float))

(defnative |java/lang/reflect/Field.getFloat(Ljava/lang/Object;)|
    (field-object object)
  (coerce (do-get field-object object :float) 'single-float))

(defnative |java/lang/reflect/Field.getInt(Ljava/lang/Object;)|
    (field-object object)
  (do-get field-object object :int))

(defnative |java/lang/reflect/Field.getLong(Ljava/lang/Object;)|
    (field-object object)
  (do-get field-object object :long))

(defnative |java/lang/reflect/Field.getShort(Ljava/lang/Object;)|
    (field-object object)
  (do-get field-object object :short))

(defnative |java/lang/reflect/Field.set(Ljava/lang/Object;Ljava/lang/Object;)|
    (field-object object value)
  (let* ((field (field-vmdata field-object))
	 (signature (cm.type-descriptor field))
	 (class (field-type field-object)))
    (setf (do-get field-object object)
	  (unbox-and-widen value class signature))))

(defnative |java/lang/reflect/Field.setBoolean(Ljava/lang/Object;Z)|
    (field-object object value)
  (setf (do-get field-object object :boolean) value))

(defnative |java/lang/reflect/Field.setByte(Ljava/lang/Object;B)|
    (field-object object value)
  (setf (do-get field-object object :byte) value))

(defnative |java/lang/reflect/Field.setChar(Ljava/lang/Object;C)|
    (field-object object value)
  (setf (do-get field-object object :char) value))

(defnative |java/lang/reflect/Field.setDouble(Ljava/lang/Object;D)|
    (field-object object value)
  (setf (do-get field-object object :double) value))

(defnative |java/lang/reflect/Field.setFloat(Ljava/lang/Object;F)|
    (field-object object value)
  (setf (do-get field-object object :float) value))

(defnative |java/lang/reflect/Field.setInt(Ljava/lang/Object;I)|
    (field-object object value)
  (setf (do-get field-object object :int) value))

(defnative |java/lang/reflect/Field.setLong(Ljava/lang/Object;J)|
    (field-object object value)
  (setf (do-get field-object object :long) value))

(defnative |java/lang/reflect/Field.setShort(Ljava/lang/Object;S)|
    (field-object object value)
  (setf (do-get field-object object :short) value))

(defun field-type (field-object)
  (let ((declaring-class
	 (class-vmdata (getfield "declaringClass" field-object)))
	(field (field-vmdata field-object)))
    (find-referenced-class declaring-class (cm.type-descriptor field))))

(defnative |java/lang/reflect/Field.getType()| (field-object)
  (class-object (field-type field-object)))
