(in-package :cloak)

(defnative |java/io/VMObjectStreamClass.hasClassInitializer(Ljava/lang/Class;)|
    (class)
  (let ((cls (class-vmdata class)))
    (if (and (typep cls 'reference-type)
	     (find "<clinit>" (cls.methods cls)
		   :key #'cm.name
		   :test #'equal))
	1
	0)))
