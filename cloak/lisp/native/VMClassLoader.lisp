(in-package :cloak)

(defstatic |java/lang/VMClassLoader.findLoadedClass(Ljava/lang/ClassLoader;Ljava/lang/String;)|
    (this name)
  (let ((c (gethash (get-string-value name)
		    (cl.classes (classloader-vmdata this)))))
    (if c
	(class-object c)
	+null+)))

(defun classloader-vmdata (classloader-object)
  (with-global-monitor ()
    (let ((x (getfield "vmdata" classloader-object)))
      (when (nullp x)
	(setf x (make-class-loader :object classloader-object))
	(setf (getfield "vmdata" classloader-object) x))
      x)))

(defstatic |java/lang/VMClassLoader.getPrimitiveClass(C)|
    (c)
  (let ((s
	 (ecase (code-char c)
	   (#\Z :boolean)
	   (#\B :byte)
	   (#\C :char)
	   (#\D :double)
	   (#\F :float)
	   (#\I :int)
	   (#\J :long)
	   (#\S :short)
	   (#\V :void))))
    (class-object (find-cloak-class nil s))))

(defstatic |java/lang/VMClassLoader.loadClass(Ljava/lang/String;Z)|
    (name resolve)
  (block nil
    (fast-handler-bind
        (lambda (o)
	  (let ((class
		 (find-cloak-class nil "java/lang/NoClassDefFoundError")))
	    (when (cloak-type-p o class)
	      ;; we throw ClassNotFoundException, not NoClassDefFoundError.
	      ;; But null is also allowed.
	      (return +null+))))
      (let ((str (get-string-value name)))
	(when (find #\/ str)
	  (throw-exception "java/lang/ClassNotFoundException" "~A" str))
	(let ((c (find-cloak-class nil (substitute #\/ #\. str))))
	  (when (eql resolve 1)
	    (link-cloak-class c))
	  (class-object c))))))

(defstatic
    |java/lang/VMClassLoader.defineClass(Ljava/lang/ClassLoader;Ljava/lang/String;[BIILjava/security/ProtectionDomain;)|
    (cl name data offset len pd)
  (let* ((bytes (map '(vector (unsigned-byte 8))
		  (lambda (x) (if (minusp x) (+ x 256) x))
		  (subseq (co.data data) offset len)))
	 (hash (sb-md5:md5sum-sequence bytes))
	 (cf (gethash hash *class-files/hashed*)))
    (unless cf 
      (incf *class-loading-counter*)
      (let ((q (heap-file-name hash))
	    (sb-heapdump:*dumpload-verbose* nil))
	(when (quick-probe-file q)
	  (setf cf (sb-heapdump:load-dumpfile q))))
      (unless cf
	(format *trace-output*
		"~&; reading ~{~2,'0X~}" (coerce hash 'list))
	(setf cf (read-cloak-class bytes :hash hash))
	(format *trace-output* " => ~A~%" (cf.name cf)))
      (setf (gethash hash *class-files/hashed*) cf))
    (let ((str (get-string-value name)))
      ;; ist es ein fehler, wenn str schon slashes enthaelt?
      (setf str (substitute #\/ #\. str))
      (when (and (not (nullp name)) (not (equal str (cf.name cf))))
	(class-format-error
	 "class file name does not match: asked to define ~A, but classfile contains ~A"
	 str
	 (cf.name cf))))
    (class-object (define-cloak-class (and cl (classloader-vmdata cl)) cf pd))))

(defstatic |java/lang/VMClassLoader.resolveClass(Ljava/lang/Class;)| (class)
  (declare (ignore class))
  ;; XXX ?
  )
