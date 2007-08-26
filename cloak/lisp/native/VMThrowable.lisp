(in-package :cloak)

;; zum debugging:
(defun ba ()
  (call-cloak-method "printStackTrace()"
		     (make-cloak-instance "java/lang/Throwable" "<init>()")))

(defnative |java/lang/VMThrowable.getStackTrace(Ljava/lang/Throwable;)| (this throwable)
  (declare (ignore throwable))
  (with-global-monitor ()
    (sb-sys:without-gcing
     (resolve-vmthrowable this))
    (let ((v (getfield "vmdata" this))
	  (elts '()))
      ;; Pfusch: 9 statt 0, weil Eclipse (MarkerAnnotationPreferences.java)
      ;; sonst ueber die ersten drei Frames stolpert.  Dabei erlauben
      ;; sie sogar ein einzelnes Extraframe am Anfang.  Wie grosszuegig!
      ;;
      ;; at java.lang.Throwable.fillInStackTrace() (Throwable.java:498)
      ;; at java.lang.Throwable.<init>(Ljava/lang/String;) (Throwable.java:159)
      ;; at java.lang.Throwable.<init>() (Throwable.java:147)
      (for ((i :from 9 :below (length v) :by 3))
	(let ((method (elt v i))
	      (class (elt v (1+ i)))
	      (pc-offset (elt v (+ i 2))))
	  (when class
	    (let* ((table (cm.line-numbers method))
		   (line
		    (loop
			for i from 0 below (length table) by 2
			when (<= (elt table i) pc-offset)
			do
			  (return (elt table (1+ i)))
			finally
			  (return -1))))
	      (push (make-cloak-instance "java/lang/StackTraceElement"
			"<init>(Ljava/lang/String;ILjava/lang/String;Ljava/lang/String;Z)"
		      (let ((str (cf.sourcefile (cls.class-file class))))
			(if str
			    (make-cloak-string str)
			    +null+))
		      line
		      (make-cloak-string
		       ;; Eclipse guckt doch tatsaechlich im Stacktrace
		       ;; nach Klassennamen.  Mit Slashes hier startet's nicht.
		       (substitute #\. #\/ (cls.name class)))
		      (make-cloak-string (cm.signature method))
		      0)
		    elts)))))
      (make-cloak-array "[Ljava/lang/StackTraceElement;"
	(make-array (length elts) :initial-contents (reverse elts))))))

;; x86-call-context copy&pasted for speed
(declaim (optimize (speed 3) (safety 0) (debug 0)))
(in-package :sb-di)
(declaim (inline cloak::x86-call-context))
(defun cloak::x86-call-context (fp &key (depth 0))
  (declare (type system-area-pointer fp)
	   (fixnum depth))
  (cond
   ((not (control-stack-pointer-valid-p fp)) nil)
   (t
    (let ((lisp-ocfp (sap-ref-sap fp (- (* (1+ ocfp-save-offset)
					   sb-vm::n-word-bytes))))
	  (lisp-ra (sap-ref-sap fp (- (* (1+ return-pc-save-offset)
					 sb-vm::n-word-bytes))))
	  (c-ocfp (sap-ref-sap fp (* 0 sb-vm:n-word-bytes)))
	  (c-ra (sap-ref-sap fp (* 1 sb-vm:n-word-bytes))))
      (cond ((and (sap> lisp-ocfp fp) (control-stack-pointer-valid-p lisp-ocfp)
		  (ra-pointer-valid-p lisp-ra)
		  (sap> c-ocfp fp) (control-stack-pointer-valid-p c-ocfp)
		  (ra-pointer-valid-p c-ra))
	     ;; Look forward another step to check their validity.
	     (let ((lisp-path-fp (x86-call-context lisp-ocfp
						   :depth (1+ depth)))
		   (c-path-fp (x86-call-context c-ocfp :depth (1+ depth))))
	       (cond ((and lisp-path-fp c-path-fp)
                       ;; Both still seem valid - choose the lisp frame.
		      #+freebsd
		      (if (sap> lisp-ocfp c-ocfp)
                        (values lisp-ra lisp-ocfp)
			(values c-ra c-ocfp))
                       #-freebsd
                       (values lisp-ra lisp-ocfp))
		     (lisp-path-fp
		      (values lisp-ra lisp-ocfp))
		     (c-path-fp
		      (values c-ra c-ocfp))
		     (t
		      nil))))
	    ((and (sap> lisp-ocfp fp) (control-stack-pointer-valid-p lisp-ocfp)
		  (ra-pointer-valid-p lisp-ra))
	     (values lisp-ra lisp-ocfp))
	    ((and (sap> c-ocfp fp) (control-stack-pointer-valid-p c-ocfp)
		  #-linux (ra-pointer-valid-p c-ra))
	     (values c-ra c-ocfp))
	    (t nil))))))

(in-package :cloak)

(defmacro do-stack ((pc class) &body body)
  (let ((fp (gensym)))
    `(sb-sys:without-gcing
      (multiple-value-bind (,fp ,pc)
	  (sb-kernel:%caller-frame-and-pc)
	(setf ,fp (sb-di::descriptor-sap ,fp))
	(let ((,class 0))
	  (while ,fp
	    ,@body
	    (setf ,class (sb-sys:sap-int (sb-sys:sap-ref-sap ,fp 0)))
	    (multiple-value-setq (,pc ,fp) (x86-call-context ,fp))))))))

(defstatic |java/lang/VMThrowable.fillInStackTrace(Ljava/lang/Throwable;)| (throwable)
  (declare (ignore throwable))
  (with-global-monitor ()
    (sb-sys:without-gcing
     (let ((n 0))
       (do-stack (pc class)
	 (incf n))
       (let* ((v (make-array (* 3 n)))
	      (i 0)
	      (result
	       (make-cloak-instance "java/lang/VMThrowable"
		   "<init>(Ljava/lang/Object;)"
		 v)))
	 (do-stack (pc class)
	   #+(or)
	   (let ((c (nth-value 1 (sb-di::compute-lra-data-from-pc pc))))
	     (when c
	       (write (list c (gethash class *ugly-class-table*)))
	       (terpri)))
	   (setf (elt v i) (sb-sys:sap-int pc))
	   (setf (elt v (1+ i)) class)
	   (incf i 3))
	 (push (sb-ext:make-weak-pointer result) *ugly-vmthrowable-list*)
	 result)))))
