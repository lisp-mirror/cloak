(in-package :cloak)

(defnative |microbench/Benchmark.getBytesConsed()| (this)
  (sb-ext:get-bytes-consed))

(defstatic |cloak/Util._break()| ()
  (break))

(defstatic |cloak/Util.prin1(Ljava/lang/Object;)| (o)
  (prin1 (data-for-output o)))

(defstatic |cloak/Util.eval(Ljava/lang/String;)| (expr)
  (eval (read-from-string (get-string-value expr))))

(defstatic |cloak/Util.println(Ljava/lang/Object;)| (o)
  (fresh-line)
  (prin1 (data-for-output o))
  (terpri))

(defun data-for-output (o)
  (cond
    ((nullp o) o)
    ((cloak-type-p o (find-cloak-class nil "java/lang/String"))
      (get-string-value o))
    ((cloak-type-p o (find-cloak-class nil "java/lang/Integer"))
      (call-cloak-method "intValue()" o))
    ((cloak-type-p o (find-cloak-class nil "java/lang/Long"))
      (call-cloak-method "longValue()" o))
    (t
      o)))

(dolist (s '(|cloak/Util.format_t(Ljava/lang/String;)|
	     |cloak/Util.format_t(Ljava/lang/String;Ljava/lang/Object;)|
	     |cloak/Util.format_t(Ljava/lang/String;Ljava/lang/Object;Ljava/lang/Object;)|
	     |cloak/Util.format_t(Ljava/lang/String;Ljava/lang/Object;Ljava/lang/Object;Ljava/lang/Object;)|
	     |cloak/Util.format_t(Ljava/lang/String;Ljava/lang/Object;Ljava/lang/Object;Ljava/lang/Object;Ljava/lang/Object;)|
	     |cloak/Util.format_t(Ljava/lang/String;Ljava/lang/Object;Ljava/lang/Object;Ljava/lang/Object;Ljava/lang/Object;Ljava/lang/Object;)|))
  (setf (fdefinition s)
	(lambda (str &rest args)
	  (apply #'format
		 t
		 (get-string-value str)
		 (mapcar #'data-for-output args)))))

(dolist (s '(|cloak/Util.format_nil(Ljava/lang/String;)|
	     |cloak/Util.format_nil(Ljava/lang/String;Ljava/lang/Object;)|
	     |cloak/Util.format_nil(Ljava/lang/String;Ljava/lang/Object;Ljava/lang/Object;)|
	     |cloak/Util.format_nil(Ljava/lang/String;Ljava/lang/Object;Ljava/lang/Object;Ljava/lang/Object;)|
	     |cloak/Util.format_nil(Ljava/lang/String;Ljava/lang/Object;Ljava/lang/Object;Ljava/lang/Object;Ljava/lang/Object;)|
	     |cloak/Util.format_nil(Ljava/lang/String;Ljava/lang/Object;Ljava/lang/Object;Ljava/lang/Object;Ljava/lang/Object;Ljava/lang/Object;)|))
  (setf (fdefinition s)
	(lambda (str &rest args)
	  (make-cloak-string
	   (apply #'format
		  nil
		  (get-string-value str)
		  (mapcar #'data-for-output args))))))

(dolist (s '(|cloak/Util.note(Ljava/lang/String;)|
	     |cloak/Util.note(Ljava/lang/String;Ljava/lang/Object;)|
	     |cloak/Util.note(Ljava/lang/String;Ljava/lang/Object;Ljava/lang/Object;)|
	     |cloak/Util.note(Ljava/lang/String;Ljava/lang/Object;Ljava/lang/Object;Ljava/lang/Object;)|
	     |cloak/Util.note(Ljava/lang/String;Ljava/lang/Object;Ljava/lang/Object;Ljava/lang/Object;Ljava/lang/Object;)|
	     |cloak/Util.note(Ljava/lang/String;Ljava/lang/Object;Ljava/lang/Object;Ljava/lang/Object;Ljava/lang/Object;Ljava/lang/Object;)|))
  (setf (fdefinition s)
	(lambda (str &rest args)
	  (apply #'note

		 (get-string-value str)
		 (mapcar #'data-for-output args)))))
