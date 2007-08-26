;;; FIXME!

(in-package :cloak)

(declaim (optimize (speed 3)
		   (safety 0)
		   (sb-c::recognize-self-calls 0)
		   (sb-ext:inhibit-warnings 0)))

(defmacro defsubst (name (&rest params) &body body)
  `(progn
     (defun ,name (,@params) ,@body)
     (define-compiler-macro ,name (&rest args)
       (cons '(lambda (,@params) ,@body) args))))

(defconstant 2^32 (expt 2 32))
(defconstant 2^31 (expt 2 31))
(defconstant +int-min+ (- 2^31))
(defconstant +int-max+ (1- 2^31))
(defconstant +int-range+ 2^32)

(defconstant 2^64 (expt 2 64))
(defconstant 2^63 (expt 2 63))
(defconstant +long-min+ (- 2^63))
(defconstant +long-max+ (1- 2^63))
(defconstant +long-range+ 2^64)

(defun canonicalize-int (n)
  #+(or)
  (loop
    (setf n (cond
	      ((> n +int-max+) (- n +int-range+))
	      ((< n +int-min+) (+ n +int-range+))
	      (t (return n)))))
  (let ((k (nth-value 1 (round n 2^32))))
    (if (eql k 2^31)
	(- k)
	k)))

(defun canonicalize-long (n)
  #+(or)
  (loop
    (setf n (cond
	      ((> n +long-max+) (- n +long-range+))
	      ((< n +long-min+) (+ n +long-range+))
	      (t (return n)))))
  (let ((k (nth-value 1 (round n 2^64))))
    (if (eql k 2^63)
	(- k)
	k)))

(defun canonicalize-n (n size)
  #+(or)
  (loop
    (setf n (cond
	      ((> n (1- (expt 2 (1- size)))) (- n (expt 2 size)))
	      ((< n (- (expt 2 (1- size)))) (+ n (expt 2 size)))
	      (t (return n)))))
  (let ((k (nth-value 1 (round n (expt 2 size)))))
    (if (eql k (expt 2 (1- size)))
	(- k)
	k)))

(defun canonicalize-n-unsigned (n size)
  #+(or)
  (let ((m (expt 2 size)))
    (loop
      (setf n (cond
		((>= n m) (- n m))
		((< n 0) (+ n m))
		(t (return n))))))
  (nth-value 1 (floor n (expt 2 size))))
