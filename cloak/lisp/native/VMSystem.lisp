(in-package :cloak)

;; Die Spec verlangt von uns hier nur IOOBE, nicht speziall AIOOBE, aber
;; AcuniaVectorTest ist da waehlerisch.
(defstatic
    |java/lang/VMSystem.arraycopy(Ljava/lang/Object;ILjava/lang/Object;II)|
    (src srcstart dst dststart len)
  (assert-nonnull src)
  (assert-nonnull dst)
  (unless (and (typep (%class src) 'cloak-array-class)
	       (typep (%class dst) 'cloak-array-class)
	       (let ((srctype (cls.element-type (%class src)))
		     (dsttype (cls.element-type (%class dst))))
		 (or ;; either the same primitive class
		     (eq srctype dsttype)
		     ;; or both reference types
		     (and (typep srctype 'reference-type)
			  (typep dsttype 'reference-type)))))
    (throw-exception "java/lang/ArrayStoreException"))
  (when (minusp len)
    (throw-exception "java/lang/ArrayIndexOutOfBoundsException" "negative length"))
  (unless (zerop len)
    (let ((dstend (+ dststart len))
	  (srcend (+ srcstart len))
	  (exceptionp nil))
      (unless (and (<= 0 srcstart)
		   (< srcstart (length (co.data src)))
		   (<= srcend (length (co.data src))))
	(throw-exception "java/lang/ArrayIndexOutOfBoundsException" "(src)"))
      (unless (and (<= 0 dststart)
		   (< dststart (length (co.data dst)))
		   (<= dstend (length (co.data dst))))
	(throw-exception "java/lang/ArrayIndexOutOfBoundsException" "(dst)"))
      (unless (or (typep (cls.element-type (%class src)) 'primitive-class)
		  (cloak-subclass-p (cls.element-type (%class src))
				    (cls.element-type (%class dst))))
	(let ((srcdata (co.data src))
	      (dsttype (cls.element-type (%class dst))))
	  (for* ((s :from srcstart :below srcend)
		 (o = (elt srcdata s)))
	    (unless (or (nullp o) (cloak-type-p o dsttype))
	      (setf exceptionp t)
	      (setf len (- s srcstart))
	      (return)))))
      (move (co.data src) (co.data dst) srcstart dststart len)
      (when exceptionp
	(throw-exception "java/lang/ArrayStoreException")))))

(defnative |java/lang/VMSystem.identityHashCode(Ljava/lang/Object;)| (object)
  (if (nullp object)
      0
      (co.hash-code object)))

;;; ?
(defstatic |java/lang/VMSystem.isWordsBigEndian()| ()
  1)

(defstatic |java/lang/VMSystem.currentTimeMillis()| ()
  (+ (truncate (* (- (get-internal-real-time) (vm.startup-time-internal *vm*))
                  1000)
               internal-time-units-per-second)
     (vm.startup-time *vm*)))

;;; from CXML:
(defun move (from to from-start to-start length)
  (if (< to-start from-start)
      (loop
	  repeat length
	  for i from from-start
	  for j from to-start
	  do (setf (aref to j) (aref from i)))
      (loop
	  repeat length
	  for i downfrom (+ from-start length -1)
	  for j downfrom (+ to-start length -1)
	  do (setf (aref to j) (aref from i)))))

(defun j/l/system ()
  (find-cloak-class nil "java/lang/System"))

(defstatic |java/lang/VMSystem.setIn(Ljava/io/InputStream;)| (in)
  (setf (getstatic "in" (j/l/system)) in))

(defstatic |java/lang/VMSystem.setOut(Ljava/io/PrintStream;)| (out)
  (setf (getstatic "out" (j/l/system)) out))

(defstatic |java/lang/VMSystem.setErr(Ljava/io/PrintStream;)| (err)
  (setf (getstatic "err" (j/l/system)) err))
