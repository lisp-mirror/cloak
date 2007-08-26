(in-package :cloak)

(defun fast-copy-structure (structure)
  (declare (type structure-object structure)
	   (optimize (speed 3) (safety 0)))
  (let* ((len (sb-kernel::%instance-length structure))
	 (res (sb-kernel::%make-instance len)))
    (declare (type fixnum len))
    (setf (sb-kernel::%instance-layout res)
	  (sb-kernel::%instance-layout structure))
    (dotimes (i (1- len))
      (declare (type fixnum i))
      (setf (sb-kernel::%raw-instance-ref/word res i)
	    (sb-kernel::%raw-instance-ref/word structure i)))
    res))

(defun %clone-object (c)
  (declare (type structure-object c)
	   (optimize (speed 3) (safety 0)))
  (let ((result (fast-copy-structure c)))
    (setf (co.monitor result) 0)
    (setf (co.hash-code result) (generate-hash-code))
    result))

(defstatic |java/lang/VMObject.getClass(Ljava/lang/Object;)| (o)
  (class-object (%class o)))

(defun cloak-array-p (object)
  (typep object 'cloak-array))

(defstatic |java/lang/VMObject.clone(Ljava/lang/Cloneable;)| (c)
  (let ((result (%clone-object c)))
    (when (cloak-array-p c)
      (setf (co.data result) (copy-seq (co.data c))))
    result))

(defnative |java/lang/VMObject.notify(Ljava/lang/Object;)| (object)
  (monitor-notify (checked-get-fat-monitor object)))

(defstatic |java/lang/VMObject.notifyAll(Ljava/lang/Object;)| (object)
  (monitor-notify-all (checked-get-fat-monitor object)))

(defstatic |java/lang/VMObject.wait(Ljava/lang/Object;JI)| (object ms ns)
  (unless (and (>= ms 0) (<= 0 ns 999999))
    (throw-exception "java/lang/IllegalArgumentException"))
  (let ((m (checked-get-fat-monitor object)))
    (handler-case
	(progn
	  (maybe-throw-interruptedexception)
	  (if (zerop (+ ms ns))
	      (monitor-wait m)
	      (handler-case
		  (sb-ext:with-timeout (min (+ (/ ms 1000) (/ ns 1000000000))
					    ;; mehr will sbcl nicht:
					    (1- (expt 2 29)))
		    (monitor-wait m))
		(sb-ext:timeout ()))))
      (interruption ()
	(maybe-throw-interruptedexception)))))
