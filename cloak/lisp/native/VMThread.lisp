(in-package :cloak)

(defun write-throwable (o &optional (stream t))
  (print-unreadable-object (o stream :identity t)
    (let* ((message (call-cloak-method "getMessage()" o))
	   (str (if (nullp message)
		    nil
		    (get-string-value message)))
	   (name (substitute #\. #\/ (cls.name (%class o)))))
      (format stream "~A~@[: ~A~]" name str))))

(defun write-throwable-to-string (o)
  (with-output-to-string (s)
    (write-throwable o s)))


;;;; thread objects

(defun thread-vmthread (thread)
  (getfield "vmThread" thread))

(defun vmthread-thread-object (vmthread)
  (getfield "thread" vmthread))

(defun vmthread-vmdata (vmthread)
  (getfield "vmData" vmthread))
(defun (setf vmthread-vmdata) (newval vmthread)
  (setf (getfield "vmData" vmthread) newval))

(defun vmthread-index (vmthread)
  (getfield "vmDataIndex" vmthread))
(defun (setf vmthread-index) (newval vmthread)
  (setf (getfield "vmDataIndex" vmthread) newval))


;;;; heavyweight monitors

(defstruct
    monitor
  ;; (monitor (:constructor make-monitor ()))
  (mutex (sb-thread:make-mutex))
  (queue (sb-thread:make-waitqueue))
  (count 0 :type fixnum))

(defun make-upgraded-monitor (count)
  (let ((m (make-monitor)))
    (sb-thread:get-mutex (monitor-mutex m))
    (setf (monitor-count m) count)
    m))

(defun monitor-debug (str m)
  (unless (eq m (vm.global-monitor *vm*))
    (let ((*print-tostring-p* nil))
      (format t "~& ~A(~D, ~A)~%" str sb-thread:*current-thread* m)
      (force-output))))

(defun enter-monitor (m)
  (cond
    ((eql (sb-thread:mutex-value (monitor-mutex m))
	  sb-thread:*current-thread*)
      #+monitor-debug (monitor-debug ">>> ENTER-MONITOR -- OK" m))
    (t
      #+monitor-debug (monitor-debug "!!! ENTER-MONITOR -- GET-MUTEX" m)
      (sb-thread:get-mutex (monitor-mutex m))
      #+monitor-debug (monitor-debug ">>> ENTER-MONITOR -- GOT-MUTEX" m)
      (assert (zerop (monitor-count m)))))
  (incf (monitor-count m)))

(defun exit-monitor (m)
  (cond
    ((eql (sb-thread:mutex-value (monitor-mutex m))
	  sb-thread:*current-thread*)
      #+monitor-debug (monitor-debug "<<< EXIT-MONITOR" m)
      (when (zerop (decf (monitor-count m)))
	#+monitor-debug (monitor-debug "!!! RELEASE-MUTEX" m)
	(sb-thread:release-mutex (monitor-mutex m)))
      t)
    (t
      #+monitor-debug (monitor-debug "exit-monitor -- NOT-MINE" m)
      nil)))

(defun monitor-wait (m)
  (let ((count (monitor-count m)))
    (setf (monitor-count m) 0)
    #+monitor-debug (monitor-debug "MONITOR-WAIT" m)
    (unwind-protect
	(sb-thread:condition-wait (monitor-queue m) (monitor-mutex m))
      #+monitor-debug (monitor-debug "MONITOR-WAIT -- unwind" m)
      (setf (monitor-count m) count))))

(defun monitor-notify (m)
  #+monitor-debug (monitor-debug "NOTIFY" m)
  (sb-thread:condition-notify (monitor-queue m)))

(defun monitor-notify-all (m)
  #+monitor-debug (monitor-debug "NOTIFY-ALL" m)
  (sb-thread:condition-broadcast (monitor-queue m)))

(defun holds-monitor-p (th m)
  (eql th (sb-thread:mutex-value (monitor-mutex m))))


;;;; lightweight locks

(declaim (notinline exit-object-monitor))
(declaim (notinline enter-object-monitor))

(declaim (inline enter-object-monitor))
(defun enter-object-monitor (object)
  (declare (optimize (speed 3) (safety 0)))
  (let ((id *thread-id*))
    (when (zerop (%cmpxchg object 2 0 id))
      ;; either not a flat lock, or flat but already locked
      (let ((thing (co.monitor object)))
	(cond
	  ((not (typep thing 'fixnum))
	    ;; it's a fat lock
	    (enter-monitor thing))
	  ((eql (logand thing #xffff) id)
	    ;; we own the flat lock.  can we just increment?
	    (if (< thing #x1000000)
		;; yes
		(setf (co.monitor object) (+ thing #x10000))
		;; no, upgrade to fat lock
		(setf (co.monitor object)
		      (make-upgraded-monitor (+ 2 (ash thing -16))))))
	  (t
	    ;; oops, a flat lock and we don't own it.  spinlock. :-(
	    (loop
	      (yield)
	      (if (zerop (%cmpxchg object 2 0 id))
		  (let ((thing (co.monitor object)))
		    (when (not (typep thing 'fixnum))
		      (enter-monitor thing)
		      (return)))
		  (let ((fat (make-monitor)))
		    (enter-monitor fat)
		    (setf (co.monitor object) fat)
		    (return))))))))))

(declaim (inline exit-object-monitor))
(defun exit-object-monitor (object)
  (declare (optimize (speed 3) (safety 0)))
  (let ((id *thread-id*)
	(thing (co.monitor object)))
    (cond
      ((eq thing id)			;nonportable but inline
	(setf (co.monitor object) 0)
	t)
      ((not (typep thing 'fixnum))
	(exit-monitor thing))
      ((eql (logand thing #xffff) id)
	(setf (co.monitor object) (- thing #x10000))
	t)
      (t
	nil))))

(defun holds-object-monitor-p (vmthread object)
  (let ((thing (co.monitor object)))
    (cond
      ((typep thing 'fixnum)
	(eql (vmthread-index vmthread) (logand thing #xffff)))
      (t
	(holds-monitor-p (vmthread-vmdata vmthread) thing)))))

(defun checked-get-fat-monitor (object)
  (unless (or (null *vmthread-object*) ;XXX bootstrapping problem
	      (holds-object-monitor-p *vmthread-object* object))
    (throw-exception "java/lang/IllegalMonitorStateException"))
  (let ((m (co.monitor object)))
    (if (typep m 'fixnum)
	(setf (co.monitor object)
	      (make-upgraded-monitor (1+ (ash m -16))))
	m)))


;;;; dealing with threads

(define-condition resumption (condition) ())
(define-condition interruption (condition) ())

(defun invoke-cloak-thread (thread-object fn &optional cleanup)
  (let* ((*thread-object* thread-object)
	 (*vmthread-object*
	  (unless (nullp thread-object)
	    (getfield "vmThread" thread-object)))
	 (*default-pathname-defaults* (vm.default-pathname-defaults *vm*))
	 (*print-length* 10)
	 (*print-level* 3)
	 (*print-circle* nil)
	 (*debugger-hook*
	  ;; better reset *print-tostring-p* before entering the debugger
	  (let ((outer-debugger-hook *debugger-hook*))
	    (lambda (c hook)
	      (declare (ignore hook))
	      (let ((*print-tostring-p* nil)
		    (*debugger-hook* outer-debugger-hook))
		(invoke-debugger c))))))
    (unless (nullp thread-object)
      (assert (not (nullp *vmthread-object*))))
    (with-jni-env ()
      (unwind-protect
	  (fast-handler-case
	   (handler-bind ((sb-kernel::undefined-alien-function-error
			   (lambda (c)
			     (throw-exception "java/lang/UnsatisfiedLinkError"
				 "~A"
			       (princ-to-string c))))
			  #+(or)
			  (sb-kernel::memory-fault-error
			   (lambda (c)
			     (print c)
			     (force-output)
			     (sb-debug::backtrace)))
			  #+sigsegv-hack
			  (sb-kernel::memory-fault-error #'sigsegv-handler))
	     (disable-floating-point-traps)
	     (unwind-protect
		 (funcall fn)
	       ;; for initial thread:
	       (enable-floating-point-traps)))
	   (lambda (o)
	     (let ((thread-death
		    (find-cloak-class nil "java/lang/ThreadDeath"))
		   (name (if (nullp thread-object)
			     "initial thread"
			     (getfield "name" *thread-object*))))
	       (unless (cloak-type-p o thread-death)
		 (format *trace-output* "~&;;; ~A in ~A~%"
			 (write-throwable-to-string o)
			 name)
		 (call-cloak-method "printStackTrace()" o)))))
	(when cleanup
	  (funcall cleanup))))))

(defun interruptedp (thread-object)
  (eq (getfield "vmDataInterrupted" (thread-vmthread thread-object)) t))

(defun (setf interruptedp) (newval thread-object)
  (setf (getfield "vmDataInterrupted" (thread-vmthread thread-object)) newval))


;;;; VMThread methods

(defstatic |java/lang/VMThread.currentThread()| ()
  *thread-object*)

(defstatic |java/lang/VMThread.holdsLock(Ljava/lang/Object;)| (object)
  (holds-object-monitor-p *vmthread-object* object))

(defnative |java/lang/VMThread.interrupt()| (vmthread)
  (with-global-monitor ()		;XXX wieso?
    (flet ((doit ()
	     (when *vm*
	       (setf (interruptedp *thread-object*) t)
	       ;; to avoid race conditions, section of code going to
	       ;; sleep after checking for interruptedness also establish
	       ;; a handler for this condition, which is otherwise ignored
	       (signal 'interruption))))
      ;; wenn noch nicht gestartet, tue nichts.  steht nicht im javadoc,
      ;; aber jdk 1.4 scheint das so zu machen.
      (when (vmthread-vmdata vmthread)	;XXX race condition
	(let ((lisp-thread (vmthread-vmdata vmthread)))
	  (if (eql lisp-thread sb-thread:*current-thread*)
	      (doit)
	      (sb-thread:interrupt-thread lisp-thread #'doit)))))))

(defun maybe-throw-interruptedexception ()
  (when (interruptedp *thread-object*)
    (setf (interruptedp *thread-object*) nil)
    (throw-exception "java/lang/InterruptedException")))

(defnative |java/lang/VMThread.sleep(JI)| (ms ns)
  (unless (and (>= ms 0) (<= 0 ns 999999))
    (throw-exception "java/lang/IllegalArgumentException"))
  (handler-case
      (progn
	(maybe-throw-interruptedexception)
	(sb-alien:with-alien
	    ((req (sb-alien:struct sb-unix::timespec))
	     (rem (sb-alien:struct sb-unix::timespec)))
	  (setf (sb-alien:slot req 'sb-unix::tv-sec)
		(min (truncate ms 1000) (1- (expt 2 31))))
	  (setf (sb-alien:slot req 'sb-unix::tv-nsec)
		(+ ns (* (mod ms 1000) 1000000)))
	  (while (eql sb-unix:EINTR
		  (nth-value
		   1
		   (sb-unix::int-syscall
		    ("nanosleep" (* (sb-alien:struct sb-unix::timespec))
				 (* (sb-alien:struct sb-unix::timespec)))
		    (sb-alien:addr req) (sb-alien:addr rem))))
	    (rotatef req rem))
	  (force-output)))
    (interruption ()
      (force-output)
      (maybe-throw-interruptedexception))))

(sb-alien:define-alien-routine ("sched_yield" %sched-yield)
    sb-alien:unsigned-long)

(defnative |java/lang/VMThread.start(J)| (vmthread stacksize)
  (declare (ignore stacksize))
  #+(or)
  (throw-exception "java/lang/RuntimeException" "no threads here")
  (let ((thread-object (getfield "thread" vmthread))
	(dpd *default-pathname-defaults*)
	(vm *vm*))
    ;; I think we need to set this here, since it would be done in the parent
    ;; by VMThread.java, thus possibly too late:
    (setf (getfield "vmThread" thread-object) vmthread)
    ;; Hier haben wir race conditions.  vm.all-threads wird erst nach dem
    ;; starten des neuen threads gemacht.  Andere Threads sehen die Aenderung
    ;; u.U. zu spaet.
    (sb-thread:make-thread
     (lambda ()
       (let ((*thread-id* 1)		;corrected below
	     (*vm* vm)
	     (*default-pathname-defaults* dpd))
	 (with-global-monitor ()
	   (loop
	       while (member *thread-id* (vm.all-thread-ids vm))
	       do
		 (incf *thread-id*)
		 (assert (< *thread-id* #.(expt 2 16))))
	   (push *thread-id* (vm.all-thread-ids vm))
	   (push sb-thread:*current-thread* (vm.all-threads vm)))
	 (catch 'exit
	   (invoke-cloak-thread
	    thread-object
	    (lambda ()
	      (setf (vmthread-vmdata vmthread) sb-thread:*current-thread*)
	      (setf (vmthread-index vmthread) *thread-id*)
	      (call-cloak-method "run()" vmthread)
	      (setf (getfield "vmThread" thread-object) +null+)))
	   (with-global-monitor ()	;XXX wieso?
	     (setf (vmthread-vmdata vmthread) nil) ;race condition, s.u.
	     (setf *vm* nil)
	     (setf (vm.all-threads vm)
		   (delete sb-thread:*current-thread* (vm.all-threads vm)))
	     (setf (vm.all-thread-ids vm)
		   (delete *thread-id* (vm.all-thread-ids vm)))))))
     :name (get-string-value (getfield "name" thread-object)))
    ;; FIXME, siehe oben
    #+nil
    (until (getfield "vmThread" thread-object)
      (%sched-yield))))

(defnative |java/lang/VMThread.isInterrupted()| (vmthread)
  (if (interruptedp (vmthread-thread-object vmthread)) 1 0))

(defstatic |java/lang/VMThread.interrupted()| ()
  (prog1
      (if (interruptedp *thread-object*) 1 0)
    (setf (interruptedp *thread-object*) nil)))

(defnative |java/lang/VMThread.nativeSetPriority(I)| (vmthread priority)
  (declare (ignore priority)))

(defnative |java/lang/VMThread.nativeStop(Ljava/lang/Throwable;)|
    (vmthread throwable)
  (whereas ((th (vmthread-vmdata vmthread))) ;race condition?
    (sb-thread:interrupt-thread
     th
     (lambda () (when *vm* (throw-exception throwable))))))

(defnative |java/lang/VMThread.resume()| (vmthread)
  (whereas ((th (vmthread-vmdata vmthread))) ;race condition?
    (sb-thread:interrupt-thread
     th
     (lambda () (when *vm* (signal 'resumption))))))

(defnative |java/lang/VMThread.suspend()| (vmthread)
  (handler-case
      (loop (sleep 6000))
    (resumption ())))

(defstatic |java/lang/VMThread.yield()| ()
  (yield))
