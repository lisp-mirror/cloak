(in-package :cloak)

;; (defun stringify-classpath (list)
;;   (with-output-to-string (s)
;;     (when list
;;       (princ (namestring (truename (car list))) s)
;;       (dolist (path (cdr list))
;; 	(princ #\: s)
;; 	(princ (namestring (truename path)) s)))))

(defun stringify-classpath (list)
  (with-output-to-string (s)
    (when list
      (let ((path (car list)))
	(princ (if path (namestring path) "") s))
      (dolist (path (cdr list))
	(princ #\: s)
	(princ (if path (namestring path) "") s)))))

(defstatic |java/lang/VMRuntime.mapLibraryName(Ljava/lang/String;)| (libname)
  (make-cloak-string
   (concatenate 'string
     "lib"
     (get-string-value libname)
     ".so")))

(defstatic
    |java/lang/VMRuntime.nativeLoad(Ljava/lang/String;Ljava/lang/ClassLoader;)|
    (filename cl)
  (declare (ignore cl))			;FIXME
  ;; FIXME: JNI_OnLoad aufrufen
  (block nil
    (let* ((str (get-string-value filename))
	   (key (or (sb-sys::unix-namestring str) str)))
      (with-global-monitor ()
	(cond
	  ((find key (vm.libraries *vm*) :test #'equal)
	    (return 1))
	  ((not (quick-probe-file str))
	    (return 0))
	  (t
	    (push key (vm.libraries *vm*)))))
      (handler-case
	  (handler-bind ((style-warning #'muffle-warning))
	    (with-localref-frame (16)
	      (let ((*%class%* (find-cloak-class nil "java/lang/Object"))
		    (sb-impl::*linkage-info*
		     (load-time-value (make-hash-table)))
		    (*pending-exception* nil))
		(sb-alien:load-shared-object str)))
	    1)
	(simple-error ()
	  0)))))

(defstatic |java/lang/VMRuntime.gc()| ()
  ;; XXX Mauve calls GC too often.  We don't have time for that.
  #+(or)
  (gc t))

;;; XXX
(defstatic |java/lang/VMRuntime.runFinalization()| ()
  )

;;; XXX
(defstatic |java/lang/VMRuntime.runFinalizationForExit()| ()
  )

(defstatic |java/lang/VMRuntime.exit(I)| (rc)
  ;; XXX need to do this in all threads
  (let ((vm *vm*))
    (with-global-monitor ()
      (while (vm.all-threads vm)
	(sb-thread:interrupt-thread (pop (vm.all-threads vm))
				    (lambda () (throw 'exit nil)))))
    (sb-thread:interrupt-thread (vm.initial-thread vm)
				(lambda () (throw 'exit rc))))
  (loop (sleep 3600)))

(defstatic |java/lang/VMRuntime.availableProcessors()| ()
  ;; XXX
  1)


(defstatic |java/lang/VMRuntime.freeMemory()| ()
;;  (warn "freeMemory() not implemented")
  (1- (expt 2 63)))

(defstatic |java/lang/VMRuntime.totalMemory()| ()
;;  (warn "totalMemory() not implemented")
  (1- (expt 2 63)))

(defstatic |java/lang/VMRuntime.maxMemory()| ()
  ;; XXX give eclipse what it wants to hear:
  64000000)

(defstatic |java/lang/VMRuntime.traceInstructions(Z)| (enable)
  (unless (zerop enable)
    (warn "traceInstructions() not implemented")))

(defstatic |java/lang/VMRuntime.traceMethodCalls(Z)| (enable)
  (unless (zerop enable)
    (warn "traceMethodCalls() not implemented")))

(defstatic |java/lang/VMRuntime.runFinalizersOnExit(Z)| (enable)
  (unless (zerop enable)
    (warn "runFinalizersOnExit() not implemented")))
