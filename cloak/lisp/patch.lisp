(in-package :sb-c)

(define-optimization-quality propagate-constraints
    (if (>= speed compilation-speed)
	3
	0)
  ("no" "maybe" "yes" "yes"))

(defvar *enable-propagate-constraints-hack* nil)

(defun reoptimize-after-type-check-max ()
  (- *reoptimize-after-type-check-max*
     (if *enable-propagate-constraints-hack*
	 (policy *lexenv*
		 (max 0 (truncate (* (- compilation-speed speed) 1.5))))
	 0)))

(defun %instance-typep (object type)
  (sb-kernel:%typep object type))

(defun ir1-phases (component)
  (declare (type component component))
  (aver-live-component component)
  (let ((*constraint-number* 0)
	(loop-count 1)
	(*delayed-ir1-transforms* nil))
    (declare (special *constraint-number* *delayed-ir1-transforms*))
    (loop
      (when (or (null *enable-propagate-constraints-hack*)
		(policy *lexenv* (< compilation-speed speed)))
	(ir1-optimize-until-done component))
      (when (or (component-new-functionals component)
		(component-reanalyze-functionals component))
	(maybe-mumble "locall ")
	(locall-analyze-component component))
      (dfo-as-needed component)
      (when (or (null *enable-propagate-constraints-hack*)
		(policy *lexenv* (>= propagate-constraints 2)))
	(maybe-mumble "constraint ")
	(constraint-propagate component))
      (when (retry-delayed-ir1-transforms :constraint)
	(maybe-mumble "Rtran "))
      (flet ((want-reoptimization-p ()
	       (or (component-reoptimize component)
		   (component-reanalyze component)
		   (component-new-functionals component)
		   (component-reanalyze-functionals component))))
	(unless (and (want-reoptimization-p)
		     ;; We delay the generation of type checks until
		     ;; the type constraints have had time to
		     ;; propagate, else the compiler can confuse itself.
		     (< loop-count (- (reoptimize-after-type-check-max) 4)))
	  (maybe-mumble "type ")
	  (generate-type-checks component)
	  (unless (want-reoptimization-p)
	    (return))))
      (when (>= loop-count (reoptimize-after-type-check-max))
	(maybe-mumble "[reoptimize limit]")
	(event reoptimize-maxed-out)
	(return))
      (incf loop-count)))

  (ir1-finalize component)
  (values))

(defun invoke-with-timer (fn name)
  (let ((real0 (get-internal-real-time))
	(run0 (get-internal-run-time)))
    (multiple-value-prog1
	(funcall fn)
      (let ((real1 (get-internal-real-time))
	    (run1 (get-internal-run-time)))
	(when (plusp (+ (- real1 real0) (- run1 run0)))
	  (format t "~&; ~Dreal, ~Drun ~A~%"
		  (- real1 real0)
		  (- run1 run0)
		  name))))))

(defmacro with-timer (&body body)
  `(invoke-with-timer
    (lambda () ,@body)
    ,(write-to-string body :pretty nil :length 3 :level 3 :escape nil)))

(defun convert-to-environment-tn (tn tn-physenv)
  (declare (type tn tn) (type physenv tn-physenv))
  (aver (member (tn-kind tn) '(:normal :debug-environment)))
  (ecase (tn-kind tn)
    (:debug-environment
     (setq tn-physenv (tn-physenv tn))
     (let* ((2env (physenv-info tn-physenv)))
       (setf (ir2-physenv-debug-live-tns 2env)
             (delete tn (ir2-physenv-debug-live-tns 2env)))))
    (:normal
     (setf (tn-local tn) nil)
     (setf (tn-local-number tn) nil)))
  (setf (tn-kind tn) :component)
  (setf (tn-physenv tn) nil)
  (values))

(defun compute-save-set (vop live-bits)
  (declare (type vop vop) (type local-tn-bit-vector live-bits))
  (let ((live (bit-vector-copy live-bits)))
    (do ((r (vop-results vop) (tn-ref-across r)))
	((null r))
      (let ((tn (tn-ref-tn r)))
	(ecase (tn-kind tn)
	  ((:normal :debug-environment)
	   (setf (sbit live (tn-local-number tn)) 0))
	  ((:environment :component)))))
    live))
