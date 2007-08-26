;;; totally out of date

(defvar *translate-safely*
    ;; The LispWorks compiler is buggy, it segfaults.  Test case:
    ;; (defun test ()
    ;;   (TAGBODY
    ;;    L6 (handler-bind ((error (lambda (c) (go L6)))) (GO l6)))) 
    ;; (compile 'test)
    #+lispworks :always
    #-lispworks :never)
(defvar *verbose-safely* t)
(defvar *tagbody-ilimit* 500)
(defvar *tagbody-vlimit* 100)           ;is this necessary?

(defun translate-safely-p (class method instructions)
  (flet ((complain (reason-fmt &rest args)
	   (when *verbose-safely*
	     (note "using safe mode for ~?: ~A.~A"
		   reason-fmt args (cls.name class) (cm.signature method)))
	   t))
    (let ((n (count-if-not #'null instructions))
	  (max-stack (cm.max-stack method))
	  (max-locals (cm.max-locals method)))
      (ecase *translate-safely*
	(:never nil)
	(:always t)
	(:auto
	  (cond
	    ((and *tagbody-ilimit* (> n *tagbody-ilimit*))
	      (complain "long method (~D instructions)" n)
	      t)
	    ((and *tagbody-vlimit*
		  (> (+ max-stack max-locals) *tagbody-vlimit*))
	      (complain "method with many locals (~D/~D)" max-stack max-locals)
	      t)
	    (t
	      nil)))))))


;;;; safe translation

(defun safe-go (pc)
  `(throw nil (values nil ,pc)))

(defun safe-stack-entry (i)
  `(elt %stack% ,i))

(defun safe-local-variable (i)
  `(elt %locals% ,i))

(defun translate-safely (*class* method instructions)
  (let ((ivector (make-array (length instructions))))
    (handler-bind (#+sbcl (sb-ext:compiler-note #'muffle-warning))
      (do-instructions (instruction instructions)
	(when (reachablep instruction)
	  (setf (elt ivector (index instruction))
		(compile nil (safely-translate-instruction instruction))))))
    (safe-function (cm.max-locals method)
		   (cm.max-stack method)
		   ivector
		   (cm.xtable method)
		   (cm.staticp method)
		   method)))

(defun safely-translate-instruction (instruction)
  `(lambda (%class% %locals% %stack% debug-info)
     (declare (ignorable %class% %locals% %stack%)
	      ;; make debug-info show up in stacktraces
	      (special debug-info))
     (catch nil
       (block nil
         ,(let ((*go* #'safe-go)
                (*stack-entry* #'safe-stack-entry)
                (*local-variable* #'safe-local-variable))
            (translate-instruction instruction))
         ,(if (next instruction)
              `(values nil ,(index (next instruction)))
              '(error "instruction returned"))))))

#+(or)
(defun safe-function (max-locals max-stack ivector xtable staticp info)
  (lambda (&rest args)
    (let* ((%class% *%class%*)		;XXX eek
	   (*class-context* (cons %class% *class-context*))
	   (locals (make-array max-locals))
	   (stack (make-array max-stack))
	   (pc 0)
	   result)
      (unless staticp			;XXX
	(push (%class (car args)) *class-context*))
      (for ((i :from 0)
	    (arg :in args))
	(setf (elt locals i) arg))
      (loop
	(block handle
	  (handler-bind
	      ((cloak-condition
		(lambda (wrapper)
		  (let ((c (maybe-unwrap wrapper)))
		    (dolist (block xtable)
		      (with-slots (start-pc end-pc handler-pc) block
			;; XXX get class from constant pool
			(let ((catch-class (catch-class %class% block)))
			  (when (and (<= start-pc pc)
				     (< pc end-pc)
				     (cloak-subclass-p (%class c) catch-class))
			    (setf (elt stack 0) c)
			    (setf pc handler-pc)
			    (return-from handle)))))))))
	    (multiple-value-setq (result pc)
	      (funcall (elt ivector pc) %class% locals stack (cons pc info)))
	    (unless pc
	      (return result))))))))


