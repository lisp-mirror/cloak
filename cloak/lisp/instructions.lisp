;;;; Copyright (c) 2003-2006 David Lichteblau <david@lichteblau.com>
;;;;
;;;; This program is free software; you can redistribute it and/or modify
;;;; it under the terms of the GNU General Public License as published by
;;;; the Free Software Foundation; either version 2 of the License, or
;;;; (at your option) any later version.
;;;;
;;;; This program is distributed in the hope that it will be useful,
;;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;;; GNU General Public License for more details.
;;;;
;;;; You should have received a copy of the GNU General Public License
;;;; along with this program; if not, write to the Free Software
;;;; Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

(in-package :cloak)

(defmacro return1 (value)
  `(return (values ,value)))

(defmacro define-load-op (name type)
  `(defop (,name (i :ushort))
       (lambda (i)
	 (declare (ignore i))
	 #+(or)
	 (unless (eq (elt (i.locals *this-instruction*) i) ,type)
	   (class-format-error "invalid load: wanted ~A, but found ~A"
			       ,type
			       (elt (i.locals *this-instruction*) i)))
	 (list :out (list ,type)))
     (:replace
      (replace-instruction
       *this-instruction*
       `((move ,(%push) ,(local-variable i ,type)))))))

(define-simple-instruction (aaload)
    ((array (:array :object)) (index :int) &out :object)
  `(checked-elt (co.data (maybe-assert-nonnull ,array)) ,index t))

(define-simple-instruction (aastore)
    ((array (:array :object)) (index :int) (value :object))
  `(checked-set-elt ,value
                    (co.data (maybe-assert-nonnull ,array))
                    ,index
                    t))

(define-simple-instruction (aconst_null) (&out :object)
  `+null+)

(define-instruction-macro (aload i) ()
  `(wide-aload ,i))
(define-load-op wide-aload :object)

(define-instruction-macro (aload_0) () `(aload 0))
(define-instruction-macro (aload_1) () `(aload 1))
(define-instruction-macro (aload_2) () `(aload 2))
(define-instruction-macro (aload_3) () `(aload 3))

(defun safe-make-array (count &key initial-element (element-type t))
  (when (minusp count)
    (throw-exception "java/lang/NegativeArraySizeException"))
  (unless (< count array-dimension-limit)
    (throw-exception "java/lang/OutOfMemoryError"
	"array-dimension-limit exceeded"))
  (make-array count
	      :initial-element initial-element
	      :element-type element-type))

(define-simple-instruction (anewarray (ib :ushort))
    ((count :int) &out (:array :object))
  (pushnew ib (cm.class-ids *method*))
  `(do-make-cloak-array
       (find-array-class-for (svref %pool% ,ib))
     (safe-make-array ,count :initial-element +null+)))

(defop (areturn) (:in :object)
  (:parser
   (remove-successor *this-instruction*))
  (:in (in) (list (%pop)))
  (:body
   `(return1 ,in)))

(define-simple-instruction (arraylength)
    ((array (:array :object)) &out :int)
  `(length (the (simple-array * (*)) (co.data (maybe-assert-nonnull ,array)))))

(defun compute-locals (type i)
  (let ((v (copy-vector (i.locals *this-instruction*))))
    (setf (elt v i) type)
    v))

(defun %verify-stack (stack-in)
  (verify-stack *this-instruction* stack-in))

(defmacro define-store-op (name type)
  `(defop (,name (i :ushort))
       (lambda (i)			;(i ushort)
	 (let ((actual-type (vector-peek (i.stack *this-instruction*))))
	   (%verify-stack (list ,type))
	   (list :n-stack-in 1
		 :locals-out (compute-locals actual-type i))))
     (:replace
      (replace-instruction
       *this-instruction*
       `((move ,(local-variable i ,type) ,(%pop)))))))

(define-instruction-macro (astore i) ()
  `(wide-astore ,i))
(define-store-op wide-astore :object)

(define-instruction-macro (astore_0) () `(astore 0))
(define-instruction-macro (astore_1) () `(astore 1))
(define-instruction-macro (astore_2) () `(astore 2))
(define-instruction-macro (astore_3) () `(astore 3))

#+nil
(defun resolve-exceptions (calling-class method)
  (let ((declaring-class
	 (find-referenced-class
	  calling-class
	  (cf.name (cm.declaring-class-file method)))))
    (mapcar (curry #'class-from-pool declaring-class) (cm.exceptions method))))

(defop (athrow)
    (lambda ()
      (let* ((stack (i.stack *this-instruction*))
	     (object (vector-peek stack)))
	(unless (eq object :object)
	  (class-format-error "dingsda"))
	(list :n-stack-in (length stack) ;XXX (?)
	      :out (list object))))
  (:parser
   (remove-successor *this-instruction*))
  (:in (in) (list (%pop)))
  (:body
   `(%throw-exception ,in)))

(define-simple-instruction (baload)
    ((array (:array :byte)) (index :int) &out :int)
  `(checked-elt (co.data (maybe-assert-nonnull ,array)) ,index '(signed-byte 8)))

(defsubst ub8-to-sb8 (x)
  (if (>= x 128)
      (- x 256)
      x))

(defsubst ub16-to-sb16 (x)
  (if (>= x #.(expt 2 15))
      (- x #.(expt 2 16))
      x))

(define-simple-instruction (bastore)
    ((array (:array :byte)) (index :int) (value :int))
  ;; XXX schoen ist das nicht
  ;; XXX fixme for boolean
  `(checked-set-elt (ub8-to-sb8 (logand ,value #xff))
                    (co.data (maybe-assert-nonnull ,array))
                    ,index
                    '(signed-byte 8)))

(define-simple-instruction (bipush (byte :byte)) (&out :int)
  byte)

(define-simple-instruction (caload)
    ((array (:array :char)) (index :int) &out :int)
  `(checked-elt (co.data (maybe-assert-nonnull ,array))
		,index
		'(unsigned-byte 16)))

(defsubst canonicalize-char (c)
  (logand c #xffff))

(define-simple-instruction (castore)
    ((array (:array :char)) (index :int) (value :int))
  `(checked-set-elt (canonicalize-char ,value)
                    (co.data (maybe-assert-nonnull ,array))
                    ,index
                    '(unsigned-byte 16)))

(define-simple-instruction (checkcast (ib :ushort))
    ((instance :object) &out :object)
  ;; FUNCALL
  (pushnew ib (cm.class-ids *method*))
  (let ((name (cf.get-constant-class *class-file* ib)))
    `(progn
       (unless (or (nullp ,instance)
		   (cloak-subclass-p (%class ,instance)
				     (svref %pool% ,ib)))
	 (throw-exception "java/lang/ClassCastException"
	     "~A/~A" ,instance ,name))
       ,instance)))

(define-simple-instruction (d2f) ((value :double) &out :float)
  `(coerce ,value 'single-float))

(define-simple-instruction (d2i) ((value :double) &out :int)
  `(d2i ,value))

(define-simple-instruction (d2l) ((value :double) &out :long)
  `(d2l ,value))

(define-simple-instruction (dadd)
    ((v1 :double) (v2 :double) &out :double)
  `(d+ ,v1 ,v2))

(define-simple-instruction (daload)
    ((array (:array :double)) (index :int) &out :double)
  `(checked-elt (co.data (maybe-assert-nonnull ,array)) ,index 'double-float))

(define-simple-instruction (dastore)
    ((array (:array :double)) (index :int) (value :double))
  `(checked-set-elt ,value
                    (co.data (maybe-assert-nonnull ,array))
                    ,index
                    'double-float))

;; XXX geht das nicht besser?
(define-simple-instruction (dcmpg)
    ((v1 :double) (v2 :double) &out :int)
  ;; this differs from dcmpl only in the NaN case
  `(cond
     ((or (nanp ,v1) (nanp ,v2)) 1)
     ((< ,v1 ,v2) -1)
     ((= ,v1 ,v2) 0)
     ((> ,v1 ,v2) 1)
     (t (error "oops"))))

;; XXX geht das nicht besser?
(define-simple-instruction (dcmpl)
    ((v1 :double) (v2 :double) &out :int)
  ;; this differs from dcmpg only in the NaN case
  `(cond
     ((or (nanp ,v1) (nanp ,v2)) -1)
     ((< ,v1 ,v2) -1)
     ((= ,v1 ,v2) 0)
     ((> ,v1 ,v2) 1)
     (t (error "oops"))))

(define-simple-instruction (dconst_0) (&out :double)
  0.0d0)

(define-simple-instruction (dconst_1) (&out :double)
  1.0d0)

(define-simple-instruction (ddiv)
    ((v1 :double) (v2 :double) &out :double)
  `(d/ ,v1 ,v2))

(define-instruction-macro (dload i) ()
  `(wide-dload ,i))
(define-load-op wide-dload :double)

(define-instruction-macro (dload_0) () `(dload 0))
(define-instruction-macro (dload_1) () `(dload 1))
(define-instruction-macro (dload_2) () `(dload 2))
(define-instruction-macro (dload_3) () `(dload 3))

(define-simple-instruction (dmul)
    ((v1 :double) (v2 :double) &out :double)
  `(d* ,v1 ,v2))

(define-simple-instruction (dneg) ((value :double) &out :double)
  `(dneg ,value))

(define-simple-instruction (drem)
    ((v1 :double) (v2 :double) &out :double)
  `(drem ,v1 ,v2))

(defop (dreturn) (:in :double)
  (:parser
   (remove-successor *this-instruction*))
  (:in (in) (list (%pop)))
  (:body
   `(return1 ,in)))

(define-instruction-macro (dstore i) ()
  `(wide-dstore ,i))
(define-store-op wide-dstore :double)

(define-instruction-macro (dstore_0) () `(dstore 0))
(define-instruction-macro (dstore_1) () `(dstore 1))
(define-instruction-macro (dstore_2) () `(dstore 2))
(define-instruction-macro (dstore_3) () `(dstore 3))

(define-simple-instruction (dsub)
    ((v1 :double) (v2 :double) &out :double)
  `(d- ,v1 ,v2))

(defun vector-peek (vector &optional (i 0))
  (let ((j (- (fill-pointer vector) i 1)))
    (if (>= j 0)
	(elt vector j)
	nil)))

(defun type1p (type)
  (not (type2p type)))

(defun type2p (type)
  (member type '(:long :double)))

(defun assert-type1 (type)
  (unless (type1p type)
    (class-format-error "inappropriate data on stack: 4.8.2(a)")))

(defop (dup)
    (lambda ()
      (let* ((stack (i.stack *this-instruction*)))
	(let ((a (vector-peek stack)))
	  (assert-type1 a)
	  (list :n-stack-in 1 :out (list a a)))))
  (:replace
   (let ((v1 (%pop))
	 (w1 (%push)) (w2 (%push)))
     (replace-instruction *this-instruction*
			  `((move ,w2 ,v1)
			    (move ,w1 ,w2))))))

(defun %tmp (copy-of)
  (let ((tmp (gensym (concatenate 'string (symbol-name copy-of) "-"))))
    (setf (getf (symbol-plist tmp) 'java-type)
	  (getf (symbol-plist copy-of) 'java-type))
    (push tmp *method-local-variables*)
    tmp))

(defop (dup_x1)
    (lambda ()
      (let* ((stack (i.stack *this-instruction*)))
	(let ((a (vector-peek stack 0))
	      (b (vector-peek stack 1)))
	  (assert-type1 a)
	  (assert-type1 b)
	  (list :n-stack-in 2 :out (list a b a)))))
  (:replace
   (let* (#|-------|#  (v1 (%pop))
	  (v2 (%pop))
	  (w3 (%push)) (w2 (%push)) (w1 (%push))
	  (tmp (%tmp v1)))
     (replace-instruction *this-instruction*
			  `((move ,tmp ,v1)
			    (move ,w1 ,v1)
			    (move ,w2 ,v2)
			    (move ,w3 ,tmp))))))

(defop (dup_x2)
    (lambda ()
      (let* ((stack (i.stack *this-instruction*)))
	(let ((a (vector-peek stack 0))
	      (b (vector-peek stack 1))
	      (c (vector-peek stack 2)))
	  (assert-type1 a)
	  (cond
	    ((type1p b)
	      (assert-type1 c)
	      (setf (i.variant *this-instruction*) 1)
	      (list :n-stack-in 3 :out (list a c b a)))
	    (t
	      (setf (i.variant *this-instruction*) 2)
	      (list :n-stack-in 2 :out (list a b a)))))))
  (:replace
   (ecase (i.variant *this-instruction*)
     (1
       (let* (#|-------|#  #|-------|#  (v1 (%pop))
	      #|-------|#  (v2 (%pop))
	      (v3 (%pop))
	      (w4 (%push)) (w3 (%push)) (w2 (%push)) (w1 (%push))
	      (top (%tmp v1)))
	 (replace-instruction *this-instruction*
			      `((move ,top ,v1)
				(move ,w1 ,v1)
				(move ,w2 ,v2)
				(move ,w3 ,v3)
				(move ,w4 ,top)))))
     (2
       (let* (#|-------|#  (v1 (%pop))
	      (v2 (%pop))
	      (w3 (%push)) (w2 (%push)) (w1 (%push))
	      (top (%tmp v1)))
	 (replace-instruction *this-instruction*
			      `((move ,top ,v1)
				(move ,w1 ,v1)
				(move ,w2 ,v2)
				(move ,w3 ,top))))))))

(defop (dup2)
    (lambda ()
      (let* ((stack (i.stack *this-instruction*)))
	(let ((a (vector-peek stack 0))
	      (b (vector-peek stack 1)))
	  (cond
	    ((type1p a)
	      (assert-type1 b)
	      (setf (i.variant *this-instruction*) 1)
	      (list :n-stack-in 2 :out (list b a b a)))
	    (t
	      (setf (i.variant *this-instruction*) 2)
	      (list :n-stack-in 1 :out (list a a)))))))
  (:replace
   (ecase (i.variant *this-instruction*)
     (1
       (let (#|-------|#  (v1 (%pop))
	     (v2 (%pop))
	     (w4 (%push)) (w3 (%push)) (w2 (%push)) (w1 (%push)))
	 (replace-instruction *this-instruction*
			      `((move ,w3 ,v1)
				(move ,w4 ,v2)
				(move ,w1 ,v1)
				(move ,w2 ,v2)))))
     (2
       (let ((v1 (%pop))
	     (w2 (%push)) (w1 (%push)))
	 (replace-instruction *this-instruction*
			      `((move ,w2 ,v1)
				(move ,w1 ,v1))))))))

(defop (dup2_x1)
    (lambda ()
      (let* ((stack (i.stack *this-instruction*)))
	(let ((a (vector-peek stack 0))
	      (b (vector-peek stack 1))
	      (c (vector-peek stack 2)))
	  (cond
	    ((type1p a)
	      (assert-type1 b)
	      (assert-type1 c)
	      (setf (i.variant *this-instruction*) 1)
	      (list :n-stack-in 3 :out (list b a c b a)))
	    (t
	      (assert-type1 b)
	      (setf (i.variant *this-instruction*) 2)
	      (list :n-stack-in 2 :out (list a b a)))))))
  (:replace
   (ecase (i.variant *this-instruction*)
     (1
       (let (#|-------|#  #|-------|#  (v1 (%pop))
	     #|-------|#  (v2 (%pop))
	     (v3 (%pop))
	     (w5 (%push)) (w4 (%push)) (w3 (%push)) (w2 (%push)) (w1 (%push)))
	 (replace-instruction *this-instruction*
			      `((move ,w1 ,v1)
				(move ,w2 ,v2)
				(move ,w3 ,v3)
				(move ,w4 ,w1)
				(move ,w5 ,w2)))))
     (2
       (let (#|-------|#  (v1 (%pop))
	     (v2 (%pop))
	     (w3 (%push)) (w2 (%push)) (w1 (%push)))
	 (replace-instruction *this-instruction*
			      `((move ,w1 ,v1)
				(move ,w2 ,v2)
				(move ,w3 ,w1))))))))

(defop (dup2_x2)
    (lambda ()
      (let* ((stack (i.stack *this-instruction*)))
	(let ((a (vector-peek stack 0))
	      (b (vector-peek stack 1))
	      (c (vector-peek stack 2))
	      (d (vector-peek stack 3)))
	  ;;    dcba
	  ;; 1: 1111 => 111111
	  ;; 2:  112 => 2112
	  ;; 3:  211 => 11211
	  ;; 4:   22 => 222
	  (cond
	    ((and (type2p a) (type2p b))
	      ;; FORM 4
	      (setf (i.variant *this-instruction*) 4)
	      (list :n-stack-in 2 :out (list a b a)))
	    ((type2p a)
	      ;; FORM 2
	      (assert-type1 b)
	      (assert-type1 c)
	      (setf (i.variant *this-instruction*) 2)
	      (list :n-stack-in 3 :out (list a c b a)))
	    ((type2p c)
	      ;; FORM 3
	      (assert-type1 b)
	      (setf (i.variant *this-instruction*) 3)
	      (list :n-stack-in 3 :out (list b a c b a)))
	    (t
	      (assert-type1 b)
	      (assert-type1 d)
	      (setf (i.variant *this-instruction*) 1)
	      (list :n-stack-in 4 :out (list b a d c b a)))))))
  (:replace
   (ecase (i.variant *this-instruction*)
     (1
       (let (#|-------|#  #|-------|#  #|-------|#  (v1 (%pop))
	     #|-------|#  #|-------|#  (v2 (%pop))
	     #|-------|#  (v3 (%pop))
	     (v4 (%pop))
	     (w6 (%push)) (w5 (%push)) (w4 (%push)) (w3 (%push)) (w2 (%push)) (w1 (%push)))
	 (replace-instruction *this-instruction*
			      `((move ,w1 ,v1)
				(move ,w2 ,v2)
				(move ,w3 ,v3)
				(move ,w4 ,v4)
				(move ,w5 ,w1)
				(move ,w6 ,w2)))))
     (2					;same as dup_x2, form 1
       (let* (#|-------|#  #|-------|#  (v1 (%pop))
	      #|-------|#  (v2 (%pop))
	      (v3 (%pop))
	      (w4 (%push)) (w3 (%push)) (w2 (%push)) (w1 (%push))
	      (top (%tmp v1)))
	 (replace-instruction *this-instruction*
			      `((move ,top ,v1)
				(move ,w1 ,v1)
				(move ,w2 ,v2)
				(move ,w3 ,v3)
				(move ,w4 ,top)))))
     (3					;same as dup2_x1, form 1
       (let (#|-------|#  #|-------|#  (v1 (%pop))
	     #|-------|#  (v2 (%pop))
	     (v3 (%pop))
	     (w5 (%push)) (w4 (%push)) (w3 (%push)) (w2 (%push)) (w1 (%push)))
	 (replace-instruction *this-instruction*
			      `((move ,w1 ,v1)
				(move ,w2 ,v2)
				(move ,w3 ,v3)
				(move ,w4 ,w1)
				(move ,w5 ,w2)))))
     (4					;same as dup_x1
       (let* (#|-------|#  (v1 (%pop))
	      (v2 (%pop))
	      (w3 (%push)) (w2 (%push)) (w1 (%push))
	      (top (%tmp v1)))
	 (replace-instruction *this-instruction*
			      `((move ,top ,v1)
				(move ,w1 ,v1)
				(move ,w2 ,v2)
				(move ,w3 ,top))))))))

(define-simple-instruction (f2d) ((value :float) &out :double)
  `(coerce ,value 'double-float))

(define-simple-instruction (f2i) ((value :float) &out :int)
  `(f2i ,value))

(define-simple-instruction (f2l) ((value :float) &out :long)
  `(f2l ,value))

(define-simple-instruction (fadd)
    ((v1 :float) (v2 :float) &out :float)
  `(f+ ,v1 ,v2))

(define-simple-instruction (faload)
    ((array (:array :float)) (index :int) &out :float)
  `(checked-elt (co.data (maybe-assert-nonnull ,array))
		,index
		'single-float))

(define-simple-instruction (fastore)
    ((array (:array :float)) (index :int) (value :float))
  `(checked-set-elt ,value
                    (co.data (maybe-assert-nonnull ,array))
                    ,index
                    'single-float))

;; XXX geht das nicht besser?
(define-simple-instruction (fcmpg) ((v1 :float) (v2 :float) &out :int)
  ;; this differs from fcmpl only in the NaN case
  `(cond
     ((or (nanp ,v1) (nanp ,v2)) 1)
     ((< ,v1 ,v2) -1)
     ((= ,v1 ,v2) 0)
     ((> ,v1 ,v2) 1)
     (t (error "unerreichbar"))))

;; XXX geht das nicht besser?
(define-simple-instruction (fcmpl) ((v1 :float) (v2 :float) &out :int)
  ;; this differs from fcmpg only in the NaN case
  `(cond
     ((or (nanp ,v1) (nanp ,v2)) -1)
     ((< ,v1 ,v2) -1)
     ((= ,v1 ,v2) 0)
     ((> ,v1 ,v2) 1)
     (t (error "unerreichbar"))))

(define-simple-instruction (fconst_0) (&out :float) 0.0s0)
(define-simple-instruction (fconst_1) (&out :float) 1.0s0)
(define-simple-instruction (fconst_2) (&out :float) 2.0s0)

(define-simple-instruction (fdiv)
    ((v1 :float) (v2 :float) &out :float)
  `(f/ ,v1 ,v2))

(define-instruction-macro (fload i) ()
  `(wide-fload ,i))
(define-load-op wide-fload :float)

(define-instruction-macro (fload_0) () `(fload 0))
(define-instruction-macro (fload_1) () `(fload 1))
(define-instruction-macro (fload_2) () `(fload 2))
(define-instruction-macro (fload_3) () `(fload 3))

(define-simple-instruction (fmul)
    ((v1 :float) (v2 :float) &out :float)
  `(f* ,v1 ,v2))

(define-simple-instruction (fneg) ((value :float) &out :float)
  `(fneg ,value))

(define-simple-instruction (frem)
    ((v1 :float) (v2 :float) &out :float)
  `(frem ,v1 ,v2))

(defop (freturn) (:in :float)
  (:parser
   (remove-successor *this-instruction*))
  (:in (in) (list (%pop)))
  (:body
   `(return1 ,in)))

(define-instruction-macro (fstore i) ()
  `(wide-fstore ,i))
(define-store-op wide-fstore :float)

(define-instruction-macro (fstore_0) () `(fstore 0))
(define-instruction-macro (fstore_1) () `(fstore 1))
(define-instruction-macro (fstore_2) () `(fstore 2))
(define-instruction-macro (fstore_3) () `(fstore 3))

(define-simple-instruction (fsub)
    ((v1 :float) (v2 :float) &out :float)
  `(f- ,v1 ,v2))

(define-instruction-macro (goto (offset :short)) ()
  `(goto_w ,offset))

(defun add-successor (ivector instruction &optional spec)
  (let ((successor
	 (etypecase spec
	   (integer (elt ivector (+ (i.index instruction) spec)))
	   (instruction spec)
	   (null (i.next instruction)))))
    (unless successor
      (class-format-error "invalid jump target: ~A" spec))
    (pushnew successor (i.successors instruction))
    (pushnew instruction (i.predecessors successor))
    (when spec
      (setf (i.has-indirect-predecessors-p successor) t))))

(defun remove-successor (instruction &optional successor)
  (setf successor (or successor (i.next instruction)))
  (when successor
    (setf (i.successors instruction)
	  (remove successor (i.successors instruction)))
    (setf (i.predecessors successor)
	  (remove instruction (i.predecessors successor)))))

(defop (goto_w (offset :int))
    (constantly nil)
  (:parser
   (remove-successor *this-instruction*)
   (add-successor .ivector. *this-instruction* offset))
  (:body
   (%go offset)))

(defop (getfield (fieldref :field))
    (lambda (fieldref)
      (%verify-stack '(:object))
      (list :n-stack-in 1
	    :out (list (ref.type-descriptor fieldref))))
  (:in (object) (list (%pop)))
  (:out (%push))
  (:body
   (pushnew (ref.member-id fieldref) (cm.field-ids *method*))
   `(,(field-reffer (ref.type-descriptor fieldref))
     (maybe-assert-nonnull ,object)
     (svref %pool% ,(ref.member-id fieldref)))))

(defop (getstatic (fieldref :field))
    (lambda (fieldref)
      (list :out (list (ref.type-descriptor fieldref))))
  (:out (%push))
  (:body
   (pushnew (ref.member-id fieldref) (cm.field-ids *method*))
;;;   (let ((c (field-from-pool *class* (ref.member-id fieldref))))
;;;     (if (eq *class* (car c))
;;;	 `(,(field-reffer (ref.member fieldref))
;;;	   (cls.static-field-values %class%)
;;;	   ,(cdr c))
;;;	 ))
   `(let ((cons (svref %pool% ,(ref.member-id fieldref))))
      (maybe-initialize-class (car cons))
      (,(field-reffer (ref.type-descriptor fieldref))
       (cls.static-field-values (car cons))
       (cdr cons)))))

(define-simple-instruction (i2b) ((value :int) &out :int)
  ;; XXX schoen ist das nicht
  `(ub8-to-sb8 (logand ,value #xff)))

(define-simple-instruction (i2c) ((value :int) &out :int)
  `(canonicalize-char ,value))

(define-simple-instruction (i2d) ((value :int) &out :double)
  `(coerce ,value 'double-float))

(define-simple-instruction (i2f) ((value :int) &out :float)
  `(coerce ,value 'single-float))

(define-simple-instruction (i2l) ((value :int) &out :long)
  value)

(define-simple-instruction (i2s) ((value :int) &out :short)
  ;; XXX schoen ist das nicht
  `(ub16-to-sb16 (logand ,value #xffff)))

(define-simple-instruction (iadd) ((v1 :int) (v2 :int) &out :int)
  `(int-+ ,v1 ,v2))

(define-simple-instruction (iaload)
    ((array (:array :int)) (index :int) &out :int)
  `(checked-elt (co.data (maybe-assert-nonnull ,array))
		,index
		'(signed-byte 32)))

(define-simple-instruction (iand) ((v1 :int) (v2 :int) &out :int)
  `(logand ,v1 ,v2))

(define-simple-instruction (iastore)
    ((array (:array :int)) (index :int) (value :int))
  `(checked-set-elt ,value
                    (co.data (maybe-assert-nonnull ,array))
                    ,index
                    '(signed-byte 32)))

(macrolet ((doit (name c)
	     `(define-simple-instruction (,name) (&out :int)
		,c)))
  (doit iconst_m1 -1)
  (doit iconst_0 0)
  (doit iconst_1 1)
  (doit iconst_2 2)
  (doit iconst_3 3)
  (doit iconst_4 4)
  (doit iconst_5 5))

(define-simple-instruction (idiv) ((v1 :int) (v2 :int) &out :int)
  `(int-truncate ,v1 ,v2))

(defmacro define-branch-op (name (&rest stack-in) &rest forms)
  `(defop (,name (offset :short))
       (lambda (offset)
	 (declare (ignore offset))
	 (%verify-stack ',stack-in)
	 (list :n-stack-in ',(length stack-in)))
     (:parser (add-successor .ivector. *this-instruction* offset))
     ,@forms))

(define-branch-op if_acmpeq (:object :object)
  (:in (v w) (list (%pop) (%pop)))
  (:body
   `(when (eq ,v ,w)
      ,(%go offset))))

(define-branch-op if_acmpne (:object :object)
  (:in (v w) (list (%pop) (%pop)))
  (:body
   `(unless (eq ,v ,w)
      ,(%go offset))))

(macrolet ((deficmp (name r)
	       `(define-branch-op ,name (:int :int)
		  (:in (b a) (list (%pop) (%pop)))
		  (:body
		   `(when (,',r ,a ,b)
		      ,(%go offset))))))
  (deficmp if_icmpeq =)
  (deficmp if_icmpne /=)
  (deficmp if_icmplt <)
  (deficmp if_icmple <=)
  (deficmp if_icmpgt >)
  (deficmp if_icmpge >=))

(macrolet ((defif (name r)
	       `(define-branch-op ,name (:int)
		  (:in (v) (list (%pop)))
		  (:body
		   `(when (,',r ,v 0)
		      ,(%go offset))))))
  (defif ifeq =)
  (defif ifne /=)
  (defif iflt <)
  (defif ifle <=)
  (defif ifgt >)
  (defif ifge >=))

(define-branch-op ifnonnull (:object)
  (:in (in) (list (%pop)))
  (:body
   `(unless (nullp ,in)
      ,(%go offset))))

(define-branch-op ifnull (:object)
  (:in (in) (list (%pop)))
  (:body
   `(when (nullp ,in)
      ,(%go offset))))

(define-instruction-macro (iinc i (c :byte)) ()
  `(wide-iinc ,i ,c))
(defop (wide-iinc (i :ushort) (c :short))
    (lambda (i c)
      (declare (ignore c))
      (list :locals-out (compute-locals :int i)))
  (:in (in) (list (local-variable i :int)))
  (:out (local-variable i :int))
  (:body
   `(int-+ ,in ,c)))

(define-instruction-macro (iload i) ()
  `(wide-iload ,i))
(define-load-op wide-iload :int)

(define-instruction-macro (iload_0) () `(iload 0))
(define-instruction-macro (iload_1) () `(iload 1))
(define-instruction-macro (iload_2) () `(iload 2))
(define-instruction-macro (iload_3) () `(iload 3))

(define-simple-instruction (imul) ((v1 :int) (v2 :int) &out :int)
  `(int-* ,v1 ,v2))

(define-simple-instruction (ineg) ((value :int) &out :int)
  `(0-int ,value))

(define-simple-instruction (instanceof (i :ushort))
    ((instance :object) &out :int)
  ;; FUNCALL
  (pushnew i (cm.class-ids *method*))
  `(if (and (not (nullp ,instance))
	    (cloak-subclass-p (%class ,instance) (svref %pool% ,i)))
       1
       0))

(defun pop-method-arguments (methodref virtualp)
  (let ((arguments '()))
    (dotimes (x (length (ref.argtype-descriptors methodref)))
      (push (%pop) arguments))
    (when virtualp
      (push (%pop) arguments))
    arguments))

(defun push-method-result (methodref)
  (if (eq :void (ref.rtype-descriptor methodref))
      nil
      (%push)))

(defun ref.signature (methodref)
  ;; wieso?
  (let* ((descriptor (ref.descriptor methodref))
	 (argtypes (subseq descriptor 0 (1+ (position #\) descriptor)))))
    (concatenate 'string (ref.name methodref) argtypes)))

(defun ihash (ref)
  (mod (sxhash (ref.signature ref)) +itable-length+))

(defop (invokeinterface (methodref :method) count o)
    (lambda (methodref count o)
      (declare (ignore count o))
      (let ((argtypes (ref.argtype-descriptors methodref))
	    (rtype (ref.rtype-descriptor methodref)))
	(let ((stack-in (cons :object argtypes)))
	  (%verify-stack stack-in)
	  (list :n-stack-in (length stack-in)
		:out (case rtype
		       (:void nil)
		       (t (list rtype)))))))
  (:in in (pop-method-arguments methodref t))
  (:out (push-method-result methodref))
  (:body
   (let (;; (class-id (ref.class-id methodref))
	 (instance (car in))
	 (arguments (cdr in))
	 (ihash (ihash methodref))
	 (string-signature (ref.signature methodref)))
;;;     (maybe-compute-method-layout (class-from-pool *class* class-id))
     `(let* ((signature ',(intern string-signature :cloak-signatures))
	     (string-signature ',string-signature)
	     (class (%class (assert-nonnull ,instance)))
	     (map (svref (cls.itable class) ,ihash))
	     (vtable-index
	      (dolist (entry map
			(let ((i
			       (imethod-vtable-index class string-signature)))
			  ;; thread safety?
			  (push (cons signature i)
				(svref (cls.itable class) ,ihash))
			  i))
		(when (eq (car entry) signature)
		  (return (cdr entry)))))) 
	(%call-cloak-method vtable-index
			    class
			    ,instance
			    ,@arguments)))))

;;;(if ...
;;;    `(multiple-value-bind (method method-class)
;;;	 (resolve-method %class% ,member-id)
;;;       (let ((class-idx
;;;	      (if (and (not (eq %class% method-class))
;;;		       (cloak-subclass-p %class% method-class))
;;;		  ,superclass-id
;;;		  ,class-id)))
;;;	 (%call-cloak-method method
;;;			     (svref %pool% class-idx)
;;;			     (assert-nonnull ,instance)
;;;			     ,@arguments))))

(defop (invokespecial (methodref :method))
    (lambda (methodref)
      (let ((argtypes (ref.argtype-descriptors methodref))
	    (rtype (ref.rtype-descriptor methodref)))
	(let ((stack-in (cons :object argtypes)))
	  (%verify-stack stack-in)
	  (list :n-stack-in (length stack-in)
		:out (case rtype
		       (:void nil)
		       (t (list rtype)))))))
  (:in in (pop-method-arguments methodref t))
  (:out (push-method-result methodref))
  (:body
   (let ((member-id (ref.member-id methodref))
	 (class-id (ref.class-id methodref))
	 (instance (car in))
	 (arguments (cdr in))
	 (cf *class-file*))
     (pushnew member-id (cm.method-ids *method*))
;;;     (maybe-compute-method-layout (class-from-pool *class* class-id))
     (if (and (cf.superp cf)
	      (not (eql (char (ref.name methodref) 0) #\<))
	      (not (equal (cf.get-constant-class cf class-id)
			  (cf.name cf))))
	 `(%call-cloak-method (svref %pool% ,member-id)
			      (svref %pool% ,(cf.superclass-index cf))
			      (assert-nonnull ,instance)
			      ,@arguments)
	 `(%call-cloak-method (svref %pool% ,member-id)
			      (svref %pool% ,class-id)
			      (assert-nonnull ,instance)
			      ,@arguments)))))

(defop (invokestatic (methodref :method))
    (lambda (methodref)
      (let ((argtypes (ref.argtype-descriptors methodref))
	    (rtype (ref.rtype-descriptor methodref)))
	(%verify-stack argtypes)
	(list :n-stack-in (length argtypes)
	      :out (case rtype
		     (:void nil)
		     (t (list rtype))))))
  (:in arguments (pop-method-arguments methodref nil))
  (:out (push-method-result methodref))
  (:body
   (let ((member-id (ref.member-id methodref))
	 (class-id (ref.class-id methodref)))
     (pushnew member-id (cm.method-ids *method*))
;;;     (maybe-compute-method-layout (class-from-pool *class* class-id))
     (pushnew class-id (cm.class-ids *method*))
     `(let
	  ;; class-id is right only because we inherit static methods
	  ((class (svref %pool% ,class-id)))
	;; ...or is it, since we need to initialize the superclass only?
	(maybe-initialize-class class)
	(%call-cloak-method (svref %pool% ,member-id)
			    class
			    ,@arguments)))))

(defop (invokevirtual (methodref :method))
    (lambda (methodref)
      (let ((argtypes (ref.argtype-descriptors methodref))
	    (rtype (ref.rtype-descriptor methodref)))
	(let ((stack-in (cons :object argtypes)))
	  (%verify-stack stack-in)
	  (list :n-stack-in (length stack-in)
		:out (case rtype
		       (:void nil)
		       (t (list rtype)))))))
  (:in in (pop-method-arguments methodref t))
  (:out (push-method-result methodref))
  (:body
   (let ((member-id (ref.member-id methodref))
	 (instance (car in))
	 (arguments (cdr in)))
     (pushnew member-id (cm.method-ids *method*))
;;;     (maybe-compute-method-layout (class-from-pool *class* class-id))
     `(%call-cloak-method (svref %pool% ,member-id)
			  (%class (maybe-assert-nonnull ,instance))
			  ,instance
			  ,@arguments))))

(define-simple-instruction (ior) ((v1 :int) (v2 :int) &out :int)
  `(logior ,v1 ,v2))

(define-simple-instruction (irem) ((v1 :int) (v2 :int) &out :int)
  `(irem ,v1 ,v2))

(defop (ireturn) (:in :int)
  (:parser
   (remove-successor *this-instruction*))
  (:in (in) (list (%pop)))
  (:body
   `(return1 ,in)))

(define-simple-instruction (ishl) ((v1 :int) (v2 :int) &out :int)
  `(int-ash ,v1 (logand ,v2 #x1f)))

(define-simple-instruction (ishr) ((v1 :int) (v2 :int) &out :int)
  `(ash ,v1 (- (logand ,v2 #x1f))))

(define-instruction-macro (istore i) ()
  `(wide-istore ,i))
(define-store-op wide-istore :int)

(define-instruction-macro (istore_0) () `(istore 0))
(define-instruction-macro (istore_1) () `(istore 1))
(define-instruction-macro (istore_2) () `(istore 2))
(define-instruction-macro (istore_3) () `(istore 3))

(define-simple-instruction (isub) ((v1 :int) (v2 :int) &out :int)
  `(int-- ,v1 ,v2))

(define-simple-instruction (iushr) ((v1 :int) (v2 :int) &out :int)
  `(ash (logand ,v1 #xffffffff) (- (logand ,v2 #x1f))))

(define-simple-instruction (ixor) ((v1 :int) (v2 :int) &out :int)
  `(logxor ,v1 ,v2))

(define-instruction-macro (jsr (offset :short)) ()
  `(jsr_w ,offset))

(defop (jsr_w (offset :int))
    (lambda (offset)
      (declare (ignore offset))
      (list :out `((:return-address ,*this-instruction*))))
  (:parser
   (remove-successor *this-instruction*)
   (add-successor .ivector. *this-instruction* offset))
  (:out (%push))
  (:body
   (let ((return-label 3)
	 (jump-label offset))
     `(progn
	(setf ,(i.output-argument *this-instruction*)
	      (vector (lambda () ,(%go return-label))))
	,(%go jump-label)))))

(define-simple-instruction (l2d) ((value :long) &out :double)
  `(coerce ,value 'double-float))

(define-simple-instruction (l2f) ((value :long) &out :float)
  `(coerce ,value 'single-float))

(define-simple-instruction (l2i) ((value :long) &out :int)
  `(ub32-to-sb32 (logand ,value #xffffffff))
;;;  `(sb64-to-sb32 ,value)
  )

(define-simple-instruction (ladd) ((v1 :long) (v2 :long) &out :long)
  `(long-+ ,v1 ,v2))

(define-simple-instruction (laload)
    ((array (:array :long)) (index :int) &out :long)
  `(checked-elt (co.data (maybe-assert-nonnull ,array))
		,index
		'(signed-byte 64)))

(define-simple-instruction (land) ((v1 :long) (v2 :long) &out :long)
  #+(or)
  `(logand ,v1 ,v2)
  `(land ,v1 ,v2))

(define-simple-instruction (lastore)
    ((array (:array :long)) (index :int) (value :long))
  `(checked-set-elt ,value
                    (co.data (maybe-assert-nonnull ,array))
                    ,index
                    '(signed-byte 64)))

(define-simple-instruction (lcmp) ((v1 :long) (v2 :long) &out :int)
  `(cond
     ((< ,v1 ,v2) -1)
     ((= ,v1 ,v2) 0)
     (t 1)))

(define-simple-instruction (lconst_0) (&out :long)
  0)
(define-simple-instruction (lconst_1) (&out :long)
  1)

(defun make-cloak-array (class data)
  (do-make-cloak-array (find-cloak-class nil class) data))

(defun do-make-cloak-array (class data)
  #-slow (declare (optimize (speed 3) (safety 0)))
  (let ((result (%clone-object (cls.prototype class))))
    (when (eq (cls.name (cls.element-type class)) :int)
      (check-type data (simple-array (signed-byte 32) (*))))
    (setf (co.data result) data)
    result))

(defun make-cloak-string (lisp-string)
  (let* ((class (find-cloak-class nil "java/lang/String"))
	 (b (decode-pseudo-utf-8 lisp-string))
	 (array
	  (progn
	    (unless (typep b '(simple-array (unsigned-byte 16)))
	      (setf b (make-array (length b)
				  :element-type '(unsigned-byte 16)
				  :initial-contents b)))
	    (make-cloak-array "[C" b)))
	 (instance
	  (progn
	    (maybe-initialize-class class)
	    (%make-cloak-object class))))
    (setf (getfield "value" instance) array)
    (setf (getfield "offset" instance) 0)
    (setf (getfield "count" instance) (length b))
    instance))

(defun intern-cloak-string (lisp-string)
  (sb-thread:with-recursive-lock (*string-cache-lock*)
    (let* ((ref (cdr (gethash lisp-string (vm.strings *vm*)))))
      (or (and ref (sb-ext:weak-pointer-value ref))
	  (let ((str (make-cloak-string lisp-string)))
	    (setf (gethash lisp-string (vm.strings *vm*))
		  (cons lisp-string (sb-ext:make-weak-pointer str)))
	    str)))))

(defun get-string-value (cloak-string)
  (assert-nonnull cloak-string)
  ;; getBytes() koennen wir hier aus bootstrappinggruenden nicht verwenden
  (let ((data (co.data (getfield "value" cloak-string)))
	(offset (getfield "offset" cloak-string))
	(count (getfield "count" cloak-string)))
    (encode-pseudo-utf-8 data offset (+ offset count))))

(define-instruction-macro (ldc index) ()
  `(ldc_w ,index))

(defop (ldc_w (index :ushort))
    (lambda (index)
      (let* ((tag (nth-value 1 (cf.get-constant *class-file* index)))
	     (type
	      (case tag
		(#.+constant_integer+ :int)
		(#.+constant_float+ :float)
		(#.+constant_string+ :string)
		(#.+constant_class+ :object)
		(t
		 (class-format-error "invalid constant reference")))))
	(list :out (list type))))
  (:out (%push))
  (:body
   (multiple-value-bind (constant tag)
       (cf.get-constant *class-file* index)
     (ecase tag
       (#.+constant_string+
	 (pushnew constant (cm.string-ids *method*))
	 `(svref %pool% ,constant))
       (#.+constant_float+
	 `(load-time-value
	   (int-to-float
	    ,(float-to-int constant))))
       (#.+constant_integer+
	 constant)
       (#.+constant_class+
	 (pushnew constant (cm.class-ids *method*))
	 `(svref %pool% ,constant))))))

(defop (ldc2_w (index :ushort))
    (lambda (index)
      (let* ((tag (nth-value 1 (cf.get-constant *class-file* index)))
	     (type
	      (case tag
		(#.+constant_long+ :long)
		(#.+constant_double+ :double)
		(t (class-format-error "invalid constant reference")))))
	(list :out (list type))))
  (:out (%push))
  (:body
   (multiple-value-bind (constant tag)
       (cf.get-constant *class-file* index)
     (if (eql tag +constant_double+)
	 `(load-time-value
	   (long-to-double
	    ,(double-to-long constant)))
	 constant))))

(define-simple-instruction (ldiv) ((v1 :long) (v2 :long) &out :long)
  `(long-truncate ,v1 ,v2))

(define-instruction-macro (lload i) ()
  `(wide-lload ,i))
(define-load-op wide-lload :long)

(define-instruction-macro (lload_0) () `(lload 0))
(define-instruction-macro (lload_1) () `(lload 1))
(define-instruction-macro (lload_2) () `(lload 2))
(define-instruction-macro (lload_3) () `(lload 3))

(define-simple-instruction (lmul) ((v1 :long) (v2 :long) &out :long)
  `(long-* ,v1 ,v2))

(define-simple-instruction (lneg) ((value :long) &out :long)
  `(0-long ,value))

(defop (lookupswitch default match-offset-pairs)
    (lambda (default match-offset-pairs)
      (declare (ignore default match-offset-pairs))
      (%verify-stack '(:int))
      (list :n-stack-in 1))
  (:parser
   (remove-successor *this-instruction*)
   (add-successor .ivector. *this-instruction* default)
   (dolist (pair match-offset-pairs)
     (add-successor .ivector. *this-instruction* (cdr pair))))
  (:in (in) (list (%pop)))
  (:body
   `(case ,in
      ,@(loop
	    for (match . offset) in match-offset-pairs
	    collect `(,match ,(%go offset)))
      (t ,(%go default)))))

(define-simple-instruction (lor) ((v1 :long) (v2 :long) &out :long)
  #+(or)
  `(logior ,v1 ,v2)
  `(lor ,v1 ,v2))

(define-simple-instruction (lrem) ((v1 :long) (v2 :long) &out :long)
  `(nth-value 1 (truncate64 ,v1 ,v2)))

(defop (lreturn) (:in :long)
  (:parser
   (remove-successor *this-instruction*))
  (:in (in) (list (%pop)))
  (:body
   `(return1 ,in)))

(define-simple-instruction (lshl) ((v :long) (n :int) &out :long)
  `(long-ash ,v (logand ,n #x3f)))

(define-simple-instruction (lshr) ((v :long) (n :int) &out :long)
  `(long-ash ,v (- (logand ,n #x3f))))

(define-instruction-macro (lstore i) ()
  `(wide-lstore ,i))
(define-store-op wide-lstore :long)

(define-instruction-macro (lstore_0) () `(lstore 0))
(define-instruction-macro (lstore_1) () `(lstore 1))
(define-instruction-macro (lstore_2) () `(lstore 2))
(define-instruction-macro (lstore_3) () `(lstore 3))

(define-simple-instruction (lsub) ((v1 :long) (v2 :long) &out :long)
  `(long-- ,v1 ,v2))

(define-simple-instruction (lushr) ((v :long) (n :int) &out :long)
  #+(or)
  `(ash (logand ,v #xffffffffffffffff) (- (logand ,n #x3f)))
  `(lushr ,v ,n))

(define-simple-instruction (lxor) ((v1 :long) (v2 :long) &out :long)
  #+(or)
  `(logxor ,v1 ,v2)
  `(lxor ,v1 ,v2))

(defop (move (%out) (%in)) nil
  (:in (in) (list %in))
  (:out %out)
  (:body in))

(define-simple-instruction (monitorenter) ((object :object))
  `(enter-object-monitor (maybe-assert-nonnull ,object)))

(define-simple-instruction (monitorexit) ((object :object))
  `(unless (exit-object-monitor (maybe-assert-nonnull ,object))
     (throw-exception "java/lang/IllegalMonitorStateException")))

(defop (multianewarray (index :ushort) d)
    (lambda (index d)
      (declare (ignore index))
      (unless (plusp d)
	(class-format-error "cannot create zero-dimensional array"))
      (%verify-stack (loop repeat d collect :int))
      (list :n-stack-in d
	    :out (list :object)))	;XXX
  (:in args (reverse (loop repeat d collect (%pop))))
  (:out (%push))
  (:body
   (pushnew index (cm.class-ids *method*))
   `(multianewarray (svref %pool% ,index) ,@args)))

(defun class-default-value (class)
  (case (cls.name class)
    (:byte 0)
    (:short 0)
    (:int 0)
    (:long 0)
    (:double 0.0d0)
    (:float 0.0s0)
    (:boolean 0)
    (:char 0)
    (t +null+)))

(defun class-element-type (class)
  (case (cls.name class)
    (:byte '(signed-byte 8))
    (:short '(signed-byte 16))
    (:int '(signed-byte 32))
    (:long '(signed-byte 64))
    (:double 'double-float)
    (:float 'single-float)
    (:boolean '(signed-byte 8))
    (:char '(unsigned-byte 16))
    (t 't)))

(defun multianewarray (class &rest dimensions)
  ;; XXX IllegalAccessError?
  (dolist (count dimensions)
    (when (minusp count)
      (throw-exception "java/lang/NegativeArraySizeException"
	  "invalid stack contents in MULTIANEWARRAY")))
  (labels ((recurse (class count &rest rest)
	     (unless (typep class 'cloak-array-class)
	       (class-format-error "MULTIANEWARRAY: not an array class"))
	     (let* ((element-type (cls.element-type class))
		    (default (class-default-value element-type))
		    (lisp-element-type (class-element-type element-type))
		    (data (safe-make-array count
					   :element-type lisp-element-type
					   :initial-element default)))
	       (when rest
		 (dotimes (i count)
		   (setf (elt data i)
			 (apply #'multianewarray element-type rest))))
	       (make-cloak-array class data))))
    (apply #'recurse class dimensions)))

(define-simple-instruction (new (index :ushort)) (&out :object)
  (pushnew index (cm.class-ids *method*))
  `(let ((class (svref %pool% ,index)))
     (maybe-initialize-class class)
     (%make-cloak-object class)))

(defop (newarray atype)
    (lambda (atype)
      (%verify-stack '(:int))
      (list :n-stack-in 1
	    :out `((:array ,(primitive-default-value
			     (ecase atype
			       (4 :boolean)
			       (5 :char)
			       (6 :float)
			       (7 :double)
			       (8 :byte)
			       (9 :short)
			       (10 :int)
			       (11 :long)))))))
  (:in (length) (list (%pop)))
  (:out (%push))
  (:body
   (let ((default (primitive-default-value
		   (ecase atype
		     (4 :boolean)
		     (5 :char)
		     (6 :float)
		     (7 :double)
		     (8 :byte)
		     (9 :short)
		     (10 :int)
		     (11 :long))))
	 (class-name
	  (ecase atype
	    (4 "[Z")
	    (5 "[C")
	    (6 "[F")
	    (7 "[D")
	    (8 "[B")
	    (9 "[S")
	    (10 "[I")
	    (11 "[J")))
	 (class-reader
	  (ecase atype
	    (4 'vm.boolean-array)
	    (5 'vm.char-array)
	    (6 'vm.float-array)
	    (7 'vm.double-array)
	    (8 'vm.byte-array)
	    (9 'vm.short-array)
	    (10 'vm.int-array)
	    (11 'vm.long-array)))
	 (element-type (ecase atype
			 (4 '(signed-byte 8))
			 (5 '(unsigned-byte 16))
			 (6 'single-float)
			 (7 'double-float)
			 (8 '(signed-byte 8))
			 (9 '(signed-byte 16))
			 (10 '(signed-byte 32))
			 (11 '(signed-byte 64)))))
     `(do-make-cloak-array
	  (or (,class-reader *vm*)
	      (setf (,class-reader *vm*)
		    (find-cloak-class nil ,class-name)))
	(safe-make-array ,length
			 :initial-element ,default
			 :element-type ',element-type)))))

(define-simple-instruction (nop) ()
  `(progn))

(defop (pop)
    (lambda ()
      (let* ((stack (i.stack *this-instruction*)))
	(let ((a (vector-peek stack 0)))
	  (assert-type1 a)
	  (list :n-stack-in 1 :out ()))))
  (:in (dummy) (list (%pop)))
  (:body
   (declare (ignore dummy))
   '(progn)))

(defop (pop2)
    (lambda ()
      (let* ((stack (i.stack *this-instruction*)))
	(let ((a (vector-peek stack 0))
	      (b (vector-peek stack 1)))
	  (cond
	    ((type1p a)
	      (assert-type1 b)
	      (list :n-stack-in 2 :out ()))
	    (t
	      (list :n-stack-in 1 :out ()))))))
  (:in (dummy) (list (%pop)))
  (:body
   (declare (ignore dummy))
   '(progn)))

(defop (putfield (fieldref :field))
    (lambda (fieldref)
      (%verify-stack (list :object (ref.type-descriptor fieldref)))
      (list :n-stack-in 2
	    :out ()))
  (:in (value object) (list (%pop) (%pop)))
  (:body
   (pushnew (ref.member-id fieldref) (cm.field-ids *method*))
   `(setf (,(field-reffer (ref.type-descriptor fieldref))
	   (maybe-assert-nonnull ,object)
	   (svref %pool% ,(ref.member-id fieldref)))
	  ,value)))

(defop (putstatic (fieldref :field))
    (lambda (fieldref)
      (%verify-stack (list (ref.type-descriptor fieldref)))
      (list :n-stack-in 1
	    :out ()))
  (:in (value) (list (%pop)))
  (:body
   (pushnew (ref.member-id fieldref) (cm.field-ids *method*))
;;;   (let ((c (field-from-pool *class* (ref.member-id fieldref))))
;;;     (if (eq *class* (car c))
;;;	 `(setf (,(field-reffer (ref.member fieldref))
;;;		 (cls.static-field-values %class%)
;;;		 ,(cdr c))
;;;		,value)
;;;	 ))
   `(let ((cons (svref %pool% ,(ref.member-id fieldref))))
      (maybe-initialize-class (car cons))
      (setf (,(field-reffer (ref.type-descriptor fieldref))
	     (cls.static-field-values (car cons))
	     (cdr cons))
	    ,value))))

(define-instruction-macro (ret i) ()
  `(wide-ret ,i))
(defop (wide-ret (i :ushort))
    (lambda (i)
      (let ((a (elt (i.locals *this-instruction*) i)))
	(unless (and (listp a) (eq (car a) :return-address))
	  (class-format-error "inappropriate local variable data: 4.8.2(a)"))
	(dolist (jsr (cdr a))
	  (add-successor nil *this-instruction* (i.next jsr)))
	nil))
  (:parser
   (remove-successor *this-instruction*)
   ;; kludge: successors are added as they are discovered during data flow ana
   )
  (:in (ret) (list (local-variable i :return-address)))
  (:body
   `(funcall (the function (elt ,ret 0)))))

(defop (return) (:in () :out ())
  (:parser
   (remove-successor *this-instruction*))
  (:body
   `(return1 nil)))

(define-simple-instruction (saload)
    ((array (:array :short)) (index :int) &out :int)
  `(checked-elt (co.data (maybe-assert-nonnull ,array))
		,index
		'(signed-byte 16)))

(define-simple-instruction (sastore)
    ((array (:array :short)) (index :int) (value :int))
  ;; XXX schoen ist das nicht
  `(checked-set-elt (ub16-to-sb16 (logand ,value #xffff))
		    (co.data (maybe-assert-nonnull ,array))
		    ,index
		    '(signed-byte 16)))

(define-simple-instruction (sipush (value :short)) (&out :int)
  ;; the spec's description of sipush is a bit funny.  It is not possible to
  ;; assemble bytes 0x80 and 0x00 into a signed short of value 0x8000.
  value)

(defop (swap)
    (lambda ()
      (let* ((stack (i.stack *this-instruction*)))
	(let ((a (vector-peek stack 0))
	      (b (vector-peek stack 1)))
	  (assert-type1 a)
	  (assert-type1 b)
	  (list :n-stack-in 2 :out (list a b)))))
  (:replace
   (let* (#|-------|# (a (%pop))
	  (b (%pop))
	  (u (%push)) (v (%push))
	  (tmp (%tmp a)))
     (replace-instruction *this-instruction*
			  `((move ,tmp ,a)
			    (move ,v ,b)
			    (move ,u ,tmp))))))

(defop (tableswitch default low high offsets)
    (lambda (default low high offsets)
      (declare (ignore default high low offsets))
      (%verify-stack '(:int))
      (list :n-stack-in 1))
  (:parser
   (remove-successor *this-instruction*)
   (add-successor .ivector. *this-instruction* default)
   (dolist (offset offsets)
     (add-successor .ivector. *this-instruction* offset)))
  (:in (in) (list (%pop)))
  (:body
   `(case ,in
      ,@(loop
	    for index from low
	    for offset in offsets
	    collect `(,index ,(%go offset)))
      (t ,(%go default)))))

(define-instruction-macro (wide) ()
  (error "can't happen"))
