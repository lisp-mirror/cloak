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

;; need to declare to python that throw-exception doesn't return, otherwise
;; it "proves" some bytecode "wrong".
(declaim (ftype (function (t &optional t &rest t) nil) throw-exception))
(declaim (ftype (function (cloak-object) nil) %throw-exception))
(declaim (ftype (function () nil) nullpointerexception))

(defconstant +itable-length+ 31)

(defvar *class-file*)
(defvar *this-instruction*)
(defvar *method*)
(defvar *method-local-variables*)
(defvar *suppress-compiler-notes* t)

(defun map-instruction-array (fn instructions)
  (for ((i :across instructions))
    (when i
      (funcall fn i))))

(defmacro do-instruction-array ((i is) &body body)
  `(block nil
     (map-instruction-array (lambda (,i) ,@body) ,is)))

(set-pprint-dispatch '(cons (member cloak-handler-bind cloak-handler-bind0))
  (pprint-dispatch '(let) nil))

(defmacro cloak-handler-bind ((&rest bindings) &body body)
  ;; Exceptions are stated either as class names, then taken from
  ;; bootstrap classloader, or as constant pool indices, then taken from
  ;; %class%, which is defined in compiled methods only...
  `(fast-handler-bind
       (lambda (o)
         (cond
           ,@(loop
                 for (xclass function) in bindings
                 collect
                   `((cloak-subclass-p
                      (%class o)
                      ;; see comment
                      ,(cond
                         ((stringp xclass)
                           `(find-cloak-class nil ,xclass))
                         ((zerop xclass)
                           `(find-cloak-class nil "java/lang/Throwable"))
                         (t
                           `(svref %pool% ,xclass))))
                     (funcall ,function o)))))
     ,@body))

(defmacro cloak-handler-bind0 ((&rest clauses) &body body)
  `(cloak-handler-bind
       (,@(loop
	      for (xclass tag) in clauses
	      collect `(,xclass (lambda (c) (setf so0 c) (go ,tag)))))
     ,@body))

(declaim (inline %class))
(locally
    (declare #.*fast*)
  (defun %class (object)
    (sb-kernel:%instance-ref object 0)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun simplify-type (type)
    (cond
      ((and (listp type) (eq (car type) :array))
	:object)			;XXX fixme
      ((and (listp type) (eq (car type) :return-address))
	type)
      (t
	(etypecase type
	  ((member :boolean :byte :char :short) :int)
	  ((member :string) :object)
	  (string :object)
	  (list (map 'list #'simplify-type type))
	  (vector (map 'vector #'simplify-type type))
	  (keyword type)))))
  (defun canonicalize-arguments (args)
    (mapcar (lambda (arg) (if (listp arg) arg (list arg :ubyte))) args)))

(defstruct (instruction-definition (:conc-name "ID."))
  typespec
  parser
  opdef
  replace
  in-parser
  out-parser
  argument-types
  (macro nil))

(defmacro defop ((name &rest args) typespec &rest forms)
  (setf args (canonicalize-arguments args))
  (let ((typespec
	 (if (and typespec (keywordp (car typespec)))
	     (destructuring-bind (&key in out) typespec
	       (setf in (listify in))
	       (setf out (listify out))
	       `(lambda (,@(mapcar #'car args))
		  (declare (ignore ,@(mapcar #'car args)))
		  (%verify-stack ',in)
		  (list :n-stack-in ,(length in)
			:out ',out)))
	     typespec))
	lisp-body
	parser-body
	replace-body
	in-vars
	in-form
	out-form)
    (dolist (form forms)
      (destructuring-bind (key &rest body) form
	(ecase key
	  (:body (setf lisp-body body))
	  (:parser (setf parser-body body))
	  (:replace (setf replace-body body))
	  (:in
	    (destructuring-bind (vars form) body
	      (setf in-vars vars
		    in-form form)))
	  (:out
	    (destructuring-bind (form) body
	      (setf out-form form))))))
    `(setf (get ',name 'instruction-definition)
	   (make-instruction-definition
	     :typespec ,typespec
	     :parser (lambda (.ivector. ,@(mapcar #'car args))
		       (declare (ignorable .ivector. ,@(mapcar #'car args)))
		       ,@parser-body)
	     :in-parser (lambda (,@(mapcar #'car args))
			  (declare (ignorable ,@(mapcar #'car args)))
			  ,in-form)
	     :out-parser (lambda (,@(mapcar #'car args))
			   (declare (ignorable ,@(mapcar #'car args)))
			   ,out-form)
	     :opdef (lambda (.in. ,@(mapcar #'car args))
		      (declare (ignorable ,@(mapcar #'car args)))
		      (destructuring-bind (,@in-vars) .in.
			,@lisp-body))
	     :replace (lambda (,@(mapcar #'car args))
			(declare (ignorable ,@(mapcar #'car args)))
			,@replace-body)
	     :argument-types ',(mapcar #'cadr args)))))

(defmacro define-instruction-macro ((name &rest args) () &body body)
  (setf args (canonicalize-arguments args))
  `(setf (get ',name 'instruction-definition)
	 (make-instruction-definition
	   :macro (lambda (,@(mapcar #'car args)) ,@body)
	   :argument-types ',(mapcar #'cadr args))))

(defmacro define-simple-instruction ((name &rest args) stack-spec &body body)
  (let* ((i (position '&out stack-spec))
	 (ins (if i (subseq stack-spec 0 i) stack-spec))
	 (out (if i (nth (1+ i) stack-spec) nil)))
    `(defop (,name ,@args)
	 (:in ,(mapcar #'cadr ins) :out ,(if out (list out) nil))
       (:in ,(mapcar #'car (reverse ins))
	    (list ,@(loop repeat (length ins) collect '(%pop))))
       ,@(when out
	   `((:out (%push))))
       (:body
	,@body))))

(defun getop (name)
  (cond
    ((get name 'instruction-definition))
    (t (error "instruction not defined: ~A" name))))

(defparameter *go*
    (lambda (pc)
      `(go ,(label pc))))
(defun %go (offset) (funcall *go* (+ (i.index *this-instruction*) offset)))

(defun %pop ()
  (let ((idx (decf (i.stack-pointer *this-instruction*))))
    (stack-entry idx (elt (i.stack *this-instruction*) idx))))

(defun %push ()
  (prog1
      (let ((idx (i.stack-pointer *this-instruction*)))
        (stack-entry idx (elt (i.stack-out *this-instruction*) idx)))
    (incf (i.stack-pointer *this-instruction*))))

(defmacro comment (&rest args)
  (declare (ignore args))
  '(progn))

(defun nmacroexpand-instruction (i)
  (when i
    (%for* (((opcode . arguments) = (cons (i.opcode i) (i.arguments i))
				  :then (apply macro arguments))
	    (macro = (id.macro (get opcode 'instruction-definition)))
	    :while macro)
        nil
      (setf (i.expanded-opcode i) opcode)
      (setf (i.expanded-arguments i) arguments))))

(defun catch-class (class xblock)
  (let ((catch-type (tc.catch-type xblock)))
    (if (zerop catch-type)
	(find-cloak-class nil "java/lang/Throwable")
	(class-from-pool class catch-type))))

(defun mark-xblock-leaders (method instructions)
  ;; fixme: beautify nested try-blocks
  (let ((max-index (length (cm.code method)))
	(first (elt instructions 0)))
    ;; no try-catch blocks known initially
    (do-instruction-array (i instructions)
      (setf (i.xblock-leader i) first)
      (setf (i.next-xblock-leader i) nil))
    ;; start a new tagbody for each try block.  cope with overlapping blocks.
    (flet ((get-instruction (index)
	     (unless (and (< -1 index max-index))
	       (class-format-error "invalid xtable entry"))
	     (let ((instruction (elt instructions index)))
	       (unless instruction (class-format-error "invalid xtable entry"))
	       instruction))
	   (mark-as-leader (instruction)
	     (let ((old-leader (i.xblock-leader instruction)))
	       (unless (eq old-leader instruction)
		 (shiftf (i.next-xblock-leader instruction)
			 (i.next-xblock-leader old-leader)
			 instruction))
	       #+(or)
	       (setf (i.next-xblock-leader old-leader) instruction)
	       (for ((i = instruction :then (i.next i))
		     :while (and i (eq (i.xblock-leader i) old-leader)))
		 (setf (i.xblock-leader i) instruction)))))
      (dolist (x (reverse (cm.xtable method))) ;keep order!
	(let ((leader (get-instruction (tc.start-pc x))))
	  (when (i.reachablep (get-instruction (tc.handler-pc x)))
	    (for* ((idx :from (tc.start-pc x) :below (tc.end-pc x))
		   (instruction = (elt instructions idx)))
	      (when instruction
		(push x (i.xblocks instruction)))))
	  (mark-as-leader leader))
	(unless (eql (tc.end-pc x) max-index)
	  (mark-as-leader (get-instruction (tc.end-pc x))))
	(mark-as-leader (get-instruction (tc.handler-pc x))))
      ;; join points reachable from outside of a tagbody are split points, too
      (loop while
	    (let (leader start end-1
		  (changedp nil))
	      (flet ((in-this-block-p (instruction)
		       (<= start (i.index instruction) end-1)))
		(for ((i = first :then (i.next i))
		      :while i)
		  (cond
		    ((not (eq (i.xblock-leader i) leader))
		      (setf leader (i.xblock-leader i))
		      (let ((next (i.next-xblock-leader leader)))
			(setf start (i.index leader)
			      end-1 (1- (if next (i.index next) max-index)))))
		    ;; not a leader yet.  should it be one?
		    ((notevery #'in-this-block-p (i.predecessors i))
		      (mark-as-leader i)
		      (setf i leader)
		      (setf changedp t)))))
	      changedp)))
    #+(or)
    (describe-xblocks instructions)))

(defun describe-xblocks (instructions)
  (let ((method (i.method (elt instructions 0))))
    (format t "~&; ~A~%" method)
    (dolist (x (cm.xtable method))
      (format t "~&; ~A~%" x))
    (do-instruction-array (i instructions)
      (format t "~&; ~D: ~A ~40T(leader: ~A, next leader: ~A)~%"
	      (i.index i)
	      (cons (i.opcode i) (i.arguments i))
	      (i.index (i.xblock-leader i))
	      (let ((next (i.next-xblock-leader (i.xblock-leader i))))
		(when next (i.index next)))))))

(defmacro with-instruction (index &body body)
  (declare (ignore index))
  `(progn ,@body))

(defun translate-instruction (instruction)
  (when
      ;; kludge: only move instructions don't have a stack, but also don't
      ;; care about the stack pointer...
      (i.stack instruction)
    (setf (i.stack-pointer instruction)
	  (fill-pointer (i.stack instruction))))
  (let ((*this-instruction* instruction))
    `(with-instruction ,(i.index instruction)
       ,(apply (id.opdef (getop (i.expanded-opcode instruction)))
	       (i.input-arguments instruction)
	       (i.expanded-arguments instruction)))))

(defun maybe-make-tagbody (tags-or-statements)
  (if (some #'symbolp tags-or-statements)
      `((tagbody ,@tags-or-statements))
      tags-or-statements))

(defun leader-basic-blocks (leader)
  ;; compute the basic blocks within leader's xblock 
  (with-collector (collect)
    (let ((tmp '()))
      (for ((instruction = leader :then (i.next instruction))
	    :while (and instruction (eq (i.xblock-leader instruction) leader)))
	(when (i.reachablep instruction)
	  (when (i.has-indirect-predecessors-p instruction)
	    (when tmp
	      (collect (reverse tmp)))
	    (setf tmp nil))
	  (push instruction tmp)))
      (when tmp
	(collect (reverse tmp))))))

(defun translate-basic-block (instructions)
  (let* ((head (list 'dummy))
	 (let head)
	 (nlets 0))
    (dolist (instruction instructions)
      (let ((form (translate-instruction instruction)))
	(if (and (i.letify instruction)
		 ;; arbitrary limit:
		 (< nlets 100))
	    (let* ((out (i.output-argument instruction))
		   (l (list 'let
			    `((,out ,form))
			    `(declare (ignorable ,out)))))
	      (incf nlets)
	      (push l (cdr let))
	      (setf let (cddr l)))
	    (setf let
		  (push (if (i.output-argument instruction)
			    `(setf ,(i.output-argument instruction) ,form)
			    form)
			(cdr let))))))
    (cdr head)))

(defun translate-xblock (leader)
  (with-collector (collect :result forms :append appending)
    (let ((blocks (leader-basic-blocks leader)))
      (dolist (block blocks)
	(unless (eq (car block) leader)
	  (collect (label (i.index (car block)))))
	(appending (translate-basic-block block))))
    (let ((result
	   (if (i.xblocks leader)
	       `((cloak-handler-bind0
		  (,@(mapcar (lambda (x)
			       (unless (zerop (tc.catch-type x))
				 (pushnew (tc.catch-type x)
					  (cm.class-ids *method*)))
			       (list (tc.catch-type x)
				     (label (tc.handler-pc x))))
			     (i.xblocks leader)))
		  ,@(maybe-make-tagbody forms)))
	       forms)))
      (if (i.has-indirect-predecessors-p leader)
	  (cons (label (i.index leader)) result)
	  result))))

(defun method-argument-variables (method)
  (let ((argument-types (cm.argtype-descriptors method)))
    (unless (cm.staticp method)
      (push :object argument-types))
    (with-collector (collect-argument :result arguments)
      (let ((i 0))
        (dolist (argtype argument-types)
	  (collect-argument (local-variable i (simplify-type argtype)))
	  (if (member argtype '(:long :double))
              (incf i 2)
              (incf i 1))))
      arguments)))


(defun initialize-control-flow (method ivector)
  ;; initialize instructions successors as a linked list
  (do-instruction-array (i ivector)
    (let ((next (i.next i)))
      (when next
	(setf (i.successors i) (list next))))
    (let ((prev (i.prev i)))
      (when prev
	(setf (i.predecessors i) (list prev)))))
  ;; let instruction parsers for branches correct their successors
  (do-instruction-array (i ivector)
    (let ((*this-instruction* i))
      (apply (id.parser (getop (i.expanded-opcode i)))
	     ivector
	     (i.expanded-arguments i))))
  ;; compute phantom edges to exception handlers
  ;; (recorded separately as EXCEPTION-HANDLERS instead of SUCCESSORS because
  ;; the stack must be handled differently for them)
  (dolist (x (cm.xtable method))
    (let ((handler (elt ivector (tc.handler-pc x))))
      (for ((i :from (tc.start-pc x) :below (tc.end-pc x)))
	(let ((i (elt ivector i)))
	  (when i
	    (pushnew handler (i.exception-handlers i))
	    (pushnew i (i.predecessors handler))
	    (setf (i.has-indirect-predecessors-p handler) t))))))
  ;; kludge: successors to RET are unknown at this point and will be added
  ;; during data flow analysis
  )

(defun replace-moves (ivector)
  (let ((head nil))
    (do-instruction-array (i ivector)
      (when (i.reachablep i)
	(setf (i.stack-pointer i) (fill-pointer (i.stack i)))
	(let* ((*this-instruction* i)
	       (replacement
		(apply (id.replace (getop (i.expanded-opcode i)))
		       (i.expanded-arguments i))))
	  (unless head
	    (setf head (or replacement i))))))
    (map-into ivector (constantly :bogus) ivector)
    head))

(defun replace-with-nop (i)
  (setf (i.expanded-opcode i) 'nop)
  (setf (i.expanded-arguments i) '())
  (setf (i.input-arguments i) '())
  (setf (i.output-argument i) nil)
  (for ((defs :across (i.definitions i)))
    (dolist (def defs)
      (setf (i.uses def) (remove i (i.uses def)))))
  (setf (i.definitions i) nil)
  (assert (null (i.uses i))))

(defun replace-instruction (i moves)
  (let* ((is
	  (mapcar (lambda (m)
		    (destructuring-bind (move to from) m
		      (assert (eq move 'move))
		      (make-instruction
			:method (i.method i)
			:index (i.index i)
			:xblock-leader (i.xblock-leader i)
			:next-xblock-leader (i.next-xblock-leader i)
			:xblocks (i.xblocks i)
			:has-indirect-predecessors-p nil
			:exception-handlers (i.exception-handlers i)
			:opcode 'move
			:reachablep (i.reachablep i)
			:arguments (list to from)
			:expanded-opcode 'move
			:expanded-arguments (list to from))))
		  moves))
	 (first (first is))
	 (last (car (last is)))
	 (preds (substitute last i (i.predecessors i)))
	 (succs (substitute first i (i.successors i)))
	 (handlers (substitute first i (i.exception-handlers i))))
    (assert first)
    (setf (i.has-indirect-predecessors-p first)
	  (i.has-indirect-predecessors-p i))
    ;; insert replacements into NEXT chain and link their successor slots
    (for (((a b) :on is)
	  :while b)
      (setf (i.next a) b)
      (setf (i.prev b) a)
      (setf (i.successors a) (list b))
      (setf (i.predecessors b) (list a)))
    (when (i.prev i)
      (setf (i.next (i.prev i)) first))
    (when (i.next i)
      (setf (i.prev (i.next i)) last))
    (setf (i.prev first) (i.prev i))
    (setf (i.next last) (i.next i))
    ;; xblock fixup
    (when (eq (i.xblock-leader i) i)
      (for ((j = first :then (i.next j))
	    :while (and j (eq (i.xblock-leader j) i)))
	(setf (i.xblock-leader j) first))
      (for ((j = (i.prev first) :then (i.prev j))
	    :while j)
	(when (eq (i.next-xblock-leader j) i)
	  (setf (i.next-xblock-leader j) first))))
    ;; link FIRST
    (dolist (p preds)
      (setf (i.exception-handlers p)
	    (substitute first i (i.exception-handlers p)))
      (setf (i.successors p) (substitute first i (i.successors p))))
    (setf (i.predecessors first) preds)
    ;; link LAST
    (dolist (s succs)
      (setf (i.predecessors s) (substitute last i (i.predecessors s))))
    (dolist (h handlers)
      (setf (i.predecessors h) (append is (remove i (i.predecessors h)))))
    (setf (i.successors last) succs)
    first))

(defun determine-arguments (head)
  (for ((i = head :then (i.next i))
	:while i)
    (when (i.reachablep i)
      (when (i.stack i)
	;; kludge: c.f. translate-instruction
	(setf (i.stack-pointer i) (fill-pointer (i.stack i))))
      (let ((op (getop (i.expanded-opcode i)))
	    (*this-instruction* i))
	(setf (i.input-arguments i)
	      (apply (id.in-parser op) (i.expanded-arguments i)))
	(setf (i.output-argument i)
	      (apply (id.out-parser op) (i.expanded-arguments i)))))))

(defun move-input (move)
  (car (i.input-arguments move)))

(defun move-output (move)
  (i.output-argument move))

(defun copy-propagate (head)
  (for ((i = head :then (i.next i))
	:while i)
    (setf (i.changedp i) nil))
  (setf (i.changedp head) t)
  (setf (i.copies head) '())
  (loop
    (let ((donep t))
      (for ((i = head :then (i.next i))
	    :while i)
	(when (i.changedp i)
	  (setf donep nil)
	  (setf (i.changedp i) nil)
	  (let ((copies (update-copies (i.copies i) i)))
	    (dolist (succ (i.successors i))
	      (merge-copies-into copies succ))
	    ;; exception handlers receive so0 overwritten with the exception
	    (setf copies (remove 'so0 copies :key #'car))
	    (setf copies (remove 'so0 copies :key #'cdr))
	    (dolist (succ (i.exception-handlers i))
	      (merge-copies-into copies succ)))))
      (when donep
	(return))))
  (for ((i = head :then (i.next i))
	:while i)
    (when (slot-boundp i 'copies)
      (let ((copies (i.copies i)))
	(setf (i.input-arguments i)
	      (mapcar (lambda (v)
			(loop
			    for w = v then original
			    for original = (cdr (find w copies :key #'car))
			    while original
			    finally (assert w) (return w)))
		      (i.input-arguments i)))))))

(defun update-copies (copies-in i)
  (let ((copies-out copies-in))
    (setf copies-out (remove (i.output-argument i) copies-out :key #'cdr))
    (setf copies-out (remove (i.output-argument i) copies-out :key #'car))
    (when (eq (i.expanded-opcode i) 'move)
      (pushnew (cons (i.output-argument i) (car (i.input-arguments i)))
	       copies-out
	       :test #'equalp))
    copies-out))

(defun merge-copies-into (copies succ)
  (cond
    ((slot-boundp succ 'copies)
      (setf (i.copies succ)
	    (remove-if-not (lambda (old)
			     (cond
			       ((find old copies :test #'equalp))
			       (t
				 (setf (i.changedp succ) t)
				 nil)))
			   (i.copies succ))))
    (t
      (setf (i.copies succ) copies)
      (setf (i.changedp succ) t))))

(defun make-set ()
  (make-hash-table))

(defun set-add (elt set)
  (if (gethash elt set)
      nil
      (setf (gethash elt set) t)))

(defun map-set (fn set)
  (maphash (lambda (k v)
	     (declare (ignore v))
	     (funcall fn k))
	   set))

(defun compute-definitions (head)
  (let ((handlers '()))
    (for ((i = head :then (i.next i))
	  :while i)
      (setf (i.changedp i) (i.reachablep i))
      (when (i.reachablep i)
	(setf (i.tmp i) (make-set))
	(setf (i.definitions i)
	      (make-array (length (i.input-arguments i)) :initial-element nil))
	(setf (i.uses i) nil)
	(setf handlers (union handlers (i.exception-handlers i)))))
    (dolist (var (method-argument-variables (i.method head)))
      (set-add (make-definition var) (i.tmp head)))
    (dolist (handler handlers)
      (set-add (make-definition 'so0) (i.tmp handler))))
  (loop
    (let ((donep t))
      (for ((i = head :then (i.next i))
	    :while i)
	(when (i.changedp i)
	  (setf donep nil)
	  (setf (i.changedp i) nil)
	  (let ((defs (i.tmp i))
		(out (i.output-argument i))
		(block nil))
	    (labels ((merge-into (def succ)
		       (let ((var (i.output-argument def)))
			 (unless (eq var block)
			   (when (set-add def (i.tmp succ))
			     (setf (i.changedp succ) t)))))
		     (merge-all-into (succ)
		       (when out
			 (merge-into i succ))
		       (map-set (lambda (def)
				  (let ((var (i.output-argument def)))
				    (unless (eq var out)
				      (merge-into def succ))))
				defs)))
	      (mapc #'merge-all-into (i.successors i))
	      ;; exception handlers receive so0 overwritten with the exception
	      (setf block 'so0)
	      (mapc #'merge-all-into (i.exception-handlers i))))))
      (when donep
	(return))))
  (for ((i = head :then (i.next i))
	:while i)
    (when (i.reachablep i)
      (let ((defs (i.tmp i)))
	(for ((arg :in (i.input-arguments i))
	      (idx :from 0))
	  (map-set (lambda (def)
		     (when (eq arg (i.output-argument def))
		       (pushnew def (elt (i.definitions i) idx))
		       (pushnew i (i.uses def))))
		   defs)
	  (assert (elt (i.definitions i) idx)))))))

(defun remove-dead-moves (head)
  (compute-definitions head)
  (for ((i = head :then (i.next i))
	:while i)
    (when (and (i.reachablep i)
	       (eq (i.expanded-opcode i) 'move)
	       (null (i.uses i)))
      (replace-with-nop i))))

(defun letify (head)
  (compute-definitions head)
  (for ((i = head :then (i.next i))
	:while i)
    (when (and (i.reachablep i)
	       (not (eq (i.expanded-opcode i) 'jsr_w))
	       (i.output-argument i))
      (let ((uses (i.uses i)))
	(when (or (null uses)
		  (let ((use (car uses)))
		    (and (null (cdr uses))
			 (eq (i.xblock-leader i) (i.xblock-leader use))
			 (< (i.index i) (i.index use))
			 (for ((j = i :then (i.next j)))
			   (unless (and j
					(eq (i.xblock-leader i)
					    (i.xblock-leader j)))
			     (return t))
			   (when (i.has-indirect-predecessors-p j)
			     (return (< (i.index use) (i.index j))))))))
	  (let ((new (%tmp (i.output-argument i))))
	    (pushnew new *method-local-variables*)
	    (rename-definition i new))
	  (setf (i.letify i) t))))))

(defun all-definitions (head)
  "Return all definitions.
   KLUDGE: except for unused non-instruction definitions."
  (let ((defs '()))
    (for ((i = head :then (i.next i))
	  :while i)
      (when (i.reachablep i)
	(for ((idefs :across (i.definitions i)))
	  (setf defs (union defs idefs)))
	(when (i.output-argument i)
	  (pushnew i defs))))
    defs))

(defun compute-minimal-webs (head)
  (compute-definitions head)
  (let ((webs
	 (mapcar (lambda (def) (cons (list def) (i.uses def)))
		 (all-definitions head))))
    (let (changed)
      (loop
	(setf changed nil)
	(for (((web1 . rest) :on webs))
	  (dolist (web2 rest)
	    (when (and (eq (i.output-argument (car (car web1)))
			   (i.output-argument (car (car web2))))
		       (intersection (cdr web1) (cdr web2)))
	      (setf (car web1) (union (car web1) (car web2)))
	      (setf (cdr web1) (union (cdr web1) (cdr web2)))
	      (setf webs (remove web2 webs))
	      (setf changed t)))
	  (when changed
	    (return)))
	(unless changed
	  (return))))
    webs))

(defun minimize-webs (head)
  (compute-definitions head)
  (let ((webs
	 (mapcar (lambda (def) (cons (list def) (i.uses def)))
		 (all-definitions head))))
    (let (changed)
      (loop
	(setf changed nil)
	(for (((web1 . rest) :on webs))
	  (dolist (web2 rest)
	    (when (and (eq (i.output-argument (car (car web1)))
			   (i.output-argument (car (car web2))))
		       (intersection (cdr web1) (cdr web2)))
	      (setf (car web1) (union (car web1) (car web2)))
	      (setf (cdr web1) (union (cdr web1) (cdr web2)))
	      (setf webs (remove web2 webs))
	      (setf changed t)))
	  (when changed
	    (return)))
	(unless changed
	  (return))))
    (dolist (web webs)
      (let* ((defs (car web))
	     (uses (cdr web))
	     (var (i.output-argument (car defs)))
	     (new (%tmp var)))
	(when
	    ;; cannot rename non-instruction definitions, because their
	    ;; variables are wired into the compiler somewhere
	    (every (lambda (def) (typep def 'instruction)) defs)
	  (dolist (def defs)
	    (setf (i.output-argument def) new))
	  (dolist (use uses)
	    (setf (i.input-arguments use)
		  (substitute new var (i.input-arguments use)))))))))

(defun check-variable (v info)
  (unless (member v *method-local-variables*)
    (error "~A not found in ~A" v info)))

(defun check-variables (head)
  (for ((i = head :then (i.next i))
	:while i)
    (when (i.reachablep i)
      (when (i.output-argument i)
	(check-variable (i.output-argument i) i))
      (dolist (arg (i.input-arguments i))
	(check-variable arg i)))))

(defun compute-liveness (head)
  (for ((i = head :then (i.next i))
	:while i)
    (setf (i.changedp i) (i.reachablep i))
    (when (i.reachablep i)
      (setf (i.tmp i) (make-set))))
  (loop
    (let ((donep t))
      (for ((i = head :then (i.next i))
	    :while i)
	(when (i.changedp i)
	  (setf donep nil)
	  (setf (i.changedp i) nil)
	  (let ((vars (i.tmp i))
		(out (i.output-argument i)))
	    (labels ((merge-into (var succ)
		       (unless (i.tmp succ)
			 (%print (i.predecessors i))
			 (error "invalid predecessor to ~A" i))
		       (when (set-add var (i.tmp succ))
			 (setf (i.changedp succ) t))))
	      (dolist (p (i.predecessors i))
		(when (i.reachablep p)	;KLUDGE
		  (dolist (in (i.input-arguments i))
		    (merge-into in p))
		  (map-set (lambda (var)
			     (unless (eq var out)
			       (merge-into var p)))
			   vars)))))))
      (when donep
	(return)))))

(defun maximize-webs (head)
  (check-variables head)
  (let ((webs (compute-minimal-webs head))
	entry-definitions)
    (compute-definitions head)
    (setf entry-definitions (i.tmp head))
    (compute-liveness head)
    (setf webs (mapcar (lambda (def) (cons (list def) (i.uses def)))
		       (all-definitions head)))
    (map-set (lambda (def)
	       (unless (find def webs :key #'caar)
		 (push (cons (list def) (i.uses def)) webs)))
	     entry-definitions)
    (let (changed)
      (loop
	(setf changed nil)
	(for (((web1 . rest) :on webs))
	  (dolist (web2 rest)
	    (when (eq (i.output-argument (car (car web1)))
		      (i.output-argument (car (car web2))))
	      (setf (car web1) (union (car web1) (car web2)))
	      (setf (cdr web1) (union (cdr web1) (cdr web2)))
	      (setf webs (remove web2 webs))
	      (setf changed t)))
	  (when changed
	    (return)))
	(unless changed
	  (return))))
    (labels ((def-conflicts-with-p (def var)
	       (or (not (eq (variable-type var)
			    (variable-type (i.output-argument def))))
		   (block nil
		     (map-set (lambda (var2)
				(when (eq var var2)
				  (return t)))
			      (if (typep def 'instruction)
				  (i.tmp def)
				  (i.tmp head))))))
	     (web-conflicts-with-p (web var)
	       (some (lambda (def)
		       (def-conflicts-with-p def var))
		     (car web)))
	     (webs-conflict-p* (w1 w2)
	       (some (lambda (def)
		       (web-conflicts-with-p w2 (i.output-argument def)))
		     (car w1)))
	     (webs-conflict-p (w1 w2)
	       (or (webs-conflict-p* w1 w2)
		   (webs-conflict-p* w2 w1))))
      (let (changed)
	(loop
	  (setf changed nil)
	  (for (((web1 . rest) :on webs))
	    (dolist (web2 rest)
	      (unless (webs-conflict-p web1 web2)
		(setf (car web1) (union (car web1) (car web2)))
		(setf (cdr web1) (union (cdr web1) (cdr web2)))
		(setf webs (remove web2 webs))
		(setf changed t)))
	    (when changed
	      (return)))
	  (unless changed
	    (return)))))
    (dolist (web webs)
      (assert (equal (car web) (remove-duplicates (car web)))))
    (let ((all (loop for web in webs append (car web))))
      (loop
	  for (this . rest) on webs do
	  (dolist (d (car this))
	    (dolist (w rest)
	      (when (find d (car w))
		(print d)))))
      (assert (equal all (remove-duplicates all))))
    (setf *method-local-variables* '(so0))
    (map-set (lambda (d)
	       (pushnew (i.output-argument d) *method-local-variables*))
	     entry-definitions)
    (dolist (web webs)
      (let ((defs (car web)))
	;; cannot rename non-instruction definitions, because their
	;; variables are wired into the compiler somewhere
	(if (every (lambda (def) (typep def 'instruction)) defs)
	    (let ((new (%tmp (i.output-argument (car defs)))))
	      (pushnew new *method-local-variables*)
	      (dolist (def defs)
		(rename-definition def new)))
	    (dolist (def defs)
	      (pushnew (i.output-argument def) *method-local-variables*))))))
  (check-variables head))

(defun rename-definition (def new-var)
  (let ((old-var (i.output-argument def)))
    (assert (eq (variable-type old-var) (variable-type new-var)))
    (setf (i.output-argument def) new-var)
    (dolist (use (i.uses def))
      (setf (i.input-arguments use)
	    (substitute new-var old-var (i.input-arguments use))))))

(defparameter *optimization-instruction-limit* 500)
(defparameter *optimization-trycatch-limit* 10)
(defparameter *optimization-variables-limit* 50)

(defun translate-bytecode (cf method ivector)
  (setf ivector (copy-seq ivector))
  (let* ((*class-file* cf)
	 (*method* method)
	 (*method-local-variables* '())
	 (ninstructions (count-if-not #'null ivector))
	 (ntrycatch (length (cm.exceptions method)))
	 (nvariables (+ (cm.max-locals method) (cm.max-stack method)))
	 (optimize
	  #+slow nil
	  #-slow (and (< ninstructions *optimization-instruction-limit*)
		      (< ntrycatch *optimization-trycatch-limit*)
		      (< nvariables *optimization-variables-limit*)
		      (< (* ninstructions ntrycatch nvariables)
			 (* *optimization-instruction-limit*
			    *optimization-trycatch-limit*
			    *optimization-variables-limit*)))))
    (unless optimize
      (format t "~&; not optimizing ~A~%" method))
    (map nil #'nmacroexpand-instruction ivector)
    (initialize-control-flow method ivector)
    (inspect-data-flow method ivector)
    (mark-xblock-leaders method ivector)
    (let ((head (replace-moves ivector)))
      (determine-arguments head)
      (when optimize
	(minimize-webs head)
	(copy-propagate head)
	(remove-dead-moves head)
	(maximize-webs head)
;;;	(letify head)
	)
      (values
       (translate-into-tagbody method head)
       optimize))))

(defun variable-declaration (v)
  (let ((type (getf (symbol-plist v) 'java-type)))
    `(type ,(lisp-type type) ,v)))

(defun leaf-method-p (head)
  (not
   (for ((i = head :then (i.next i))
	 :while i)
     (when (and i (member (i.opcode i)
			  '(invokevirtual
			    invokestatic
			    invokespecial
			    invokeinterface)))
       (return t)))))

(defun translate-into-tagbody (method head)
  (let ((code
         (maybe-make-tagbody
          (with-collector (nil :append appending)
            (for ((leader = head :then (i.next-xblock-leader leader))
                  :while leader)
	      (appending (translate-xblock leader)))))))
    (let* ((args (method-argument-variables method))
           (other-locals (set-difference *method-local-variables* args)))
      `(lambda (,@args)
         (declare (ignorable ,@args)
                  ,@(mapcar #'variable-declaration args))
         (block nil
	   (sb-sys:with-pinned-objects (%class%) ;for stackwalker
	     (let* ((%pool% (cls.pool %class%))
		    ,@(mapcar
		       (lambda (v)
			 (let* ((type (getf (symbol-plist v) 'java-type))
				(default (primitive-default-value type)))
			   `(,v ,default)))
		       other-locals))
	       (declare (ignorable %pool% ,@other-locals)
			,@(mapcar #'variable-declaration other-locals))
	       ,@code)))))))

(defvar *debug-early-class-format-error* nil)

(defun class-format-error (str &rest args)
  (unless (and (boundp '*vm*) *vm*)
    (error 'class-format-error))
  (when *debug-early-class-format-error*
    (when (eq *debug-early-class-format-error* :recursion)
      (error "recursive ClassFormatError, giving up"))
    (error "ClassFormatError: ~?" str args))
  (let* ((*debug-early-class-format-error* :recursion)
	 (description (format nil "ClassFormatError: ~?" str args))
	 (exception (make-cloak-instance "java/lang/ClassFormatError"
			"<init>(Ljava/lang/String;)"
		      (make-cloak-string description))))
    (throw-exception exception)))

(defun make-fill-pointered-vector (length &optional (fill-pointer length))
  (make-array length :fill-pointer fill-pointer :initial-element nil))

(defun inspect-data-flow (method instructions)
  (do-instruction-array (i instructions)
    (setf (i.locals i) (make-fill-pointered-vector (cm.max-locals method))))
  (let ((entry (elt instructions 0))
	(argtypes (cm.argtype-descriptors method)))
    (setf (i.stack entry) (make-fill-pointered-vector (cm.max-stack method) 0))
    (setf (i.reachablep entry) t)
    (unless (cm.staticp method)
      (push :object argtypes))
    (let ((i 0))
      (for ((argument :in argtypes))
	(setf (elt (i.locals entry) i) (simplify-type argument))
	(when (member argument '(:long :double))
	  (incf i)
	  (setf (elt (i.locals entry) i) :hole))
	(incf i)))
    (setf (i.changedp entry) t))
  (loop
    (let ((donep t))
      (do-instruction-array (instruction instructions)
	(when (i.changedp instruction)
	  (setf donep nil)
	  (frob-instruction instruction)))
      (when donep
	(return)))))

(defun copy-vector (vector)
  (if (array-has-fill-pointer-p vector)
      (let ((l (array-dimension vector 0))
	    (n (fill-pointer vector)))
	(setf (fill-pointer vector) l)
	(prog1
	    (make-array l :fill-pointer n :initial-contents vector)
	  (setf (fill-pointer vector) n)))
      (make-array (array-dimension vector 0) :initial-contents vector)))

;;; locals: load-op-description, store-op-description, wide-iinc
;;; successors: [adfil]?return, goto_w, (bodp: if_acmpeq, if_acmpne,
;;;  if_icmp*, if.., ifnnonnull, ifnull), jsr_w, lookupswitch, wide-ret,
;;;  tableswitch
;;; variable OUT: getfield, getstatic, ldc2?_w, (multia)?newarray
;;; variable IN,OUT: invoke*, putfield, putstatic
;;; XXX dup, _x1, _x2, dup2, _x1, _x2, pop2?, swap
;;; ew: athrow
;;; ??? fconst

(defun verify-stack (instruction stack-in)
  (unless (>= (fill-pointer (i.stack instruction))
	      (length stack-in))
    (class-format-error "stack underflow: 4.8.2(f)"))
  (let ((needed-stack (copy-vector (i.stack instruction))))
    (decf (fill-pointer needed-stack) (length stack-in))
    (dolist (type (simplify-type stack-in))
      (vector-push type needed-stack))
    (merge-stack-into instruction needed-stack t)))

(defun frob-instruction (instruction)
  (destructuring-bind
      (&key (locals-out (i.locals instruction))
	    (n-stack-in 0)
	    ((:out stack-out) '()))
      (let ((*this-instruction* instruction))
	(apply (id.typespec (get (i.expanded-opcode instruction)
				 'instruction-definition))
	       (i.expanded-arguments instruction)))
    #+cloak-debug-dataflow
    (format t "~&; ~A: ~:A => ~:A, sets locals to ~A~%"
	    instruction (i.stack instruction) stack-out locals-out)
    (unless (<= (+ (fill-pointer (i.stack instruction))
		   (- n-stack-in)
		   (length stack-out))
		(array-dimension (i.stack instruction) 0))
      (class-format-error "stack overflow: 4.8.2(e)"))
    (let ((new-stack (copy-vector (i.stack instruction))))
      (decf (fill-pointer new-stack) n-stack-in)
      (dolist (type (simplify-type stack-out))
	(vector-push type new-stack))
      (setf (i.stack-out instruction) new-stack)
      (dolist (successor (i.successors instruction))
	(merge-stack-into successor new-stack)
	(merge-locals-into successor locals-out)))
    (let (exception-stack)
      (dolist (handler (i.exception-handlers instruction))
	#+cloak-debug-dataflow
	(format t "~&; ~A: merging from ~A to ~A~%"
		(i.method instruction)
		instruction
		handler-pc)
	(unless exception-stack
	  (setf exception-stack (copy-vector (i.stack instruction)))
	  (setf (fill-pointer exception-stack) 1)
	  (setf (elt exception-stack 0) :object)) ;fixme
	(merge-stack-into handler exception-stack)
	(merge-locals-into handler locals-out))))
  (setf (i.changedp instruction) nil))

(defun merge-stack-into (instruction new-stack &optional pretendp)
  ;; XXX Was macht PRETENDP?
  (cond
    ((not (i.stack instruction))
      #+cloak-debug-dataflow
      (format t "~&; recording new stack for ~A; ~A~%"
	      instruction
	      new-stack)
      (assert (not pretendp))
      (setf (i.stack instruction) new-stack
	    (i.changedp instruction) t)
      (setf (i.reachablep instruction) t))
    ((not (eql (fill-pointer (i.stack instruction)) (fill-pointer new-stack)))
      (class-format-error "bytecode constraint violated: 4.8.2(b)"))
    ((loop
	 for i from 0
	 for known across (i.stack instruction)
	 for new across new-stack
	 always (cond
		  ((equal known new))	;sind gleich
		  ((and (eq known :object) ;:RETURN-ADDRESS zaehlt als Objekt
			(listp new)
			(eq (car new) :return-address)))
		  ((and (eq new :object) ;ditto
			(listp known)
			(eq (car known) :return-address))
		    (unless pretendp
		      (setf (elt (i.stack instruction) i) :object)
		      (setf (i.changedp instruction) t))
		    t)
		  ((and (listp known)	;verschiedene :RETURN-ADDRESSes mergen
			(listp new)
			(eq (car known) :return-address)
			(eq (car new) :return-address))
		    (unless pretendp
		      (when (set-exclusive-or (cdr known) (cdr new))
			(setf (elt (i.stack instruction) i)
			      (remove-duplicates (append known (cdr new))))
			(setf (i.changedp instruction) t)))
		    t))))
    (t
      (class-format-error "bytecode constraint violated: 4.8.2(a)"))))

(defun merge-locals-into (instruction new-locals)
  (let ((known-locals (i.locals instruction)))
    (for* ((i :from 0 :below (length new-locals))
	   (known = (elt known-locals i))
	   (new = (elt new-locals i)))
      (cond
	((or (null new) (equalp known new))
	  #+cloak-debug-dataflow
	  (format t "~&; ~A[~D] == ~A~%" instruction i known))
	((null known)
	  #+cloak-debug-dataflow
	  (format t "~&; ~A[~D] <= ~A~%" instruction i new)
	  (setf (elt known-locals i) new
		(i.changedp instruction) t))
	((and (eq known :object)	;:RETURN-ADDRESS zaehlt als Objekt
	      (listp new)
	      (eq (car new) :return-address)))
	((and (eq new :object)		;ditto
	      (listp known)
	      (eq (car known) :return-address))
	  (setf (elt known-locals i) :object)
	  (setf (i.changedp instruction) t)
	  t)
	((and (listp known)		;verschiedene :RETURN-ADDRESSes mergen
	      (listp new)
	      (eq (car known) :return-address)
	      (eq (car new) :return-address))
	  (when (set-exclusive-or (cdr known) (cdr new))
	    (setf (elt known-locals i)
		  (remove-duplicates (append known (cdr new))))
	    (setf (i.changedp instruction) t))
	  t)
	(t
	  (setf (elt known-locals i) nil
		(i.changedp instruction) t))))))

(defvar *labels* (make-array 65536 :initial-element nil))
(defvar *locals/object* (make-array 65536 :initial-element nil))
(defvar *locals/int* (make-array 65536 :initial-element nil))
(defvar *locals/long* (make-array 65536 :initial-element nil))
(defvar *locals/float* (make-array 65536 :initial-element nil))
(defvar *locals/double* (make-array 65536 :initial-element nil))
(defvar *stack-entries/object* (make-array 65536 :initial-element nil))
(defvar *stack-entries/int* (make-array 65536 :initial-element nil))
(defvar *stack-entries/long* (make-array 65536 :initial-element nil))
(defvar *stack-entries/float* (make-array 65536 :initial-element nil))
(defvar *stack-entries/double* (make-array 65536 :initial-element nil))

(defun intern-label (i table prefix)
  (or
   (elt table i)
   (let ((name (concatenate 'string prefix (princ-to-string i))))
     (setf (elt table i) (intern name :cloak)))))

(defparameter *local-variable*
    (lambda (i type)
      (let* ((type (cond
                     ((listp type) (car type))
                     (t type)))
             (l
              (ecase type
                (:int    (intern-label i *locals/int* "LI"))
                (:long   (intern-label i *locals/long* "LL"))
                (:float  (intern-label i *locals/float* "LF"))
                (:double (intern-label i *locals/double* "LD"))
                ((:object :return-address)
                  (intern-label i *locals/object* "LO")))))
	(assert type)
        (setf (getf (symbol-plist l) 'java-type) type) 
        (pushnew l *method-local-variables*)
        l)))
(defun local-variable (i type)
  (funcall *local-variable* i type))

(defun label (i)
  (intern-label i *labels* "L"))

(defun lisp-type (type)
  (ecase type
    (:int '(signed-byte 32))
    (:long '(signed-byte 64))
    (:float 'single-float)
    (:double 'double-float)
    ((:object :return-address)
      t
      #+nil `(or cloak-object (eql ,+null+)))))

(defparameter *stack-entry*
    (lambda (i type)
      (let* ((type (cond
                     ((listp type) (car type))
                     (t type)))
             (l
              (ecase type
                (:int    (intern-label i *stack-entries/int* "SI"))
                (:long   (intern-label i *stack-entries/long* "SL"))
                (:float  (intern-label i *stack-entries/float* "SF"))
                (:double (intern-label i *stack-entries/double* "SD"))
                ((:object :return-address)
                  (intern-label i *stack-entries/object* "SO")))))
        (setf (getf (symbol-plist l) 'java-type) type)
        (setf (stack-variable-p l) t)
        (pushnew l *method-local-variables*)
        l)))
(defun stack-entry (i &optional type)
  (funcall *stack-entry* i type))

(defun stack-variable-p (symbol)
  (getf (symbol-plist symbol) 'stackp))

(defun (setf stack-variable-p) (newval symbol)
  (setf (getf (symbol-plist symbol) 'stackp) newval))

(defun variable-type (symbol)
  (getf (symbol-plist symbol) 'java-type))

(defun (setf variable-type) (newval symbol)
  (setf (getf (symbol-plist symbol) 'java-type) newval))

(setf (getf (symbol-plist 'so0) 'java-type) :object)
(setf (stack-variable-p 'so0) t)

(defun nullpointerexception ()
  (throw-exception
   (make-cloak-instance "java/lang/NullPointerException" "<init>()")))

(defsubst assert-nonnull (object)
  #+sigsegv-hack (assert object)
  (when (nullp object)
    (nullpointerexception))
  object)

#+(and sigsegv-hack (not slow))
(progn
  (defun sigsegv-handler (c)
    (declare (ignore c))
    (throw-exception
     (make-cloak-instance "java/lang/NullPointerException" "<init>()")))

  (defsubst maybe-assert-nonnull (object)
    object))

#-(and sigsegv-hack (not slow))
(defsubst maybe-assert-nonnull (object)
  (assert-nonnull object))

(defun checked-elt (sequence index &optional element-type)
  (declare (ignore element-type))
  (unless (and (<= 0 index) (< index (length sequence)))
    (throw-exception "java/lang/ArrayIndexOutOfBoundsException" "~A/[0,~D)"
      index
      (length sequence)))
  (elt sequence index))

(define-compiler-macro checked-elt
    (&whole form sequence index &optional element-type)
  (let ((evaluated-type
         (cond
           ((and (consp element-type) (eq (car element-type) 'quote))
             (cadr element-type))
           ((eq element-type 't) t)
           (t nil))))
    (if evaluated-type
        `(let ((sequence
                #-slow (the (simple-array ,evaluated-type (*)) ,sequence)
                #+slow ,sequence)
               (index ,index))
           #+slow (check-type sequence (simple-array ,evaluated-type (*)))
           (unless (and (<= 0 index) (< index (length sequence)))
             (throw-exception "java/lang/ArrayIndexOutOfBoundsException"))
           (aref sequence index))
        form)))

;; no compiler macros on SETF functions in SBCL

(defun (setf checked-elt) (newval sequence index)
  (checked-set-elt newval sequence index))

(defun checked-set-elt (newval sequence index &optional element-type)
  (declare (ignore element-type))
  (unless (and (<= 0 index) (< index (length sequence)))
    (throw-exception "java/lang/ArrayIndexOutOfBoundsException" "~A/[0,~D)"
      index
      (length sequence)))
  (setf (elt sequence index) newval))

(define-compiler-macro checked-set-elt
    (&whole form newval sequence index &optional element-type)
  (let ((evaluated-type
         (cond
           ((and (consp element-type) (eq (car element-type) 'quote))
             (cadr element-type))
           ((eq element-type 't) t)
           (t nil))))
    (if evaluated-type
        `(let ((sequence
                #-slow (the (simple-array ,evaluated-type (*)) ,sequence)
                #+slow ,sequence)
               (index ,index)
               (newval ,newval))
           #+slow (check-type sequence (simple-array ,evaluated-type (*)))
           (unless (and (<= 0 index) (< index (length sequence)))
             (throw-exception "java/lang/ArrayIndexOutOfBoundsException"))
           (setf (aref sequence index) newval))
        form)))

(defun find-field (name class)
  ;; return class and field
  (loop
      for c = class then (cls.superclass c)
      while c do
	(let ((field (find name (cls.fields c) :key #'cm.name :test #'equal)))
	  (when field
	    (return (values c field))))))

(defun getstatic (name class-object)
  (multiple-value-bind (class field)
      (find-field name class-object)
    (funcall (field-reffer (cm.type-descriptor field))
	     (cls.static-field-values class-object)
	     (vtable-index class field))))

(defun getfield (name object)
  (multiple-value-bind (class field)
      (find-field name (%class object))
    (funcall (field-reffer (cm.type-descriptor field))
	     object
	     (vtable-index class field))))

(defun (setf getstatic) (newval name class-object)
  (multiple-value-bind (class field)
      (find-field name class-object)
    (funcall (field-setter (cm.type-descriptor field))
	     (cls.static-field-values class-object)
	     (vtable-index class field)
	     newval)))

;; Siivola 2007.  When not to use SETF-functions.  http://random-state.net
(defsetf getfield setfield)
(defun setfield (name object newval)
  (multiple-value-bind (class field)
      (find-field name (%class object))
    (funcall (field-setter (cm.type-descriptor field))
	     object
	     (vtable-index class field)
	     newval)))

(defun cached-getfield (box name object)
  (let ((runtime-class (%class object)))
    (unless (eql runtime-class (car box))
      (setf (cdr box)
	    (multiple-value-bind (class field)
		(find-field name (%class object))
	      (let ((reffer (field-reffer (cm.type-descriptor field)))
		    (index (vtable-index class field)))
		(lambda (object)
		  (funcall reffer object index)))))
      (setf (car box) runtime-class))
    (funcall (cdr box) object)))

(defun cached-setfield (box name object newval)
  (let ((runtime-class (%class object)))
    (unless (eql runtime-class (car box))
      (setf (cdr box)
	    (multiple-value-bind (class field)
		(find-field name (%class object))
	      (let ((reffer (field-setter (cm.type-descriptor field)))
		    (index (vtable-index class field)))
		(lambda (object newval)
		  (funcall reffer object index newval)))))
      (setf (car box) runtime-class))
    (funcall (cdr box) object newval)))

(define-compiler-macro getfield (&whole whole name object)
  (if (stringp name)
      `(cached-getfield (load-time-value (cons nil nil)) ,name ,object)
      whole))

(define-compiler-macro setfield (&whole whole name object newval)
  (if (stringp name)
      `(cached-setfield (load-time-value (cons nil nil)) ,name ,object ,newval)
      whole))

;;;(defmacro with-fields ((&rest fields) object &body body)
;;;  (with-gensyms (o)
;;;    `(let ((,o ,object))
;;;       (symbol-macrolet
;;;	   (,@(mapcar (lambda (name)
;;;			`(,name (getfield ,(symbol-name name) ,o)))
;;;		fields))
;;;	 ,@body))))

;; this needs to be fast
(defsubst cloak-subclass-p (c1 c2)
  (dolist (c (cls.cpl c1) nil)
    (when (eq c c2)
      (return t))))

(defsubst cloak-type-p (o c)
  (cloak-subclass-p (%class o) c))

(defun precompile-class-files (wild-pathname &optional force)
  (mapc (rcurry #'precompile-class-file force) (directory wild-pathname)))

(defun precompile-class-file (pathname &optional (force t))
  (let* ((bytes (file-contents pathname))
	 (hash (sb-md5:md5sum-sequence bytes)))
    (when (or force (not (quick-probe-file (heap-file-name hash))))
      (let ((cf (read-cloak-class bytes
				  :cached-pathname pathname
				  :cached-mtime (quick-file-write-date
						 pathname)
				  :hash hash))
	    (*string-cache* (make-hash-table :test 'equal)))
	(compile-class-file cf)
	cf))))

(defun precompile-zip-files (wild-pathname &optional force)
  (whereas ((failed
	     (loop
		 for pathname in (directory wild-pathname)
		 append (%precompile-zip-file pathname force))))
    (warn "some classes failed to compile:~_ ~A" failed)))

(defun precompile-zip-file (pathname &optional force)
  (whereas ((failed (%precompile-zip-file pathname force)))
    (warn "some classes failed to compile:~_ ~A" failed)))

(defun %precompile-zip-file (pathname &optional force)
  (let* ((failed nil)
	 (namestring (namestring pathname))
	 (mtime (quick-file-write-date namestring)))
    (zip:with-zipfile (zip pathname)
      (zip:do-zipfile-entries (name entry zip)
	(when (string-suffix-p name ".class")
	  (let* ((bytes (zip:zipfile-entry-contents entry))
		 (hash (sb-md5:md5sum-sequence bytes)))
	    (if (or force (not (quick-probe-file (heap-file-name hash))))
		(handler-case
		    (let ((cf (read-cloak-class bytes
						:cached-pathname namestring
						:cached-mtime mtime
						:hash hash))
			  (*string-cache* (make-hash-table :test 'equal)))
		      (compile-class-file cf))
		  (class-format-error (c)
		    (push name failed)
		    (format t "~&; delaying error in ~A:~_  ~A~%" name c)))
		(format t "~&; skipping cached class ~A~%" name))))))
    failed))

(defvar *load-class-file*)
(defvar *compiler-lock* (sb-thread:make-mutex))
(defun compile-class-file (cf)
  (sb-thread:with-mutex (*compiler-lock*)
    (let* ((p (merge-pathnames "tmp.lisp" cloak-system:*source-directory*))
	   (*load-class-file* cf))
      (format t "~&; translating ~A~%" (cf.name cf))
      (force-output)
      (translate-class-file cf p)
      (let* (#+(or) (sb-c::*compile-progress* t)
	     (sb-c::*enable-propagate-constraints-hack* t)
	     (nnotes 0)
             (*compile-print* nil)
	     (fasl
	      (handler-bind ((sb-ext:compiler-note
			      (lambda (c)
				(when *suppress-compiler-notes*
				  (incf nnotes)
				  (muffle-warning c)))))
		(compile-file p))))
	(unless (zerop nnotes)
	  (format t "~&; WARNING: ~D compiler note~:P suppressed~%" nnotes))
	(delete-file p)
	(force-output)
	(handler-bind
	    ;; shut up!  couldn't care less about undefined aliens
	    ((style-warning #'muffle-warning))
	  (load fasl :verbose t)))
      (dolist (m (cf.methods cf))
	(clean-out-compiler-data m)
	(let ((fn (cm.method-function m)))
	  (when fn
	    (unless (or (cm.abstractp m) (cm.nativep m))
	      (fill-in-line-number-table m))
	    (clean-out-debug-junk (sb-heapdump::simple-fun-code-object fn)))))
      (setf (cf.compiledp cf) t)
      (dump-class-file cf))))

(defun clean-out-compiler-data (method)
  (setf (cm.code method) nil)
  (setf (cm.xtable method) nil))

(defun clean-out-debug-junk (code)
  (setf (sb-kernel::%code-debug-info code) nil)
  ;; avoid symbol fixups:
  (loop
      for fn = (sb-kernel:%code-entry-points code)
      then (sb-kernel:%simple-fun-next fn)
      while fn
      do
	(setf (sb-kernel:%simple-fun-name fn)
	      (write-to-string (sb-kernel:%simple-fun-name fn)
			       :escape nil
			       :pretty nil))
	(setf (sb-kernel:%simple-fun-arglist fn) '("bogus arglist"))
	(setf (sb-kernel:%simple-fun-type fn) 'function)))

(defun translate-method (cf method)
  (let ((*gensym-counter* 0))
    (unless (or (cm.nativep method) (cm.abstractp method))
      (let* ((code (fetch-instructions
		    method
		    (make-buffer (cm.code method)))))
        (multiple-value-bind (expr optimizep)
	    (translate-bytecode cf method code)
	  (assert (eq (car expr) 'lambda))
	  (when (cm.synchronizedp method)
	    (setf expr
		  (let ((object (gensym)))
		    `(lambda ,(second expr) ;XXX
		       (let ((,object
			      ,(if (cm.staticp method)
				   '(class-object %class%)
				   (car (second expr)))))
			 (enter-object-monitor ,object)
			 (unwind-protect
			     (,expr ,@(second expr))
			   (exit-object-monitor ,object)))))))
	  (let* ((name
		  (make-symbol (format nil "~A.~A"
				       (cf.name cf)
				       (cm.signature method)))))
	    (values
	     `(lambda (%class%)
		(flet ((,name ,@(rest expr)))
		  #',name))
	     optimizep)))))))

(defun native-method-name (cf method signaturep)
  (with-output-to-string (s)
    (flet ((mangle (str)
	     (for ((c :across str))
	       (cond
		 ((eql c #\/) (write-char #\_ s))
		 ((eql c #\_) (write-string "_1" s))
		 ((eql c #\;) (write-string "_2" s))
		 ((eql c #\[) (write-string "_3" s))
		 ((< (char-code c) 128)
		   (write-char c s))
		 (t
		   (format s "_0~4,'0X" (char-code c)))))))
      (write-string "Java_" s)
      (mangle (cf.name cf))
      (write-string "_" s)
      (mangle (cm.name method))
      (when signaturep
	(write-string "__" s)
	(let ((sig (cm.signature method)))
	  (mangle (subseq sig (1+ (position #\( sig)) (1- (length sig)))))))))

(defun alien-type (keyword)
  (ecase keyword
    (:byte `(sb-alien:signed 8))
    (:short `(sb-alien:signed 16))
    (:int `(sb-alien:signed 32))
    (:long `(sb-alien:signed 64))
    (:boolean `(sb-alien:unsigned 8))
    (:char `(sb-alien:unsigned 16))
    (:float 'single-float)
    (:double 'double-float)
    (:void 'sb-alien:void)
    (:object '(* t))))

(defun simplify-to-keyword (type)
  (case type
    ((:byte :boolean :char :short :int :long :float :double)
      type)
    (t
      :object)))

(defun fasl-translate-native-method (cf method i)
  (let* ((argtypes (cm.argtype-descriptors method))
	 (rtype (cm.rtype-descriptor method))
	 (short-name (native-method-name cf method nil))
	 (long-name (native-method-name cf method t))
	 (lisp-name
	  (format nil "~A.~A" (cf.name cf) (cm.signature method)))
	 (short-symbol (gensym))
	 (long-symbol (gensym))
	 (alien-rtype
	  (alien-type (simplify-to-keyword rtype)))
	 (arg-decls
	  (mapcar (lambda (type)
		    (list (gensym) type))
		  (list* '(* t)
			 '(* t)
			 (loop
			     for arg in argtypes
			     collect (alien-type
				      (simplify-to-keyword arg))))))
	 (arg-names (loop repeat (length argtypes) collect (gensym)))
	 (staticp (cm.staticp method))
	 (extended-arg-names (if staticp arg-names (cons 'this arg-names)))
	 (call
	  `(funcall fn
		    *jni-env-sap*
		    ,(if staticp
			 '(jclass %class%)
			 '(new-local-ref this))
		    ,@(loop
			  for type in argtypes
			  for name in arg-names
			  collect
			    (if (eq (simplify-type type) :object)
				`(new-local-ref ,name)
				name)))))
    `(progn
       (sb-alien:define-alien-routine (,short-name ,short-symbol)
	   ,alien-rtype
	 ,@arg-decls)
       ,@(when (< (length long-name) 256)
	   `((sb-alien:define-alien-routine (,long-name ,long-symbol)
		 ,alien-rtype
	       ,@arg-decls)))
       (macrolet ((%maybe-synchronized (form)
		    ,(if (cm.synchronizedp method)
			 (let ((object
				(if staticp
				    '(class-object %class%)
				    'this)))
			   ``(progn
			       (enter-object-monitor ,',object)
			       (unwind-protect
				   ,form
				 (exit-object-monitor ,',object))))
			 'form)))
	 (let ((method (elt (cf.methods *load-class-file*) ,i)))
	   (setf (cm.method-function method)
		 (lambda (%class%)
		   (block nil
		     (let* ((lisp-symbol (find-symbol ,lisp-name :cloak))
			    (fn (cond
				  ((null %class%)
				    ;; called by add-to-ugly-class-table
				    nil)
				  ((and lisp-symbol (fboundp lisp-symbol))
				    (let ((fn (symbol-function lisp-symbol)))
				      (return
					(lambda (,@extended-arg-names)
					  (sb-sys:with-pinned-objects
					      ;; for stackwalker:
					      (%class%)
					    (%maybe-synchronized
					     (funcall fn ,@extended-arg-names)))))))
				  ((cloak::foreign-symbol-bound-p ,short-name)
				    #',short-symbol)
				  ((cloak::foreign-symbol-bound-p ,long-name)
				    #',long-symbol)
				  (t
				    (throw-exception
					"java/lang/UnsatisfiedLinkError"
					"~A"
				      ,long-name)))))
		       (lambda (,@extended-arg-names)
			 (sb-sys:with-pinned-objects (%class%) ;for stackwalker
			   (%maybe-synchronized
			    (with-localref-frame (,(+ (length argtypes) 170))
			      (let* ((*%class%* %class%)
				     (*pending-exception* nil))
				(prog1
				    ,(if (eq (simplify-type rtype) :object)
					 `(resolve-ref (sb-alien:alien-sap ,call))
					 call)
				  (when *pending-exception*
				    (throw 'java-exception
				      *pending-exception*)))))))))))))))))

(defun write-source (form stream &optional (terpri t))
  (write form :stream stream :pretty nil :readably t :escape t :circle t)
  (when terpri
    (terpri stream)))

(defun fasl-translate-normal-method (cf method i)
  (multiple-value-bind (lambda optimizep)
      (translate-method cf method)
    (values
     `(let ((m (elt (the list (cf.methods *load-class-file*)) ,i)))
	,@(unless optimizep		;try to reduce code size explosion:
	    `((declare (notinline assert-nonnull
				  maybe-assert-nonnull
				  maybe-initialize-class
				  cloak-subclass-p
				  checked-elt
				  checked-set-elt))))
	(setf (cm.method-function m) ,lambda)
	(setf (cm.method-ids m) ',(cm.method-ids method))
	(setf (cm.field-ids m) ',(cm.field-ids method))
	(setf (cm.class-ids m) ',(cm.class-ids method))
	(setf (cm.string-ids m) ',(cm.string-ids method)))
     optimizep)))

(defun translate-class-file (cf lisp-pathname)
  (with-open-file (s lisp-pathname :direction :output :if-exists :supersede)
    (write-line "(in-package :cloak)" s)
    (write-line "(declaim (optimize sb-c::compute-debug-fun))" s)
    (for ((i :from 0)
	  (method :in (cf.methods cf)))
      (unless (cm.abstractp method)
	(multiple-value-bind (form optimizep)
	    (if (cm.nativep method)
		(values (fasl-translate-native-method cf method i) t)
		(fasl-translate-normal-method cf method i))
	  (unless (cm.nativep method)
	    (setf (cm.line-numbers method)
		  (build-line-table form (cm.line-numbers method))))
	  (let* ((*package* (find-package :cloak))
		 (fast '((speed 3) (safety 0) (sb-ext:inhibit-warnings 0)
			 (compilation-speed 0)))
		 (slow '((speed 1) (safety 0) sb-ext:inhibit-warnings
			 compilation-speed))
		 (policy (if optimizep fast slow)))
	    (write-source `(declaim (optimize ,@policy)) s)
	    (write-source form s)))))))

(defun find-java-line-number (i table)
  (cdr (find-if (rcurry #'<= i) table :key #'car)))

(defvar *instruction-number-table*)
(defvar *current-form-number*)
(defun build-line-table (form java-line-table)
  (let ((*current-form-number* 0)
	(*instruction-number-table* '()))
    (%build-instruction-table form (make-hash-table))
    (dolist (cons *instruction-number-table*)
      (setf (cdr cons) (find-java-line-number (cdr cons) java-line-table)))
    *instruction-number-table*))
;;; copy&paste from ir1tran:
(defun %build-instruction-table (form seen)
  (unless (gethash form seen)
    (setf (gethash form seen) t)
    (when (and (consp form) (eq (car form) 'with-instruction))
      (push (cons *current-form-number* (second form))
	    *instruction-number-table*))
    (incf *current-form-number*)
    (let ((subform form)
	  (trail form))
      (macrolet ((frob ()
		   '(progn
		      (when (atom subform) (return))
		      (let ((fm (car subform)))
			(when (consp fm)
			  (%build-instruction-table fm seen)))
		      (setq subform (cdr subform))
		      (when (eq subform trail) (return)))))
	(loop
	  (frob)
	  (frob)
	  (setq trail (cdr trail)))))))

(defun fill-in-line-number-table (method)
  (let* ((component
	  (sb-heapdump::simple-fun-code-object (cm.method-function method)))
	 (locations (code-component-locations component))
	 (lines (cm.line-numbers method))
	 prev)
    (assert (eq component
		(sb-heapdump::simple-fun-code-object
		 (sb-kernel:%closure-fun
		  (funcall (cm.method-function method) nil)))))
    (setf locations
	  (sort locations #'< :key #'sb-di::compiled-code-location-pc))
    (let ((result '()))
      (dolist (loc locations)
	(let* ((form (sb-di::code-location-form-number loc))
	       (line (find-java-line-number form lines)))
	  (when (and line (not (eql prev line)))
	    (push line result)
	    (push (sb-di::compiled-code-location-pc loc) result)
	    (setf prev line))))
      (setf (cm.line-numbers method) (coerce result 'vector))
      ;; (caller can get rid of all debug info now)
      )))

(defun code-component-locations (component)
  (let ((fun-map
	 (sb-c::compiled-debug-info-fun-map
	  (sb-kernel::%code-debug-info
	   component))))
    (loop
	for debug-fun across fun-map
	unless (integerp debug-fun)
	append (debug-fun-locations debug-fun component))))

(defun debug-fun-locations (debug-fun component)
  (let ((debug-debug-fun
	 (sb-di::%make-compiled-debug-fun debug-fun component)))
    (when (sb-di::debug-fun-blocks debug-debug-fun)
      (let ((blocks (sb-di::parse-compiled-debug-blocks debug-debug-fun)))
	(loop
	    for block across blocks
	    append
	      (coerce (sb-di::compiled-debug-block-code-locations block)
		      'list))))))
