;;; Copyright (c) 1999-2004 David Lichteblau <david@lichteblau.com>
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

(defmacro whereas (bindings &body body)
  (if bindings
      (destructuring-bind (name value)
	  (car bindings)
	`(let ((,name ,value))
	   (when ,name
	     (whereas ,(cdr bindings)
	       ,@body))))
      `(progn ,@body)))

(defun missing ()
  (error "missing slot value"))

(defmacro with-gensyms ((&rest symbols) &body body)
  `(let (,@(loop
	       for symbol in symbols
	       collect `(,symbol (copy-symbol ',symbol))))
     ,@body))

(defmacro with-collector
    ((&optional (collect 'collect) &key result nconc append) &body body)
  (unless collect (setf collect '#:collect))
  (check-type collect symbol)
  (check-type result symbol)
  (with-gensyms (head tail)
    `(let* ((,head (cons nil '()))
	    (,tail ,head))
       (labels ((,collect (frob)
		  (setf ,tail (setf (cdr ,tail) (cons frob '())))
		  frob)
		,@(when nconc
		    `((,nconc (frobs)
			      (setf ,tail (last (setf (cdr ,tail) frobs))))))
		,@(when append
		    `((,append (frobs)
			       (dolist (frob frobs)
				 (,collect frob))))))
	 (symbol-macrolet (,@(when result
			       `((,result (cdr ,head)))))
	   ,@body))
       ,@(unless result
	   `((cdr ,head))))))

(defun for-aux (kind clauses body-form finally-forms)
  `(loop ,@(loop
	       for firstp = t then nil
	       for %clauses = clauses then (rest %clauses)
	       for clause = (first %clauses) then (first %clauses)
	       while (and %clauses (listp clause))
	       append (cons (ecase kind
			      (for (if firstp 'as 'and))
			      (for* 'as))
			    clause)
	       into result
	       finally (return (append result %clauses)))
       do (progn ,body-form)
       finally (progn) ,@finally-forms))

(defmacro %for ((&rest clauses) body-form &rest finally-forms)
  (for-aux 'for clauses body-form finally-forms))

(defmacro %for* ((&rest clauses) body-form &rest finally-forms)
  (for-aux 'for* clauses body-form finally-forms))

(defmacro for ((&rest clauses) &rest body-forms)
  `(%for ,clauses
       (progn ,@body-forms)))

(defmacro for* ((&rest clauses) &rest body-forms)
  `(%for* ,clauses
       (progn ,@body-forms)))

(defmacro while (condition &rest body-forms)
  `(for (:while ,condition)
     ,@body-forms))

(defmacro until (condition &rest body-forms)
  `(for (:until ,condition)
     ,@body-forms))

(defmacro do-sequence ((elt seq) &body body)
  `(block nil
     (map nil (lambda (x)
		,(if (listp elt)
		     `(destructuring-bind ,elt x ,@body)
		     `(let ((,elt x)) ,@body)))
	  ,seq)))

(defun listify (form)
  (if (listp form) form (list form)))

(defun string-suffix-p (str suffix)
  (and (<= (length suffix) (length str))
       (loop
	   for j :from 0 :below (length suffix)
	   and i :from (- (length str) (length suffix))
	   always (char= (char str i) (char suffix j)))))

(defun starts-with-p (string prefix)
  (let ((mismatch (mismatch string prefix)))
    (or (null mismatch) (= mismatch (length prefix)))))

(defmacro n++ (place &optional (delta 1) &environment env)
  "post-increment version of incf"
  (multiple-value-bind (dummies vals newval setter getter)
      (get-setf-expansion place env)
    `(let* (,@(mapcar #'list dummies vals) (,(car newval) ,getter))
       (prog1
	   ,(car newval)
	 (incf ,(car newval) ,delta)
	 ,setter))))

(defun adjust-vector-exponentially (vector new-dimension set-fill-pointer-p)
  (let ((d (array-dimension vector 0)))
    (when (< d new-dimension)
      (loop
	  do (setf d (* 2 d))
	  while (< d new-dimension))
      (adjust-array vector d))
    (when set-fill-pointer-p
      (setf (fill-pointer vector) new-dimension))))

(defun curry (fn x)
  (lambda (&rest args)
    (apply fn x args)))

(defun rcurry (fn x)
  (lambda (&rest args)
    (apply fn (append args (list x)))))

(defun compose (f g)
  (lambda (x) (funcall f (funcall g x))))

(defun note (str &rest format-args)
  (format t "~&; ~?~%" str format-args)
  (force-output))

(defun %print (x)
  (fresh-line)
  (write x :readably nil :escape t :level nil :length nil :circle nil)
  (fresh-line)
  (force-output)
  (values))

;;;(defvar *gc-time* 0)
;;;
;;;(defun reset-gc-time ()
;;;  (setf *gc-time* 0))
;;;
;;;(defun before-gc ()
;;;  (decf *gc-time* (get-internal-run-time)))
;;;
;;;(defun after-gc ()
;;;  (incf *gc-time* (get-internal-run-time)))
;;;
;;;(pushnew 'before-gc SB-EXT:*BEFORE-GC-HOOKS*)
;;;(pushnew 'after-gc SB-EXT:*AFTER-GC-HOOKS*)
