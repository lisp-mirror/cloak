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

(defvar *constants*)
(defvar *buffer*)

(defun assemble-class
    (name super &key interfaces flags fields methods attributes filename)
  (declare (optimize (safety 3)))
  (let* ((*constants* (make-constant-pool))
	 (*buffer* (make-output-buffer))
	 (this (put-constant :class (put-constant :utf8 name)))
	 (super (put-constant :class (put-constant :utf8 super)))
	 (interfaces			;translate to constant pool offsets
	  (map 'vector
	    (lambda (i) (put-constant :class (put-constant :string i)))
	    interfaces)))
    ;; actual assembly step
    (map nil #'assemble-method! methods)
    ;; for side effect on constant pool
    (let ((*buffer* nil))
      (emit* #'emit-field-or-method fields) 
      (emit* #'emit-field-or-method methods)
      (emit* #'emit-attribute attributes))
    ;; actual output
    (emit-int #xcafebabe)
    (emit-short (cdr *smallest-supported-version*))
    (emit-short (car *smallest-supported-version*))
    (emit* #'emit-constant *constants*)
    (emit-short (encode-class-access-flags flags))
    (emit-short this)
    (emit-short super)
    (emit* #'emit-short interfaces)
    (emit* #'emit-field-or-method fields)
    (emit* #'emit-field-or-method methods)
    (emit* #'emit-attribute attributes)
    (when filename
      (with-open-file (s filename :direction :output :if-exists :rename)
	(write-sequence *buffer* s)))
    *buffer*))

(defun assemble-method! (m)
  (destructuring-bind (&key (locals '()) (stack 0) &allow-other-keys) m
    (let ((code (cdr (member :code m))))
      (setf (car code)
	    (assemble-code (car code) :locals locals :stack stack)))))

(defun simple-instruction-length (opcode)
  (with-reducer (smash '+ 1)
    (dolist (type (argument-types (get opcode 'instruction-definition)))
      (smash
       (ecase type
	 ((:ubyte :byte) 1)
	 ((:ushort :short) 2)
	 (:int 4)
	 (:method 2))))))

(defun assemble-code (code &key stack locals)
  (let ((*buffer* (make-output-buffer))
	(labels (make-hash-table))
	(pos 0))
    (emit-short stack)
    (emit-short (if (numberp locals) locals (length locals)))
    (dolist (i code)
      (if (symbolp i)
	  (setf (gethash i labels) pos)
	  (incf pos (simple-instruction-length (car i)))))
    (emit-int pos)
    (setf pos 0)
    (dolist (i code)
      (unless (symbolp i)
	(emit-byte (position (car i) *opcodes*))
	(for ((type :in (argument-types (get (car i) 'instruction-definition)))
	      (arg :in (cdr i)))
	  (typecase arg
	    (symbol (setf arg (- (gethash arg labels) pos)))
	    (string (setf arg (put-constant :string arg))))
	  (ecase type
	    ((:ubyte :byte) (emit-byte arg))
	    ((:ushort :short) (emit-short arg))
	    (:int (emit-int arg))
	    (:method (emit-short arg))))
	(incf pos (simple-instruction-length (car i)))))
    (emit-short 0)			;exceptions not supported yet
    (emit-short 0)			;attributes not supported yet
    *buffer*))

(defun make-output-buffer ()
  (make-array 42
	      :element-type '(unsigned-byte 8)
	      :adjustable t
	      :fill-pointer 0))

(defun emit-byte (b)
  (when *buffer*
    (vector-push-extend b *buffer*)))

(defun emit-short (s)
  (when *buffer*
    (vector-push-extend (ldb (byte 8 8) s) *buffer*)
    (vector-push-extend (ldb (byte 8 0) s) *buffer*)))

(defun emit-int (s)
  (when *buffer*
    (vector-push-extend (ldb (byte 8 24) s) *buffer*)
    (vector-push-extend (ldb (byte 8 16) s) *buffer*)
    (vector-push-extend (ldb (byte 8 08) s) *buffer*)
    (vector-push-extend (ldb (byte 8 00) s) *buffer*)))

(defun emit* (fn vector)
  (emit-short (length vector))
  (map nil fn vector))

(defun emit-constant (constant)
  (when constant
    (ecase (car constant)
      (:class
	(emit-byte +constant_class+)
	(emit-short (cadr constant)))
      (:string
	(emit-byte +constant_string+)
	(emit-short (cadr constant)))
      (:utf8
	(emit-byte +constant_utf8+)
	(emit* #'emit-byte (string-to-utf-8 (cadr constant))))))
  nil)

(defun make-constant-pool ()
  (make-array 42 :adjustable t :fill-pointer 1 :initial-element nil))

(defun put-constant (type value)
  (when (eq type :string)
    (setf value (put-constant :utf8 value)))
  (cond
    ((position (list type value) *constants* :test #'equal))
    (t
      (prog1
	  (length *constants*)
	(vector-push-extend (list type value) *constants*)))))

(defun encode-class-access-flags (flags)
  (let ((result 0))
    (dolist (flag flags)
      (let ((mask
	     (ecase flag
	       (:public +acc_public+)
	       (:final +acc_final+)
	       (:super +acc_super+)
	       (:interface +acc_interface+)
	       (:abstract +acc_abstract+))))
	(setf result (logior mask result))))
    result))

(defun encode-member-access-flags (flags)
  (let ((result 0))
    (dolist (flag flags)
      (let ((mask
	     (ecase flag
	       (:public +acc_public+)
	       (:private +acc_private+)
	       (:protected +acc_protected+)
	       (:static +acc_static+)
	       (:final +acc_final+)
	       (:synchronized +acc_synchronized+)
	       (:volatile +acc_volatile+)
	       (:transient +acc_transient+)
	       (:native +acc_native+)
	       (:abstract +acc_abstract+)
	       (:strict +acc_strict+))))
	(setf result (logior mask result))))
    result))

(defun emit-field-or-method (member)
  (destructuring-bind (&key flags name descriptor attributes code stack locals)
      member
    (declare (ignore stack locals))
    (when code
      (push (cons "Code" code) attributes))
    (emit-short (encode-member-access-flags flags))
    (emit-short (put-constant :utf8 name))
    (emit-short (put-constant :utf8 descriptor))
    (emit* #'emit-attribute attributes)))

(defun emit-attribute (attribute)
  (emit-short (put-constant :utf8 (car attribute)))
  (emit-int (length (cdr attribute)))
  (map nil #'emit-byte (cdr attribute)))

(defun emit-proper ()
  (let ((methods
	 `((:name "main"
		  :descriptor "([Ljava/lang/String;)V"
		  :flags (:public :static)
		  :code ((aload_0)	;fig 7.37, p. 204 Muchnick
			 b1 (dup) (ifnonnull b3)
			 b2 (goto b4)
			 b3 (dup) (ifnonnull b5)
			 b4 (goto b6)
			 b5 (goto b6)
			 b6 (return))
		  :stack 2
		  :locals 1))))
    (assemble-class "Proper"
		    "java/lang/Object"
		    :methods methods
		    :flags '(:public :super)
		    :attributes '(("SourceFile" . #(0 1)))
		    :filename "Proper.class")))
