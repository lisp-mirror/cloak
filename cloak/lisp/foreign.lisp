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

(defun call-cloak-method (signature instance &rest args)
  (apply-cloak-method signature instance args))

(defmethod apply-cloak-method (signature (instance t) args)
  (apply #'%call-cloak-method
	 (multiple-value-bind (m c)
	     (get-cloak-method signature (%class instance))
	   (vtable-index c m))
	 (%class instance)
	 instance
	 args))

(defmethod apply-cloak-method (signature (class reference-type) args)
  (maybe-initialize-class class)
  (apply #'%call-cloak-method
	 (multiple-value-bind (m c)
	     (get-cloak-method signature class)
	   (vtable-index c m))
	 class
	 args))

(defun make-cloak-instance (class constructor-signature &rest arguments)
  (when (stringp class)
    (setf class (find-cloak-class nil class)))
  (maybe-initialize-class class)
  (let ((instance (%make-cloak-object class)))
    (apply-cloak-method constructor-signature instance arguments)
    instance))

(defun get-cloak-method (signature class &optional null-ok-p)
  (unless (typep class 'cloak-array-class)
    (maybe-compute-method-layout class))
  (let ((match
	 (find signature (cls.methods class) :key #'cm.signature :test #'string=)))
    (cond
      (match (values match class))
      ((null (cls.superclass class)) (if null-ok-p nil (error "no such method")))
      (t (get-cloak-method signature (cls.superclass class) null-ok-p)))))
