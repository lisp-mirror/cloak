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

(declaim (optimize (speed 3)
		   (safety 0)
		   (sb-c::recognize-self-calls 0)
		   (sb-ext:inhibit-warnings 0)))

(defun ub32-to-sb32 (i)
  (declare (type (unsigned-byte 32) i))
  (ub32-to-sb32 i))

(defun sb64-to-sb32 (i)
  (declare (type (signed-byte 64) i))
  (sb64-to-sb32 i))

(defsubst sb32-to-ub32 (i)
  (declare (type (signed-byte 32) i))
  (logand #xffffffff i))

(defun int-+ (a b)
  (declare (type (signed-byte 32) a b))
  (int-+ a b))

(defun int-* (a b)
  (declare (type (signed-byte 32) a b))
  (int-* a b))

(defun 0-int (a)
  (declare (type (signed-byte 32) a))
  (0-int a))

;; fixme, VOP nehmen
(defsubst int-- (a b)
  (declare (type (signed-byte 32) a b))
  (int-+ a (0-int b)))

;;; the x86 IDIV instruction raises some funny exception instead of
;;; returning -2^31 on "overflow":
(defsubst int-truncate (a b)
  (declare (type (signed-byte 32) a b)
	   (optimize (speed 3) (safety 0)))
  (if (and (eql b -1) (eql a #.(- 2^31)))
      a
      (values (the (signed-byte 32) (truncate a b)))))
(defsubst irem (a b)
  (declare (type (signed-byte 32) a b)
	   (optimize (speed 3) (safety 0)))
  (if (eql b -1)
      0
      (nth-value 1 (the (signed-byte 32) (truncate a b)))))

(defun int-ash (a b)
  (declare (type (signed-byte 32) a b))
  (int-ash a b))

(defun long-+ (a b)
  (declare (type (signed-byte 64) a b))
  (long-+ a b))

(defun long-- (a b)
  (declare (type (signed-byte 64) a b))
  (long-- a b))

(defun long-* (a b)
  (declare (type (signed-byte 64) a b))
  (long-* a b))

(defun truncate64 (a b)
  (declare (type (signed-byte 64) a b))
  (truncate64 a b))

(defsubst long-truncate (a b)
  (declare (type (signed-byte 64) a b)
	   (optimize (speed 3) (safety 0)))
  (if (and (eql b -1) (eql a #.(- 2^63)))
      a
      (values (truncate64 a b))))

(defun negate64 (x)
  (declare (type (signed-byte 64) x))
  (negate64 x))

(defsubst 0-long (a)
  (declare (type (signed-byte 64) a))
  (negate64 a))

(defun lor (x y)
  (declare (type (signed-byte 64) x y))
  (lor x y))

(defun lxor (x y)
  (declare (type (signed-byte 64) x y))
  (lxor x y))

(defun land (x y)
  (declare (type (signed-byte 64) x y))
  (land x y))

(defun long-ash (a n)
  (declare (type (signed-byte 64) a)
	   (type (signed-byte 32) n))
  (long-ash a n))

(defun lushr (a n)
  (declare (type (signed-byte 64) a)
	   (type (signed-byte 32) n))
  (lushr a n))

(defun l2i (x)
  (declare (type (signed-byte 64) x))
  (l2i x))

(defun %raw-instance-ref/signed (instance index)
  (declare (fixnum index))
  (%raw-instance-ref/signed instance index))
(defun %raw-instance-set/signed (instance index new-value)
  (declare (fixnum index)
	   (type (signed-byte 32) new-value))
  (%raw-instance-set/signed instance index new-value))

(defun %raw-instance-ref/signed64 (instance index)
  (declare (fixnum index))
  (%raw-instance-ref/signed64 instance index))
(defun %raw-instance-set/signed64 (instance index new-value)
  (declare (fixnum index)
	   (type (signed-byte 64) new-value))
  (%raw-instance-set/signed64 instance index new-value))

(defsetf %raw-instance-ref/signed %raw-instance-set/signed)
(defsetf %raw-instance-ref/signed64 %raw-instance-set/signed64)

(defun %cmpxchg (instance offset old new)
  (declare (fixnum offset))
  (%cmpxchg instance offset old new))

(defun vector-sap (x)
  ;; bah: function vector-sap wants simple-unboxed-array for no good reason
  (declare (type (simple-vector *) x))
  (vector-sap x))

(defun f2i (x)
  (declare (type single-float x))
  (f2i x))

(defun d2i (x)
  (declare (type double-float x))
  (d2i x))

(defun f2l (x)
  (declare (type single-float x))
  (f2l x))

(defun d2l (x)
  (declare (type double-float x))
  (d2l x))
