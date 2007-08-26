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

(in-package :sb-c)

(declaim (optimize (speed 3)
		   (safety 0)
		   (sb-c::recognize-self-calls 0)
		   (sb-ext:inhibit-warnings 0)))

(defknown cloak::ub32-to-sb32 ((unsigned-byte 32))
    (signed-byte 32)
  (movable foldable flushable))

(defknown cloak::sb64-to-sb32 ((signed-byte 64))
    (signed-byte 32)
  (movable foldable flushable))

(defknown cloak::int-+
    ((signed-byte 32) (signed-byte 32))
    (signed-byte 32)
  (movable foldable flushable))

(defknown cloak::int-*
    ((signed-byte 32) (signed-byte 32))
    (signed-byte 32)
  (movable foldable flushable))

(defknown cloak::0-int ((signed-byte 32)) (signed-byte 32)
  (movable foldable flushable))

(defknown cloak::int-ash
    ((signed-byte 32) (signed-byte 32))
    (signed-byte 32)
  (movable foldable flushable))

(defknown cloak::long-+ ((signed-byte 64) (signed-byte 64))
    (signed-byte 64)
  (movable foldable flushable))

(defknown cloak::long-- ((signed-byte 64) (signed-byte 64))
    (signed-byte 64)
  (movable foldable flushable))

(defknown cloak::long-* ((signed-byte 64) (signed-byte 64))
    (signed-byte 64)
  (movable foldable flushable))

(defknown cloak::truncate64 ((signed-byte 64) (signed-byte 64))
    (values (signed-byte 64) (signed-byte 64))
  (movable foldable flushable))

(defknown cloak::negate64 ((signed-byte 64))
    (signed-byte 64)
  (movable foldable flushable))

(defknown cloak::lor ((signed-byte 64) (signed-byte 64))
    (signed-byte 64)
  (movable foldable flushable))

(defknown cloak::lxor ((signed-byte 64) (signed-byte 64))
    (signed-byte 64)
  (movable foldable flushable))

(defknown cloak::land ((signed-byte 64) (signed-byte 64))
    (signed-byte 64)
  (movable foldable flushable))

(defknown cloak::long-ash ((signed-byte 64) (signed-byte 32))
    (signed-byte 64)
  (movable foldable flushable))

(defknown cloak::lushr ((signed-byte 64) (signed-byte 32))
    (signed-byte 64)
  (movable foldable flushable))

(defknown cloak::l2i ((signed-byte 64))
    (signed-byte 32)
  (movable foldable flushable))

(defknown cloak::%raw-instance-ref/signed (t fixnum) (signed-byte 32)
  (flushable))
(defknown cloak::%raw-instance-set/signed (t fixnum (signed-byte 32))
  (signed-byte 32)
  (unsafe))

(defknown cloak::%raw-instance-ref/signed64 (t fixnum)
  (signed-byte 64)
  (flushable))
(defknown cloak::%raw-instance-set/signed64 (t fixnum (signed-byte 64))
  (signed-byte 64)
  (unsafe))

(defknown cloak::%cmpxchg (instance index t t)
  fixnum
  (unsafe))

(defknown cloak::vector-sap (simple-vector)
    system-area-pointer
  (flushable))

(defknown cloak::f2i (single-float)
    (signed-byte 32)
  (movable foldable flushable))

(defknown cloak::d2i (double-float)
    (signed-byte 32)
  (movable foldable flushable))

(defknown cloak::f2l (single-float)
    (signed-byte 64)
  (movable foldable flushable))

(defknown cloak::d2l (double-float)
    (signed-byte 64)
  (movable foldable flushable))
