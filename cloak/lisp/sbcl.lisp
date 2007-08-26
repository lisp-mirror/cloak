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

(defun nanp (float)
  (sb-ext:float-nan-p float))

(defun double-to-long (d)
  (canonicalize-long
   (logior (ash (sb-kernel:double-float-high-bits d) 32)
	   (sb-kernel:double-float-low-bits d))))

(defun long-to-double (l)
  (sb-kernel:make-double-float
   (canonicalize-int (ldb (byte 32 32) l))
   (canonicalize-n-unsigned (ldb (byte 32 0) l) 32)))

(defun float-to-int (d)
  (canonicalize-int (sb-kernel:single-float-bits d)))

(defun int-to-float (l)
  (sb-kernel:make-single-float (canonicalize-int l)))

(defun get-host-by-address (ipaddr-array)
  (handler-case
      (sb-bsd-sockets:host-ent-name
       (sb-bsd-sockets:get-host-by-address
	ipaddr-array))
    (sb-bsd-sockets:host-not-found-error ()
      (throw-exception "java/net/UnknownHostException"))))

(defun get-host-by-name (name)
  (handler-case
      (sb-bsd-sockets:host-ent-address (sb-bsd-sockets:get-host-by-name name))
    (sb-bsd-sockets:host-not-found-error ()
      (throw-exception "java/net/UnknownHostException"))))

(defun make-socket (inaddr-array port)
  (let* ((protocol (sb-bsd-sockets:get-protocol-by-name "tcp"))
	 (socket (make-instance 'sb-bsd-sockets:inet-socket
		   :type :stream
		   :protocol protocol)))
    (sb-bsd-sockets:socket-connect socket inaddr-array port)
    socket))

(defun socket-stream (socket &key (element-type '(signed-byte 8)))
  (sb-bsd-sockets:socket-make-stream
   socket
   :input t
   :output t
   :element-type element-type))

(defconstant +socket-error+ 'sb-bsd-sockets::socket-error)

(defun gc (&optional full)
  (sb-ext:gc :full full))

(defun string-to-utf-8 (string)
  (map 'vector #'char-code string))

(defun utf-8-to-string (bytes)
  (map 'string #'code-char bytes))

(defun decode-pseudo-utf-8 (lisp-string)
  (with-input-from-string (s lisp-string)
    (let ((tmp (make-array (length lisp-string)
                           :element-type '(unsigned-byte 16)))
          (i 0))
      (while (listen s)
        (setf (elt tmp i)
              (utf8-read-character (lambda () (char-code (read-char s)))))
        (incf i))
      (if (= i (length tmp))
          tmp
          (subseq tmp 0 i)))))

(defun encode-pseudo-utf-8 (ucs16 &optional (start 0) (end (length ucs16)))
  ;; taken from SBCL, octets.lisp
  (declare (optimize speed (safety 0)))
  (with-output-to-string (s)
    (flet ((add-byte (b)
	     (declare (type (unsigned-byte 8) b))
	     (write-char (code-char b) s)))
      (declare (inline add-byte))
      (loop
	  for i from start below end
	  for code = (elt ucs16 i)
	  do
	    (ecase (sb-impl::char-len-as-utf8 code)
	      (1
		(add-byte code))
	      (2
		(add-byte (logior #b11000000 (ldb (byte 5 6) code)))
		(add-byte (logior #b10000000 (ldb (byte 6 0) code))))
	      (3
		(add-byte (logior #b11100000 (ldb (byte 4 12) code)))
		(add-byte (logior #b10000000 (ldb (byte 6 6) code)))
		(add-byte (logior #b10000000 (ldb (byte 6 0) code))))
	      (4
		(add-byte (logior #b11110000 (ldb (byte 3 18) code)))
		(add-byte (logior #b10000000 (ldb (byte 6 12) code)))
		(add-byte (logior #b10000000 (ldb (byte 6 6) code)))
		(add-byte (logior #b10000000 (ldb (byte 6 0) code)))))))))

(defun compile-labeled-function (name lambda-expression)
  (funcall
   (compile nil `(lambda ()
		   (flet ((,name ,@(rest lambda-expression)))
		     #',name)))))

(defvar *standard-floating-point-modes* nil)

(defun disable-floating-point-traps ()
  (unless *standard-floating-point-modes*
    (setf *standard-floating-point-modes* (sb-int:get-floating-point-modes)))
  (sb-int:set-floating-point-modes :traps nil))

(defun enable-floating-point-traps ()
  (assert *standard-floating-point-modes*)
  (apply #'sb-int:set-floating-point-modes *standard-floating-point-modes*))

(defsubst f+ (a b) (+ a b))
(defsubst f/ (a b) (/ a b))
(defsubst f* (a b) (* a b))
(defsubst f- (a b) (- a b))
(defsubst fneg (a) (- a))

(defsubst d+ (a b) (+ a b))
(defsubst d/ (a b) (/ a b))
(defsubst d* (a b) (* a b))
(defsubst d- (a b) (- a b))
(defsubst dneg (a) (- a))

(sb-alien:define-alien-routine "fmod" double-float
  (f double-float)
  (g double-float))

(sb-alien:define-alien-routine "fmodf" single-float
  (f single-float)
  (g single-float))

(defvar +nan+ (long-to-double 9221120237041090560))

(defun drem (f g)
  (if (zerop g)
      +nan+
      (fmod f g)))

(defun frem (f g)
  (if (zerop g)
      (float +nan+ 1.0s0)
      (fmodf f g)))

(defun directoryp (pathname)
  (eq :directory (sb-unix:unix-file-kind pathname)))

(progn					;from CLX dependent.lisp
  (declaim (inline yield))
  (defun yield ()
    (declare (optimize speed (safety 0)))
    (sb-alien:alien-funcall
     (sb-alien:extern-alien "sched_yield" (function sb-alien:int)))
    (values)))

;; PROBE-FILE, FILE-WRITE-DATE, and UNIX-NAMESTRING are slow.  We don't
;; need any pathnames or wildcard namestrings, so let's just pass strings
;; to sb-unix directly.
(defun quick-probe-file (namestring)
  (check-type namestring string)
  (values (sb-unix:unix-stat namestring)))
(defun quick-file-write-date (namestring)
  (check-type namestring string)
  (let ((mtime (nth-value 10 (sb-unix:unix-stat namestring))))
    (if mtime
	(+ sb-impl::unix-to-universal-time mtime)
	nil)))

;; Warnungen stilvoll abfangen.
;; Kludge: Called for side effect, too.
(defun foreign-symbol-bound-p (name)
  (sb-thread:with-mutex (sb-impl::*foreign-lock*)
    (let ((found t))
      (handler-bind ((style-warning
		      (lambda (c)
			(setf found nil)
			(muffle-warning c))))
	(sb-impl::write-linkage-table-entry
	 (sb-impl::linkage-info-address
	  (gethash (cons name nil) sb-impl::*linkage-info*))
	 (sb-impl::ensure-dynamic-foreign-symbol-address name nil)
	 nil))
      found)))

(write-line "; patching foreign-reinit")
(sb-ext:without-package-locks
 (defun sb-impl::foreign-reinit ()
   (handler-bind ((style-warning #'muffle-warning))
     (sb-impl::reopen-shared-objects)
     (sb-impl::update-linkage-table))))
