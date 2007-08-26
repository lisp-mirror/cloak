(defpackage :cloak-system
  (:use :cl :asdf)
  (:export #:*source-directory*))
(in-package :cloak-system)

;; Fixme: ISTR that simply depending on the contrib didn't always work.
(require :sb-regpair)

;; Fixme, this doesn't belong here.
;; (And I-E-L shouldn't exist in the first place, but that's not our fault.)
(setf sb-ext:*inline-expansion-limit*
      (max 1000 sb-ext:*inline-expansion-limit*))

(defun pathname-directory-pathname (pathname)
  (make-pathname :host (pathname-host pathname)
		 :device (pathname-device pathname)
		 :directory (pathname-directory pathname)))

(defparameter *source-directory*
    (namestring (pathname-directory-pathname (truename #.*load-pathname*))))

(defclass silent-source-file (cl-source-file) ())

(defmethod perform :around ((o compile-op) (s silent-source-file))
  (handler-bind (#+sbcl (sb-ext:compiler-note #'muffle-warning))
    (let ((*compile-verbose* t)		;make OpenMCL open up a little
	  (*compile-print* nil))	;make SBCL STFU
      (call-next-method))))

(defsystem "cloak"
  :default-component-class silent-source-file
  :depends-on (:sb-md5 :sb-regpair :sb-bsd-sockets :sb-posix :sb-heapdump :zip)
  :serial t
  :components
  ((:file "lisp/package")
   (:file "lisp/misc")
   (:file "lisp/utf-8")
   (:file "lisp/word")
   (:file "lisp/fndb")
   (:file "lisp/fnvops")
   (:file "lisp/fns")
   (:file "lisp/sbcl")
   (:file "lisp/patch")
   (:file "lisp/constants")
   (:file "lisp/buffer")
   (:file "lisp/parser")
   (:file "lisp/jnidefs")
   (:file "lisp/split-sequence")
   (:file "lisp/compiler")
   (:file "lisp/instructions")
   (:file "lisp/loader")
   (:file "lisp/foreign")
   (:file "lisp/jnifns")
   (:file "lisp/examples")
   (:file "lisp/native/Array")
   (:file "lisp/native/Constructor")
   (:file "lisp/native/Field")
   (:file "lisp/native/Method")
   (:file "lisp/native/Object")
   (:file "lisp/native/Util")
   (:file "lisp/native/VMClass")
   (:file "lisp/native/VMClassLoader")
   (:file "lisp/native/VMDouble")
   (:file "lisp/native/VMFloat")
   (:file "lisp/native/VMObject")
   (:file "lisp/native/VMRuntime")
   (:file "lisp/native/VMThread")
   (:file "lisp/native/VMString")
   (:file "lisp/native/VMSystem")
   (:file "lisp/native/VMSystemProperties")
   (:file "lisp/native/VMThrowable")
   (:file "lisp/native/VMStackWalker")
   (:file "lisp/native/VMAccessController")))
