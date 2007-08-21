;;; -*- show-trailing-whitespace: t; indent-tabs: nil -*-

;;; Copyright (c) 2007 David Lichteblau. All rights reserved.

;;; Redistribution and use in source and binary forms, with or without
;;; modification, are permitted provided that the following conditions
;;; are met:
;;;
;;;   * Redistributions of source code must retain the above copyright
;;;     notice, this list of conditions and the following disclaimer.
;;;
;;;   * Redistributions in binary form must reproduce the above
;;;     copyright notice, this list of conditions and the following
;;;     disclaimer in the documentation and/or other materials
;;;     provided with the distribution.
;;;
;;; THIS SOFTWARE IS PROVIDED BY THE AUTHOR 'AS IS' AND ANY EXPRESSED
;;; OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
;;; WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
;;; ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY
;;; DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
;;; DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE
;;; GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
;;; INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
;;; WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
;;; NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;;; SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

(in-package :java-cloak-compat)

(defun give-up (fmt &rest args)
  (format *error-output* "Error: ~?~%" fmt args)
  (sb-ext:quit :unix-status 1))

(defun main ()
  (destructuring-bind (name &rest args)
      (cdr sb-ext:*posix-argv*)
    (cond
      ((equal name "java") (java args))
      ((equal name "javac") (javac args))
      ((equal name "javap") (javap args))
      ((equal name "javah") (javah args))
      ((equal name "serialver") (serialver args))
      (t
       (give-up "undefined command: ~A" name)))))

(defparameter *help*
  "Usage: cloak [OPTION ...] CLASS [ARG ...]
       cloak -jar [OPTION ...] JARFILE [ARG ...]

  --cp ELT:...             system class path
  --classpath ELT:...      synonym for --cp
  --help                   print this help, then exit
  --version                print version number, then exit

  --Xexception-mode trace  log all exceptions being thrown
  --Xexception-mode N      BREAK on exception number N
  --Xverbose               verbose startup and exit

Options can be specified with `-' or `--'.~%")

(defparameter *version*
  "java version 1.4.2-or-so
CLOAK Virtual Machine, running on ~A ~A (~A ~A ~A)

Copyright (C) 2003-2007 David Lichteblau~%")

(defun parse-exception-mode (str)
  (cond
    ((null str)
     nil)
    ((equal str "trace")
     :trace)
    ((ignore-errors (parse-integer str))
     (parse-integer str))
    (t
     (give-up "bogus exception-mode, expected trace or a number"))))

(defun parse-classpath (str)
  (split-sequence:split-sequence #\: str))

(defun optionp (arg)
  (and (plusp (length arg)) (eql (elt arg 0) #\-)))

(defun getopt (args specs)
  (let ((alist '())
	(errors '()))
    (loop while args do
	 (let ((arg (car args)))
	   (unless (optionp arg)
	     (return))
	   (pop args)
	   (let* ((name (string-left-trim "-" arg))
		  (spec (assoc name specs :test #'equal)))
	     (if spec
		 (let* ((kind (second spec))
			(value (and kind (pop args))))
		   (push (cons name value) alist))
		 (let ((next (car args)))
		   (when (optionp next)
		     (pop args))
		   (push name errors))))))
    (values args alist errors)))

(defun java (args)
  (multiple-value-bind (rest alist errors)
      (getopt args '(("classpath" t)
		     ("cp" t)
		     ("Xexception-mode" t)
		     ("jar")
		     ("help")
		     ("version")
		     ("Xverbose")))
    (labels ((findopt (name)
	       (assoc name alist :test 'equal))
	     (optvalue (name)
	       (cdr (findopt name))))
      (cond
	((findopt "help")
	 (format t *help*))
	((findopt "version")
	 (format t *version*
		 (lisp-implementation-type)
		 (lisp-implementation-version)
		 (software-type)
		 (software-version)
		 (machine-type)))
	(t
	 (unless rest
	   (give-up "argument expected"))
	 (when errors
	   (warn "ignoring unrecognized command line arguments: ~A" errors))
	 (let* ((classpath (or (optvalue "classpath") (optvalue "cp")))
		(verbosep (findopt "Xverbose"))
		(cloak::*exception-mode*
		 (parse-exception-mode (optvalue "Xexception-mode"))))
	   (destructuring-bind (class &rest args)
	       rest
	     (let ((jar nil))
	       (when (findopt "jar")
		 (setf jar class)
		 (setf class nil))
	       (cloak::start-vm class
				:jar jar
				:show-statistics-p verbosep
				:classpath (parse-classpath classpath)
				:arguments args)))))))))

(defun javac (args)
  (cloak::start-vm "com.sun.tools.javac.Main"
		   :show-statistics-p nil
		   :classpath (list "/home/david/cloakbuild/java-cloak-compat/"
				    #+nil (directory "/home/david/cloakdist/eclipse/plugins/*.jar")
				    "/home/david/ecj.jar")
		   :arguments args)
  (fresh-line))

(defun javap (args)
  (cloak::start-vm "gnu.classpath.tools.JavapMain"
		   :show-statistics-p nil
		   :classpath (list "/home/david/cp-tools.jar")
		   :arguments args))

(defun javah (args)
  (cloak::start-vm "gnu.classpath.tools.JavahMain"
		   :show-statistics-p nil
		   :classpath (list "/home/david/cp-tools.jar")
		   :arguments args))

(defun serialver (args)
  (cloak::start-vm "gnu.classpath.tools.SerialVer"
		   :show-statistics-p nil
		   :classpath (list "/home/david/cp-tools.jar")
		   :arguments args))
