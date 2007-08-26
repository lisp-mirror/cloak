(in-package :cloak)

(defun abcl (&rest args)
  (start-vm "org.armedbear.lisp.Main"
            :arguments args
            :classpath '("/home/david/src/j/src/")))

(defun sisc ()
  (start-vm "sisc.REPL"
	    :working-directory "/home/dave/src/sisc/"
	    :classpath '("sisc.jar"
			 "sisc-opt.jar"
			 "sisc-lib.jar")
	    :properties `(("sisc.home" . "/home/dave/src/sisc"))))

(defun test ()
  (time (start-vm "Test")))

(defun foo ()
  (time (start-vm "Foo")))

(defvar *mauve-directory* nil)

#+(or)
(setf *mauve-directory* "/home/david/src/mauve/")

(defun mauve (verbosep &optional (pathname "classes"))
  (check-type verbosep boolean)
  (unless *mauve-directory*
    (cerror "But I'm David" "*mauve-directory* not set")
    (setf *mauve-directory* "/home/david/src/mauve/"))
  (with-open-file
      (*standard-input* (merge-pathnames pathname *mauve-directory*))
    (start-vm
     "gnu/testlet/SimpleTestHarness"
     :classpath (list *mauve-directory*)
     ;; :arguments (list* "-debug" (if verbosep '("-verbose") nil))
     :arguments (if verbosep '("-verbose") nil)
     :working-directory *mauve-directory*)))

(declaim (special *exception-mode*))

(defun dribble-mauve (&rest args)
  (with-open-file
      (*standard-output*
       (merge-pathnames "MAUVE.current" cloak-system:*source-directory*)
       :direction :output
       :if-exists :supersede)
    (let ((*error-output* *standard-output*))
      (let ((*exception-mode* nil))
	(apply #'mauve t args)
	#+nil (apply #'mauve nil args)))))

(defun eclipse ()
  (start-vm "org/eclipse/core/launcher/Main"
	    :classpath '("/home/david/cloakdist/eclipse/startup.jar")
	    :arguments '("-os" "linux"
			 "-ws" "gtk"
			 "-arch" "x86"
			 "-name" "Eclipse"
			 "-showsplash" "600"
			 "-exitdata" "78003"
			 "-consoleLog" "-debug")))

(defun times ()
  (time
   (flet ((doit (class)
            #+sbcl (sb-ext:gc :full t)
            (start-vm class)))
     (doit "Test")
     (doit "Test")
     (doit "microbench.Main"))))

(defun dribble-times ()
  (with-open-file
      (*standard-output*
       (merge-pathnames "TIMES.current" cloak-system:*source-directory*)
       :direction :output
       :if-exists :supersede)
    (let ((*error-output* *standard-output*))
      (times))))

#+(or)
(precompile-zip-file "/home/david/src/classpath/lib/glibj.zip")

#+(or)
(precompile-zip-files "/home/david/cloakdist/eclipse/**/*.jar")

#+(or)
(let ((sb-heapdump:*dumpload-verbose* nil))
  (dolist (p (directory "/home/david/src/lisp/cloak/cache/*.heap"))
    (let ((cf (sb-heapdump:load-dumpfile p)))
      (when (cf.interfacep cf)
	(print p)))))

;; get rid of this!
(defun do-make-cloak-object (class)
  (%make-cloak-object class))
