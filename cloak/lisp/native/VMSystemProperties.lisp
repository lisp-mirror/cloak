(in-package :cloak)

(defstatic |gnu/classpath/VMSystemProperties.preInit(Ljava/util/Properties;)|
    (prties)
  (flet ((doit (p v)
	   (setf p (make-cloak-string p))
	   (setf v (make-cloak-string v))
	   (call-cloak-method
	    "setProperty(Ljava/lang/String;Ljava/lang/String;)" prties p v)))
    (doit "java.version"
	  "1.4.2."			;XXX fuer eclipse
	  #+(or)
	  (let ((configuration
		 (find-cloak-class nil "gnu/classpath/Configuration")))
            (maybe-initialize-class configuration)
	    (get-string-value
             (getfield "CLASSPATH_VERSION" configuration))))
    (doit "java.vendor" "GNU Classpath")
    (doit "java.vendor.url"
	  "http://www.gnu.org/software/classpath/classpath.html")
    (doit "java.home" (namestring (truename *source-directory*)))
    (doit "java.vm.specification.version" "Second Edition")
    (doit "java.vm.specification.vendor" "Sun Microsystems Inc.")
    (doit "java.vm.specification.name" "Java Virtual Machine Specification")
    (doit "java.vm.version" "0")
    (doit "java.vm.vendor" "David Lichteblau")
    (doit "java.vm.name" "CLOAK Virtual Machine")
    (doit "java.specification.version" "1.1")
    (doit "java.specification.vendor" "Sun Microsystems Inc.")
    (doit "java.specification.name" "Java Platform API Specification")
    (doit "java.class.version" "47.0")	;XXX
    (doit "java.class.path" (stringify-classpath *java.class.path*))
    (doit "java.boot.class.path"
	  (stringify-classpath (mapcar #'second *bootstrap-classpath*)))
    (doit "java.library.path"
	  (concatenate 'string
	    (namestring (truename cloak-system::*source-directory*))
	    ":"
	    (SB-POSIX:GETENV "LD_LIBRARY_PATH")))
    (doit "java.io.tmpdir" "/tmp")
    (doit "java.tmpdir" "/tmp")
    (doit "java.compiler" "CLOAK")
    (doit "java.ext.dirs" "")
    (doit "os.name" "Linux")
    (doit "os.arch" "x86")
    (doit "os.version" "fixme")
    (doit "file.separator" "/")
    (doit "path.separator" ":")
    (doit "line.separator" (string #\newline))
    (doit "user.name" "Random J. Hacker")
    (doit "user.home" (namestring (user-homedir-pathname)))
    (doit "user.dir" (namestring *default-pathname-defaults*))
    #+nil (doit "gnu.classpath.awt.gtk.portable.native.sync" "true")
    (for (((p . v) :in (vm.properties *vm*)))
      (doit p v))))
