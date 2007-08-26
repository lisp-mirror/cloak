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

;;; exported variables

(defvar *default-classpath* '())
(defvar *print-tostring-p* t)

;;; internal variables
;;; fixme: move most of these as slots into *vm* (?)

(defvar *vm*)
(defvar *class-files/boot* (make-hash-table :test 'equal))
(defvar *class-files/hashed* (make-hash-table :test 'equalp))
(defvar *bootstrap-classpath*)
(defvar *thread-object*)
(defvar *vmthread-object*)
(defvar *exception-counter*)
(defvar *array-vtable*)
(defvar *array-method-layout*)
(defvar *class-initialization-counter*)
(defvar *class-counter*)
(defvar *class-loading-counter*)
(defvar *%class%*)
(defvar *pending-exception*)
(defvar *java.class.path*)
(defvar *ugly-class-table*)
(defvar *ugly-vmthrowable-list*)
(defvar *early-bootstrap-p*)
(declaim (fixnum *thread-id*))
(defvar *thread-id*)


;;; structures

(defstruct (cloak-object
	    (:conc-name "CO.")
	    (:constructor nil))
  (hash-code (error "oops?"))
  (monitor (error "oops?")))

(defstruct (cloak-array
	    (:include cloak-object)
	    (:conc-name "CO.")
	    (:constructor %make-cloak-array (hash-code monitor data)))
  (data (error "oops")))

(defstruct (class-loader (:conc-name "CL."))
  (classes (make-hash-table :test 'equal))
  (mutex (sb-thread:make-mutex))
  (object))

(defstruct (abstract-class
	    (:include sb-kernel:layout)
	    (:conc-name "CLS."))
  (class-loader)
  name
  (superclass)
  (interfaces)
  (object nil)
  (cached-array-class nil)
  jni-reference
  access-flags
  jdi-signature)

(defstruct (reference-type
	    (:include abstract-class)
	    (:conc-name "CLS."))
  (class-file)
  (subclasses '())
  (initializedp nil)			;fixme: not a boolean; rename
  (linkedp nil)
  static-field-values
  (vtable-length nil)
  tagged-slot-counter
  raw-slot-counter
  tagged-static-slot-counter
  raw-static-slot-counter
  (method-layout)
  itable
  (field-layout)
  prototype
  (pool)
  cpl
  (jni-method-references)
  prev-code-start
  protection-domain
  (%inner-class-table :unparsed))

(defstruct (cloak-class (:include reference-type)))
(defstruct (cloak-interface (:include reference-type)))
(defstruct (primitive-class (:include abstract-class)))

(defstruct (cloak-array-class
	    (:include reference-type)
	    (:conc-name "CLS."))
  (layout)
  (element-type))

(defstruct (cloak-array-class-file (:include class-file)))

(defstruct (virtual-machine (:conc-name "VM."))
  (bootstrap-loader (make-class-loader))
  (global-monitor (make-monitor))
  (all-threads '())
  (all-thread-ids '())
  (startup-time-internal (get-internal-real-time))
  (startup-time (* (get-universal-time) 1000))
  (strings (make-hash-table :test 'equal))
  default-pathname-defaults
  initial-thread
  properties
  boolean-array
  char-array
  float-array
  double-array
  byte-array
  short-array
  int-array
  long-array
  javavm-pointer
  globalref-tables
  globalref-free-pointers
  (libraries '()))

(defmethod print-object ((object cloak-array-class) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (princ (cls.jdi-signature object) stream)))

(defmethod print-object ((object primitive-class) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (princ (cls.jdi-signature object) stream)))

(defmethod print-object ((object cloak-object) stream)
  (print-unreadable-object
      (object stream :type nil :identity (not *print-tostring-p*))
    ;; use toString() only when requested, since running Java in here
    ;; might be a bad idea.  We always want to see the class name, but 
    ;; only once, so omit it if already contained in the toString() result.
    (let ((str (when *print-tostring-p*
                 (get-string-value
                  (call-cloak-method "toString()" object))))
          (name (substitute #\. #\/ (cls.name (%class object)))))
      (cond
        ((nullp str) (write-string name stream))
        ((starts-with-p str name) (write-string str stream))
        (t (format stream "~A: ~A" name str))))))


;;; loading and linking of classes
;;; (and everything else)

(defun clr ()
  (clrhash *class-files/boot*)
  (clrhash *class-files/hashed*)
  nil)

(cloak::defsubst nullp (a)
  (eql a +null+))

(declaim (inline cls.vtable))
(locally
    (declare #.*fast*)
  (defun cls.vtable (class)
    (sb-kernel:layout-info class)))

(defun (setf cls.vtable) (newval class)
  (setf (sb-kernel:layout-info class) newval))

(defmethod print-object ((object reference-type) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (princ (cls.name object) stream)))

(macrolet ((defproxy (cls cf)
	     `(defun ,cls (class)
                (,cf (cls.class-file class)))))
  (defproxy cls.constants cf.constants)
  (defproxy cls.superclass-name cf.superclass-name)
  (defproxy cls.superclass-index cf.superclass-index)
  (defproxy cls.interface-names cf.interface-names)
  (defproxy cls.fields cf.fields)
  (defproxy cls.methods cf.methods)
  (defproxy cls.attributes cf.attributes))

(defun cls.get-constant (class i)
  (cf.get-constant (cls.class-file class) i))

(defun cls.get-constant-class (class i)
  (cf.get-constant-class (cls.class-file class) i))

(defun class-from-pool (class i)
  (or (svref (cls.pool class) i)
      (setf (svref (cls.pool class) i)
	    (find-cloak-class
	     (cls.class-loader class)
	     (cls.get-constant-class class i)))))

(defun string-from-pool (class i)
  (or (svref (cls.pool class) i)
      (setf (svref (cls.pool class) i)
	    (intern-cloak-string (cls.get-constant class i)))))

(defmethod vtable-index (class (member cloak-method))
  (assert (or (not (cls.interfacep class))
	      (equal (cm.name member) "<clinit>")))
  (gethash (cm.signature member) (cls.method-layout class)))

(defun imethod-vtable-index (class signature)
  (gethash signature (cls.method-layout class)))

(defmethod vtable-index (class (member cloak-field))
  (elt (cls.field-layout class) (cm.static-index member)))

(defmethod (setf vtable-index) (newval class (member cloak-field))
  (setf (elt (cls.field-layout class) (cm.static-index member)) newval))

(defun method-from-pool (class i)
  (or (svref (cls.pool class) i)
      (setf (svref (cls.pool class) i)
	    (multiple-value-bind (method c)
		(resolve-method class i)
	      (link-cloak-class c)
	      (vtable-index c method)))))

(defun field-from-pool (class i)
  (or (svref (cls.pool class) i)
      (setf (svref (cls.pool class) i)
            (let* ((field (resolve-field class i))
                   (fclass
                    (find-referenced-class
                     class
                     (cf.name (cm.declaring-class-file field))))
                   (vindex
                    (progn
                      (link-cloak-class fclass)
                      (vtable-index fclass field))))
              (if (cm.staticp field)
                  (cons fclass vindex)
                  vindex)))))

(defun resolve-method (caller index)
  (let* ((ref (cf.get-constant-method (cls.class-file caller) index))
	 (name (ref.name ref))
	 (descriptor (ref.descriptor ref))
	 (class (class-from-pool caller (ref.class-id ref))))
    (maybe-compute-method-layout class)	;felder linken fuer vtable-index
    (when (typep class 'cloak-array-class) ;XXX
;;;      (setf class (if (equal name "clone")
;;;		      (find-cloak-class nil "java/lang/Cloneable")
;;;		      (find-cloak-class nil "java/lang/Object")))
      (setf class (find-cloak-class nil "java/lang/Object")))
    (flet ((find-my-method (class)
	     (dolist (method (cls.methods class))
	       (when (and (equal name (cm.name method))
			  (equal descriptor (cm.descriptor method))) ;XXX
		 (return method)))))
      (values (or (if (ref.interfacep ref)
		      (progn
			(assert (cls.interfacep class))
			(or (some-superinterface #'find-my-method class)
			    (find-my-method (cls.superclass class))))
		      (or (some-proper-superclass #'find-my-method class)
			  (some-proper-superclass
			   (lambda (c)
			     (some-superinterface #'find-my-method c))
			   class)))
		  (error "no such method ~A/~A in ~A"
				      name
				      descriptor
				      class))
	      class))))

(defun resolve-field (caller index)
  (let* ((ref (cf.get-constant-field (cls.class-file caller) index))
	 (name (ref.name ref))
	 (class (class-from-pool caller (ref.class-id ref))))
    (maybe-compute-method-layout class)	;felder linken fuer vtable-index
    (flet ((find-my-field (class)
	     (find name (cls.fields class) :key #'cm.name :test #'equal)))
      (cond
	((some-proper-superclass #'find-my-field class))
	((some-proper-superclass
	  (lambda (c)
	    (some-superinterface #'find-my-field c))
	  class))
	(t (class-format-error "no such field ~A ~A in ~A" name class))))))

(defun %make-cloak-object (class)
  (declare (optimize (speed 3) (safety 0)))
  (maybe-initialize-class class)
  (%clone-object (cls.prototype class)))

(defun make-initial-thread ()
  (let* ((*thread-object* +null+)
	 (root
	  (getstatic "root"
		   (maybe-initialize-class
		    (find-cloak-class nil "java/lang/ThreadGroup"))))
	 (vmthread
	  (%make-cloak-object
	      (maybe-initialize-class
	       (find-cloak-class nil "java/lang/VMThread"))))
	 (cls (find-cloak-class nil "java/lang/Thread"))
	 (thread
	  (progn
	    (maybe-initialize-class cls)
	    (%make-cloak-object cls))))
    ;; von Hand initialisieren, denn der Konstruktor versucht den
    ;; SystemClassLoader zu erzeugen, was nicht geht ohne einen Thread
    ;; zu haben...  Feld `contextClassLoader' zunaechst frei lassen!
    (setf (getfield "vmThread" thread) vmthread)
    (setf (getfield "name" thread) (make-cloak-string "initial thread"))
    (setf (getfield "priority" thread) 5) ;thread.NORM_PRIORITY
    (setf (getfield "daemon" thread) 0)
    (setf *thread-object* thread)
    (setf (getfield "thread" vmthread) thread)
    (call-cloak-method "addThread(Ljava/lang/Thread;)" root thread)
    (setf (getfield "group" thread) root)
    (call-cloak-method "newChildThread(Ljava/lang/Thread;)"
		       (maybe-initialize-class
			(find-cloak-class nil "java/lang/InheritableThreadLocal"))
		       thread)
    (setf (vmthread-vmdata vmthread) sb-thread:*current-thread*)
    (setf (vmthread-index vmthread) 1)
    thread))

(defun lookup-bootstrap-classpath ()
  (list
   (list :directory
	 (namestring (merge-pathnames "java/" *source-directory*))
	 nil)
   (flet ((probe (x)
	    (quick-probe-file
	     (namestring (merge-pathnames x *source-directory*)))))
     (cond
       ((not (probe "java/java/lang/VMString.class"))
	(error "cannot find CLOAK-specific .class-files, did you run `make'?"))
       ((probe "classpath.zip")
	(let ((pathname (merge-pathnames "classpath.zip" *source-directory*)))
	  (list :zip
		(namestring pathname)
		(zip:open-zipfile pathname))))
       ((not (probe "classpath/"))
	(error "link to classpath in the cloak directory missing"))
       ((not (probe "classpath/java/lang/Object.class"))
	(error "system classes not found, is your classpath link correct?"))
       (t
	(list :directory
	      (namestring
	       (merge-pathnames "classpath/" *source-directory*))
	      nil))))))

(declaim (inline invoke-with-global-monitor))
(defun invoke-with-global-monitor (fn)
  (let ((gm (vm.global-monitor *vm*)))
    (enter-monitor gm)
    (unwind-protect
	(funcall fn)
      (exit-monitor gm))))

(defun resolve-synonym-stream (s)
  (while (typep s 'synonym-stream)
    (setf s (symbol-value (synonym-stream-symbol s))))
  s)

(defun run (class &rest args)
  (start-vm class :arguments args))

;; XXX diese funktion ist eine katastrophe
;; XXX siehe auch invoke-cloak-thread
(defun start-vm
    (initial-class
     &key arguments
	  (show-statistics-p t)
	  (classpath *default-classpath*)
	  (working-directory *default-pathname-defaults*)
	  properties
	  jar)
  (when jar
    (push jar classpath))
  (setf *exception-counter* 0)
  (setf *array-vtable* nil)
  (setf *class-loading-counter* 0)
  (setf *class-initialization-counter* 0)
  (setf *class-counter* 0)
  (setf *ugly-vmthrowable-list* nil)
  (setf *early-bootstrap-p* t)
  (handler-bind ((style-warning #'muffle-warning))
    (let ((sb-impl::*linkage-info* (load-time-value (make-hash-table))))
      (sb-alien:load-shared-object "libcloak.so")))
  (dolist (sym '("install_trampolines" "va_int" "va_longlong" "va_double"))
    (cloak::foreign-symbol-bound-p sym))
  (with-jni-invoke-interface (javavm-pointer)
    (locally
	(declare (optimize sb-c::stack-allocate-dynamic-extent))
      (let* ((real-time0 (get-internal-real-time))
	     (run-time0 (get-internal-run-time))
	     (*default-pathname-defaults* (pathname working-directory))
	     (bootstrap-classpath (lookup-bootstrap-classpath))
	     (globalref-tables (make-array 24))
	     (vm
	      (make-virtual-machine
	       :default-pathname-defaults *default-pathname-defaults*
	       :initial-thread sb-thread:*current-thread*
	       :properties properties))
	     (*vm* vm)
	     (*java.class.path* classpath) ;fuer vmsystemproperties:
	     (*thread-id* 1)
	     (stdin (resolve-synonym-stream *standard-input*))
	     (stdout (resolve-synonym-stream *standard-output*))
	     (stderr (resolve-synonym-stream *error-output*)))
	(declare (dynamic-extent globalref-tables))
	(check-type stdin sb-sys:fd-stream)
	(check-type stdout sb-sys:fd-stream)
	(check-type stderr sb-sys:fd-stream)
	(setf (vm.globalref-tables vm) globalref-tables)
	(setf (vm.globalref-free-pointers vm)
	      (make-array 24 :initial-element 0))
	(fill globalref-tables nil)
	(setf (svref globalref-tables 0) (make-globalref-table 256))
	(push 1 (vm.all-thread-ids *vm*))
	(setf (vm.javavm-pointer vm) javavm-pointer)
	(install-ugly-class-table)
	(unwind-protect
	    (invoke-cloak-thread
	     +null+			;set below
	     (lambda ()
	       (setf *bootstrap-classpath* bootstrap-classpath)
	       (load-primitive-classes)
	       (maybe-initialize-class
		;; Must not initialize String first or something.
		(find-cloak-class nil "java/lang/Object"))
	       (maybe-initialize-class
		;; XXX c.f. MAKE-CLOAK-STRING
		(find-cloak-class nil "java/lang/VMString"))
	       (maybe-initialize-class
		;; XXX c.f. MAKE-CLOAK-STRING
		(find-cloak-class nil "java/lang/String"))
	       (setf *thread-object* (make-initial-thread))
	       (setf *vmthread-object* (thread-vmthread *thread-object*))
	       (format t "~&; starting ~S~%"
		       (cons (or initial-class jar) arguments))
	       (force-output)
	       ;; jetzt koennen wir den Thread vervollstaendigen:
	       (setf (getfield "contextClassLoader" *thread-object*)
		     (call-cloak-method
		      "getSystemClassLoader()"
		      (find-cloak-class nil "java/lang/ClassLoader")))
	       (let ((fc (find-cloak-class
			  nil
			  "gnu/java/nio/channels/FileChannelImpl")))
		 (maybe-initialize-class fc)
		 (setf (getfield "fd" (getstatic "in" fc))
		       (sb-sys:fd-stream-fd stdin))
		 (setf (getfield "fd" (getstatic "out" fc))
		       (sb-sys:fd-stream-fd stdout))
		 (setf (getfield "fd" (getstatic "err" fc))
		       (sb-sys:fd-stream-fd stderr)))
	       (let ((jlc (find-cloak-class nil "java/lang/Class")))
		 (initialize-class jlc)
		 (class-object jlc)
		 (setf *early-bootstrap-p* nil))
	       (catch 'exit
		 (cond
		   (initial-class
		     (apply #'start-class initial-class arguments))
		   (jar
		     (apply #'start-jar jar arguments))
		   (t
		     (error "neither INITIAL-CLASS or JAR supplied")))))
	     (lambda ()
	       (while (with-global-monitor () (vm.all-threads *vm*))
		 #+nil (print (vm.all-threads *vm*))
		 (sleep 1))
	       (flush-streams)
	       (with-localref-frame (16)
		 (let ((*%class%* (find-cloak-class nil "java/lang/Object"))
		       (*pending-exception* nil))
		   (dolist (lib (vm.libraries *vm*))
		     (sb-alien::dlclose-or-lose
		      (find lib sb-alien::*shared-objects*
			    :key #'sb-alien::shared-object-file
			    :test #'equal)))))))
	  (uninstall-ugly-class-table))
	(when show-statistics-p
	  (let ((real-time (- (get-internal-real-time) real-time0))
		(run-time (- (get-internal-run-time) run-time0))
		(n internal-time-units-per-second))
	    (note "~,3Fs cpu time, ~,3Fs real time" (/ run-time n) (/ real-time n))
	    (show-vm-statistics)))
	  (finish-output)))))

(defmacro with-new-vm ((&rest start-vm-arguments) &body body)
  `(start-vm (lambda () (progn ,@body) 0) ,@start-vm-arguments))

(defun flush-streams ()
  (call-cloak-method
   "flush()"
   (getstatic "out" (find-cloak-class nil "java/lang/System")))
  (call-cloak-method
   "flush()"
   (getstatic "err" (find-cloak-class nil "java/lang/System"))))

(defun show-vm-statistics ()
  (note "Read ~D~:* class~[es~;~:;es~] from disk (~D in cache now)."
	*class-loading-counter*
	(hash-table-count *class-files/hashed*))
  (note "Loaded ~D~:* class~[es~;~:;es~], initialized ~D."
	*class-counter*
	*class-initialization-counter*))

(defun start-class (initial-class &rest args)
  (call-cloak-method "main([Ljava/lang/String;)"
		     (find-cloak-class
		      (classloader-vmdata
		       (call-cloak-method
			"getSystemClassLoader()"
			(find-cloak-class nil "java/lang/ClassLoader")))
		      (substitute #\/ #\. initial-class))
		     (make-cloak-array "[Ljava/lang/String;"
		       (map 'vector #'make-cloak-string args))))

(defun start-jar (jar &rest args)
  (call-cloak-method "main([Ljava/lang/String;)"
		     (class-vmdata
		      (call-cloak-method "getMainClass(Ljava/lang/String;)"
					 (find-cloak-class nil "cloak/Util")
					 (make-cloak-string jar)))
		     (make-cloak-array "[Ljava/lang/String;"
		       (map 'vector #'make-cloak-string args))))

(defun find-referenced-class (referencing-class name)
  (find-cloak-class (cls.class-loader referencing-class) name))

;; Find the class or throw noclassdeffounderror.
(defun find-cloak-class (class-loader name)
  (or (%find-cloak-class class-loader name)
      (throw-exception "java/lang/NoClassDefFoundError")))

;; Find the class or try to return NIL as an optimization.  (The
;; exception being element types of array classes and non-bootstrap
;; class loaders.)
(defun %find-cloak-class (class-loader name)
  (progn
    (cond
      ;; DWIM if it's already a class.  -- FIXME, das ist doch pfusch
      ((or (typep name 'reference-type) (typep name 'primitive-class))
	name)
      ;; Primitive classes are cached in the bootstrap loader.
      ((symbolp name)
	(let ((cl (vm.bootstrap-loader *vm*)))
	  (sb-thread:with-recursive-lock ((cl.mutex cl))
	    (gethash name (cl.classes cl)))))
      ;; Already in the loader's cache?
      ((let ((cl (or class-loader (vm.bootstrap-loader *vm*))))
	 (sb-thread:with-recursive-lock ((cl.mutex cl))
	   (gethash name (cl.classes cl)))))
      ;; Handle array classes:  
      ((zerop (length name)) nil)
      ((char= (char name 0) #\[)
	(let ((element-type (%parse-field-descriptor name :start 1)))
	  (unless element-type
	    (throw-exception "java/lang/NoClassDefFoundError"))
	  (find-array-class-for (find-cloak-class class-loader element-type))))
      ;; Handle normal classes:
      (class-loader
	(load-user-class class-loader name))
      (t
	(load-bootstrap-class name)))))

(defvar *hash-code* 0)
(defsubst generate-hash-code ()
  (setf *hash-code* (int-+ *hash-code* 1)))

(defun find-array-class-for (element-type)
  (assert (not (eq (cls.name element-type) :void)))
  (sb-thread:with-recursive-lock
      ((cl.mutex (or (cls.class-loader element-type)
		     (vm.bootstrap-loader *vm*))))
    (whereas ((cached (cls.cached-array-class element-type)))
      (return-from find-array-class-for cached))
    (unless *array-vtable*
      (let ((object (find-cloak-class nil "java/lang/Object")))
	(setf *array-vtable* (cls.vtable object))
	(setf *array-method-layout* (cls.method-layout object))))
    (let* ((name (concatenate 'string "[" (cls.jdi-signature element-type)))
	   (class-file
	    (make-cloak-array-class-file
	     :cached-pathname nil
	     :cached-mtime nil
	     :name name
	     :constants #()
	     :superclass-name "java/lang/Object"
	     :interface-names #("java/lang/Cloneable" "java/io/Serializable")
	     :methods '()
	     :access-flags (array-class-modifiers element-type)))
	   (class
	    (make-layout #'make-cloak-array-class
			 'cloak-array
			 nil
			 nil
			 :class-loader (cls.class-loader element-type)
			 :class-file class-file
			 :access-flags (array-class-modifiers element-type)
			 :jdi-signature (concatenate 'string
					  "["
					  (cls.jdi-signature element-type))
			 :name name
			 :initializedp :initialized
			 :linkedp t
			 :element-type element-type
			 :method-layout *array-method-layout*
			 :superclass (find-cloak-class nil "java/lang/Object")
			 :interfaces (list (find-cloak-class nil "java/lang/Cloneable")
					   (find-cloak-class nil "java/io/Serializable"))
			 :vtable-length (length *array-vtable*) ))
	   (actual-cl (or (cls.class-loader element-type) (vm.bootstrap-loader *vm*))) 
	   (prototype (%make-cloak-array (generate-hash-code) nil nil)))
      (setf (cls.vtable class) *array-vtable*)
      (setf (sb-kernel:%instance-ref prototype 0) class)
      (setf (cls.prototype class) prototype)
      (setf (gethash name (cl.classes actual-cl)) class)
      (setf (cls.cached-array-class element-type) class)
      (setf (cls.cpl class)
	    (append (if (typep element-type 'reference-type)
			(mapcar #'find-array-class-for (cls.cpl element-type))
			(list class))
		    ;; object, cloneable, serializable:
		    (list (cls.superclass class))
		    (cls.interfaces class)))
      class)))

(defun load-user-class (class-loader name)
  (let ((c
	 (class-vmdata
	  (call-cloak-method "loadClass(Ljava/lang/String;Z)"
			     (cl.object class-loader)
			     (make-cloak-string (substitute #\. #\/ name))
			     1))))
    (setf (gethash (cls.name c) (cl.classes class-loader)) c)
    c))

(defun find-purported-class (name)
  (loop for (kind namestring zip) in *bootstrap-classpath* do
       (ecase kind
	 (:directory
	  (let* ((namestring (concatenate 'string namestring name ".class"))
		 (mtime (quick-file-write-date namestring)))
	    (when mtime
	      (return (values namestring mtime nil)))))
	 (:zip
	  (let* ((%name (concatenate 'string name ".class"))
		 (entry (zip:get-zipfile-entry %name zip)))
	    (when entry
	      (return
		(values namestring
			(quick-file-write-date namestring)
			entry))))))))

(defun cache-valid-p (cf namestring mtime)
  (let ((sp (cf.cached-pathname cf)))
    (and (equal sp namestring)
	 (eql (cf.cached-mtime cf) mtime))))

(defun load-bootstrap-class (name)
  (let ((cached-class-file (gethash name *class-files/boot*)))
    (multiple-value-bind (namestring mtime zip)
	(find-purported-class name)
      (cond
	((null namestring)
	  nil)
	((and cached-class-file
	      (cache-valid-p cached-class-file namestring mtime))
	  (define-cloak-class nil cached-class-file))
	(t
	  (let* ((bytes (if zip
			    (zip:zipfile-entry-contents zip)
			    (file-contents namestring)))
		 (hash (sb-md5:md5sum-sequence bytes))
		 (cf (or (gethash hash *class-files/hashed*)
			 (let ((q (heap-file-name hash)))
			   (if (quick-probe-file q)
			       (let* ((sb-heapdump:*dumpload-verbose* nil)
				      ;; FIXME: this should probably be a lock
				      ;; in sb-heapdump instead.
				      (cf (with-global-monitor ()
					    (sb-heapdump:load-dumpfile q))))
				 ;; update the .class/.jar pathname:
				 (setf (cf.cached-pathname cf) namestring)
				 (setf (cf.cached-mtime cf) mtime)
				 cf)
			       (progn
				 #+(or)
				 (format *trace-output*
					 "~&; reading from ~A~%"
					 namestring)
				 (incf *class-loading-counter*)
				 (read-cloak-class bytes
						   :cached-pathname namestring
						   :cached-mtime mtime
						   :hash hash)))))))
	    (setf (gethash name *class-files/boot*) cf)
	    (setf (gethash hash *class-files/hashed*) cf)
	    (define-cloak-class nil cf)))))))

(defmethod jdi-signature (cf)
  (concatenate 'string "L" (cf.name cf) ";"))

(defparameter *dump-uncompiled-classes* nil)

(defun define-cloak-class (class-loader class-file &optional (pd +null+))
  (incf *class-counter*)
  (let ((class
	 (make-layout
	  (if (cf.interfacep class-file)
	      #'make-cloak-interface
	      #'make-cloak-class)
	  'cloak-object
	  nil
	  nil
	  :name (cf.name class-file)
	  :jdi-signature (jdi-signature class-file)
	  :access-flags (cf.access-flags class-file)
	  :class-loader class-loader
	  :class-file class-file
	  :method-layout (make-hash-table :test 'equal)
	  :field-layout (make-array (length (cf.fields class-file))
				    :initial-element nil)
	  :pool (make-array (length (cf.constants class-file))
			    :initial-element nil)
	  :protection-domain pd)))
    (load-superclasses class)
    (setf (cls.cpl class)
	  (let ((cpl (if (cls.superclass class)
			 (cls.cpl (cls.superclass class))
			 '())))
	    (dolist (i (cls.interfaces class))
	      (dolist (j (cls.cpl i))
		(pushnew j cpl)))
	    (push class cpl)
	    cpl))
    (when (and *dump-uncompiled-classes* (not (cf.dumpedp class-file)))
      (dump-class-file class-file))
    (let* ((loader (or class-loader (vm.bootstrap-loader *vm*)))
	   (table (cl.classes loader)))
      (sb-thread:with-recursive-lock ((cl.mutex loader))
	(or (gethash (cf.name class-file) table)
	    (setf (gethash (cf.name class-file) table) class))))))

(defmacro with-object-monitor (object &body body)
  (let ((o (gensym)))
    `(let ((,o ,object))
       (when ,o
	 (enter-object-monitor ,o))
       (unwind-protect
	   (progn ,@body)
	 (when ,o
	   (exit-object-monitor ,o))))))

(defun maybe-compute-method-layout (class)
  (with-object-monitor
      (unless *early-bootstrap-p*
	(class-object class))
    (unless (cls.vtable-length class)
      (let ((super (cls.superclass class))
	    (tagged-static-slot-counter 3)
	    (raw-static-slot-counter 0)
	    tagged-slot-counter
	    raw-slot-counter)
	(cond
	  (super
	    (maybe-compute-method-layout super)
	    (setf (cls.vtable-length class) (cls.vtable-length super))
	    (setf tagged-slot-counter (cls.tagged-slot-counter super))
	    (setf raw-slot-counter (cls.raw-slot-counter super)))
	  (t
	    (setf (cls.vtable-length class) 0)
	    (setf tagged-slot-counter 3) ;skip layout,monitor,hash
	    (setf raw-slot-counter 0)))
	(let ((table (cls.method-layout class)))
	  (when super
	    (maphash (lambda (k v) (setf (gethash k table) v))
		     (cls.method-layout super)))
	  (dolist (method (cls.methods class))
	    (let ((sig (cm.signature method)))
	      (unless (gethash sig table)
		(setf (gethash sig table) (n++ (cls.vtable-length class))))))
	  ;; total laestig: wenn wir von einem interface erben, aber eine
	  ;; der interfacemethoden nicht deklarieren, naemlich in einer
	  ;; abstrakten klasse, dann muss zwar eine unserer unterklassen
	  ;; diese methode letztlich bereitstellen, aber es ist durchaus
	  ;; gestattet, dass ein invokevirtual direkt auf uns gemacht
	  ;; wird.  deshalb muessen wir hier fuer eine "fremde" methode einen
	  ;; vtable-index reservieren.  daher ueberhaupt der ganze aerger mit
	  ;; der hash-table.
	  (dolist (c (cls.cpl class))
	    (when (cls.interfacep c)
	      (dolist (method (cls.methods c))
		(let ((sig (cm.signature method)))
		  (unless (gethash sig table)
		    (setf (gethash sig table)
			  (n++ (cls.vtable-length class)))))))))
	(dolist (field (cls.fields class))
	  (setf (vtable-index class field)
		(if (cm.staticp field)
		    (cond
		      ((member (cm.type-descriptor field)
			       '(:float :boolean :byte :int :char :short))
			(n++ raw-static-slot-counter))
		      ((member (cm.type-descriptor field) '(:long :double))
			(prog1
			    (n++ raw-static-slot-counter)
			  (incf raw-static-slot-counter)))
		      (t
			(n++ tagged-static-slot-counter)))
		    (cond
		      ((member (cm.type-descriptor field)
			       '(:float :boolean :byte :int :char :short))
			(n++ raw-slot-counter))
		      ((member (cm.type-descriptor field) '(:long :double))
			(prog1
			    (n++ raw-slot-counter)
			  (incf raw-slot-counter)))
		      (t
			(n++ tagged-slot-counter))))))
	(setf (cls.tagged-slot-counter class) tagged-slot-counter)
	(setf (cls.raw-slot-counter class) raw-slot-counter)
	(setf (cls.tagged-static-slot-counter class) tagged-static-slot-counter)
	(setf (cls.raw-static-slot-counter class) raw-static-slot-counter)))))

;; This doesn't actually work like handler-bind: fn runs with unwound stack.
;; But it is convenient to have.
(defmacro fast-handler-bind (fn &body body)
  `(block .fast-handler-bind.
     (let ((exception
	    (catch 'java-exception
	      (return-from .fast-handler-bind. (progn ,@body)))))
       (funcall ,fn exception)
       (throw 'java-exception exception))))

(defmacro fast-handler-case (form fn)
  `(block .fast-handler-case.
     (funcall ,fn
              (block .handle.
                (fast-handler-bind
                    (lambda (x)
                      (return-from .handle. x))
                  (return-from .fast-handler-case. ,form))))))

(defun link-cloak-class (class)
  (unless (cls.linkedp class)
    (setf (cls.linkedp class) t)
    (link-superclasses class)
    (let ((super (cls.superclass class)))
      (when super
	(pushnew class (cls.subclasses super))))
    (maybe-compute-method-layout class)
    (setf (cls.vtable class) (make-array (cls.vtable-length class)))
    (setf (cls.jni-method-references class)
	  (make-array (cls.vtable-length class) :initial-element nil))
    (setf (cls.itable class) (make-array +itable-length+ :initial-element nil))
    (prepare-stub-methods class)
    (prepare-class class)
    (setf (cls.object class) nil)
    #+(or)
    (for (((class-name . signature) :in *auto-trace-methods*))
      (when (equal class-name (cls.name class))
	(trace-cloak signature class))))
  class)

(defun cls.inner-class-table (class)
  (let ((x (cls.%inner-class-table class)))
    (if (eq x :unparsed)
	(setf (cls.%inner-class-table class)
	      (compute-inner-class-table class))
	x)))

(defun compute-inner-class-table (class)
  (let* ((cf (cls.class-file class))
         (a (find "InnerClasses" (cf.attributes cf) :key 'car :test 'equal)))
    (if a
	(let* ((b (make-buffer (cdr a)))
	       (n (fetch-short b)))
	  (with-collector ()
	    (dotimes (x n)
	      (let ((inner (fetch-short b))
		    (outer (fetch-short b))
		    (inner-name (fetch-short b))
		    (inner-flags (fetch-short b)))
		(collect
		 (list
		  (class-from-pool class inner)
		  (if (zerop outer) nil (class-from-pool class outer))
		  (cf.get-constant cf inner-name)
		  inner-flags))))))
	nil)))

(defun maybe-compile-class (class)
  (let ((cf (cls.class-file class)))
    (unless (cf.compiledp cf)
;;;      (fast-handler-case
;;;       (handler-case
;;;	   (compile-class-file cf)
;;;	 (class-format-error ()
;;;	   (warn "ignoring class format error")))
;;;       (lambda (o)
;;;	 (let ((classformaterror
;;;		(find-cloak-class nil "java/lang/ClassFormatError")))
;;;	   (assert (cloak-type-p o classformaterror))
;;;	   (warn "~A" (write-throwable-to-string o)))))
      (handler-case
	  (let ((*string-cache* (vm.strings *vm*)))
	    (compile-class-file cf))
	(class-format-error (c)
	  (throw-exception "java/lang/ClassFormatError" "~A" c))))))

(defun load-superclasses (class)
  (let ((referenced-classes '()))
    (labels ((recurse (loader class)
	       (setf class (find-cloak-class loader class))
	       (unless (member class referenced-classes)
		 (push class referenced-classes)
		 (let ((super (cls.superclass-name class)))
		   (setf (cls.superclass class)
			 (if super
			     (recurse (cls.class-loader class) super)
			     nil)))
		 (setf (cls.interfaces class)
		       (map 'list
			 (curry #'recurse (cls.class-loader class))
			 (cls.interface-names class))))
	       class))
      (recurse (cls.class-loader class) class))))

(defun link-superclasses (class)
  (let ((referenced-classes '()))
    (labels ((recurse (class)
	       (unless (member class referenced-classes)
		 (push class referenced-classes)
		 (let ((super (cls.superclass class)))
		   (when super (recurse super)))
		 (mapc #'recurse (cls.interfaces class))
		 (link-cloak-class class))
	       class))
      (recurse class))))

(defun make-layout
    (constructor super n-tagged-slots n-untagged-slots &rest initargs)
  (let ((base-layout (sb-kernel::compiler-layout-or-lose super)))
    (apply constructor
	   :classoid (sb-kernel:layout-classoid base-layout)
	   :inherits (concatenate 'simple-vector
		       (sb-kernel:layout-inherits base-layout)
		       (vector base-layout))
	   :depthoid (1+ (sb-kernel:layout-depthoid base-layout))
	   :length (+ (sb-kernel:layout-length base-layout)
		      (or n-tagged-slots 0)
		      (or n-untagged-slots 0))
	   :n-untagged-slots (or n-untagged-slots 0)
	   :invalid nil
	   initargs)))

(defun %add-slots (layout n-tagged n-untagged)
  (incf (sb-kernel:layout-length layout) (+ n-tagged n-untagged))
  (incf (sb-kernel:layout-n-untagged-slots layout) n-untagged)
  layout)

(defun prepare-class (class)
  (setf (cls.static-field-values class)
	(sb-kernel::%make-instance-with-layout
	 (make-layout #'sb-kernel:make-layout
			      'cloak-object
			      (- (cls.tagged-static-slot-counter class) 3)
			      (cls.raw-static-slot-counter class))))
  (do-sequence (field (cls.fields class))
    (when (cm.staticp field)
      (prepare-field class (cls.static-field-values class) field)))
  (%add-slots class
	      (- (cls.tagged-slot-counter class) 3)
	      (cls.raw-slot-counter class))
  (let ((prototype (sb-kernel::%make-instance-with-layout class)))
    (setf (co.monitor prototype) 0)
    ;; prototype doesn't need a hash code
    (do ((c class (cls.superclass c)))
	((null c))
      (do-sequence (field (cls.fields c))
	(unless (cm.staticp field)
	  (prepare-field c prototype field))))
    (setf (cls.prototype class) prototype)))

(defun primitive-default-value (type)
  (case type
    ((:boolean :byte :int :char :short :long) 0)
    (:float 0.0s0)
    (:double 0.0d0)
    (t +null+)))

(defun field-reffer (descriptor)
  (case descriptor
    ((:boolean :byte :int :char :short)
      'cloak::%raw-instance-ref/signed)
    (:long 'cloak::%raw-instance-ref/signed64)
    (:float 'sb-kernel:%raw-instance-ref/single)
    (:double 'sb-kernel:%raw-instance-ref/double)
    (:object 'sb-kernel:%instance-ref)
    (t
      (assert (stringp descriptor))
      'sb-kernel:%instance-ref)))

(defun field-setter (descriptor)
  (case descriptor
    ((:boolean :byte :int :char :short)
      'cloak::%raw-instance-set/signed)
    (:long 'cloak::%raw-instance-set/signed64)
    (:float 'sb-kernel:%raw-instance-set/single)
    (:double 'sb-kernel:%raw-instance-set/double)
    (:object 'sb-kernel:%instance-set)
    (t
      (assert (stringp descriptor))
      'sb-kernel:%instance-set)))

(defun prepare-field (class instance field)
  (let ((newval (primitive-default-value (cm.type-descriptor field)))
	(idx (vtable-index class field)))
    (funcall (field-setter (cm.type-descriptor field)) instance idx newval)))

(defvar *real-collect-garbage* #'sb-kernel::collect-garbage)

(defun install-ugly-class-table ()
  (setf *ugly-class-table* (make-hash-table))
  (sb-ext:without-package-locks
   (setf (fdefinition 'sb-kernel::collect-garbage)
	 (lambda (gen)
	   (prog1
	       (funcall *real-collect-garbage* gen)
	     (update-ugly-class-table))))))

(defun uninstall-ugly-class-table ()
  (sb-ext:without-package-locks
   (setf (fdefinition 'sb-kernel::collect-garbage) *real-collect-garbage*))
  (setf *ugly-class-table* nil))

(defun update-ugly-class-table ()
  (handler-bind ((condition #'invoke-debugger))
    (with-global-monitor ()
      (sb-sys:without-gcing
       (dolist (ref *ugly-vmthrowable-list*)
	 (let ((value (sb-ext:weak-pointer-value ref)))
	   (when value
	     (resolve-vmthrowable value))))
       (setf *ugly-vmthrowable-list* nil)
       (let ((outdated '()))
	 (maphash (lambda (address ref)
		    (let ((class (sb-ext:weak-pointer-value ref)))
		      (cond
			((null class)
			  (remhash address *ugly-class-table*))
			((not (eql address (sb-kernel:get-lisp-obj-address class)))
			  (remhash address *ugly-class-table*)
			  (push class outdated))
			(class
			  (update-ugly-method-table class)))))
		  *ugly-class-table*)
	 (mapc #'%add-to-ugly-class-table outdated))))))

(defun resolve-stackframe (pc class-descriptor)
  (let* ((ref (gethash class-descriptor *ugly-class-table*))
	 (class
	  (if ref
	      (sb-ext:weak-pointer-value ref)
	      nil))
	 (method nil)
	 (pc-offset nil))
    (when class
      (%for ((j :from 0)
	     (cons :across (cls.prev-code-start class)))
	  (when cons
	    (let* ((start (car cons))
		   (end (+ start (code-size (cdr cons)))))
	      (when (and (<= start pc) (< pc end))
		(setf method (elt (cls.methods class) j))
		(setf pc-offset (- pc start))
		(return))))
;;;	(format t "~&; ignoring bogus frame class pointer ~A~%" class)
	(setf class nil)))
    (values method class pc-offset)))

(defun resolve-vmthrowable (vmthrowable)
  (let ((v (getfield "vmdata" vmthrowable)))
    (for ((i :from 0 :below (length v) :by 3))
      (let ((y (elt v i))
	    (x (elt v (1+ i))))
	(when (integerp x)
	  (setf (values (elt v i) (elt v (1+ i)) (elt v (+ i 2)))
		(resolve-stackframe y x)))))))

(defun %add-to-ugly-class-table (class)
  (setf (gethash (sb-kernel:get-lisp-obj-address class) *ugly-class-table*)
	(sb-ext:make-weak-pointer class))
  (update-ugly-method-table class))

(defun code-start (code)
  (sb-sys:sap-int (sb-kernel:code-instructions code)))

(defun code-size (code)
  (* 4 (sb-kernel:%code-code-size code)))

(defun code-end (code)
  (+ (code-start code) (code-size code)))

(defun add-to-ugly-class-table (class)
  (%add-to-ugly-class-table class)
  (let* ((methods (cls.methods class))
	 (table (make-array (length methods) :initial-element nil)))
    (for ((i :from 0)
	  (m :in methods))
      (let ((g (cm.method-function m)))
	(when g
	  (let ((code (sb-disassem::fun-code g)))
	    (setf (elt table i) (cons (code-start code) code))))))
    (setf (cls.prev-code-start class) table)))

(defun update-ugly-method-table (class)
  (for ((i :from 0)
	(cons :across (cls.prev-code-start class)))
    (when cons
      (setf (car cons) (code-start (cdr cons))))))

(defun initialize-class (class)
  (let ((class-object
	 (unless *early-bootstrap-p*
	   (class-object class))))
    (with-object-monitor class-object
      (loop
	(let ((state (cls.initializedp class)))
	  (cond 
	    ((null state)
	      (setf (cls.initializedp class) sb-thread:*current-thread*)
	      (return))
	    ((eq state :error)
	     (throw-exception "java/lang/NoClassDefFoundError"
			      "Error in static initializer."))
	    ((or (eq state :initialized)
		 (eql state sb-thread:*current-thread*))
	      (return-from initialize-class))
	    (t
	      (assert class-object)
	      (|java/lang/VMObject.wait(Ljava/lang/Object;JI)|
	       class-object
	       0
	       0))))))
    (incf *class-initialization-counter*)
    (link-cloak-class class)
    (maybe-compile-class class)
    (with-global-monitor ()
      (sb-sys:without-gcing
       (add-to-ugly-class-table class)))
    (do ((c (cls.superclass class) (cls.superclass c)))
	((null c))
      (maybe-initialize-class c))
    (let ((exception nil))
      (fast-handler-case
          (%initialize-class class)
        (lambda (x)
	  (setf exception x)))
      (with-object-monitor class-object
	(setf (cls.initializedp class) (if exception :error :initialized))
	(when class-object
	  (|java/lang/VMObject.notifyAll(Ljava/lang/Object;)| class-object)))
      (when exception
	(unless (cloak-type-p exception
			      (find-cloak-class nil "java/lang/Error"))
	  (setf exception
	    (make-cloak-instance "java/lang/ExceptionInInitializerError"
				 "<init>(Ljava/lang/Throwable;)"
				 exception)))
	(throw-exception exception)))))

(cloak::defsubst maybe-initialize-class (class)
  (unless (eq :initialized (cls.initializedp class))
    (initialize-class class))
  class)

(defun %call-cloak-method (vtable-index class &rest arguments)
  ;; need a function version for (APPLY %CCM)
  (apply (the function (svref (cls.vtable class) vtable-index)) arguments))

(define-compiler-macro %call-cloak-method (vtable-index class &rest arguments)
  ;; one reason this is a macro: profiling output wouldn't be readable if
  ;; everything was called by the same function
  `(funcall (the function (svref (cls.vtable ,class) ,vtable-index)) ,@arguments))

(defun %initialize-class (class)
  (dolist (f (cls.fields class))
    (when (cm.staticp f)
      (let ((constant-index (cm.constant-value f)))
	(when constant-index
	  (multiple-value-bind (constant tag)
	      (cls.get-constant class constant-index)
	    (funcall (field-setter (cm.type-descriptor f))
		     (cls.static-field-values class)
		     (vtable-index class f)
		     (if (eql tag +constant_string+)
			 (string-from-pool class constant)
			 constant)))))))
  (let ((clinit
	 (find "<clinit>()" (cls.methods class) :key #'cm.signature :test #'string=)))
    (when clinit
      (%call-cloak-method (vtable-index class clinit) class))))

(defmacro defnative (name (this &rest args) &body body)
  `(defun ,name (,this ,@args)
     (declare (ignorable ,this))
     ,@body))

(defmacro defstatic (name (&rest args) &body body)
  `(defun ,name (,@args) ,@body))

(defun array-class-modifiers (element-type)
  ;; XXX abstract steht nicht im Javadoc, wird aber erwartet.  Muss wohl
  ;; irgendwo anders spezifiziert sein.
  (logandc2 (logior (cls.access-flags element-type)
		    +acc_final+
		    +acc_abstract+)
	    +acc_interface+))

(defun load-primitive-classes ()
  (for ((name :in '(:byte :char :double :float :int :long :short :boolean
		    :void))
	(signature :across "BCDFIJSZV"))
    (setf (gethash name (cl.classes (vm.bootstrap-loader *vm*)))
	  (make-layout
	   #'make-primitive-class
	   'cloak-object		;bogus, ignored
	   nil
	   nil
	   :class-loader nil
	   :name name
	   :superclass nil
	   :interfaces nil
	   :jdi-signature (string signature)
	   :access-flags (logior +acc_public+ +acc_final+)))))

(defun array-class-for (element-class)
  (find-cloak-class
   (cls.class-loader element-class)
   (concatenate 'string "[L" (cls.name element-class) ";")))

(defun prepare-stub-methods (class)
  (let ((super (cls.superclass class)))
    (when super
      (setf (subseq (cls.vtable class) 0 (length (cls.vtable super)))
	    (cls.vtable super))))
  (dolist (method (cls.methods class))
    (when
	;; Pfusch alles!
	(or (not (cls.interfacep class))
	    (equal (cm.name method) "<clinit>"))
      (let ((i (vtable-index class method))
	    (method method))		;closure!
	(setf (elt (cls.vtable class) i)
	      (lambda (&rest args)
		(let* ((closure-generator
			(or (cm.method-function method)
			    ;; XXX bitrot
			    ;; (compile-method class method)
			    ))
		       (closure (funcall closure-generator class)))
		  (dolist (id (cm.method-ids method))
		    (method-from-pool class id))
		  (dolist (id (cm.field-ids method))
		    (field-from-pool class id))
		  (dolist (id (cm.class-ids method))
		    (class-from-pool class id))
		  (dolist (id (cm.string-ids method))
		    (string-from-pool class id))
		  (setf (cm.method-function method) closure-generator)
		  (setf (vtable-entry class i) closure)
		  (apply closure args))))))))

(defun (setf vtable-entry) (newval class i)
  (let ((oldval (elt (cls.vtable class) i)))
    (labels ((doit (class)
	       (let ((vtable (cls.vtable class)))
		 (when (eq (elt vtable i) oldval)
		   (setf (elt vtable i) newval)
		   (mapc #'doit (cls.subclasses class))))))
      (doit class))))

(defvar *exception-mode* nil
  "One of NIL, :TRACE, :BREAK, or an exception number.")

(defun throw-exception (x &optional format-string &rest arguments)
  #-slow (declare (optimize (speed 3) (safety 0)))
  (when (typep x '(or reference-type string))
    (setf x (if format-string
		(make-cloak-instance x
		    "<init>(Ljava/lang/String;)"
		  (make-cloak-string
		   (apply #'format nil format-string arguments)))
		(make-cloak-instance x "<init>()"))))
  (%throw-exception x))

(defun %throw-exception (x)
  ;; #-slow (declare (optimize (speed 3) (safety 0)))
  (when *exception-mode*
    (format *debug-io* "~&; [~D] About to throw ~A.~%" *exception-counter* x)
    (force-output *debug-io*))
  (when (and (stringp *exception-mode*)
	     (cloak-type-p x (find-cloak-class nil *exception-mode*)))
    (sb-debug::backtrace))
  (when (or (eq *exception-mode* :break)
	    (eql *exception-mode* *exception-counter*))
    (break))
  (when *exception-mode*
    (incf *exception-counter*))
  (throw 'java-exception x))

(defun write-cloak (o &optional (stream t))
  (write o :stream stream))

(defun write-cloak-to-string (o)
  (with-output-to-string (s)
    (write-cloak o s)))
