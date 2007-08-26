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

(dolist (name *all-jni-names*)
  (makunbound (sb-int:symbolicate "<" name ">")))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun extended-alien-type (keyword)
    (case keyword
      (:string 'sb-alien:c-string)
      (:pointer '(* t))
      (t (alien-type keyword)))))

(defun free-sap (sap)
  (sb-alien:alien-funcall
   (sb-alien:extern-alien "free"
			  (function (values) sb-sys:system-area-pointer))
   sap))

(defmacro define-jni-function (name rtype (&rest args) &body body)
  (let* ((declaration
	  (if (eq (and (listp (car body)) (caar body)) 'declare)
	      (car body)
	      nil))
	 (form
	  `(fast-handler-case
	       (progn ,@(if declaration (cdr body) body))
	     (lambda (x)
	       (setf *pending-exception* x)
	       ,(if (member rtype '(:object :pointer))
		    '(sb-sys:int-sap 0)
		    0)))))
    `(sb-alien::define-alien-callback ,name
	 ,(case rtype
	    (:void 'sb-alien:int)	;define-alien-callback bug
	    (:string '(* t))		;siehe unten
	    (t (extended-alien-type rtype)))
	 ((env (* (* (sb-alien:struct JNINativeInterface))))
	  ,@(loop
		for (name type) in args
		collect `(,name ,(extended-alien-type type))))
       (declare (ignorable env) ,@(cdr declaration))
       ,@(with-collector (collect)
	   (for (((name type) :in args))
	     (case type
	       (:pointer
		 (collect
		  `(setf ,name (sb-alien:alien-sap ,name))))
	       (:object
		 (collect
		  `(setf ,name (resolve-ref (sb-alien:alien-sap ,name))))))))
       ,(case rtype
	  (:object
	    `(let ((result ,form))
	       (etypecase result
		 ((or null cloak-object)
		   (new-local-ref result))
		 (sb-sys:system-area-pointer result))))
	  (:string
	    ;; das muss doch einfacher gehen 
	    `(let ((str ,form))
	       (if (stringp str)
		   (let* ((n (length str))
			  (cstr
			   (sb-alien:alien-sap
			    (sb-alien:make-alien
			     (array (sb-alien:unsigned 8) nil)
			     (1+ n)))))
		     (for ((i :from 0 :below n))
		       (setf (sb-sys:sap-ref-8 cstr i) (char-code (char str i))))
		     (setf (sb-sys:sap-ref-8 cstr n) 0)
		     cstr)
		   str)))
	  (:void
	    ;; see above
	    `(progn ,form 0))
	  (t
	    form)))))

(define-jni-function <unimplemented-callback> :int ()
  (error "unimplemented JNI function"))

(define-jni-function <getversion> :int ()
  #x00010002)

(define-jni-function <findclass> :pointer ((name :string))
  (jclass (find-cloak-class (cls.class-loader *%class%*) name)))

(define-jni-function <getobjectclass> :pointer ((obj :object))
  (if (nullp obj)
      (sb-sys:int-sap 0)
      (jclass (%class obj))))

(define-jni-function <isinstanceof> :boolean ((obj :object) (cls :object))
  (setf cls (class-vmdata cls))
  (if (cloak-type-p obj cls) 1 0))

(define-jni-function <issameobject> :boolean ((o :object) (p :object))
  (if (eq o p) 1 0))

(define-jni-function <isassignablefrom> :boolean ((c :object) (d :object))
  (setf c (class-vmdata c))
  (setf d (class-vmdata d))
  (if (cloak-subclass-p c d) 1 0))

(define-jni-function <getsuperclass> :pointer ((class :object))
  (setf class (class-vmdata class))
  (jclass (cls.superclass class)))

(define-jni-function <throw> :int ((x :object))
  (%throw-exception x))

(define-jni-function <thrownew> :int ((class :object) (message :string))
  (throw-exception (class-vmdata class) "~A" message))

(define-jni-function <exceptionoccurred> :object ()
  *pending-exception*)

(define-jni-function <exceptioncheck> :boolean ()
  (if (nullp *pending-exception*) 0 1))

(define-jni-function <exceptiondescribe> :void ()
  (unless (nullp *pending-exception*)
    (warn "exception in native code: ~A" *pending-exception*)
    (setf *pending-exception* +null+)))

(define-jni-function <exceptionclear> :void ()
  (setf *pending-exception* +null+))

(define-jni-function <fatalerror> :void ((msg :string))
  (error "FatalError(~A)" msg))

(define-jni-function <newglobalref> :pointer ((obj :object))
  (new-global-ref obj))

(define-jni-function <deleteglobalref> :void ((ref :pointer))
  (delete-global-ref ref))

(define-jni-function <deletelocalref> :void ((ref :pointer))
  (delete-local-ref ref))

(define-jni-function <newlocalref> :object ((obj :object))
  obj)

(define-jni-function <pushlocalframe> :int ((n :int))
  #-strict-localref-frames (declare (ignore n))
  #+strict-localref-frames (push-localref-frame n)
  0)

(define-jni-function <poplocalframe> :object ((obj :object))
  #+strict-localref-frames (pop-localref-frame)
  obj)

(defun jfield (c f)
  (let ((x (* 2 (1+ (vtable-index c f)))))
    (when (cm.staticp f)
      (incf x))
    (when (keywordp (cm.type-descriptor f))
      (setf x (- x)))
    x))

(defun jfield-vtable-index (x)
  (1- (abs (truncate x 2))))

(defun jfield-primitive-p (x)
  (minusp x))

(defun jfield-static-p (x)
  (oddp x))

(define-jni-function <getfieldid> :int
    ((class :object) (name :string) (signature :string))
  (declare (ignore signature))
  (setf class (class-vmdata class))
  (maybe-initialize-class class)
  (multiple-value-bind (c f)
      (find-field name class)
    (assert (not (cm.staticp f)))
    (if c
	(jfield c f)
	0)))

(define-jni-function <getstaticfieldid> :int
    ((class :object) (name :string) (signature :string))
  (declare (ignore signature))
  (setf class (class-vmdata class))
  (maybe-initialize-class class)
  (multiple-value-bind (c f)
      (find-field name class)
    (assert (and (eq class c) (cm.staticp f)))
    (if c
	(jfield c f)
	0)))

(define-jni-function <fromreflectedfield> :int ((field :object))
  (jfield (class-vmdata (field-class-object field)) (field-vmdata field)))

(define-jni-function <toreflectedfield> :object
    ((class :object) (jfield :int))
  (setf class (class-vmdata class))
  (let* ((idx (jfield-vtable-index jfield))
	 (primitivep (jfield-primitive-p jfield))
	 (staticp (jfield-static-p jfield))
	 (f (dolist (f (cls.fields class))
	      (when (and (eql idx (vtable-index class f))
			 (eq primitivep (keywordp (cm.type-descriptor f)))
			 (eq staticp (cm.staticp f)))
		(return f)))))
    (make-cloak-instance
	(find-cloak-class nil "java/lang/reflect/Field")
	"<init>(Ljava/lang/Class;Ljava/lang/String;I)"
      (class-object class)
      (make-cloak-string (cm.name f))
      (cm.static-index f))))

(macrolet ((define-accessor (type)
	     `(progn
		(define-jni-function ,(sb-int:symbolicate "<GET" type "FIELD>")
		    ,type
                    ((obj :object) (jfield :int))
		  (,(field-reffer type) obj (jfield-vtable-index jfield)))
		(define-jni-function ,(sb-int:symbolicate "<SET" type "FIELD>")
		    :void
                    ((obj :object) (jfield :int) (value ,type))
		  (,(field-setter type) obj (jfield-vtable-index jfield) value))
		(define-jni-function
		    ,(sb-int:symbolicate "<GETSTATIC" type "FIELD>")
		    ,type
                    ((class :object) (jfield :int))
		  (setf class (class-vmdata class))
		  (,(field-reffer type)
		   (cls.static-field-values class)
		   (jfield-vtable-index jfield)))
		(define-jni-function
		    ,(sb-int:symbolicate "<SETSTATIC" type "FIELD>")
		    :void
                    ((class :object) (jfield :int) (value ,type))
		  (setf class (class-vmdata class))
		  (,(field-setter type)
		   (cls.static-field-values class)
		   (jfield-vtable-index jfield)
		   value)))))
  (define-accessor :object)
  (define-accessor :boolean)
  (define-accessor :byte)
  (define-accessor :char)
  (define-accessor :short)
  (define-accessor :int)
  ;; (define-accessor :long)
  (define-accessor :float)
  ;; (define-accessor :double)
  )

;; long und double in callbacks funktionieren nicht, also erstmal von hand:
(PROGN
 (DEFINE-JNI-FUNCTION <GETLONGFIELD>
     :LONG
   ((OBJ :OBJECT) (JFIELD :INT))
   (CLOAK::%RAW-INSTANCE-REF/SIGNED64 OBJ (jfield-vtable-index JFIELD)))
 (DEFINE-JNI-FUNCTION <SETLONGFIELD>
     :VOID
   ((OBJ :OBJECT) (JFIELD :INT) (lo :int) (hi :int))
   (CLOAK::%RAW-INSTANCE-SET/SIGNED64
    OBJ
    (jfield-vtable-index JFIELD)
    (+ (ash hi 32) (logand #xffffffff lo))))
 (DEFINE-JNI-FUNCTION <GETSTATICLONGFIELD>
     :LONG
   ((CLASS :OBJECT) (JFIELD :INT))
   (SETF CLASS (CLASS-VMDATA CLASS))
   (CLOAK::%RAW-INSTANCE-REF/SIGNED64
    (CLS.STATIC-FIELD-VALUES CLASS)
    (jfield-vtable-index JFIELD)))
 (DEFINE-JNI-FUNCTION <SETSTATICLONGFIELD>
     :VOID
   ((CLASS :OBJECT) (JFIELD :INT) (lo :int) (hi :int))
   (SETF CLASS (CLASS-VMDATA CLASS))
   (CLOAK::%RAW-INSTANCE-SET/SIGNED64
    (CLS.STATIC-FIELD-VALUES CLASS)
    (jfield-vtable-index JFIELD)
    (+ (ash hi 32) (logand #xffffffff lo)))))
(PROGN
 (DEFINE-JNI-FUNCTION <GETDOUBLEFIELD>
     :DOUBLE
   ((OBJ :OBJECT) (JFIELD :INT))
   (SB-KERNEL:%RAW-INSTANCE-REF/DOUBLE OBJ (jfield-vtable-index JFIELD)))
 (DEFINE-JNI-FUNCTION <SETDOUBLEFIELD>
     :VOID
   ((OBJ :OBJECT) (JFIELD :INT) (lo :int) (hi :int))
   (SB-KERNEL:%RAW-INSTANCE-SET/DOUBLE
    OBJ
    (jfield-vtable-index JFIELD)
    (long-to-double (+ (ash hi 32) (logand #xffffffff lo)))))
 (DEFINE-JNI-FUNCTION <GETSTATICDOUBLEFIELD>
     :DOUBLE
   ((CLASS :OBJECT) (JFIELD :INT))
   (SETF CLASS (CLASS-VMDATA CLASS))
   (SB-KERNEL:%RAW-INSTANCE-REF/DOUBLE
    (CLS.STATIC-FIELD-VALUES CLASS)
    (jfield-vtable-index JFIELD)))
 (DEFINE-JNI-FUNCTION <SETSTATICDOUBLEFIELD>
     :VOID
   ((CLASS :OBJECT) (JFIELD :INT) (lo :int) (hi :int))
   (SETF CLASS (CLASS-VMDATA CLASS))
   (SB-KERNEL:%RAW-INSTANCE-SET/DOUBLE
    (CLS.STATIC-FIELD-VALUES CLASS)
    (jfield-vtable-index JFIELD)
    (long-to-double (+ (ash hi 32) (logand #xffffffff lo))))))

(defstruct jmethod
  vtable-index
  method)

(defun jmethod (c m)
  (let ((v (cls.jni-method-references c))
	(i (cm.static-index m)))
    (or (elt v i)
	(setf (elt v i)
	      (new-global-ref
	       (make-jmethod
		:vtable-index (if (cls.interfacep c)
				  nil
				  (vtable-index c m))
		:method m))))))

(define-jni-function <getmethodid> :pointer
    ((class :object) (name :string) (signature :string))
  (setf class (class-vmdata class))
  (cond
    ((typep class 'reference-type)
      (maybe-initialize-class class)
      (setf signature (subseq signature 0 (1+ (position #\) signature))))
      (multiple-value-bind (m c)
	  (and (typep class 'reference-type)
	       (get-cloak-method (concatenate 'string name signature) class t))
	(cond
	  ((null m)
	    (sb-sys:int-sap 0))
	  (t
	    (assert (not (cm.staticp m)))
	    (jmethod c m)))))
    (t
      (sb-sys:int-sap 0))))

(define-jni-function <getstaticmethodid> :pointer
  ((class :object) (name :string) (signature :string))
  (setf class (class-vmdata class))
  (cond
    ((typep class 'reference-type)
      (maybe-initialize-class class)
      (setf signature (subseq signature 0 (1+ (position #\) signature))))
      (multiple-value-bind (m c)
	  (get-cloak-method (concatenate 'string name signature) class t)
	(cond
	  ((null m)
	    (sb-sys:int-sap 0))
	  (t
	    (assert (cm.staticp m))
	    (jmethod c m)))))
    (t
      (sb-sys:int-sap 0))))

(define-jni-function <fromreflectedmethod> :pointer ((method :object))
  (jmethod (method-declaring-class method) (method-vmdata method)))

(define-jni-function <toreflectedmethod> :object
    ((class :object) (method :object))
  (setf class (class-vmdata class))
  (let ((m (jmethod-method method)))
    (make-cloak-instance
	(find-cloak-class nil "java/lang/reflect/Method")
	"<init>(Ljava/lang/Class;Ljava/lang/String;I)"
      (class-object class)
      (make-cloak-string (cm.name m))
      (cm.static-index m))))

(sb-alien:define-alien-routine "va_int" sb-alien:int
  (foo (* t)))

(sb-alien:define-alien-routine ("va_int" va-sap) (* t)
  (foo (* t)))

(sb-alien:define-alien-routine "va_longlong" (sb-alien:signed 64)
  (foo (* t)))

(sb-alien:define-alien-routine "va_double" sb-alien:double
  (foo (* t)))

(defun fetch-stdargs (jmethod ap)
  (let ((v (make-array 1 :element-type '(unsigned-byte 32))))
    (setf (elt v 0) (sb-sys:sap-int ap))
    (sb-sys:with-pinned-objects (v)
      (loop
	  with ptr = (sb-sys:vector-sap v)
	  for type in (cm.argtype-descriptors (jmethod-method jmethod))
	  collect
	    (ecase (simplify-type type)
	      (:int
		(va-int ptr))
	      (:long
		(va-longlong ptr))
	      (:float
		(float (va-double ptr) 1.0s0))
	      (:double
		(va-double ptr))
	      (:object
		(resolve-ref (sb-alien:alien-sap (va-sap ptr)))))))))

(defun unbox-args (jmethod array)
  (loop
      for i from 0 by 8
      for type in (cm.argtype-descriptors (jmethod-method jmethod))
      collect
	(case type
	  ((:byte :boolean)
	    (sb-sys:signed-sap-ref-8 array i))
	  (:char
	    (sb-sys:sap-ref-16 array i))
	  (:short
	    (sb-sys:signed-sap-ref-16 array i))
	  (:int
	    (sb-sys:signed-sap-ref-32 array i))
	  (:long
	    (sb-sys:signed-sap-ref-64 array i))
	  (:float
	    (sb-sys:sap-ref-single array i))
	  (:double
	    (sb-sys:sap-ref-double array i))
	  (t
	    (resolve-ref (sb-sys:int-sap (sb-sys:sap-ref-32 array i)))))))

(defun %apply-virtual (obj jmethod args)
  (apply #'%call-cloak-method
	 (jmethod-vtable-index jmethod)
	 (%class obj)
	 obj
	 args))

(defun %apply-nonvirtual (obj class jmethod args)
  (apply #'%call-cloak-method
	 (jmethod-vtable-index jmethod)
	 class
	 obj
	 args))

(defun %apply-static (class jmethod args)
  (apply #'%call-cloak-method
	 (jmethod-vtable-index jmethod)
	 class
	 args))

(define-jni-function <allocobject> :object ((class :object))
  (setf class (class-vmdata class))
  (assert (eq :initialized (cls.initializedp class)))
  (%make-cloak-object class))

(define-jni-function
    <newobjectv>
    :object
    ((class :object) (jmethod :object) (ap :pointer))
  (setf class (class-vmdata class))
  (assert (eq :initialized (cls.initializedp class)))
  (let ((instance (%make-cloak-object class)))
    (%apply-nonvirtual instance class jmethod (fetch-stdargs jmethod ap))
    instance))

(define-jni-function
    <newobjecta>
    :object
    ((class :object) (jmethod :object) (args :pointer))
  (setf class (class-vmdata class))
  (assert (eq :initialized (cls.initializedp class)))
  (let ((instance (%make-cloak-object class)))
    (%apply-nonvirtual instance class jmethod (unbox-args jmethod args))
    instance))

(macrolet
    ((frob (type)
       `(progn
	  (define-jni-function
	      ,(sb-int:symbolicate "<CALL" type "METHODV>")
	      ,type
	      ((obj :object) (jmethod :object) (ap :pointer))
	    (%apply-virtual obj jmethod (fetch-stdargs jmethod ap)))
	  (define-jni-function
	      ,(sb-int:symbolicate "<CALL" type "METHODA>")
	      ,type
	      ((obj :object) (jmethod :object) (args :pointer))
	    (%apply-virtual obj jmethod (unbox-args jmethod args)))
	  (define-jni-function
	      ,(sb-int:symbolicate "<CALLNONVIRTUAL" type "METHODV>")
	      ,type
	      ((obj :object) (class :object) (jmethod :object) (ap :pointer))
	    (setf class (class-vmdata class))
	    (%apply-nonvirtual obj class jmethod (fetch-stdargs jmethod ap)))
	  (define-jni-function
	      ,(sb-int:symbolicate "<CALLNONVIRTUAL" type "METHODA>")
	      ,type
	      ((obj :object) (class :object) (jmethod :object) (args :pointer))
	    (setf class (class-vmdata class))
	    (%apply-nonvirtual obj class jmethod (unbox-args jmethod args)))
	  (define-jni-function
	      ,(sb-int:symbolicate "<CALLSTATIC" type "METHODV>")
	      ,type
	      ((class :object) (jmethod :object) (ap :pointer))
	    (setf class (class-vmdata class))
	    (%apply-static class jmethod (fetch-stdargs jmethod ap)))
	  (define-jni-function
	      ,(sb-int:symbolicate "<CALLSTATIC" type "METHODA>")
	      ,type
	      ((class :object) (jmethod :object) (args :pointer))
	    (setf class (class-vmdata class))
	    (%apply-static class jmethod (unbox-args jmethod args))))))
  (frob :void)
  (frob :object)
  (frob :boolean)
  (frob :byte)
  (frob :char)
  (frob :short)
  (frob :int)
  (frob :long)
  (frob :float)
  (frob :double))

(define-jni-function <monitorenter> :int ((obj :object))
  (enter-object-monitor obj)
  0)

(define-jni-function <monitorexit> :int ((obj :object))
  (exit-object-monitor obj)
  0)

(define-jni-function <newstring> :object ((data :pointer) (length :int))
  (let* ((class (find-cloak-class nil "java/lang/String"))
	 (b (make-array length :element-type '(unsigned-byte 16)))
	 (array (make-cloak-array "[C" b)))
    (dotimes (i length)
      (setf (elt b i) (sb-sys:sap-ref-16 data (* i 2))))
    (make-cloak-instance class "<init>([CIIZ)" array 0 length 1)))

(define-jni-function <getstringlength> :int ((str :object))
  (getfield "count" str))

(define-jni-function <getstringchars> :pointer ((str :object) (copy :pointer))
  (let* ((data (co.data (getfield "value" str)))
	 (offset (getfield "offset" str))
	 (count (getfield "count" str))
	 (result
	  (sb-alien:alien-sap
	   (sb-alien:make-alien (array (sb-alien:unsigned 16) nil) count))))
    (for ((i :from 0 :below count)
	  (j :from offset))
      (setf (sb-sys:sap-ref-16 result (* i 2)) (elt data j)))
    (unless (zerop (sb-sys:sap-int copy))
      (setf (sb-sys:sap-ref-8 copy 0) 1))
    result))

(define-jni-function <getstringregion>
    :void
    ((str :object) (start :int) (len :int) (buf :pointer))
  (let* ((data (co.data (getfield "value" str)))
	 (offset (getfield "offset" str))
	 (count (getfield "count" str))
	 (limit (+ offset count)))
    (unless (and (< (1- offset) start limit)
		 (plusp len)
		 (<= (+ offset start len) limit))
      (throw-exception "java/lang/ArrayIndexOutOfBoundsException"))
    (for ((i :from (+ offset start))
	  (j :from 0)
	  :repeat len)
      (setf (sb-sys:sap-ref-16 buf (* j 2)) (elt data i)))))

(define-jni-function <releasestringchars> :void ((str :object) (data :pointer))
  (declare (ignorable str))
  (free-sap data))

(defparameter <getstringcritical> <getstringchars>)
(defparameter <releasestringcritical> <releasestringchars>)


;; XXX Achtung Pfusch:

(define-jni-function <newstringutf> :object ((utf8 :string))
  (make-cloak-string utf8))

(define-jni-function <getstringutflength> :int ((str :object))
  (length (get-string-value str)))

(define-jni-function <getstringutfchars> :string ((str :object))
  (get-string-value str))

(define-jni-function <releasestringutfchars> :void
    ((str :object) (data :pointer))
  (declare (ignorable str))
  (free-sap data))

(define-jni-function <getstringutfregion>
    :void
    ((str :object) (start :int) (len :int) (buf :pointer))
  (let* ((data (co.data (getfield "value" str)))
	 (offset (getfield "offset" str))
	 (count (getfield "count" str))
	 (limit (+ offset count)))
    (unless (and (< (1- offset) start limit)
		 (plusp len)
		 (<= (+ offset start len) limit))
      (throw-exception "java/lang/ArrayIndexOutOfBoundsException"))
    (let ((tmp (cloak::encode-pseudo-utf-8 data
					   (+ offset start)
					   (+ offset start len))))
      (for ((i :from 0 :below (length tmp))
	    (j :from 0))
	(setf (sb-sys:sap-ref-8 buf j) (char-code (elt tmp i)))))))

(define-jni-function <getarraylength> :int ((array :object))
  (length (co.data array)))

(define-jni-function <newobjectarray> :object
    ((length :int) (element-type :object) (initial-element :object))
  (setf element-type (class-vmdata element-type))
  (do-make-cloak-array
    (find-array-class-for element-type)
    (make-array length :initial-element initial-element)))

(define-jni-function <getobjectarrayelement> :object
    ((array :object) (index :int))
  (checked-elt (co.data array) index))

(define-jni-function <setobjectarrayelement> :void
    ((array :object) (index :int) (newval :object))
  (unless (or (nullp newval)
	      (cloak-type-p newval (cls.element-type (%class array))))
    (throw-exception "java/lang/ArrayStoreException"))
  (setf (checked-elt (co.data array) index) newval))

(macrolet ((doit (name class-name lisp-type)
	     `(define-jni-function ,(sb-int:symbolicate "<NEW" name "ARRAY>")
		  :object
		  ((length :int))
		(do-make-cloak-array
		    (find-cloak-class nil ,class-name)
		  (make-array length
			      :element-type ',lisp-type
			      :initial-element ,(coerce 0 lisp-type))))))
  (doit :boolean "[Z" (signed-byte 8))
  (doit :byte    "[B" (signed-byte 8))
  (doit :char    "[C" (unsigned-byte 16))
  (doit :short   "[S" (signed-byte 16))
  (doit :int     "[I" (signed-byte 32))
  (doit :long    "[J" (signed-byte 64))
  (doit :float   "[F" single-float)
  (doit :double  "[D" double-float))

(defconstant +jni-commit+ 1)
(defconstant +jni-abort+ 2)

(macrolet
    ((doit (type size sap-ref)
       `(progn
	  (defun ,(sb-int:symbolicate "GET-PRIMITIVE-" type "-ARRAY")
	      (array copy)
	    (let* ((data (co.data array))
		   (n (length data))
		   (result
		    (sb-alien:alien-sap
		     (sb-alien:make-alien (array ,(alien-type type) nil) n))))
	      (dotimes (i n)
		(setf (,sap-ref result (* i ,size)) (elt data i)))
	      (unless (zerop (sb-sys:sap-int copy))
		(setf (sb-sys:sap-ref-8 copy 0) 1))
	      result))
	  (defun ,(sb-int:symbolicate "RELEASE-PRIMITIVE-" type "-ARRAY")
	      (array newdata mode)
	    (unless (eql mode +jni-abort+)
	      (let* ((data (co.data array))
		     (n (length data)))
		(dotimes (i n)
		  (setf (elt data i) (,sap-ref newdata (* i ,size))))))
	    (unless (eql mode +jni-commit+)
	      (free-sap newdata)))
	  
	  (define-jni-function
	      ,(sb-int:symbolicate "<GET" type "ARRAYELEMENTS>")
	      :pointer
	      ((array :object) (copy :pointer))
	    (,(sb-int:symbolicate "GET-PRIMITIVE-" type "-ARRAY")
	     array
	     copy))
	  (define-jni-function
	      ,(sb-int:symbolicate "<RELEASE" type "ARRAYELEMENTS>")
	      :void
	      ((array :object) (newdata :pointer) (mode :int))
	    (,(sb-int:symbolicate "RELEASE-PRIMITIVE-" type "-ARRAY")
	     array
	     newdata
	     mode))
	  
	  (define-jni-function
	      ,(sb-int:symbolicate "<GET" type "ARRAYREGION>")
	      :void
	      ((array :object) (start :int) (len :int) (buf :pointer))
	    (let* ((data (co.data array))
		   (n (length data)))
	      (unless (and (< -1 start n)
			   (plusp len)
			   (<= (+ start len) n))
		(throw-exception "java/lang/ArrayIndexOutOfBoundsException"))
	      (for ((i :from start)
		    (j :from 0)
		    :repeat len)
		(setf (,sap-ref buf (* j ,size)) (elt data i)))))
	  (define-jni-function
	      ,(sb-int:symbolicate "<SET" type "ARRAYREGION>")
	      :void
	      ((array :object) (start :int) (len :int) (buf :pointer))
	    (let* ((data (co.data array))
		   (n (length data)))
	      (unless (and (< -1 start n)
			   (plusp len)
			   (<= (+ start len) n))
		(throw-exception "java/lang/ArrayIndexOutOfBoundsException"))
	      (for ((i :from start)
		    (j :from 0)
		    :repeat len)
		(setf (elt data i) (,sap-ref buf (* j ,size)))))))))
  (doit :boolean 1 sb-sys:sap-ref-8)
  (doit :byte 1 sb-sys:signed-sap-ref-8)
  (doit :char 2 sb-sys:sap-ref-16)
  (doit :short 2 sb-sys:signed-sap-ref-16)
  (doit :int 4 sb-sys:signed-sap-ref-32)
  (doit :long 8 sb-sys:signed-sap-ref-64)
  (doit :float 4 sb-sys:sap-ref-single)
  (doit :double 8 sb-sys:sap-ref-double))

(define-jni-function <getjavavm> :int ((out :pointer))
  (setf (sb-sys:sap-ref-word out 0)
	(sb-sys:sap-int (vm.javavm-pointer *vm*)))
  0)

(define-jni-function <getprimitivearraycritical>
    :pointer
    ((array :object) (copy :pointer))
  (ecase (cls.name (cls.element-type (%class array)))
    (:boolean (get-primitive-boolean-array array copy))
    (:byte (get-primitive-byte-array array copy))
    (:char (get-primitive-char-array array copy))
    (:short (get-primitive-short-array array copy))
    (:int (get-primitive-int-array array copy))
    (:long (get-primitive-long-array array copy))
    (:float (get-primitive-float-array array copy))
    (:double (get-primitive-double-array array copy))))

(define-jni-function <releaseprimitivearraycritical>
    :pointer
    ((array :object) (newdata :pointer) (mode :int))
  (ecase (cls.name (cls.element-type (%class array)))
    (:boolean (release-primitive-boolean-array array newdata mode))
    (:byte (release-primitive-byte-array array newdata mode))
    (:char (release-primitive-char-array array newdata mode))
    (:short (release-primitive-short-array array newdata mode))
    (:int (release-primitive-int-array array newdata mode))
    (:long (release-primitive-long-array array newdata mode))
    (:float (release-primitive-float-array array newdata mode))
    (:double (release-primitive-double-array array newdata mode))))


;;;; Invocation API

(define-jni-function <unimplemented-javavm-callback> :int ()
  (error "Sorry: Invocation API not supported"))

(sb-alien::define-alien-callback <getenv>
    (sb-alien:signed 32)
    ((vm (* (* (sb-alien:struct JNIInvokeInterface))))
     (out (* t))
     (version (sb-alien:signed 32)))
  (declare (ignore version		;fixme
		   vm))
  (assert *jni-env-alien*)
  (setf (sb-sys:sap-ref-word (sb-alien:alien-sap out) 0)
	(sb-sys:sap-int *jni-env-sap*))
  0)

(sb-alien::define-alien-callback <attachcurrentthread>
    (sb-alien:signed 32)
    ((vm (* (* (sb-alien:struct JNIInvokeInterface))))
     (out (* t))
     (args (* t)))
  (declare (ignore vm args))
  (assert *jni-env-alien*)
  (setf (sb-sys:sap-ref-word (sb-alien:alien-sap out) 0)
	(sb-sys:sap-int *jni-env-sap*))
  0)
