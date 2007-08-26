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

(declaim (special *vm*))
(defconstant +null+ #+sigsegv-hack 0 #-sigsegv-hack nil)


(defmacro with-global-monitor ((&optional) &body body)
  `(invoke-with-global-monitor (lambda () ,@body)))


;;;; global and local references

(defun resolve-ref (sap)
  (if (zerop (sb-sys:sap-int sap))
      +null+
      (sb-kernel:make-lisp-obj (sb-sys:sap-ref-word sap 0))))


;;;; global references

(defun make-globalref-table (n)
  (let ((tab (make-array n :initial-element nil)))
    (dotimes (i (1- n))
      (setf (elt tab i) (1+ i)))
    tab))

(defun new-global-ref (object)
  (with-global-monitor ()
    (dotimes (i (length (vm.globalref-tables *vm*)) (error "oops?"))
      (let ((table (elt (vm.globalref-tables *vm*) i))
	    (ptr (elt (vm.globalref-free-pointers *vm*) i)))
	(unless table
	  (setf table (make-globalref-table
		       (* 2 (length (elt (vm.globalref-tables *vm*) (1- i))))))
	  (setf (elt (vm.globalref-tables *vm*) i) table)) 
	(when ptr
	  (assert (integerp ptr))
	  (check-type (elt table ptr) (or null integer))
	  (shiftf (elt (vm.globalref-free-pointers *vm*) i)
		  (elt table ptr)
		  object)
	  (check-type (elt (vm.globalref-free-pointers *vm*) i)
	      (or null integer))
	  (return (sb-sys:sap+ (cloak::vector-sap table) (* ptr 4))))))))

(defun delete-global-ref (sap)
  (with-global-monitor ()
    (dotimes (i (length (vm.globalref-tables *vm*)) (error "oops?"))
      (let* ((table (elt (vm.globalref-tables *vm*) i))
	     (table-sap (cloak::vector-sap table))
	     (table-end-sap (sb-sys:sap+ table-sap (* (length table) 4))))
	(when (and (sb-sys:sap<= table-sap sap)
		   (sb-sys:sap< sap table-end-sap))
	  (let ((ptr (/ (sb-sys:sap- sap table-sap) 4)))
	    (setf (elt table ptr) (elt (vm.globalref-free-pointers *vm*) i))
	    (setf (elt (vm.globalref-free-pointers *vm*) i) ptr)
	    (return)))))))

(defun jclass (class)
  (or (cls.jni-reference class)
      (setf (cls.jni-reference class)
	    (new-global-ref (class-object class)))))


;;;; local references

(defmacro with-localref-frame ((n) &body body)
  `(invoke-with-localref-frame (lambda () ,@body) ,n))

(defvar *localref-frames*)
(defvar *localref-frame-stack*)

(defun invoke-with-localref-frame (fn n)
  (push-localref-frame n)
  (let ((head *localref-frame-stack*))
    (unwind-protect
	(funcall fn)
      #+strict-localref-frames (assert (eq head *localref-frame-stack*))
      (until (eq head *localref-frame-stack*)
	(pop-localref-frame))
      (pop-localref-frame))))

(defstruct (framespec
	    (:conc-name "FR.")
	    (:constructor make-framespec (index fill-pointer)))
  index
  fill-pointer)

(defun push-localref-frame (n)
  (let* ((fr (car *localref-frame-stack*))
	 (frame-index (fr.index fr))
	 (fill-pointer (fr.fill-pointer fr))
	 (l (length (svref *localref-frames* frame-index))))
    (unless (< (+ fill-pointer n) l)
      (incf frame-index)
      (setf fill-pointer 0)
      (unless (svref *localref-frames* frame-index)
	(setf (svref *localref-frames* frame-index)
	      (make-array (* 2 l) :initial-element nil))))
    (push (make-framespec frame-index fill-pointer) *localref-frame-stack*)))

(defun pop-localref-frame ()
  (let* ((cur (pop *localref-frame-stack*))
	 (prev (car *localref-frame-stack*))
	 (start (if (eql (fr.index cur) (fr.index prev))
		    (fr.fill-pointer prev)
		    0)))
    (fill (svref *localref-frames* (fr.index cur))
	  nil
	  :start start
	  :end (fr.fill-pointer cur))))

(defun new-local-ref (object)
  (when (nullp object)
    (return-from new-local-ref (sb-sys:int-sap 0)))
  (let* ((cur (car *localref-frame-stack*))
	 (frame (svref *localref-frames* (fr.index cur)))
	 (fp (fr.fill-pointer cur)))
    (if (and (< fp (length frame)) (null (elt frame fp)))
	(prog2
	    (setf (elt frame fp) object)
	    (sb-sys:sap+ (cloak::vector-sap frame) (* fp 4))
	  (incf (fr.fill-pointer cur)))
	(let* ((prev (cadr *localref-frame-stack*))
	       (base (if (eql (fr.index cur) (fr.index prev))
			 (fr.fill-pointer prev)
			 0)))
	  (loop
	      for i from base below fp
	      when (null (elt frame i))
	      do
		(setf (elt frame i) object)
		(return (sb-sys:sap+ (cloak::vector-sap frame) (* i 4)))
	      finally
		#+strict-localref-frames (error "localref frame overflow")
		(warn "localref frame overflow")
		(push-localref-frame 16)
		(return (new-local-ref object)))))))

(defun find-local-ref (sap)
  (for ((i :from (fr.index (car *localref-frame-stack*))
	   :downto 0))
    (let* ((frame (svref *localref-frames* i))
	   (frame-sap (cloak::vector-sap frame))
	   (frame-end-sap (sb-sys:sap+ frame-sap (* (length frame) 4))))
      (when (and (sb-sys:sap<= frame-sap sap)
		 (sb-sys:sap< sap frame-end-sap))
	(return (values i (/ (sb-sys:sap- sap frame-sap) 4)))))))

(defun delete-local-ref (sap)
  (declare (ignore sap))
  ;; tja, das sofort zu nullen kam mir total clever vor, bevor ich versucht
  ;; habe, anderer leuts tollen jni code zu starten
  #+(or)
  (setf (sb-sys:sap-ref-word sap 0) (sb-kernel:get-lisp-obj-address nil)))


;;;;
;;;; The JNIEnv structure
;;;;

(sb-alien:define-alien-type nil
    (sb-alien:struct
     JNINativeInterface
     (reserved0 (* t))
     (reserved1 (* t))
     (reserved2 (* t))
     (reserved3 (* t))
     (GetVersion (* t))
     (DefineClass (* t))
     (FindClass (* t))
     (FromReflectedMethod (* t))
     (FromReflectedField (* t))
     (ToReflectedMethod (* t))
     (GetSuperclass (* t))
     (IsAssignableFrom (* t))
     (ToReflectedField (* t))
     (Throw (* t))
     (ThrowNew (* t))
     (ExceptionOccurred (* t))
     (ExceptionDescribe (* t))
     (ExceptionClear (* t))
     (FatalError (* t))
     (PushLocalFrame (* t))
     (PopLocalFrame (* t))
     (NewGlobalRef (* t))
     (DeleteGlobalRef (* t))
     (DeleteLocalRef (* t))
     (IsSameObject (* t))
     (NewLocalRef (* t))
     (EnsureLocalCapacity (* t))
     (AllocObject (* t))
     (NewObject (* t))
     (NewObjectV (* t))
     (NewObjectA (* t))
     (GetObjectClass (* t))
     (IsInstanceOf (* t))
     (GetMethodID (* t))
     (CallObjectMethod (* t))
     (CallObjectMethodV (* t))
     (CallObjectMethodA (* t))
     (CallBooleanMethod (* t))
     (CallBooleanMethodV (* t))
     (CallBooleanMethodA (* t))
     (CallByteMethod (* t))
     (CallByteMethodV (* t))
     (CallByteMethodA (* t))
     (CallCharMethod (* t))
     (CallCharMethodV (* t))
     (CallCharMethodA (* t))
     (CallShortMethod (* t))
     (CallShortMethodV (* t))
     (CallShortMethodA (* t))
     (CallIntMethod (* t))
     (CallIntMethodV (* t))
     (CallIntMethodA (* t))
     (CallLongMethod (* t))
     (CallLongMethodV (* t))
     (CallLongMethodA (* t))
     (CallFloatMethod (* t))
     (CallFloatMethodV (* t))
     (CallFloatMethodA (* t))
     (CallDoubleMethod (* t))
     (CallDoubleMethodV (* t))
     (CallDoubleMethodA (* t))
     (CallVoidMethod (* t))
     (CallVoidMethodV (* t))
     (CallVoidMethodA (* t))
     (CallNonvirtualObjectMethod (* t))
     (CallNonvirtualObjectMethodV (* t))
     (CallNonvirtualObjectMethodA (* t))
     (CallNonvirtualBooleanMethod (* t))
     (CallNonvirtualBooleanMethodV (* t))
     (CallNonvirtualBooleanMethodA (* t))
     (CallNonvirtualByteMethod (* t))
     (CallNonvirtualByteMethodV (* t))
     (CallNonvirtualByteMethodA (* t))
     (CallNonvirtualCharMethod (* t))
     (CallNonvirtualCharMethodV (* t))
     (CallNonvirtualCharMethodA (* t))
     (CallNonvirtualShortMethod (* t))
     (CallNonvirtualShortMethodV (* t))
     (CallNonvirtualShortMethodA (* t))
     (CallNonvirtualIntMethod (* t))
     (CallNonvirtualIntMethodV (* t))
     (CallNonvirtualIntMethodA (* t))
     (CallNonvirtualLongMethod (* t))
     (CallNonvirtualLongMethodV (* t))
     (CallNonvirtualLongMethodA (* t))
     (CallNonvirtualFloatMethod (* t))
     (CallNonvirtualFloatMethodV (* t))
     (CallNonvirtualFloatMethodA (* t))
     (CallNonvirtualDoubleMethod (* t))
     (CallNonvirtualDoubleMethodV (* t))
     (CallNonvirtualDoubleMethodA (* t))
     (CallNonvirtualVoidMethod (* t))
     (CallNonvirtualVoidMethodV (* t))
     (CallNonvirtualVoidMethodA (* t))
     (GetFieldID (* t))
     (GetObjectField (* t))
     (GetBooleanField (* t))
     (GetByteField (* t))
     (GetCharField (* t))
     (GetShortField (* t))
     (GetIntField (* t))
     (GetLongField (* t))
     (GetFloatField (* t))
     (GetDoubleField (* t))
     (SetObjectField (* t))
     (SetBooleanField (* t))
     (SetByteField (* t))
     (SetCharField (* t))
     (SetShortField (* t))
     (SetIntField (* t))
     (SetLongField (* t))
     (SetFloatField (* t))
     (SetDoubleField (* t))
     (GetStaticMethodID (* t))
     (CallStaticObjectMethod (* t))
     (CallStaticObjectMethodV (* t))
     (CallStaticObjectMethodA (* t))
     (CallStaticBooleanMethod (* t))
     (CallStaticBooleanMethodV (* t))
     (CallStaticBooleanMethodA (* t))
     (CallStaticByteMethod (* t))
     (CallStaticByteMethodV (* t))
     (CallStaticByteMethodA (* t))
     (CallStaticCharMethod (* t))
     (CallStaticCharMethodV (* t))
     (CallStaticCharMethodA (* t))
     (CallStaticShortMethod (* t))
     (CallStaticShortMethodV (* t))
     (CallStaticShortMethodA (* t))
     (CallStaticIntMethod (* t))
     (CallStaticIntMethodV (* t))
     (CallStaticIntMethodA (* t))
     (CallStaticLongMethod (* t))
     (CallStaticLongMethodV (* t))
     (CallStaticLongMethodA (* t))
     (CallStaticFloatMethod (* t))
     (CallStaticFloatMethodV (* t))
     (CallStaticFloatMethodA (* t))
     (CallStaticDoubleMethod (* t))
     (CallStaticDoubleMethodV (* t))
     (CallStaticDoubleMethodA (* t))
     (CallStaticVoidMethod (* t))
     (CallStaticVoidMethodV (* t))
     (CallStaticVoidMethodA (* t))
     (GetStaticFieldID (* t))
     (GetStaticObjectField (* t))
     (GetStaticBooleanField (* t))
     (GetStaticByteField (* t))
     (GetStaticCharField (* t))
     (GetStaticShortField (* t))
     (GetStaticIntField (* t))
     (GetStaticLongField (* t))
     (GetStaticFloatField (* t))
     (GetStaticDoubleField (* t))
     (SetStaticObjectField (* t))
     (SetStaticBooleanField (* t))
     (SetStaticByteField (* t))
     (SetStaticCharField (* t))
     (SetStaticShortField (* t))
     (SetStaticIntField (* t))
     (SetStaticLongField (* t))
     (SetStaticFloatField (* t))
     (SetStaticDoubleField (* t))
     (NewString (* t))
     (GetStringLength (* t))
     (GetStringChars (* t))
     (ReleaseStringChars (* t))
     (NewStringUTF (* t))
     (GetStringUTFLength (* t))
     (GetStringUTFChars (* t))
     (ReleaseStringUTFChars (* t))
     (GetArrayLength (* t))
     (NewObjectArray (* t))
     (GetObjectArrayElement (* t))
     (SetObjectArrayElement (* t))
     (NewBooleanArray (* t))
     (NewByteArray (* t))
     (NewCharArray (* t))
     (NewShortArray (* t))
     (NewIntArray (* t))
     (NewLongArray (* t))
     (NewFloatArray (* t))
     (NewDoubleArray (* t))
     (GetBooleanArrayElements (* t))
     (GetByteArrayElements (* t))
     (GetCharArrayElements (* t))
     (GetShortArrayElements (* t))
     (GetIntArrayElements (* t))
     (GetLongArrayElements (* t))
     (GetFloatArrayElements (* t))
     (GetDoubleArrayElements (* t))
     (ReleaseBooleanArrayElements (* t))
     (ReleaseByteArrayElements (* t))
     (ReleaseCharArrayElements (* t))
     (ReleaseShortArrayElements (* t))
     (ReleaseIntArrayElements (* t))
     (ReleaseLongArrayElements (* t))
     (ReleaseFloatArrayElements (* t))
     (ReleaseDoubleArrayElements (* t))
     (GetBooleanArrayRegion (* t))
     (GetByteArrayRegion (* t))
     (GetCharArrayRegion (* t))
     (GetShortArrayRegion (* t))
     (GetIntArrayRegion (* t))
     (GetLongArrayRegion (* t))
     (GetFloatArrayRegion (* t))
     (GetDoubleArrayRegion (* t))
     (SetBooleanArrayRegion (* t))
     (SetByteArrayRegion (* t))
     (SetCharArrayRegion (* t))
     (SetShortArrayRegion (* t))
     (SetIntArrayRegion (* t))
     (SetLongArrayRegion (* t))
     (SetFloatArrayRegion (* t))
     (SetDoubleArrayRegion (* t))
     (RegisterNatives (* t))
     (UnregisterNatives (* t))
     (MonitorEnter (* t))
     (MonitorExit (* t))
     (GetJavaVM (* t))

     ;; JNI 1.2 functions
     (GetStringRegion (* t))
     (GetStringUTFRegion (* t))
     (GetPrimitiveArrayCritical (* t))
     (ReleasePrimitiveArrayCritical (* t))
     (GetStringCritical (* t))
     (ReleaseStringCritical (* t))
     (NewWeakGlobalRef (* t))
     (DeleteWeakGlobalRef (* t))
     (ExceptionCheck (* t))

     ;; JNI 1.4 functions
     (NewDirectByteBuffer (* t))
     (GetDirectBufferAddress (* t))
     (GetDirectBufferCapacity (* t))))

(defmacro with-jni-env ((&optional) &body body)
  `(invoke-with-jni-env (lambda () ,@body)))

(defvar *jni-env-alien*)
(defvar *jni-env-sap*)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *all-jni-names*
      '(GetVersion DefineClass FindClass FromReflectedMethod FromReflectedField
	ToReflectedMethod GetSuperclass IsAssignableFrom ToReflectedField Throw
	ThrowNew ExceptionOccurred ExceptionDescribe ExceptionClear FatalError
	PushLocalFrame PopLocalFrame NewGlobalRef DeleteGlobalRef DeleteLocalRef
	IsSameObject NewLocalRef EnsureLocalCapacity AllocObject NewObject
	NewObjectV NewObjectA GetObjectClass IsInstanceOf GetMethodID
	CallObjectMethod CallObjectMethodV CallObjectMethodA CallBooleanMethod
	CallBooleanMethodV CallBooleanMethodA CallByteMethod CallByteMethodV
	CallByteMethodA CallCharMethod CallCharMethodV CallCharMethodA
	CallShortMethod CallShortMethodV CallShortMethodA CallIntMethod
	CallIntMethodV CallIntMethodA CallLongMethod CallLongMethodV
	CallLongMethodA CallFloatMethod CallFloatMethodV CallFloatMethodA
	CallDoubleMethod CallDoubleMethodV CallDoubleMethodA CallVoidMethod
	CallVoidMethodV CallVoidMethodA CallNonvirtualObjectMethod
	CallNonvirtualObjectMethodV CallNonvirtualObjectMethodA
	CallNonvirtualBooleanMethod CallNonvirtualBooleanMethodV
	CallNonvirtualBooleanMethodA CallNonvirtualByteMethod
	CallNonvirtualByteMethodV CallNonvirtualByteMethodA
	CallNonvirtualCharMethod CallNonvirtualCharMethodV
	CallNonvirtualCharMethodA CallNonvirtualShortMethod
	CallNonvirtualShortMethodV CallNonvirtualShortMethodA
	CallNonvirtualIntMethod CallNonvirtualIntMethodV
	CallNonvirtualIntMethodA CallNonvirtualLongMethod
	CallNonvirtualLongMethodV CallNonvirtualLongMethodA
	CallNonvirtualFloatMethod CallNonvirtualFloatMethodV
	CallNonvirtualFloatMethodA CallNonvirtualDoubleMethod
	CallNonvirtualDoubleMethodV CallNonvirtualDoubleMethodA
	CallNonvirtualVoidMethod CallNonvirtualVoidMethodV
	CallNonvirtualVoidMethodA GetFieldID GetObjectField GetBooleanField
	GetByteField GetCharField GetShortField GetIntField GetLongField
	GetFloatField GetDoubleField SetObjectField SetBooleanField SetByteField
	SetCharField SetShortField SetIntField SetLongField SetFloatField
	SetDoubleField GetStaticMethodID CallStaticObjectMethod
	CallStaticObjectMethodV CallStaticObjectMethodA CallStaticBooleanMethod
	CallStaticBooleanMethodV CallStaticBooleanMethodA CallStaticByteMethod
	CallStaticByteMethodV CallStaticByteMethodA CallStaticCharMethod
	CallStaticCharMethodV CallStaticCharMethodA CallStaticShortMethod
	CallStaticShortMethodV CallStaticShortMethodA CallStaticIntMethod
	CallStaticIntMethodV CallStaticIntMethodA CallStaticLongMethod
	CallStaticLongMethodV CallStaticLongMethodA CallStaticFloatMethod
	CallStaticFloatMethodV CallStaticFloatMethodA CallStaticDoubleMethod
	CallStaticDoubleMethodV CallStaticDoubleMethodA CallStaticVoidMethod
	CallStaticVoidMethodV CallStaticVoidMethodA GetStaticFieldID
	GetStaticObjectField GetStaticBooleanField GetStaticByteField
	GetStaticCharField GetStaticShortField GetStaticIntField
	GetStaticLongField GetStaticFloatField GetStaticDoubleField
	SetStaticObjectField SetStaticBooleanField SetStaticByteField
	SetStaticCharField SetStaticShortField SetStaticIntField
	SetStaticLongField SetStaticFloatField SetStaticDoubleField NewString
	GetStringLength GetStringChars ReleaseStringChars NewStringUTF
	GetStringUTFLength GetStringUTFChars ReleaseStringUTFChars
	GetArrayLength NewObjectArray GetObjectArrayElement
	SetObjectArrayElement NewBooleanArray NewByteArray NewCharArray
	NewShortArray NewIntArray NewLongArray NewFloatArray NewDoubleArray
	GetBooleanArrayElements GetByteArrayElements GetCharArrayElements
	GetShortArrayElements GetIntArrayElements GetLongArrayElements
	GetFloatArrayElements GetDoubleArrayElements ReleaseBooleanArrayElements
	ReleaseByteArrayElements ReleaseCharArrayElements
	ReleaseShortArrayElements ReleaseIntArrayElements
	ReleaseLongArrayElements ReleaseFloatArrayElements
	ReleaseDoubleArrayElements GetBooleanArrayRegion GetByteArrayRegion
	GetCharArrayRegion GetShortArrayRegion GetIntArrayRegion
	GetLongArrayRegion GetFloatArrayRegion GetDoubleArrayRegion
	SetBooleanArrayRegion SetByteArrayRegion SetCharArrayRegion
	SetShortArrayRegion SetIntArrayRegion SetLongArrayRegion
	SetFloatArrayRegion SetDoubleArrayRegion RegisterNatives
	UnregisterNatives MonitorEnter MonitorExit GetJavaVM GetStringRegion
	GetStringUTFRegion GetPrimitiveArrayCritical
	ReleasePrimitiveArrayCritical GetStringCritical ReleaseStringCritical
	NewWeakGlobalRef DeleteWeakGlobalRef ExceptionCheck NewDirectByteBuffer
	GetDirectBufferAddress GetDirectBufferCapacity)))

#+(or)
(declaim (optimize (speed 3) (safety 0)))

(sb-alien:define-alien-routine "install_trampolines" sb-alien:void
  (jnienv (* t)))

(declaim (special <unimplemented-callback>))

(defun invoke-with-jni-env (fn)
  (sb-alien:with-alien
      ((env (sb-alien:struct JNINativeInterface)))
    (setf (sb-alien:slot env 'reserved0) (sb-sys:int-sap 0))
    (setf (sb-alien:slot env 'reserved1) (sb-sys:int-sap 0))
    (setf (sb-alien:slot env 'reserved2) (sb-sys:int-sap 0))
    (setf (sb-alien:slot env 'reserved3) (sb-sys:int-sap 0))
    (macrolet
	((doit ()
	   `(progn
	      ,@(loop
		    for name in *all-jni-names*
		    collect
		      (let ((lisp-name (sb-int:symbolicate "<" name ">")))
			`(setf (sb-alien:slot env ',name)
			       (sb-alien:alien-sap
				(if (boundp ',lisp-name)
				    (symbol-value ',lisp-name)
				    <unimplemented-callback>))))))))
      (doit))
    (locally
	(declare (optimize sb-c::stack-allocate-dynamic-extent))
      (let ((v (make-array 1 :element-type '(unsigned-byte 32)))
	    (*localref-frames* (make-array 24))
	    (*localref-frame-stack* (list (make-framespec 0 0))))
	(declare (dynamic-extent v *localref-frames*))
	(fill *localref-frames* nil)
	(setf (elt v 0)
	      (sb-sys:sap-int
	       (sb-alien:alien-sap (sb-alien:addr env))))
	(setf (svref *localref-frames* 0)
	      (make-array 256 :initial-element nil))
	(let ((*jni-env-alien* env)
	      (*jni-env-sap* (sb-sys:vector-sap v)))
	  (install-trampolines *jni-env-sap*)
	  (funcall fn))))))


;;;;
;;;; JNIInvokeInterface
;;;;

(sb-alien:define-alien-type nil
    (sb-alien:struct
     JNIInvokeInterface
     (reserved0 (* t))
     (reserved1 (* t))
     (reserved2 (* t))
     (DestroyJavaVM (* t))
     (AttachCurrentThread (* t))
     (DetachCurrentThread (* t))
     (GetEnv (* t))
     (AttachCurrentThreadAsDaemon (* t))))

(defmacro with-jni-invoke-interface ((javavm-pointer) &body body)
  `(invoke-with-jni-invoke-interface (lambda (,javavm-pointer) ,@body)))

(declaim (special <attachcurrentthread>
		  <getenv>
		  <unimplemented-javavm-callback>))

(defun invoke-with-jni-invoke-interface (fn)
  (sb-alien:with-alien
      ((javavm (sb-alien:struct JNIInvokeInterface)))
    (setf (sb-alien:slot javavm 'reserved0) (sb-sys:int-sap 0))
    (setf (sb-alien:slot javavm 'reserved1) (sb-sys:int-sap 0))
    (setf (sb-alien:slot javavm 'reserved2) (sb-sys:int-sap 0))
    (setf (sb-alien:slot javavm 'GetEnv) (sb-alien:alien-sap <getenv>))
    (setf (sb-alien:slot javavm 'AttachCurrentThread)
	  (sb-alien:alien-sap <attachcurrentthread>))
    (setf (sb-alien:slot javavm 'DestroyJavaVM)
	  (sb-alien:alien-sap <unimplemented-javavm-callback>))
    (setf (sb-alien:slot javavm 'DetachCurrentThread)
	  (sb-alien:alien-sap <unimplemented-javavm-callback>))
    (setf (sb-alien:slot javavm 'AttachCurrentThreadAsDaemon)
	  (sb-alien:alien-sap <unimplemented-javavm-callback>))
    (locally
	(declare (optimize sb-c::stack-allocate-dynamic-extent))
      (let ((v (make-array 1 :element-type '(unsigned-byte 32))))
	(declare (dynamic-extent v))
	(setf (elt v 0)
	      (sb-sys:sap-int
	       (sb-alien:alien-sap (sb-alien:addr javavm))))
	(funcall fn (sb-sys:vector-sap v))))))
