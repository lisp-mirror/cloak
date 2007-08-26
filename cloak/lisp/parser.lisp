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

(define-condition class-format-error (error) ())

(defstruct (class-file (:conc-name "CF."))
  (cached-pathname (missing))
  (cached-mtime (missing))
  (version)
  (constants)
  (access-flags)
  (name)
  (superclass-name)
  (superclass-index)
  (interface-names)
  (fields)
  (methods)
  (attributes)
  (compiledp)
  (hash)
  (dumpedp))

(defmethod print-object ((object class-file) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (princ (cf.name object) stream)))

(defstruct (class-member (:conc-name "CM."))
  (declaring-class-file)
  static-index
  access-flags
  name
  descriptor
  (field-ids '())
  (method-ids '())
  (class-ids '())
  (string-ids '()))

(defstruct (cloak-field
	    (:include class-member)
            (:conc-name "CM.")
	    (:constructor %make-cloak-field))
  constant-value
  %type-descriptor)

(defstruct (cloak-method
            (:include class-member)
            (:conc-name "CM.")
            (:constructor %make-cloak-method))
  (max-stack)
  (max-locals)
  (code)
  exceptions
  (xtable)
  (method-function nil)
  signature
  %rtype-descriptor
  %argtype-descriptors
  ;; (i) after parsing, this holds the java `instruction to line number' table
  ;; (ii) during compilation, it maps from form numbers to line numbers
  ;; (iii) after compilation, it maps from PCs to line numbers
  line-numbers)

(defstruct (try-catch-block
	    (:conc-name "TC.")
	    (:constructor make-try-catch-block
			  (start-pc end-pc handler-pc catch-type)))
  (start-pc (missing) :type fixnum)
  (end-pc (missing) :type fixnum)
  (handler-pc (missing) :type fixnum)
  (catch-type (missing) :type fixnum))

(defmethod print-object ((object try-catch-block) stream)
  (print-unreadable-object (object stream :type t)
    (format stream "[~A ~A) ->(~A) ~A"
	    (tc.start-pc object)
	    (tc.end-pc object)
	    (tc.catch-type object)
	    (tc.handler-pc object))))

(defmethod print-object ((object cloak-field) stream)
  (print-unreadable-object (object stream :type t)
    (format stream "~A ~A" (cm.descriptor object) (cm.name object))))

(defmethod print-object ((object cloak-method) stream)
  (print-unreadable-object (object stream :type t)
    (format stream "~A.~A~A"
	    (cf.name (cm.declaring-class-file object))
	    (cm.name object)
	    (cm.descriptor object))))

(defparameter *smallest-supported-version* '(45 . 3))
(defparameter *largest-supported-version* '(50 . 0))

(defun version<= (a b)
  (if (= (car a) (car b))
      (<= (cdr a) (cdr b))
      (< (car a) (car b))))

(defun file-contents (p)
  (with-open-file (s p :element-type '(unsigned-byte 8))
    (let ((bytes
	   (make-array (file-length s) :element-type '(unsigned-byte 8))))
      (read-sequence bytes s)
      bytes)))

(defun read-cloak-class (bytes &key hash cached-pathname cached-mtime)
  (let* ((buffer (make-buffer bytes))
	 (class-file (make-class-file
		      ;; use the namestring to avoid heapdumping pathnames
                      :cached-pathname (when cached-pathname
					 (namestring cached-pathname))
                      :cached-mtime (when cached-pathname
				      cached-mtime))))
    (%read-cloak-class class-file buffer)
    (setf (cf.hash class-file) (or hash (sb-md5:md5sum-sequence bytes)))
    class-file))

(defun %read-cloak-class (class s)
  (let ((magic (fetch-int s)))
    (unless (= magic #xcafebabe)
      (class-format-error "invalid magic number ~X" magic)))
  (let ((minor (fetch-short s))
	(major (fetch-short s)))
    (setf (cf.version class) (cons major minor)))
  (unless (and (version<= *smallest-supported-version* (cf.version class))
	       (version<= (cf.version class) *largest-supported-version*))
    (class-format-error "unsupported class file version"))
  (setf (cf.constants class) (fetch-constants s))
  (setf (cf.access-flags class) (fetch-short s))
;;;  (unless (or (cf.superp class) (cf.interfacep class))
;;;    (warn "class file lacks ACC_SUPER access"))
  (let ((this-class (fetch-short s)))
    (setf (cf.name class) (cf.get-constant-class class this-class)))
  (let ((super (fetch-short s)))
    (setf (cf.superclass-index class) super)
    (setf (cf.superclass-name class)
	  (if (equal (cf.name class) "java/lang/Object")
	      nil
	      (cf.get-constant-class class super))))
  (setf (cf.interface-names class) (fetch-interface-names class s))
  (setf (cf.fields class) (fetch-members #'make-cloak-field class s))
  (setf (cf.methods class) (fetch-members #'make-cloak-method class s))
  (setf (cf.attributes class) (fetch-attributes class s))
  class)

(defun cf.get-constant (class i)
  (let ((pair (elt (cf.constants class) i)))
    (values (cdr pair) (car pair))))

(defun cf.get-constant-class (cf i)
  (cf.get-constant cf (cf.get-constant cf i)))

(defun check-class-ref (class i)
  (assert (eql (nth-value 1 (cf.get-constant class i)) +constant_class+))
  i)

(defun fetch-interface-names (class s)
  (let* ((n (fetch-short s))
	 (result (make-array n :initial-element nil)))
    (for* ((i :from 0 :below n)
	   (id = (fetch-short s)))
      (setf (elt result i) (cf.get-constant-class class id)))
    result))

(defun fetch-members (constructor cloak-class s)
  (with-collector ()
    (dotimes (i (fetch-short s))
      (let ((member (fetch-member constructor cloak-class s)))
	(setf (cm.static-index member) i)
	(collect member)))))

(defun fetch-member (constructor class-file s)
  (let* ((access-flags (fetch-short s))
	 (name-index (fetch-short s))
	 (descriptor-index (fetch-short s))
	 (attributes (fetch-attributes class-file s)))
    (funcall constructor
	     :declaring-class-file class-file
	     :access-flags access-flags
	     :name (cf.get-constant class-file name-index)
	     :descriptor (cf.get-constant class-file descriptor-index)
	     :attributes attributes)))

(macrolet ((deflag (name)
	     `(progn
		(defun ,(sb-int:symbolicate "CLS." name "P") (cls)
		  (logtest ,(sb-int:symbolicate "+ACC_" name "+")
			   (cls.access-flags cls)))
		(defun ,(sb-int:symbolicate "CF." name "P") (cf)
		  (logtest ,(sb-int:symbolicate "+ACC_" name "+")
			   (cf.access-flags cf))))))
  (deflag "PUBLIC")
  (deflag "FINAL")
  (deflag "SUPER")
  (deflag "INTERFACE")
  (deflag "ABSTRACT"))

(macrolet ((deflag (name)
	     `(defun ,(sb-int:symbolicate "CM." name "P") (cm)
		(logtest ,(sb-int:symbolicate "+ACC_" name "+")
			 (cm.access-flags cm)))))
  (deflag "PUBLIC")
  (deflag "PRIVATE")
  (deflag "PROTECTED")
  (deflag "STATIC")
  (deflag "FINAL")
  (deflag "SYNCHRONIZED")
  (deflag "VOLATILE")
  (deflag "TRANSIENT")
  (deflag "NATIVE")
  (deflag "ABSTRACT")
  (deflag "STRICT"))

(defun fetch-attributes (class s)
  (let* ((n (fetch-short s))
	 (result (make-array n :initial-element nil)))
    (for ((i from 0 below n))
      (let* ((name-index (fetch-short s))
	     (attribute-length (fetch-int s))
	     (info
	      (make-array attribute-length :element-type '(unsigned-byte 8)))
	     (attribute (cons (cf.get-constant class name-index) info)))
	(fetch-sequence info s)
	(setf (elt result i) attribute)))
    result))

(defun fetch-constants (s)
  (let* ((n (fetch-short s))
	 (result (make-array n :initial-element '(-1 . :invalid)))
	 (i 1))
    (while (< i n)
      (let ((pair (fetch-constant s)))
	(setf (elt result i) pair)
	(incf i (case (car pair)
		  ((#.+constant_long+ #.+constant_double+) 2)
		  (t 1)))))
    result))

(defvar *string-cache* nil)
(defvar *string-cache-lock* (sb-thread:make-mutex))
(defun intern-lisp-string (lisp-string)
  (if *string-cache*
      (sb-thread:with-recursive-lock (*string-cache-lock*)
	(or (car (gethash lisp-string *string-cache*))
	    (progn
	      (setf (gethash lisp-string *string-cache*)
		    (cons lisp-string nil))
	      lisp-string)))
      lisp-string))

(defun fetch-constant (s)
  (let ((tag (fetch-byte s)))
    (cons tag
	  (ecase tag
	    (#.+constant_class+
	      (fetch-short s))
	    (#.+constant_fieldref+
	      (cons (fetch-short s) (fetch-short s)))
	    (#.+constant_methodref+
	      (cons (fetch-short s) (fetch-short s)))
	    (#.+constant_interfacemethodref+
	      (cons (fetch-short s) (fetch-short s)))
	    (#.+constant_string+
	      (fetch-short s))
	    (#.+constant_integer+
	      (canonicalize-int (fetch-int s)))
	    (#.+constant_float+
	      (int-to-float (canonicalize-int (fetch-int s))))
	    (#.+constant_long+
	      (canonicalize-long (fetch-long s)))
	    (#.+constant_double+
	      (long-to-double (canonicalize-long (fetch-long s))))
	    (#.+constant_nameandtype+
	      (cons (fetch-short s) (fetch-short s)))
	    (#.+constant_utf8+
	      (let* ((length (fetch-short s))
		     (v (make-array length :element-type '(unsigned-byte 8))))
		(for ((i :from 0 :below length))
		  (setf (aref v i) (fetch-byte s)))
		(intern-lisp-string (utf-8-to-string v))))))))

(defun find-attribute (name alist)
  (find name alist :key 'car :test 'equal))

(defun cf.sourcefile (cf)
  (let ((i (cdr (find-attribute "SourceFile" (cf.attributes cf)))))
    (if i
	(cf.get-constant cf (+ (ash (elt i 0) 8) (elt i 1)))
	nil)))

(defun parse-line-number-table (code-attributes)
  (let ((bytes (cdr (find-attribute "LineNumberTable" code-attributes))))
    (if bytes
	(let* ((s (make-buffer bytes))
	       (n (fetch-short s)))
	  (sort (loop
		    repeat n
		    collect
		      (cons (fetch-short s) (fetch-short s)))
		#'>
		:key #'car))
	nil)))

(defun make-cloak-method
    (&rest initargs &key declaring-class-file attributes &allow-other-keys)
  (let ((code-attribute (find-attribute "Code" attributes))
	(exceptions (find-attribute "Exceptions" attributes)))
    (when code-attribute
      (let* ((s (make-buffer (cdr code-attribute)))
             (max-stack (fetch-short s))
             (max-locals (fetch-short s))
             (code-length (fetch-int s))
             (code (make-array code-length :element-type '(unsigned-byte 8))))
        (fetch-sequence code s)
        (let* ((xtable-length (fetch-short s)))
          (with-collector (collect-xblock :result xtable)
            ;; order matters
            (dotimes (x xtable-length)
              (collect-xblock
               (make-try-catch-block
		(fetch-short s)
		(fetch-short s)
		(fetch-short s)
		(let ((i (fetch-short s)))
		  (unless (zerop i)
		    (check-class-ref declaring-class-file i))
		  i))))
            (let ((attributes (fetch-attributes declaring-class-file s)))
              (setf initargs
                    (list* :max-stack max-stack
                           :max-locals max-locals
                           :code code
                           :xtable xtable
			   :line-numbers (parse-line-number-table attributes)
                           initargs)))))))
    (when exceptions
      (setf initargs
	    (list* :exceptions
		   (let* ((s (make-buffer (cdr exceptions)))
			  (n (fetch-short s)))
		     (with-collector ()
		       (dotimes (x n)
			 (let ((index (fetch-short s)))
			   (collect
			    (check-class-ref declaring-class-file index))))))
		   initargs)))
    (let ((instance (apply #'%make-cloak-method
			   :allow-other-keys t ;wg. attributes
			   initargs)))
      (let* ((descriptor (cm.descriptor instance))
             (argtypes (subseq descriptor 0 (1+ (position #\) descriptor)))))
        (setf (cm.signature instance)
              (concatenate 'string (cm.name instance) argtypes)))
      instance)))

(defun make-cloak-field (&rest initargs &key attributes &allow-other-keys)
  (let ((constant-value
	 (find "ConstantValue" attributes :test 'equal :key 'car)))
    (when constant-value
      (let* ((bytes (cdr constant-value))
	     (index (logior (ash (elt bytes 0) 8) (elt bytes 1))))
	(setf initargs (list* :constant-value index initargs))))
    (apply #'%make-cloak-field
	   :allow-other-keys t		;wg. attributes
	   initargs)))

(defparameter *opcodes*
    #(NOP ACONST_NULL ICONST_M1 ICONST_0 ICONST_1 ICONST_2 ICONST_3
      ICONST_4 ICONST_5 LCONST_0 LCONST_1 FCONST_0 FCONST_1 FCONST_2
      DCONST_0 DCONST_1 BIPUSH SIPUSH LDC LDC_W LDC2_W ILOAD LLOAD FLOAD
      DLOAD ALOAD ILOAD_0 ILOAD_1 ILOAD_2 ILOAD_3 LLOAD_0 LLOAD_1 LLOAD_2
      LLOAD_3 FLOAD_0 FLOAD_1 FLOAD_2 FLOAD_3 DLOAD_0 DLOAD_1 DLOAD_2
      DLOAD_3 ALOAD_0 ALOAD_1 ALOAD_2 ALOAD_3 IALOAD LALOAD FALOAD DALOAD
      AALOAD BALOAD CALOAD SALOAD ISTORE LSTORE FSTORE DSTORE ASTORE
      ISTORE_0 ISTORE_1 ISTORE_2 ISTORE_3 LSTORE_0 LSTORE_1 LSTORE_2
      LSTORE_3 FSTORE_0 FSTORE_1 FSTORE_2 FSTORE_3 DSTORE_0 DSTORE_1
      DSTORE_2 DSTORE_3 ASTORE_0 ASTORE_1 ASTORE_2 ASTORE_3 IASTORE
      LASTORE FASTORE DASTORE AASTORE BASTORE CASTORE SASTORE POP POP2
      DUP DUP_X1 DUP_X2 DUP2 DUP2_X1 DUP2_X2 SWAP IADD LADD FADD DADD
      ISUB LSUB FSUB DSUB IMUL LMUL FMUL DMUL IDIV LDIV FDIV DDIV IREM
      LREM FREM DREM INEG LNEG FNEG DNEG ISHL LSHL ISHR LSHR IUSHR LUSHR
      IAND LAND IOR LOR IXOR LXOR IINC I2L I2F I2D L2I L2F L2D F2I F2L
      F2D D2I D2L D2F I2B I2C I2S LCMP FCMPL FCMPG DCMPL DCMPG IFEQ IFNE
      IFLT IFGE IFGT IFLE IF_ICMPEQ IF_ICMPNE IF_ICMPLT IF_ICMPGE
      IF_ICMPGT IF_ICMPLE IF_ACMPEQ IF_ACMPNE GOTO JSR RET TABLESWITCH
      LOOKUPSWITCH IRETURN LRETURN FRETURN DRETURN ARETURN RETURN
      GETSTATIC PUTSTATIC GETFIELD PUTFIELD INVOKEVIRTUAL INVOKESPECIAL
      INVOKESTATIC INVOKEINTERFACE NIL NEW NEWARRAY ANEWARRAY
      ARRAYLENGTH ATHROW CHECKCAST INSTANCEOF MONITORENTER MONITOREXIT
      WIDE MULTIANEWARRAY IFNULL IFNONNULL GOTO_W JSR_W BREAKPOINT NIL
      NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL
      NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL
      NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL
      IMPDEP1 NIL))

(defstruct (definition
	    (:conc-name "I.")
	    (:constructor make-definition (output-argument)))
  output-argument
  (uses '()))

(defstruct (instruction (:include definition) (:conc-name "I."))
  method
  index
  (next nil)
  (prev nil)
  
  ;; decoded instruction:
  opcode
  arguments
  expanded-opcode
  expanded-arguments
  variant

  ;; flowgraph
  (predecessors nil)
  (successors nil)

  ;; Buchhaltung fuer Datenflussanalyse:
  (reachablep nil)
  (changedp nil)
  (stack nil)
  (stack-out nil)
  locals
  xblock-leader
  next-xblock-leader
  (xblocks '())
  (has-indirect-predecessors-p nil)
  copies
  (tmp nil)
  (definitions nil)
  (exception-handlers nil)
  (letify nil)
  
  ;; Buchhaltung waehrend Codegenerierung
  input-arguments
  stack-pointer)

(defmethod print-object ((object instruction) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "@~D ~S"
	    (i.index object)
	    (cons (i.opcode object)
		  (i.arguments object)))))

(defun fetch-instructions (method code-buffer)
  (let* ((n (+ (length (buf.bytes code-buffer)) 2)) ;leave space for entry and exit
	 (vector (make-array n :initial-element nil))
	 (prev nil))
    (while (listen-buffer code-buffer)
      (let* ((pos (buf.pos code-buffer))
	     (opcode (fetch-byte code-buffer))
	     (opsymbol (elt *opcodes* opcode)))
	(unless opsymbol
	  (class-format-error "invalid opcode"))
	(let ((i (fetch-instruction method code-buffer opsymbol pos)))
	  (setf (elt vector pos) i)
	  (when prev
	    (setf (i.next prev) i))
	  (setf (i.prev i) prev)
	  (setf prev i))))
    (unless prev (class-format-error "empty method"))
    (for ((i :across vector)
	  (j :from 0))
      (when i
	(assert (= (i.index i) j))))
    vector))

(defun fetch-instruction (method buffer opcode pos)
  (let ((instruction-call
	 (fetch-instruction-call (cm.declaring-class-file method)
				 buffer
				 opcode)))
    (make-instruction
      :method method
      :index pos
      :opcode (car instruction-call)
      :arguments (cdr instruction-call))))

(defun fetch-instruction-call (class-file buffer opcode)
  (case opcode
    (tableswitch			;variable length
      (setf (buf.pos buffer) (* (ceiling (buf.pos buffer) 4) 4)) ;fixme
      (let ((default (canonicalize-int (fetch-int buffer)))
	    (low (canonicalize-int (fetch-int buffer)))
	    (high (canonicalize-int (fetch-int buffer))))
	(list opcode
	      default low high
	      (loop
		  repeat (1+ (- high low))
		  collect (canonicalize-int (fetch-int buffer))))))
    (lookupswitch			;variable length
      (setf (buf.pos buffer) (* (ceiling (buf.pos buffer) 4) 4)) ;fixme
      (let ((default (canonicalize-int (fetch-int buffer)))
	    (npairs (fetch-int buffer)))
	(unless (< npairs (expt 2 31))
	  (class-format-error "lookupswitch restriction"))
	(list opcode
	      default
	      (loop
		  repeat npairs
		  collect (cons (canonicalize-int (fetch-int buffer))
				(canonicalize-int (fetch-int buffer)))))))
    (wide				;prefix
      (let ((pseudo-opcode
	     (case (elt *opcodes* (fetch-byte buffer))
	       (iload 'wide-iload) (fload 'wide-fload) (aload 'wide-aload)
	       (lload 'wide-lload) (dload 'wide-dload) (istore 'wide-istore)
	       (fstore 'wide-fstore) (astore 'wide-astore)
	       (lstore 'wide-lstore) (dstore 'wide-dstore)
	       (ret 'wide-ret) (iinc 'wide-iinc)
	       (t (class-format-error "misplaced wide tag")))))
	(fetch-instruction-call class-file buffer pseudo-opcode)))
    (t					;fixed length
      (cons opcode
	    (mapcar (lambda (type)
		      (ecase type
			(:ubyte (fetch-byte buffer))
			(:byte (canonicalize-n (fetch-byte buffer) 8))
			(:ushort (fetch-short buffer))
			(:short (canonicalize-n (fetch-short buffer) 16))
			(:int (canonicalize-int (fetch-int buffer)))
			(:method
			  (cf.get-constant-method class-file (fetch-short buffer)))
			(:field
			  (cf.get-constant-field class-file (fetch-short buffer)))))
		    (id.argument-types (get opcode 'instruction-definition)))))))

;; XXX falsch benannt.  gemeint ist "oberklasse" -- nicht "oberinterface",
;; klingt aber nach "echter oberklasse", dabei ist CLASS durchaus
;; eingeschlossen.
(defun some-proper-superclass (test class)
  (loop
      for c = class then (cls.superclass c)
      while c
      thereis (funcall test c)))

(defun some-superinterface (test class)
  (or (funcall test class)
      (some (curry #'some-superinterface test) (cls.interfaces class))))

(defstruct (member-reference (:conc-name "REF."))
  member-id
  class-id
  name
  descriptor)

(defstruct (field-reference
	    (:include member-reference)
	    (:conc-name "REF."))
  %type-descriptor)

(defstruct (method-reference
	    (:include member-reference)
	    (:conc-name "REF."))
  interfacep
  %rtype-descriptor
  %argtype-descriptors)

(defun cf.get-constant-method (calling-class-file index)
  (multiple-value-bind (cons tag)
      (cf.get-constant calling-class-file index)
    (destructuring-bind (class-id &rest name-and-type) cons
      (destructuring-bind (name &rest descriptor)
	  (cf.get-constant calling-class-file name-and-type)
	(make-method-reference
	 :member-id index
	 :class-id class-id
	 :name (cf.get-constant calling-class-file name)
	 :descriptor (cf.get-constant calling-class-file descriptor)
	 :interfacep (eql tag +constant_interfacemethodref+))))))

(defun cf.get-constant-field (accessing-class-file index)
  (destructuring-bind (class-id &rest name-and-type)
      (cf.get-constant accessing-class-file index)
    (destructuring-bind (name &rest descriptor)
	(cf.get-constant accessing-class-file name-and-type)
      (make-field-reference
       :member-id index
       :class-id class-id
       :name (cf.get-constant accessing-class-file name)
       :descriptor (cf.get-constant accessing-class-file descriptor)))))


;;;; parsing type descriptors into s-expressions

(defun %parse-field-descriptor (str &key start junk-allowed)
  (block nil
    (let ((buf (if (stringp str) (make-cbuffer str) str)))
      (when start
	(setf (cbuf.pos buf) start))
      (labels ((fetch ()
		 (case (if (listen-cbuffer buf)
			   (fetch-char buf)
			   nil)
		   (#\[ (let ((i (1- (cbuf.pos buf))))
			  (fetch)
			  (subseq (cbuf.string buf) i (cbuf.pos buf))))
		   (#\B :byte)
		   (#\C :char)
		   (#\D :double)
		   (#\F :float)
		   (#\I :int)
		   (#\J :long)
		   (#\S :short)
		   (#\Z :boolean)
		   (#\L (let* ((bytes (cbuf.string buf))
			       (s (position #\; bytes :start (cbuf.pos buf))))
			  (unless s
			    (return nil))
			  (let ((substr (subseq bytes (cbuf.pos buf) s)))
			    (setf (cbuf.pos buf) (1+ s))
			    (when (find #\[ substr)
			      (return nil))
			    substr)))
		   (t
		     (return nil)))))
	(prog1
	    (fetch)
	  (when (and (not junk-allowed) (listen-cbuffer buf))
	    (return nil)))))))

(defun parse-field-descriptor (str &key start junk-allowed)
  (or (%parse-field-descriptor str :start start :junk-allowed junk-allowed)
      (error "invalid field descriptor: ~S/~D" str start)))

(defun parse-method-descriptor (str)
  (let ((buf (make-cbuffer str)))
    (assert (char= (fetch-char buf) #\())
    (with-collector (collect :result args)
      (%for ((c = (peek-cbuffer buf))
	     :until (char= c #\)))
	  (collect (parse-field-descriptor buf :junk-allowed t))
	(fetch-char buf)
	(let ((rtype
	       (case (peek-cbuffer buf)
		 (#\V (fetch-char buf) :void)
		 (t (parse-field-descriptor buf)))))
	  (return (values args rtype)))))))


;;;; descriptor sexpr caching, first for member objects...

(defun cm.type-descriptor (field)
  (or (cm.%type-descriptor field)
      (setf (cm.%type-descriptor field)
	    (parse-field-descriptor (cm.descriptor field)))))

(defun ensure-method-descriptor (method)
  (unless (cm.%rtype-descriptor method)
    (setf (values (cm.%argtype-descriptors method)
		  (cm.%rtype-descriptor method))
	  (parse-method-descriptor (cm.descriptor method)))))

(defun cm.rtype-descriptor (method)
  (ensure-method-descriptor method)
  (cm.%rtype-descriptor method))

(defun cm.argtype-descriptors (method)
  (ensure-method-descriptor method)
  (cm.%argtype-descriptors method))

;;; ... then all oven again for member references

(defun ref.type-descriptor (fieldref)
  (or (ref.%type-descriptor fieldref)
      (setf (ref.%type-descriptor fieldref)
	    (parse-field-descriptor (ref.descriptor fieldref)))))

(defun ensure-methodref-descriptor (methodref)
  (unless (ref.%rtype-descriptor methodref)
    (setf (values (ref.%argtype-descriptors methodref)
		  (ref.%rtype-descriptor methodref))
	  (parse-method-descriptor (ref.descriptor methodref)))))

(defun ref.rtype-descriptor (methodref)
  (ensure-methodref-descriptor methodref)
  (ref.%rtype-descriptor methodref))

(defun ref.argtype-descriptors (methodref)
  (ensure-methodref-descriptor methodref)
  (ref.%argtype-descriptors methodref))


;;;; heap file dumping

(defun heap-file-name (hash)
  (concatenate 'string
    cloak-system:*source-directory*
    (format nil "cache/~{~2,'0X~}.heap" (coerce hash 'list))))

(defvar *dumping-lock* (sb-thread:make-mutex))
(defun dump-class-file (cf)
  (sb-thread:with-mutex (*dumping-lock*)
    (let ((tmp (merge-pathnames "tmp.heap" cloak-system:*source-directory*))
	  (sb-heapdump:*dump-verbose* nil))
      ;; get rid of cached descriptors created during compilation, needed only
      ;; by JNI and reflection at runtime
      (dolist (cm (cf.methods cf))
	(setf (cm.%rtype-descriptor cm) nil)
	(setf (cm.%argtype-descriptors cm) nil))
      (dolist (cm (cf.fields cf))
	(setf (cm.%type-descriptor cm) nil))
      (setf (cf.dumpedp cf) t)
      ;; FIXME: We already nuke all debug info we can find after extracting the
      ;; data we need.  However, some debug info seems to evade us, so let's at
      ;; least suppress dumping it.
      (flet ((customizer (x)
	       (if (typep x '(or sb-c::compiled-debug-fun
			      sb-c::compiled-debug-info))
		   (values nil nil)
		   t)))
	(sb-heapdump:dump-object cf tmp
				 :if-exists :supersede
				 :customizer #'customizer))
      (rename-file tmp (heap-file-name (cf.hash cf))))))
