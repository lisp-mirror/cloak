(in-package :sb-vm)

(declaim (optimize (speed 3)
                   (safety 0)
                   (sb-c::recognize-self-calls 0)
                   (sb-ext:inhibit-warnings 0)))


;;; storage classes SIGNED64-REG and SIGNED64-STACK

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *qword-register-names* (make-array 56 :initial-element nil)))

(macrolet ((defreg (name offset size)
             (let ((offset-sym (symbolicate name "-OFFSET"))
                   (names-vector (symbolicate "*" size "-REGISTER-NAMES*")))
               `(progn
                  (eval-when (:compile-toplevel :load-toplevel :execute)
                    ;; EVAL-WHEN is necessary because stuff like #.EAX-OFFSET
                    ;; (in the same file) depends on compile-time evaluation
                    ;; of the DEFCONSTANT. -- AL 20010224
                    (def!constant ,offset-sym ,offset))
                  (setf (svref ,names-vector ,offset-sym)
                        ,(symbol-name name)))))
           ;; FIXME: It looks to me as though DEFREGSET should also
           ;; define the related *FOO-REGISTER-NAMES* variable.
           (defregset (name &rest regs)
             `(eval-when (:compile-toplevel :load-toplevel :execute)
                (defparameter ,name
                  (list ,@(mapcar (lambda (name)
                                    (symbolicate name "-OFFSET"))
                                  regs))))))
  (defreg eax+ecx #o01 :qword)
  (defreg eax+edx #o02 :qword)
  (defreg eax+ebx #o03 :qword)
  (defreg eax+esi #o06 :qword)
  (defreg eax+edi #o07 :qword)

  (defreg ecx+edx #o12 :qword)
  (defreg ecx+ebx #o13 :qword)
  (defreg ecx+esi #o16 :qword)
  (defreg ecx+edi #o17 :qword)

  (defreg edx+ebx #o23 :qword)
  (defreg edx+esi #o26 :qword)
  (defreg edx+edi #o27 :qword)

  (defreg ebx+esi #o36 :qword)
  (defreg ebx+edi #o37 :qword)

  (defreg esi+edi #o67 :qword)

  (defregset *qword-regs*
      ;;    0       1       2       3       4       5       6       7
      #||#       eax+ecx eax+edx eax+ebx                 eax+esi eax+edi ;0
      #||#               ecx+edx ecx+ebx                 ecx+esi ecx+edi ;1
      #||#                       edx+ebx                 edx+esi edx+edi ;2
      #||#                                               ebx+esi ebx+edi ;3
      #||#                                                               ;4
      #||#                                                               ;5
      #||#                                                       esi+edi ;6
      )

  ;; alternative definition (non-overlapping pairs):
  #+(or)
  (defregset *qword-regs* eax+ecx edx+ebx esi+edi))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (define-storage-base register-pairs :finite :size 64))

(defmacro define-late-storage-class (sc-name &rest def)
  (let* ((old (gethash sc-name *backend-sc-names*))
         (index
          (if old
              (sc-number old)
              (1+ (position-if #'identity
                               sb-c:*backend-sc-numbers*
                               :from-end t))))
         (constant-name (symbolicate sc-name "-SC-NUMBER")))
    `(eval-when (:compile-toplevel :load-toplevel :execute)
       (define-storage-class ,sc-name ,index ,@def)
       (defconstant ,constant-name ,index))))

(define-late-storage-class signed64-stack stack
  :element-size 2)

(define-late-storage-class signed64-reg register-pairs
  :locations #.*qword-regs*
  :element-size 1
  :constant-scs (immediate)
  :save-p t
  :alternate-scs (signed64-stack))


;;; SIGNED64 registers are not actually machine registers, so we need to
;;; take them apart.

(defun make-signed64-half (offset &optional stackp)
  (make-random-tn :kind :normal
                  :sc (sc-or-lose (if stackp 'signed-stack 'signed-reg))
                  :offset offset))

(defun signed64-high (x)
  (ecase (sc-name (tn-sc x))
    (signed64-reg   (make-signed64-half (* 2 (logand (tn-offset x) 7))))
    (signed64-stack (make-signed64-half (tn-offset x) t))))

(defun signed64-low (x)
  (ecase (sc-name (tn-sc x))
    (signed64-reg   (make-signed64-half (* 2 (ash (tn-offset x) -3))))
    (signed64-stack (make-signed64-half (1+ (tn-offset x)) t))))

(defmacro move-signed64 (y x)
  `(let ((x ,x)
         (y ,y))
     (let ((yh (signed64-high y))
           (yl (signed64-low y))
           (xh (signed64-high x))
           (xl (signed64-low x)))
       (cond
         ((and (sc-is y signed64-reg)
               (sc-is x signed64-reg)
               (eql (tn-offset yh) (tn-offset xl)))
           (move yl xl)
           (move yh xh))
         (t
           (move yh xh)
           (move yl xl))))))
