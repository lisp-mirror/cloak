(in-package :sb-vm)

(declaim (optimize (speed 3)
                   (safety 0)
                   (sb-c::recognize-self-calls 0)
                   (sb-ext:inhibit-warnings 0)))


(define-move-fun (load-stack-64 5) (vop x y)
  ((signed64-stack) (signed64-reg))
  (inst mov (signed64-low y) (signed64-low x))
  (inst mov (signed64-high y) (signed64-high x)))

(define-move-fun (store-stack-64 5) (vop x y)
  ((signed64-reg) (signed64-stack))
  (inst mov (signed64-low y) (signed64-low x))
  (inst mov (signed64-high y) (signed64-high x)))

(define-move-fun (load-wide-number 1) (vop x y)
  ((immediate) (signed64-reg))
  (inst mov (signed64-high y) (ldb (byte 32 32) (tn-value x)))
  (inst mov (signed64-low y) (ldb (byte 32 0) (tn-value x))))

(define-vop (move-to-signed64-c)
  (:args (x :scs (constant)))
  (:results (y :scs (signed64-reg)))
  (:note "constant (signed-byte 64) load")
  (:generator 1
    (inst mov (signed64-high y) (ldb (byte 32 32) (tn-value x)))
    (inst mov (signed64-low y) (ldb (byte 32 0) (tn-value x)))))
(define-move-vop move-to-signed64-c :move
  (constant) (signed64-reg))

(define-vop (move-to-signed64/fixnum)
  (:args (x :scs (any-reg descriptor-reg)))
  (:results (y :scs (signed64-reg)))
  (:arg-types tagged-num)
  (:note "fixnum to register pair untagging")
  (:generator 1
    (move (signed64-low y) x)
    (inst sar (signed64-low y) 2)
    (move (signed64-high y) (signed64-low y))
    (inst sar (signed64-high y) 30)))
(define-move-vop move-to-signed64/fixnum :move
  (any-reg descriptor-reg) (signed64-reg))

(define-vop (move-to-signed64/integer)
  (:args (x :scs (descriptor-reg)))
  (:results (y :scs (signed64-reg)))
  (:note "integer to untagged register pair coercion")
  (:generator 4
    ;; Expect that ecx,edx,esi,edi may be wired.  Move X into a temporary saved
    ;; manually, leaving eax+ebx free for Y even if all others are wired.
    (let ((tmp
           (make-signed64-half
            (cond
              ((>= (tn-offset y) 8) eax-offset)
              ((eql (tn-offset y) eax+ecx-offset) edx-offset)
              (t ecx-offset))))
          (one-word-bignum (gen-label))
          (fixnum (gen-label))
          (done (gen-label)))
      (inst push tmp)
      (move tmp x)

      (inst test tmp 3)
      (inst jmp :z fixnum)

      (loadw (signed64-low y)
             tmp
             bignum-digits-offset
             other-pointer-lowtag)

      ;; high used as a temporary here:
      (loadw (signed64-high y) tmp 0 other-pointer-lowtag)
      (inst cmp
            (signed64-high y)
            (logior (ash (1- (+ bignum-digits-offset 1))
                         n-widetag-bits)
                    bignum-widetag))
      (inst jmp :eq one-word-bignum)

      (loadw (signed64-high y)
             tmp
             (1+ bignum-digits-offset)
             other-pointer-lowtag)
      (inst jmp done)

      (emit-label ONE-WORD-BIGNUM)
      (inst mov (signed64-high y) (signed64-low y))
      (inst sar (signed64-high y) 31)
      (inst sar (signed64-high y) 1)
      (inst jmp done)

      (emit-label FIXNUM)
      (inst sar tmp 2)
      (move (signed64-low y) tmp)
      (inst mov (signed64-high y) tmp)
      (inst sar (signed64-high y) 31)
      (inst sar (signed64-high y) 1)

      (emit-label DONE)
      (inst pop tmp))))
(define-move-vop move-to-signed64/integer :move
  (descriptor-reg) (signed64-reg))

(define-vop (move-from-signed64/fixnum)
  (:args (x :scs (signed64-reg)))
  (:results (y :scs (any-reg descriptor-reg)))
  (:result-types tagged-num)
  (:note "register pair to fixnum tagging")
  (:generator 1
    (move y (signed64-low x))
    (inst shl y 2)))
(define-move-vop move-from-signed64/fixnum :move
  (signed64-reg) (any-reg descriptor-reg))

;; KLUDGE: This VOP is too long and needs to be moved into an
;; assembly routine.
(define-vop (move-from-signed64)
  ;; Move input argument to the stack to save registers, otherwise we run
  ;; out of register pairs in :MOVE-ARG.
  (:args (xx :scs (signed64-reg)))
  (:temporary (:sc unsigned-reg :from :eval) alloc)
  (:temporary (:sc signed64-stack) x)
  (:results (y :scs (any-reg descriptor-reg)))
  (:node-var node)
  (:note "signed register pair to integer coercion")
  (:generator 30
    (move (signed64-high x) (signed64-high xx))
    (move (signed64-low x) (signed64-low xx))
    (let ((done (gen-label))
          (unsigned32 (gen-label))
          (one-word-bignum (gen-label))
          (two-word-bignum (gen-label))
          (L1 (gen-label)))
      ;; high word all zeros?
      (move y (signed64-high x))
      (inst test y y)
      (inst jmp :eq unsigned32)
      ;; no

      ;; all ones?
      (move y (signed64-high x))
      (inst sar y 1)
      (inst cmp y (signed64-high x))
      (inst jmp :ne two-word-bignum)
      ;; yes

      (inst mov y (signed64-low x))
      (inst test y #x80000000)
      (inst jmp :z two-word-bignum)     ;0...
      (inst shl y 1)
      (inst jmp :o one-word-bignum)     ;10...
      (inst shl y 1)
      (inst jmp :o one-word-bignum)     ;110...
      ;; 111...
      ;; return the (signed) fixnum
      (inst jmp done)

      (emit-label unsigned32)
      (inst mov y (signed64-low x))
      (inst test y #x80000000)
      (inst jmp :nz two-word-bignum)    ;1...
      (inst shl y 1)
      (inst jmp :o one-word-bignum)     ;01...
      (inst shl y 1)
      (inst jmp :o one-word-bignum)     ;001...
      ;; 000...
      ;; return the (unsigned) fixnum
      (emit-label done)

      (assemble (*elsewhere*)
         (emit-label two-word-bignum)
         (inst mov y (logior (ash (1- (+ bignum-digits-offset 2))
                                  n-widetag-bits)
                             bignum-widetag))
         (inst jmp L1)
         (emit-label one-word-bignum)
         (inst mov y (logior (ash (1- (+ bignum-digits-offset 1))
                                  n-widetag-bits)
                             bignum-widetag))
         (emit-label L1)
         (pseudo-atomic
          (allocation alloc (pad-data-block (+ bignum-digits-offset 2)) node)
          (storew y alloc)
          (inst lea y (make-ea :byte :base alloc :disp other-pointer-lowtag))
          (move alloc (signed64-low x))
          (storew alloc
                  y
                  bignum-digits-offset
                  other-pointer-lowtag)
          (move alloc (signed64-high x))
          (storew alloc
                  y
                  (1+ bignum-digits-offset)
                  other-pointer-lowtag))
         (inst jmp done)))))
(define-move-vop move-from-signed64 :move
  (signed64-reg) (descriptor-reg))

(define-vop (word64-move)
  (:args (x :scs (signed64-reg) :load-if (not (location= x y))))
  (:results (y :scs (signed64-reg) :load-if (not (location= x y))))
  (:effects)
  (:affected)
  (:note "signed64 move")
  (:generator 0
    (move-signed64 y x)))
(define-move-vop word64-move :move
  (signed64-reg) (signed64-reg))

(define-vop (signed64-to-signed)
  (:args (x :scs (signed64-reg)))
  (:results (y :scs (signed-reg)))
  (:effects)
  (:affected)
  (:note "cast from double word to signed word")
  (:generator 0
    (move y (signed64-low x))))
(define-move-vop signed64-to-signed :move
  (signed64-reg) (signed-reg))

(define-vop (signed64-to-unsigned)
  (:args (x :scs (signed64-reg)))
  (:results (y :scs (unsigned-reg)))
  (:effects)
  (:affected)
  (:note "cast from double word to unsigned word")
  (:generator 0
    (move y (signed64-low x))))
(define-move-vop signed64-to-unsigned :move
  (signed64-reg) (unsigned-reg))

(define-vop (unsigned-to-signed64)
  (:args (x :scs (unsigned-reg)))
  (:results (y :scs (signed64-reg)))
  (:arg-types unsigned-num)
  (:effects)
  (:affected)
  (:note "cast from unsigned word to double word")
  (:generator 0
    (move (signed64-low y) x)
    (inst xor (signed64-high y) (signed64-high y))))
(define-move-vop unsigned-to-signed64 :move
  (unsigned-reg) (signed64-reg))

(define-vop (signed-to-signed64)
  (:args (x :scs (signed-reg)))
  (:results (y :scs (signed64-reg)))
  (:arg-types signed-num)
  (:effects)
  (:affected)
  (:note "cast from signed word to double word")
  (:generator 0
    (move (signed64-low y) x)
    (move (signed64-high y) x)
    (inst sar (signed64-high y) 1)
    (inst sar (signed64-high y) 31)))
(define-move-vop signed-to-signed64 :move
  (signed-reg) (signed64-reg))

(define-vop (move-signed64-arg)
  (:args (x :scs (signed64-reg) :target y)
         (fp :scs (any-reg) :load-if (not (sc-is y sap-reg))))
  (:results (y))
  (:note "signed64 argument move")
  (:generator 0
    (sc-case y
      ((signed64-reg)
       (move-signed64 y x))
      ((signed64-stack)
       (aver (/= (tn-offset fp) esp-offset)) ;c-call
       (storew (signed64-high x) fp (- (+ 1 (tn-offset y))))
       (storew (signed64-low x)  fp (- (+ 2 (tn-offset y))))))))

(define-move-vop move-signed64-arg :move-arg
  (descriptor-reg any-reg signed-reg unsigned-reg signed64-reg)
  (signed64-reg))

(define-move-vop move-word-arg :move-arg
  (signed64-reg)
  (signed-reg unsigned-reg))

(define-move-vop move-arg :move-arg
  (signed64-reg) (any-reg descriptor-reg))
