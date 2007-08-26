(in-package :sb-vm)

(declaim (optimize (speed 3)
                   (safety 0)
                   (sb-c::recognize-self-calls 0)
                   (sb-ext:inhibit-warnings 0)))


(define-vop (fast-negate/signed64 fast-safe-arith-op)
  (:translate %negate)
  (:args (x :scs (signed64-reg)))
  (:results (r :scs (signed64-reg)))
  (:arg-types signed64-num)
  (:result-types signed64-num)
  (:generator 3
    (move-signed64 r x)
    (inst neg (signed64-low r))
    (inst adc (signed64-high r) 0)
    (inst neg (signed64-high r))))

(define-vop (fast-+/signed64=>signed64 fast-safe-arith-op)
  (:translate +)
  (:args (a :scs (signed64-reg) :target result)
         (b :scs (signed64-reg) :to :save))
  (:results (result :scs (signed64-reg) :from (:argument 0)))
  (:arg-types signed64-num signed64-num)
  (:result-types signed64-num)
  (:generator 6
    (move-signed64 result a)
    (inst add (signed64-low result) (signed64-low b))
    (inst adc (signed64-high result) (signed64-high b))))

(define-vop (fast--/signed64=>signed64 fast-safe-arith-op)
  (:translate -)
  (:args (a :scs (signed64-reg) :target result)
         (b :scs (signed64-reg) :to :save))
  (:results (result :scs (signed64-reg) :from (:argument 0)))
  (:arg-types signed64-num signed64-num)
  (:result-types signed64-num)
  (:generator 6
    (move-signed64 result a)
    (inst sub (signed64-low result) (signed64-low b))
    (inst sbb (signed64-high result) (signed64-high b))))

(define-vop (fast-logior/signed64 fast-safe-arith-op)
  (:translate logior)
  (:args (x :scs (signed64-reg) :target r)
         (y :scs (signed64-reg) :to :save))
  (:results (r :scs (signed64-reg)))
  (:arg-types signed64-num signed64-num)
  (:result-types signed64-num)
  (:generator 4
    (move-signed64 r x)
    (inst or (signed64-low r) (signed64-low y))
    (inst or (signed64-high r) (signed64-high y))))

(define-vop (fast-logxor/signed64 fast-safe-arith-op)
  (:translate logxor)
  (:args (x :scs (signed64-reg) :target r)
         (y :scs (signed64-reg) :to :save))
  (:results (r :scs (signed64-reg)))
  (:arg-types signed64-num signed64-num)
  (:result-types signed64-num)
  (:generator 4
    (move-signed64 r x)
    (inst xor (signed64-low r) (signed64-low y))
    (inst xor (signed64-high r) (signed64-high y))))

(define-vop (fast-logand/signed64 fast-safe-arith-op)
  (:translate logand)
  (:args (x :scs (signed64-reg) :target r)
         (y :scs (signed64-reg) :to :save))
  (:results (r :scs (signed64-reg)))
  (:arg-types signed64-num signed64-num)
  (:result-types signed64-num)
  (:generator 4
    (move-signed64 r x)
    (inst and (signed64-low r) (signed64-low y))
    (inst and (signed64-high r) (signed64-high y))))

(define-vop (fast-*/signed64=>signed64 fast-safe-arith-op)
  (:translate *)
  ;; Need to move both arguments to the stack because temporaries eax and edx
  ;; each block one of our three register pairs.
  (:args (aa :scs (signed64-reg))
         (bb :scs (signed64-reg)))
  (:temporary (:sc signed-reg :offset edx-offset :from :eval) edx)
  (:temporary (:sc unsigned-reg :offset eax-offset :from :eval) eax)
  (:temporary (:sc signed64-stack) a)
  (:temporary (:sc signed64-stack) b)
  (:results (r :scs (signed64-reg) :from :result))
  (:arg-types signed64-num signed64-num)
  (:result-types signed64-num)
  (:generator 4
    (move (signed64-high a) (signed64-high aa))
    (move (signed64-low a) (signed64-low aa))
    (move (signed64-high b) (signed64-high bb))
    (move (signed64-low b) (signed64-low bb))
    (move eax (signed64-low a))
    (inst mul eax (signed64-low b))
    (move (signed64-high r) edx)
    (move (signed64-low r) eax)
    (move eax (signed64-low a))
    (inst imul eax (signed64-high b))
    (inst add (signed64-high r) eax)
    (move eax (signed64-high a))
    (inst imul eax (signed64-low b))
    (inst add (signed64-high r) eax)))

;; KLUDGE: This one is insanely long and needs to be moved into an
;; assembly routine.
(define-vop (fast-truncate/signed64=>signed64 fast-safe-arith-op)
  (:translate truncate)
  (:args (a* :scs (signed64-reg))
         (b* :scs (signed64-reg) :target b))
  (:results (q* :scs (signed64-reg))
            (r* :scs (signed64-reg)))
  (:temporary (:sc signed64-reg :from (:argument 1)) b)
  (:temporary (:sc signed64-stack) a)
  (:temporary (:sc signed64-stack) q)
  (:temporary (:sc signed64-stack) r)
  (:temporary (:sc signed-stack) qsign)
  (:temporary (:sc signed-stack) rsign)
  (:temporary (:sc signed-reg
                   :offset edx-offset
                   :from (:argument 1)
                   :to :result)
              edx)
  (:temporary (:sc unsigned-reg
                   :offset eax-offset
                   :from (:argument 1)
                   :to :result)
              eax)
  (:temporary (:sc unsigned-reg
                   :offset ecx-offset
                   :from (:argument 1)
                   :to :result)
              ecx)
  (:arg-types signed64-num signed64-num)
  (:result-types signed64-num signed64-num)
  (:generator 4
    (move (signed64-high a) (signed64-high a*))
    (move (signed64-low a) (signed64-low a*))
    ;; apparently arguments must not be modified, so move b to a temporary b*
    ;; and target b to b*, so that the move can be a no-op
    (move-signed64 b b*)

    (inst mov qsign 0)
    (move ecx (signed64-high a))
    (inst test ecx ecx)
    (inst jmp :s negate-a)
    L4
    (move ecx qsign)
    (move rsign ecx)
    (move ecx (signed64-high b))
    (inst test ecx ecx)
    (inst jmp :s negate-b)
    DOIT
    (move ecx (signed64-high b))
    (inst test ecx ecx)
    (inst jmp :z 64/32=>64)
    (inst cmp (signed64-high b) (signed64-high a))
    (inst jmp :a trivial)
    (inst bsr ecx (signed64-high b))
    (inst sub ecx 31)
    (inst neg ecx)
    (inst jmp :z both-huge)
    ;; Bh_Bl := Bh_Bl << ECX
    (move eax (signed64-low b))
    (inst shld (signed64-high b) eax :cl)
    (inst shl (signed64-low b) :cl)
    ;; EDX_Ah_Al := Ah_Al << ECX
    (inst xor edx edx)
    (move eax (signed64-high a))
    (inst shld edx eax :cl)
    (move eax (signed64-low a))
    (inst shld (signed64-high a) eax :cl)
    (inst shl (signed64-low a) :cl)
    ;; (Ql, Ah) := EDX_Ah / Bh
    (move eax (signed64-high a))
    (inst div eax (signed64-high b))
    (move (signed64-low q) eax)
    (move (signed64-high a) edx)
    ;; EDX_EAX := Ql * Bl
    (move eax (signed64-low q))
    (inst mul eax (signed64-low b))
    (inst cmp edx (signed64-high a))
    (inst jmp :a we-guessed-wrong)
    (inst jmp :ne L2)
    (inst cmp eax (signed64-low a))
    (inst jmp :a we-guessed-wrong)
    L2
    ;; Rh_Rl := (Ah_Al - EDX_EAX) >> ECX
    (inst mov (signed64-high q) 0)
    (inst sub (signed64-low a) eax)
    (inst sbb (signed64-high a) edx)
    (move eax (signed64-low a))
    (move edx (signed64-high a))
    (inst shrd eax edx :cl)
    (move (signed64-low r) eax)
    (move (signed64-high r) edx)
    (inst shr (signed64-high r) :cl)
    (inst jmp sign)

    WE-GUESSED-WRONG
    (inst dec (signed64-low q))
    (inst sub eax (signed64-low b))
    (inst sbb edx (signed64-high b))
    (inst jmp l2)

    NEGATE-A
    (inst not qsign)
    (inst neg (signed64-low a))
    (inst adc (signed64-high a) 0)
    (inst neg (signed64-high a))
    (inst jmp l4)

    NEGATE-B
    (inst not qsign)
    (inst neg (signed64-low b))
    (inst adc (signed64-high b) 0)
    (inst neg (signed64-high b))
    (inst jmp doit)

    64/32=>64
    (inst cmp (signed64-low b) (signed64-high a))
    (inst jmp :a 64/32=>32)
    ;; (Qh, EDX) := Ah / Bl
    (inst mov edx 0)
    (move eax (signed64-high a))
    (inst div eax (signed64-low b))
    (move (signed64-high q) eax)
    ;; (Ql, Rl) := EDX_Al / Bl
    (move eax (signed64-low a))
    (inst div eax (signed64-low b))
    (move (signed64-low q) eax)
    (move (signed64-low r) edx)
    (inst mov (signed64-high r) 0)
    (inst jmp sign)

    64/32=>32
    ;; (Q, R) := Ah_Al / Bl
    (move edx (signed64-high a))
    (move eax (signed64-low a))
    (inst div eax (signed64-low b))
    (move (signed64-low q) eax)
    (inst mov (signed64-high q) 0)
    (move (signed64-low r) edx)
    (inst mov (signed64-high r) 0)
    (inst jmp sign)

    TRIVIAL
    (move edx (signed64-high a))
    (move eax (signed64-low a))
    (move (signed64-high r) edx)
    (move (signed64-low r) eax)
    (inst mov (signed64-high q) 0)
    (inst mov (signed64-low q) 0)
    (inst jmp sign)

    BOTH-HUGE
    (inst cmp (signed64-high a) (signed64-high b))
    (inst jmp :a A-EVEN-BIGGER)
    (inst cmp (signed64-low a) (signed64-low b))
    (inst jmp :ae A-EVEN-BIGGER)
    (inst mov (signed64-low q) 0)
    L1
    (inst mov (signed64-high q) 0)
    (move eax (signed64-low a))
    (move edx (signed64-high a))
    (move (signed64-low r) eax)
    (move (signed64-high r) edx)
    (inst jmp sign)
    A-EVEN-BIGGER
    (inst sub (signed64-low a) (signed64-low b))
    (inst sbb (signed64-high a) (signed64-high b))
    (inst mov (signed64-low q) 1)
    (inst jmp l1)

    SIGN
    (move ecx qsign)
    (inst test ecx ecx)
    (inst jmp :z l3)
    (inst neg (signed64-low q))
    (inst adc (signed64-high q) 0)
    (inst neg (signed64-high q))
    L3
    (move ecx rsign)
    (inst test ecx ecx)
    (inst jmp :z done)
    (inst neg (signed64-low r))
    (inst adc (signed64-high r) 0)
    (inst neg (signed64-high r))
    DONE
    (move (signed64-high q*) (signed64-high q))
    (move (signed64-low q*) (signed64-low q))
    (move (signed64-high r*) (signed64-high r))
    (move (signed64-low r*) (signed64-low r))))

(define-vop (fast-ash/signed64=>signed64 fast-safe-arith-op)
  (:translate ash)
  (:args (number :scs (signed64-reg) :target result)
         (amount :scs (signed-reg) :target ecx))
  (:arg-types signed64-num signed-num)
  (:results (result :scs (signed64-reg) :from (:argument 0)))
  (:result-types signed64-num)
  (:temporary (:sc signed-reg :offset ecx-offset :from (:argument 1)) ecx)
  (:generator 10
    (move-signed64 result number)
    (move ecx amount)
    (inst or ecx ecx)
    (inst jmp :ns left)
    (inst neg ecx)
    (inst cmp ecx 31)
    (inst jmp :be right<=31)
    (inst cmp ecx 63)
    (inst jmp :be right<=63)
    (inst mov ecx 63)
    RIGHT<=63
    (inst sub ecx 32)
    (move (signed64-low result) (signed64-high result))
    (inst sar (signed64-low result) :cl)
    (inst sar (signed64-high result) 31)
    (inst sar (signed64-high result) 1)
    (inst jmp done)

    RIGHT<=31
    (inst shrd (signed64-low result) (signed64-high result) :cl)
    (inst sar (signed64-high result) :cl)
    (inst jmp done)

    LEFT
    (inst cmp ecx 31)
    (inst jmp :be left<=31)
    ;; note that shl only looks at (ecx & 0x1f), but because of our result
    ;; type, ecx <= 63 or result == 0, and in both cases the following works:
    (inst sub ecx 32)
    (move (signed64-high result) (signed64-low result))
    (inst mov (signed64-low result) 0)
    (inst shl (signed64-high result) :cl)
    (inst jmp done)

    LEFT<=31
    (inst shld (signed64-high result) (signed64-low result) :cl)
    (inst shl (signed64-low result) :cl)

    DONE))

(define-vop (fast-if-</signed64 fast-conditional)
  (:args (x :scs (signed64-reg))
         (y :scs (signed64-reg)))
  (:arg-types signed64-num signed64-num)
  (:note "inline (signed-byte 64) comparison")
  (:translate <)
  (:generator 7
    (inst cmp (signed64-high x) (signed64-high y))
    (inst jmp (if not-p :g :l) target)
    (inst jmp (if not-p :l :g) no)
    (inst cmp (signed64-low x) (signed64-low y))
    (inst jmp (if not-p :ae :b) target)
    NO))

(define-vop (fast-if->/signed64 fast-conditional)
  (:args (x :scs (signed64-reg))
         (y :scs (signed64-reg)))
  (:arg-types signed64-num signed64-num)
  (:note "inline (signed-byte 64) comparison")
  (:translate >)
  (:generator 7
    (inst cmp (signed64-high x) (signed64-high y))
    (inst jmp (if not-p :l :g) target)
    (inst jmp (if not-p :g :l) no)
    (inst cmp (signed64-low x) (signed64-low y))
    (inst jmp (if not-p :be :a) target)
    NO))

;;;(define-vop (fast-if-eql/signed64 fast-conditional)
;;;  (:args (x* :scs (signed64-reg) :target x)
;;;      (y :scs (signed64-reg)))
;;;  (:arg-types signed64-num signed64-num)
;;;  (:temporary (:sc signed64-reg :from (:argument 0)) x)
;;;  (:note "inline (signed-byte 64) comparison")
;;;  (:translate eql)
;;;  (:generator 7
;;;    (move-signed64 x x*)
;;;    (inst xor (signed64-low x) (signed64-low y))
;;;    (inst xor (signed64-high x) (signed64-high y))
;;;    (inst or (signed64-high x) (signed64-low x))
;;;    (inst jmp (if not-p :nz :z) target)))

(define-vop (fast-if-eql/signed64 fast-conditional)
  (:args (x :scs (signed64-reg))
         (y :scs (signed64-reg)))
  (:arg-types signed64-num signed64-num)
  (:note "inline (signed-byte 64) comparison")
  (:translate eql)
  (:generator 7
    (inst cmp (signed64-low x) (signed64-low y))
    (inst jmp :ne (if not-p target no))
    (inst cmp (signed64-high x) (signed64-high y))
    (inst jmp (if not-p :ne :e) target)
    NO))

(macrolet ((frob (name translate to-sc to-type)
             `(define-vop (,name)
                (:args (x :scs (signed64-reg)))
                (:results (y :scs (,to-sc)))
                (:arg-types signed64-num)
                (:result-types ,to-type)
                (:policy :fast-safe)
                (:note "inline float coercion")
                (:translate ,translate)
                (:vop-var vop)
                (:save-p :compute-only)
                (:temporary (:sc signed64-stack) tmp)
                (:generator 6
                 (move (signed64-high tmp) (signed64-high x))
                 (move (signed64-low tmp) (signed64-low x))
                 (with-empty-tn@fp-top(y)
                   (note-this-location vop :internal-error)
                   (inst fildl (signed64-low tmp)))))))
  (frob %single-float/signed64 %single-float single-reg single-float)
  (frob %double-float/signed64 %double-float double-reg double-float))

(macrolet ((frob (trans from-sc from-type round-p)
             `(define-vop (,(symbolicate trans "/" from-type "=>SIGNED64"))
               (:args (x :scs (,from-sc) :target fr0))
               (:temporary (:sc double-reg :offset fr0-offset
                            :from :argument :to :result) fr0)
               ,@(unless round-p
                  '((:temporary (:sc unsigned-stack) stack-temp)
                    (:temporary (:sc unsigned-stack) scw)
                    (:temporary (:sc any-reg) rcw)))
               (:temporary (:sc signed64-stack) tmp)
               (:results (y :scs (signed64-reg)))
               (:arg-types ,from-type)
               (:result-types signed64-num)
               (:translate ,trans)
               (:policy :fast-safe)
               (:note "inline float truncate")
               (:vop-var vop)
               (:save-p :compute-only)
               (:generator 5
                ,@(unless round-p
                   '((note-this-location vop :internal-error)
                     ;; Catch any pending FPE exceptions.
                     (inst wait)))
                ;; Normal mode (for now) is "round to best".
                (unless (zerop (tn-offset x))
                  (copy-fp-reg-to-fr0 x))
                ,@(unless round-p
                   '((inst fnstcw scw)  ; save current control word
                     (move rcw scw)     ; into 16-bit register
                     (inst or rcw (ash #b11 10)) ; CHOP
                     (move stack-temp rcw)
                     (inst fldcw stack-temp)))
                (inst fistpl (signed64-low tmp))
                (move (signed64-high y) (signed64-high tmp))
                (move (signed64-low y) (signed64-low tmp))
                (inst fld fr0) ; copy fr0 to at least restore stack.
                ,@(unless round-p
                   '((inst fldcw scw)))))))
  (frob %unary-truncate single-reg single-float nil)
  (frob %unary-truncate double-reg double-float nil)
  (frob %unary-round single-reg single-float t)
  (frob %unary-round double-reg double-float t))
