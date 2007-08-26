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

(in-package :sb-vm)

(declaim (optimize (speed 3)
		   (safety 0)
		   (sb-c::recognize-self-calls 0)
		   (sb-ext:inhibit-warnings 0)))

(define-vop (cloak::vop-ub32-to-sb32 fast-safe-arith-op)
  (:translate cloak::ub32-to-sb32)
  (:args (x :scs (unsigned-reg) :target r))
  (:arg-types unsigned-num)
  (:results (r :scs (signed-reg) :from (:argument 0)))
  (:result-types signed-num)
  (:generator 0
    (move r x)))

(define-vop (cloak::vop-sb64-to-sb32 signed64-to-signed)
  (:translate cloak::sb64-to-sb32)
  (:args (x :scs (signed64-reg)))
  (:results (y :scs (signed-reg)))
  (:arg-types signed64-num)
  (:result-types signed-num))

(define-vop (cloak::fast-+-mod32/signed=>signed fast-+/signed=>signed)
  (:translate cloak::int-+))

(define-vop (cloak::fast-+-c-mod32/signed=>signed fast-+-c/signed=>signed)
  (:translate cloak::int-+))

(define-vop (cloak::fast-*-mod32/signed=>signed fast-*/signed=>signed)
  (:translate cloak::int-*))

(define-vop (cloak::fast-*-c-mod32/signed=>signed fast-*-c/signed=>signed)
  (:translate cloak::int-*))

(define-vop (cloak::fast-negate-mod32/signed=>signed fast-negate/signed)
  (:translate cloak::0-int))

(define-vop (cloak::fast-ash-mod32/signed=>signed fast-ash/signed=>signed)
  (:translate cloak::int-ash))

(define-vop (cloak::long-+ fast-+/signed64=>signed64)
  (:translate cloak::long-+))

(define-vop (cloak::long-- fast--/signed64=>signed64)
  (:translate cloak::long--))

(define-vop (cloak::long-* fast-*/signed64=>signed64)
  (:translate cloak::long-*))

(define-vop (cloak::long-truncate fast-truncate/signed64=>signed64)
  (:translate cloak::truncate64))

(define-vop (cloak::long-negate fast-negate/signed64)
  (:translate cloak::negate64))

(define-vop (cloak::lor fast-logior/signed64)
  (:translate cloak::lor))

(define-vop (cloak::lxor fast-logxor/signed64)
  (:translate cloak::lxor))

(define-vop (cloak::land fast-logand/signed64)
  (:translate cloak::land))

(define-vop (cloak::long-ash fast-ash/signed64=>signed64)
  (:translate cloak::long-ash))

#+(or)
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

(define-vop (cloak::lushr fast-safe-arith-op)
  (:translate cloak::lushr)
  (:args (number :scs (signed64-reg) :target result)
	 (amount :scs (signed-reg) :target ecx))
  (:arg-types signed64-num signed-num)
  (:results (result :scs (signed64-reg) :from (:argument 0)))
  (:result-types signed-byte-64)
  (:temporary (:sc signed-reg :offset ecx-offset :from (:argument 1)) ecx)
  (:generator 5
    (move-signed64 result number)
    (move ecx amount)
    (inst and ecx 63)
    (inst cmp ecx 31)
    (inst jmp :be right<=31)
    (inst sub ecx 32)
    (move (signed64-low result) (signed64-high result))
    (inst shr (signed64-low result) :cl)
    (inst xor (signed64-high result) (signed64-high result))
    (inst jmp done)

    RIGHT<=31
    (inst shrd (signed64-low result) (signed64-high result) :cl)
    (inst shr (signed64-high result) :cl)

    DONE))

(define-vop (cloak::l2i fast-safe-arith-op)
  (:translate cloak::l2i)
  (:args (x :scs (signed64-reg) :target y))
  (:results (y :scs (signed-reg)))
  (:arg-types signed64-num)
  (:result-types signed-byte-32)
  (:generator 0
    (move y (signed64-low x))))

(define-vop (cloak::raw-instance-ref/signed)
  (:translate cloak::%raw-instance-ref/signed)
  (:policy :fast-safe)
  (:args (object :scs (descriptor-reg)) (index :scs (any-reg)))
  (:arg-types * tagged-num)
  (:temporary (:sc unsigned-reg) tmp)
  (:results (value :scs (signed-reg)))
  (:result-types signed-num)
  (:generator 5
    (loadw tmp object 0 instance-pointer-lowtag)
    (inst shr tmp n-widetag-bits)
    (inst shl tmp 2)
    (inst sub tmp index)
    (inst mov
	  value
	  (make-ea :dword
		   :base object
		   :index tmp
		   :disp (- (* (1- instance-slots-offset) n-word-bytes)
			    instance-pointer-lowtag)))))

(define-vop (cloak::raw-instance-set/signed)
  (:translate cloak::%raw-instance-set/signed)
  (:policy :fast-safe)
  (:args (object :scs (descriptor-reg))
	 (index :scs (any-reg))
	 (value :scs (signed-reg) :target result))
  (:arg-types * tagged-num signed-num)
  (:temporary (:sc unsigned-reg) tmp)
  (:results (result :scs (signed-reg)))
  (:result-types signed-num)
  (:generator 5
    (loadw tmp object 0 instance-pointer-lowtag)
    (inst shr tmp n-widetag-bits)
    (inst shl tmp 2)
    (inst sub tmp index)
    (inst mov
	  (make-ea :dword
		   :base object
		   :index tmp
		   :disp (- (* (1- instance-slots-offset) n-word-bytes)
			    instance-pointer-lowtag))
	  value)
    (move result value)))

(define-vop (cloak::raw-instance-ref/signed64)
  (:translate cloak::%raw-instance-ref/signed64)
  (:policy :fast-safe)
  (:args (object :scs (descriptor-reg) :to :save)
	 (index :scs (any-reg) :to :save))
  (:arg-types * tagged-num)
  (:temporary (:sc unsigned-reg) tmp)
  (:results (value :scs (signed64-reg) :from :load))
  (:result-types signed-byte-64)
  (:generator 5
    (loadw tmp object 0 instance-pointer-lowtag)
    (inst shr tmp n-widetag-bits)
    (inst shl tmp 2)
    (inst sub tmp index)
    (inst mov
	  (signed64-high value)
	  (make-ea :dword
		   :base object
		   :index tmp
		   :disp (- (* (1- instance-slots-offset) n-word-bytes)
			    instance-pointer-lowtag)))
    (inst mov
	  (signed64-low value)
	  (make-ea :dword
		   :base object
		   :index tmp
		   :disp (- (* (- instance-slots-offset 2) n-word-bytes)
			    instance-pointer-lowtag)))))

(define-vop (cloak::raw-instance-set/signed64)
  (:translate cloak::%raw-instance-set/signed64)
  (:policy :fast-safe)
  (:args (object :scs (descriptor-reg))
	 (index :scs (any-reg))
	 (value :scs (signed64-reg) :target result))
  (:arg-types * tagged-num signed-byte-64)
  (:temporary (:sc unsigned-reg) tmp)
  (:results (result :scs (signed64-reg)))
  (:result-types signed-byte-64)
  (:generator 5
    (loadw tmp object 0 instance-pointer-lowtag)
    (inst shr tmp n-widetag-bits)
    (inst shl tmp 2)
    (inst sub tmp index)
    (inst mov
	  (make-ea :dword
		   :base object
		   :index tmp
		   :disp (- (* (- instance-slots-offset 2) n-word-bytes)
			    instance-pointer-lowtag))
	  (signed64-low value))
    (inst mov
	  (make-ea :dword
		   :base object
		   :index tmp
		   :disp (- (* (1- instance-slots-offset) n-word-bytes)
			    instance-pointer-lowtag))
	  (signed64-high value))
    (move-signed64 result value)))

;; fixme: as Nikodemus pointed out, this VOP isn't needed.
;; The normal version from SBCL should do fine.
(define-vop (cloak::%cmpxchg)
  (:translate cloak::%cmpxchg)
  (:args (object :scs (descriptor-reg) :to :eval)
	 (slot :scs (any-reg) :to :result)
	 (old-value :scs (descriptor-reg any-reg) :target eax)
	 (new-value :scs (descriptor-reg any-reg)))
  (:arg-types instance positive-fixnum * *)
  (:temporary (:sc descriptor-reg :offset eax-offset
		   :from (:argument 2) :to :result :target result)  eax)
  (:results (result :scs (descriptor-reg any-reg)))
  ;(:guard (backend-featurep :i486))
  (:policy :fast-safe)
  (:generator 5
    (move eax old-value)
    (inst lock)
    (inst cmpxchg (make-ea :dword :base object :index slot :scale 1
			   :disp (- (* instance-slots-offset n-word-bytes)
				    instance-pointer-lowtag))
	  new-value)
    (inst jmp :z eins)
    (inst mov result (fixnumize 0))
    (inst jmp ende)
    eins
    (inst mov result (fixnumize 1))
    ende))

(define-vop (cloak::vector-sap)
  (:translate cloak::vector-sap)
  (:policy :fast-safe)
  (:args (vector :scs (descriptor-reg) :target sap))
  (:results (sap :scs (sap-reg)))
  (:result-types system-area-pointer)
  (:generator 2
    (move sap vector)
    (inst add
	  sap
	  (- (* vector-data-offset n-word-bytes) other-pointer-lowtag))))

;; In truncation instructions, Java wants 0 for NaN, and -2^32 or 2^32-1
;; on overflow.  Make it so.
(macrolet
    ((doit (name type sc)
       `(define-vop (,name)
	  (:args (x :scs (,sc)))
	  (:temporary (:sc signed-stack) scw2)
	  (:temporary (:sc unsigned-stack) scw)
	  (:temporary (:sc unsigned-reg :offset eax-offset) eax)
	  (:results (y :scs (signed-reg)))
	  (:arg-types ,type)
	  (:result-types signed-num)
	  (:translate ,name)
	  (:policy :fast-safe)
	  (:save-p :compute-only)
	  (:generator 20
	    (let ((oops (gen-label))
		  (done (gen-label))
		  (nan (gen-label)))
	      (inst wait)
	      (pseudo-atomic
	       (with-tn@fp-top (x)
		 (inst fnstcw scw)
		 (move eax scw)
		 (inst or eax (ash #b11 10)) ;truncate towards zero
		 (move scw2 eax)
		 (inst fldcw scw2)
		 (inst fist scw2)
		 (inst fldcw scw)
		 (inst mov y scw2)
		 (inst cmp y #x80000000)
		 (inst jmp :z oops)
		 (emit-label done)
		 
		 (assemble (*elsewhere*)
		   (emit-label oops)
		   (inst ftst)		;detect nan and sign
		   (inst fnstsw)	;to eax
		   (inst sahf)
		   (inst jmp :p nan)
		   (inst adc y -1)
		   (inst jmp done)

		   (emit-label nan)
		   (inst xor y y)
		   (inst jmp done)))))))))
  (doit cloak::f2i single-float single-reg)
  (doit cloak::d2i double-float double-reg))

(macrolet
    ((doit (name type sc)
       `(define-vop (,name)
	  (:args (x :scs (,sc) :target fr0))
	  (:temporary (:sc double-reg :offset fr0-offset) fr0)
	  (:temporary (:sc signed-stack) scw2)
	  (:temporary (:sc signed64-stack) tmp)
	  (:temporary (:sc unsigned-stack) scw)
	  (:temporary (:sc unsigned-reg :offset eax-offset) eax)
	  (:results (y :scs (signed64-reg)))
	  (:arg-types ,type)
	  (:result-types signed64-num)
	  (:translate ,name)
	  (:policy :fast-safe)
	  (:save-p :compute-only)
	  (:generator 20
	    (let ((oops (gen-label))
		  (done (gen-label))
		  (nan (gen-label)))
	      (inst wait)
	      (pseudo-atomic
	       ;; there's no fistl instruction, so we can't do the ftst
	       ;; after the fact.  and with-tn@fp-top doesn't work either
	       ;; for that matter...
	       (unless (zerop (tn-offset x))
		 (copy-fp-reg-to-fr0 x))
	       (inst ftst)
	       (inst fnstcw scw)
	       (move eax scw)
	       (inst or eax (ash #b11 10)) ;truncate towards zero
	       (move scw2 eax)
	       (inst fldcw scw2)
	       (inst fistpl (signed64-low tmp))
	       (inst fldcw scw)
	       (move (signed64-high y) (signed64-high tmp))
	       (move (signed64-low y) (signed64-low tmp))
	       (inst cmp (signed64-high y) #x80000000)
	       (inst jmp :ne done)
	       (inst cmp (signed64-low y) 0)
	       (inst jmp :e oops)
	       (emit-label done)
	       (inst fld fr0)		;copy fr0 to at least restore stack.

	       (assemble (*elsewhere*)
	         (emit-label oops)
		 (inst fnstsw)		;to eax
		 (inst sahf)
		 (inst jmp :p nan)
		 (inst adc (signed64-low y) -1)
		 (inst adc (signed64-high y) -1)
		 (inst jmp done)
		
		 (emit-label nan)
		 (inst xor (signed64-low y) (signed64-low y))
		 (inst xor (signed64-high y) (signed64-high y))
		 (inst jmp done))))))))
  (doit cloak::f2l single-float single-reg)
  (doit cloak::d2l double-float double-reg))
