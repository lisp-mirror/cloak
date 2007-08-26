;; (Use random-int-forms.lsp with double *maximum-random-int-bits*
;; to *really* test this contrib.  Here just some very basic tests.)

(defpackage :sb-regpair-test
  (:use :cl :sb-rt))

(in-package :sb-regpair-test)

(rem-all-tests)

(declaim (optimize (speed 3) (safety 0) (debug 0) (sb-ext:inhibit-warnings 1)))

(defun signed64-< (a b)
  (declare (type (signed-byte 64) a b))
  (< a b))

(defun signed64-<= (a b)
  (declare (type (signed-byte 64) a b))
  (<= a b))

(defun signed64-> (a b)
  (declare (type (signed-byte 64) a b))
  (> a b))

(defun signed64->= (a b)
  (declare (type (signed-byte 64) a b))
  (> a b))

(defun signed64-eql (a b)
  (declare (type (signed-byte 64) a b))
  (eql a b))

(defun signed64-not-eql (a b)
  (declare (type (signed-byte 64) a b))
  (not (eql a b)))

(defun signed64-truncate (a b)
  (declare (type (signed-byte 64) a b))
  (multiple-value-bind (c d)
      (truncate a b)
    (logxor c d)))

(defvar *n-test-iterations* 10000)

(declaim (optimize (sb-ext:inhibit-warnings 3)))

;;;(defmacro deftest (name form result)
;;;  (declare (ignore name))
;;;  `(assert (eql ,form ,result)))


(deftest basic.1
    (loop
        for f in '(signed64-< signed64-> signed64-eql
                   signed64-<= signed64->= signed64-not-eql)
        for g in '(< > eql <= >= /=)
        do
          (format t "~&;; comparing ~A with ~A" f g)
          (dolist (k '(64 48 32 16))
            (dolist (l '(64 48 32 16))
              (dotimes (x *n-test-iterations*)
                (let* ((a (- (random (expt 2 k)) (expt 2 (1- k))))
                       (b (- (random (expt 2 l)) (expt 2 (1- l))))
                       (actual (funcall f a b))
                       (expected (funcall g a b)))
                  (unless (eq actual expected)
                    (error "~A => ~A, wanted ~A"
                           (list f a b) actual expected))))))
          (format t " ok~&"))
  nil)


;; the following three failed because truncation modified an argument TN:

(DEFTEST MISC.1
    (LET* ((FN1
            '(LAMBDA (C)
              (DECLARE
               (TYPE (INTEGER -2509539973275357 307138139370397) C))
              (DECLARE
               (OPTIMIZE (SPACE 2)
                (SAFETY 1)
                (SB-C:INSERT-STEP-CONDITIONS 0)
                (SPEED 3)
                (COMPILATION-SPEED 0)
                (DEBUG 2)))
              (MOD 536870911 c)))
           (FN2
            '(LAMBDA (C)
              (DECLARE (NOTINLINE MIN MOD))
              (DECLARE
               (OPTIMIZE (COMPILATION-SPEED 3)
                (DEBUG 0)
                (SPACE 2)
                (SAFETY 1)
                (SB-C:INSERT-STEP-CONDITIONS 0)
                (SPEED 3)))
              (MOD 536870911 c)))
           (VALS
            '(-139242990082387))
           (V1 (APPLY (COMPILE NIL FN1) VALS))
           (V2 (APPLY (COMPILE NIL FN2) VALS)))
          (IF (EQL V1 V2) :GOOD (LIST V1 V2)))
  :GOOD)

(DEFTEST MISC.2
         (LET* ((FN1
                 '(LAMBDA (A B C D)
                    (DECLARE (TYPE (INTEGER -12753328152637 7095426891440) A))
                    (DECLARE (TYPE (INTEGER -9360 80136811250778) B))
                    (DECLARE (TYPE (INTEGER -4496778193 9477197336451) C))
                    (DECLARE (TYPE (INTEGER 290 451008085731007426) D))
                    (DECLARE (IGNORABLE A B C D))
                    (DECLARE
                     (OPTIMIZE (DEBUG 1)
                               (SPACE 2)
                               (COMPILATION-SPEED 1)
                               (SPEED 3)
                               (SAFETY 2)
                               (SB-C:INSERT-STEP-CONDITIONS 0)))
                   (REM (- C B C) -10)))
                (FN2
                 '(LAMBDA (A B C D)
                    (DECLARE (NOTINLINE MIN - REM))
                    (DECLARE
                     (OPTIMIZE (SPACE 2)
                               (DEBUG 2)
                               (SB-C:INSERT-STEP-CONDITIONS 0)
                               (COMPILATION-SPEED 3)
                               (SAFETY 2)
                               (SPEED 2)))
                   (REM (- C B C) -10)))
                (VALS
                 '(2564632883841 76722715549658 3297591191825
                   117574508097306983))
                (V1 (APPLY (COMPILE NIL FN1) VALS))
                (V2 (APPLY (COMPILE NIL FN2) VALS)))
           (IF (EQL V1 V2) :GOOD (LIST V1 V2)))
         :GOOD)

(DEFTEST MISC.3
         (LET* ((FN1
                 '(LAMBDA (A B C D)
                    (DECLARE (TYPE (INTEGER 14 20393469362751) A))
                    (DECLARE (TYPE (INTEGER -6752 7409126) B))
                    (DECLARE
                     (TYPE (INTEGER -4377487038334041 -621584236252022) C))
                    (DECLARE (TYPE (INTEGER -314725 479690) D))
                    (DECLARE (IGNORABLE A B C D))
                    (DECLARE
                     (OPTIMIZE (SAFETY 3)
                               (COMPILATION-SPEED 1)
                               (SB-C:INSERT-STEP-CONDITIONS 0)
                               (DEBUG 2)
                               (SPACE 0)
                               (SPEED 3)))
                    (REM
                     (IF (LDB-TEST (BYTE 15 0) C)
                         (LET* ((*S4* (MAKE-ARRAY NIL :INITIAL-ELEMENT 0)))
                           A)
                         B)
                     (MIN -67 0))))
                (FN2
                 '(LAMBDA (A B C D)
                    (DECLARE (NOTINLINE MIN MAKE-ARRAY BYTE LDB-TEST REM))
                    (DECLARE
                     (OPTIMIZE (SPEED 1)
                               (SPACE 1)
                               (DEBUG 1)
                               (SAFETY 0)
                               (COMPILATION-SPEED 3)
                               (SB-C:INSERT-STEP-CONDITIONS 0)))
                    (REM
                     (IF (LDB-TEST (BYTE 15 0) C)
                         (LET* ((*S4* (MAKE-ARRAY NIL :INITIAL-ELEMENT 0)))
                           A)
                         B)
                     (MIN -67 0))))
                (VALS '(6000002766545 6210485 -683005776915248 180043))
                (V1 (APPLY (COMPILE NIL FN1) VALS))
                (V2 (APPLY (COMPILE NIL FN2) VALS)))
           (IF (EQL V1 V2) :GOOD (LIST V1 V2)))
         :GOOD)
