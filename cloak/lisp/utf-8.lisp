;;;; from CMUCL UNICODE-BRANCH, probably written by Brian Spilsbury

;;; -*- Log: code.log; Package: Lisp -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;; **********************************************************************
;;;
;;; Loader for Spice Lisp.
;;; Written by Skef Wholey and Rob MacLachlan.
;;;

(in-package :cloak)

(defun utf8-read-character (reader)
  (labels ((fetch ()
             (funcall reader))
           (illegal-sequence ()
             (class-format-error "illegal UTF-8 sequence")))
    (let ((c (fetch)))
      (cond
        ((< c #x80)
            c)
        ((< c #xe0)
          (let* ((c1 (fetch)))
            (if (not (and c1
                          (< (logxor c1 #x80) #x40)))
                (illegal-sequence)
                (let ((wc (logior
                            (ash (logand c #x1f) 6)
                            (logxor c1 #x80))))
                  wc))))
        ((< c #xf0)
          (let* ((c1 (fetch))
                 (c2 (fetch)))
            (if (not (and c2
                          (< (logxor c1 #x80) #x40)
                          (< (logxor c2 #x80) #x40)
                          (or (>= c #xe1)
                              (>= c1 #xa0))))
                (illegal-sequence)
                (let ((wc (logior
                            (ash (logand c #x0f) 12)
                            (ash (logxor c1 #x80) 6)
                                 (logxor c2 #x80))))
                  wc))))
        ((< c #xf8)
         (let* ((c1 (fetch))
                (c2 (fetch))
                (c3 (fetch)))
           (if (not (and c3
                         (< (logxor c1 #x80) #x40)
                         (< (logxor c2 #x80) #x40)
                         (< (logxor c3 #x80) #x40)
                         (or (>= c #xf1)
                             (>= c1 #x90))))
               (illegal-sequence)
               (let ((wc (logior
                           (ash (logand c  #x07) 18)
                           (ash (logxor c1 #x80) 12)
                           (ash (logxor c2 #x80) 6)
                                (logxor c3 #x80))))
                 wc))))
        ((< c #xfc)
         (let ((c1 (fetch))
               (c2 (fetch))
               (c3 (fetch))
               (c4 (fetch)))
           (if (not (and c4
                         (< (logxor c1 #x80) #x40)
                         (< (logxor c2 #x80) #x40)
                         (< (logxor c3 #x80) #x40)
                         (< (logxor c4 #x80) #x40)
                         (or (>= c #xf9)
                             (>= c1 #x88))))
               (illegal-sequence)
               (let ((wc (logior
                           (ash (logand c  #x03) 24)
                           (ash (logxor c1 #x80) 18)
                           (ash (logxor c2 #x80) 12)
                           (ash (logxor c3 #x80)  6)
                                (logxor c4 #x80))))
                 wc))))
        ((< c #xfe)
         (let ((c1 (fetch))
               (c2 (fetch))
               (c3 (fetch))
               (c4 (fetch))
               (c5 (fetch)))
           (if (not (and c5
                         (< (logxor c1 #x80) #x40)
                         (< (logxor c2 #x80) #x40)
                         (< (logxor c3 #x80) #x40)
                         (< (logxor c4 #x80) #x40)
                         (< (logxor c5 #x80) #x40)
                         (or (>= c #xfd)
                             (>= c1 #x84))))
               (illegal-sequence)
               (let ((wc (logior
                           (ash (logand c  #x03) 30)
                           (ash (logxor c1 #x80) 24)
                           (ash (logxor c2 #x80) 18)
                           (ash (logxor c3 #x80) 12)
                           (ash (logxor c4 #x80)  6)
                                (logxor c5 #x80))))
                 wc))))
        (t (illegal-sequence))))))
