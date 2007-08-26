(in-package :cloak)


;;;; (unsigned-byte 8) buffer

(defstruct (buffer
	    (:conc-name "BUF.")
	    (:constructor make-buffer (bytes)))
  (bytes (missing) :type (simple-array (unsigned-byte 8) (*)))
  (pos 0 :type fixnum))

(defmethod print-object ((object buffer) stream)
  (print-unreadable-object (object stream :type t :identity nil)
    (format stream " ~S @ ~D"
	    (buf.bytes object)
	    (buf.pos object))))

(defun listen-buffer (buf)
  (< (buf.pos buf) (length (buf.bytes buf))))

(defun peek-buffer (buf)
  (elt (buf.bytes buf) (buf.pos buf)))

(defun fetch-byte (buf)
  (let ((pos (buf.pos buf)))
    (setf (buf.pos buf) (1+ pos))
    (elt (buf.bytes buf) pos)))

(defun fetch-sequence (vector buffer)
  (let ((start (buf.pos buffer)))
    (setf (buf.pos buffer) (+ start (length vector)))
    (replace vector (buf.bytes buffer) :start2 start)))

(defun fetch-short (s)
  (logior (ash (fetch-byte s) 8) (fetch-byte s)))

(defun fetch-int (s)
  (logior (ash (fetch-short s) 16) (fetch-short s)))

(defun fetch-long (s)
  (logior (ash (fetch-int s) 32) (fetch-int s)))


;;;; character buffer

(defstruct (cbuffer
	    (:conc-name "CBUF.")
	    (:constructor make-cbuffer (string)))
  (string (missing) :type (simple-array character (*)))
  (pos 0 :type fixnum))

(defun listen-cbuffer (buf)
  (< (cbuf.pos buf) (length (cbuf.string buf))))

(defun peek-cbuffer (buf)
  (char (cbuf.string buf) (cbuf.pos buf)))

(defun fetch-char (buf)
  (let ((pos (cbuf.pos buf)))
    (setf (cbuf.pos buf) (1+ pos))
    (elt (cbuf.string buf) pos)))
