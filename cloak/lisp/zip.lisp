(in-package :cloak)

(defun open-java-zipfile (pathname)
  (make-cloak-instance "java/util/zip/ZipFile" "<init>(Ljava/lang/String;)"
    (make-cloak-string (namestring pathname))))

(defun zipfile-size (zipfile)
  (call-cloak-method "size()" zipfile))

(defgeneric zipfile-name (zipfile))

(defmethod zipfile-name ((zipfile cloak-object))
  (get-string-value (call-cloak-method "getName()" zipfile)))

(defmethod zipfile-name ((zipfile zip:zipfile))
  (namestring (truename (pathname (zip::zipfile-stream zipfile)))))

(defmethod zip:get-zipfile-entry (name (zipfile cloak-object))
  (let ((%name (make-cloak-string name)))
    (call-cloak-method "getEntry(Ljava/lang/String;)" zipfile %name)))

(defun zipfile-entries (zipfile)
  (let ((e (call-cloak-method "entries()" zipfile)))
    (loop
	until (zerop (call-cloak-method "hasMoreElements()" e))
	collect (call-cloak-method "nextElement()" e))))

(defun zipfile-entry-stream (zipfile entry)
  (call-cloak-method "getInputStream(Ljava/util/zip/ZipEntry;)" zipfile entry))

(defgeneric zipfile-entry-contents (entry zipfile))

(defmethod zipfile-entry-contents ((entry cloak-object) zipfile)
  (let* ((n (call-cloak-method "getSize()" entry))
	 (v (make-array n :initial-element 0 :element-type '(signed-byte 8)))
	 (w (make-cloak-array "[B" v))
	 (s (zipfile-entry-stream zipfile entry)))
    (let ((i 0))
      (while (< i n)
	(incf i (call-cloak-method "read([BII)" s w i (- n i)))))
    (for* ((i :from 0 :below n)		;byte is signed...
	   (b = (svref v i)))
      (when (minusp b)
	(setf (svref v i) (+ b 256))))
    v))

(defmethod zipfile-entry-contents ((entry zip::zipfile-entry) zipfile)
  (declare (ignore zipfile))
  (zip:zipfile-entry-contents entry))
