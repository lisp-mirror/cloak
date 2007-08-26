(in-package :sb-vm)

(declaim (optimize (speed 0)
                   (safety 3)
                   (sb-c::recognize-self-calls 0)
                   (sb-ext:inhibit-warnings 0)))


;;; register SIGNED64-REG in primitive types

(dolist (primtype '(positive-fixnum
                    unsigned-byte-31
                    unsigned-byte-32
                    fixnum
                    signed-byte-32
                    signed-byte-64))
  (flet ((fix (it)
           (setf (sb-c::primitive-type-scs it)
                 (append (sb-c::primitive-type-scs it)
                         (list (sc-number (sc-or-lose 'signed64-reg)))))))
    (fix (primitive-type-or-lose primtype))
    (fix (meta-primitive-type-or-lose primtype))))


;;; update cost vectors in the VOP-INFO structures

(in-package :sb-c)

(defun depurify-template (name)
  (let* ((parse (gethash name *backend-parsed-vops*))
         (info (copy-vop-info (gethash name *backend-template-names*))))
    (setf (gethash name *backend-template-names*) info)
    (dolist (fun-name (vop-parse-translate parse))
      (let ((fun-info (fun-info-or-lose fun-name)))
        (setf (fun-info-templates fun-info)
              (adjoin-template info (fun-info-templates fun-info)))))
    info))

(defun reset-costs-and-restrictions (info parse)
  (multiple-value-bind (arg-costs arg-scs)
      (compute-costs-and-restrictions-list (vop-parse-args parse) t)
    (multiple-value-bind (result-costs result-scs)
        (compute-costs-and-restrictions-list (vop-parse-results parse) nil)
      (setf (vop-info-arg-costs info) arg-costs)
      (setf (vop-info-arg-load-scs info) arg-scs)
      (setf (vop-info-result-costs info) result-costs)
      (setf (vop-info-result-load-scs info) result-scs)
      (setf (vop-info-more-arg-costs info)
            (if (vop-parse-more-args parse)
                (compute-loading-costs-if-any (vop-parse-more-args parse) t)
                nil))
      (setf (vop-info-more-result-costs info)
            (if (vop-parse-more-results parse)
                (compute-loading-costs-if-any (vop-parse-more-results parse)
                                              nil)
                nil)))))

(maphash (lambda (name parse)
           (reset-costs-and-restrictions (depurify-template name) parse))
         *backend-parsed-vops*)


(flet ((refresh-template (vopinfo)
         (if vopinfo
             (gethash (vop-info-name vopinfo) *backend-template-names*)
             nil)))

  ;; refresh PRIMITIVE-TYPE-COST slots pointing to old VOP-INFOs
  (dolist (table (list *backend-meta-primitive-type-names*
                       *backend-primitive-type-names*))
    (maphash (lambda (name ptype)
               (declare (ignore name))
               (setf (primitive-type-check ptype)
                     (refresh-template (primitive-type-check ptype))))
             table))

  ;; refresh MOVE-VOP tables pointing to old VOP-INFOs
  (loop for sc being each hash-value in *backend-sc-names* do
        (dolist (v (list (sc-move-vops sc) (sc-move-arg-vops sc)))
          (dotimes (x (length v))
            (setf (elt v x) (mapcar #'refresh-template (elt v x)))))))
