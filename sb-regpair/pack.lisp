;;; tell the compiler about register pairs

(in-package :sb-vm)

(defvar *add-location-conflicts* #'sb-c::add-location-conflicts)

(defun add-location-conflicts/sb64 (tn sc offset optimize)
  (funcall *add-location-conflicts* tn sc offset optimize)
  (case (sb-name (sc-sb sc))
    (registers
      (let ((o (truncate offset 2))
            (regs (sc-or-lose 'signed64-reg)))
        (dotimes (hi 8)
          (funcall *add-location-conflicts*
                   tn regs (logior (ash o 3) hi) optimize))
        (dotimes (lo 8)
          (funcall *add-location-conflicts*
                   tn regs (logior (ash lo 3) o) optimize))))
    (register-pairs
      (add-location-conflicts/sb64
       tn (sc-or-lose 'any-reg) (* 2 (logand offset 7)) optimize)
      (add-location-conflicts/sb64
       tn (sc-or-lose 'any-reg) (* 2 (ash offset -3)) optimize))))

(setf (fdefinition 'sb-c::add-location-conflicts)
      #'add-location-conflicts/sb64)

(defvar *load-tn-conflicts-in-sc* #'sb-c::load-tn-conflicts-in-sc)

(defun load-tn-conflicts-in-sc/sb64 (op sc offset ignore-live)
  (or (funcall *load-tn-conflicts-in-sc* op sc offset ignore-live)
      (case (sb-name (sc-sb sc))
        (registers
          (let ((o (truncate offset 2))
                (regs (sc-or-lose 'signed64-reg)))
            (loop for i from 0 below 8 thereis
                  (or (funcall *load-tn-conflicts-in-sc*
                               op
                               regs
                               (logior (ash o 3) i)
                               ignore-live)
                      (funcall *load-tn-conflicts-in-sc*
                               op
                               regs
                               (logior (ash i 3) o)
                               ignore-live)))))
        (register-pairs
          (or (load-tn-conflicts-in-sc/sb64
               op
               (sc-or-lose 'any-reg)
               (* 2 (logand offset 7))
               ignore-live)
              (load-tn-conflicts-in-sc/sb64
               op
               (sc-or-lose 'any-reg)
               (* 2 (ash offset -3))
               ignore-live)))
        (t
          nil))))

(setf (fdefinition 'sb-c::load-tn-conflicts-in-sc)
      #'load-tn-conflicts-in-sc/sb64)

(in-package :sb-c)
(defun unpack-for-load-tn (sc op)
  (declare (type sc sc) (type tn-ref op))
  (let ((sb (sc-sb sc))
        (normal-tns (ir2-component-normal-tns
                     (component-info *component-being-compiled*)))
        (node (vop-node (tn-ref-vop op)))
        (fallback nil))
    (flet ((unpack-em (victims)
             (unless *repack-blocks*
               (setq *repack-blocks* (make-hash-table :test 'eq)))
             (setf (gethash (vop-block (tn-ref-vop op)) *repack-blocks*) t)
             (dolist (victim victims)
               (event unpack-tn node)
               (unpack-tn victim))
             (throw 'unpacked-tn nil)))
      (dolist (loc (sc-locations sc))
        (declare (type index loc))
        (block SKIP
          (collect ((victims nil adjoin))
            (do ((i loc (1+ i))
                 (end (+ loc (sc-element-size sc))))
                ((= i end))
              (declare (type index i end))
              ;; <<<
              (labels ((doit (sb i)
                         (let ((victim (svref (finite-sb-live-tns sb) i)))
                           (when victim
                             (unless (find-in #'tn-next victim normal-tns)
                               (return-from SKIP))
                             (victims victim))))
                       ;; ===
                       (sb64 (sb i)
                         (doit sb i)
                         (case (sb-name sb)
                           (sb-vm::registers
                             (dotimes (j 8)
                               (doit (sb-or-lose 'sb-vm::register-pairs)
                                     (logior (ash j 3) (truncate i 2)))
                               (doit (sb-or-lose 'sb-vm::register-pairs)
                                     (logior (ash (truncate i 2) 3) j))))
                           (sb-vm::register-pairs
                             (sb64 (sb-or-lose 'sb-vm::registers)
                                   (* 2 (logand i 7)))
                             (sb64 (sb-or-lose 'sb-vm::registers)
                                   (* 2 (ash i -3)))))))
                (sb64 sb i)))
            ;; >>>

            (let ((conf (load-tn-conflicts-in-sc op sc loc t)))
              (cond ((not conf)
                     (unpack-em (victims)))
                    ((eq conf :overflow))
                    ((not fallback)
                     (cond ((find conf (victims))
                            (setq fallback (victims)))
                           ((find-in #'tn-next conf normal-tns)
                            (setq fallback (list conf))))))))))

      (when fallback
        (event unpack-fallback node)
        (unpack-em fallback))))

  nil)

(defun compute-live-tns (block vop)
  (declare (type ir2-block block) (type vop vop))
  (unless (eq block *live-block*)
    (init-live-tns block))

  (do ((current *live-vop* (vop-prev current)))
      ((eq current vop)
       (do ((res (vop-results vop) (tn-ref-across res)))
           ((null res))
         (let* ((tn (tn-ref-tn res))
                (sc (tn-sc tn)))
           (when (eq (sb-kind (sc-sb sc)) :finite)
             (labels ((doit (sc start)
                        (let ((sb (sc-sb sc)))
                          (do ((offset start (1+ offset))
                               (end (+ start (sc-element-size sc))))
                              ((= offset end))
                            (declare (type index offset end))
                            (setf (svref (finite-sb-live-tns sb) offset) nil))))
                      (sb64 (sc start)
                        (doit sc start)
                        (case (sb-name (sc-sb sc))
                          (registers
                            (let ((o (truncate start 2))
                                  (regs (sc-or-lose 'signed64-reg)))
                              (loop for i from 0 below 8 do
                                    (doit regs (logior (ash o 3) i))
                                    (doit regs (logior (ash i 3) o)))))
                          (register-pairs
                            (sb64 (sc-or-lose 'any-reg)
                                  (* 2 (logand start 7)))
                            (sb64 (sc-or-lose 'any-reg)
                                  (* 2 (ash start -3)))))))
               (sb64 sc (tn-offset tn)))))))
    (do ((ref (vop-refs current) (tn-ref-next-ref ref)))
        ((null ref))
      (let* ((tn (tn-ref-tn ref))
             (sc (tn-sc tn)))
        (when (eq (sb-kind (sc-sb sc)) :finite)
          (labels ((doit (sc start)
                     (let* ((sb (sc-sb sc))
                            (tns (finite-sb-live-tns sb)))
                       (do ((offset start (1+ offset))
                            (end (+ start (sc-element-size sc))))
                           ((= offset end))
                         (declare (type index offset end))
                         (if (tn-ref-write-p ref)
                             (setf (svref tns offset) nil)
                             (let ((old (svref tns offset)))
                               (aver (or (null old) (eq old tn)))
                               (setf (svref tns offset) tn))))))
                   (sb64 (sc start)
                     (doit sc start)
                     (case (sb-name (sc-sb sc))
                       (registers
                         (let ((o (truncate start 2))
                               (regs (sc-or-lose 'signed64-reg)))
                           (dotimes (i 8)
                             (doit regs (logior (ash o 3) i))
                             (doit regs (logior (ash i 3) o)))))
                       (register-pairs
                         (sb64 (sc-or-lose 'any-reg)
                               (* 2 (logand start 7)))
                         (sb64 (sc-or-lose 'any-reg)
                               (* 2 (ash start -3)))))))
            (sb64 sc (tn-offset tn)))))))

  (setq *live-vop* vop)
  (values))
