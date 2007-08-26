(defpackage :sb-regpair-system
  (:use :asdf :cl))
(in-package :sb-regpair-system)

(defclass vop-defining-file (cl-source-file) ())
(defmethod perform :around ((o load-op) (s vop-defining-file))
  (sb-ext:with-unlocked-packages (:sb-c)
    (call-next-method)))
;; c.f. fixme in make-operand-parse-temp:
(defmethod perform :around ((o compile-op) (s vop-defining-file))
  (sb-ext:with-unlocked-packages (:sb-c)
    (call-next-method)))

(defsystem sb-regpair
    #+sb-building-contrib :pathname
    #+sb-building-contrib "SYS:CONTRIB;SB-REGPAIR;"
    :default-component-class vop-defining-file
    :serial t
    :components ((:file "vm")
		 (:file "move")
		 (:file "pack")
		 (:file "boot")
		 (:file "arith")))

(defmethod perform :after ((o load-op) (c (eql (find-system 'sb-regpair))))
  (provide 'sb-regpair))

(defmethod perform ((o test-op) (c (eql (find-system 'sb-regpair))))
  (oos 'load-op 'sb-regpair-tests)
  (oos 'test-op 'sb-regpair-tests))

(defsystem sb-regpair-tests
    :depends-on (sb-rt)
    :components ((:file "test")))

(defmethod perform ((o test-op) (c (eql (find-system :sb-regpair-tests))))
  (or (funcall (find-symbol "DO-TESTS" "SB-RT"))
      (error "test-op failed")))
