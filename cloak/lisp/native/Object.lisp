(in-package :cloak)

(defnative |java/lang/Object.getClass()| (this)
  (class-object (%class this)))
