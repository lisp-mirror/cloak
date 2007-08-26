(in-package :cloak)

(defconstant +constant_class+ 7)
(defconstant +constant_fieldref+ 9)
(defconstant +constant_methodref+ 10)
(defconstant +constant_interfacemethodref+ 11)
(defconstant +constant_string+ 8)
(defconstant +constant_integer+ 3)
(defconstant +constant_float+ 4)
(defconstant +constant_long+ 5)
(defconstant +constant_double+ 6)
(defconstant +constant_nameandtype+ 12)
(defconstant +constant_utf8+ 1)

(defconstant +acc_public+ #x0001)
(defconstant +acc_private+ #x0002)
(defconstant +acc_protected+ #x0004)
(defconstant +acc_static+ #x0008)
(defconstant +acc_final+ #x0010)
(defconstant +acc_synchronized+ #x0020)
(defconstant +acc_super+ #x0020)
(defconstant +acc_volatile+ #x0040)
(defconstant +acc_transient+ #x0080)
(defconstant +acc_native+ #x0100)
(defconstant +acc_interface+ #x0200)
(defconstant +acc_abstract+ #x0400)
(defconstant +acc_strict+ #x0800)