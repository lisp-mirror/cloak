(in-package :cloak)

#+(or)
(slurp "java/lang" "java/io" "java/math")

(defun slurp (&rest packages)
  (dolist (package packages)
    (dolist (file (directory
		   (merge-pathnames
		    (concatenate 'string "classpath/" package "/")
		    *source-directory*)
		   #+allegro :directories-are-files #+allegro nil))
      (when (equal (pathname-type file) "class")
	(handler-case
	    (let ((*noclassdeffounderror-protect* t))
	      (find-cloak-class
	       :fixme
	       (concatenate 'string package "/" (pathname-name file))))
	  (serious-condition (c)
	    (print c)))))))

#+(or)
(check-native-methods)

(slurp
 "jazzlib"
 "java"
 "java/lang"
 "java/lang/ref"
 "java/lang/reflect"
 #+(or) "java/awt"
 #+(or) "java/awt/image"
 #+(or) "java/awt/image/renderable"
 #+(or) "java/awt/peer"
 #+(or) "java/awt/event"
 #+(or) "java/awt/im"
 #+(or) "java/awt/im/spi"
 #+(or) "java/awt/dnd"
 #+(or) "java/awt/dnd/peer"
 #+(or) "java/awt/datatransfer"
 #+(or) "java/awt/color"
 #+(or) "java/awt/font"
 #+(or) "java/awt/geom"
 #+(or) "java/awt/print"
 "java/io"
 "java/util"
 "java/util/regex"
 "java/util/jar"
 "java/util/zip"
 "java/util/logging"
 "java/util/prefs"
 "java/net"
 "java/nio"
 "java/nio/channels"
 "java/nio/channels/spi"
 "java/nio/charset"
 "java/nio/charset/spi"
 "java/beans"
 "java/beans/beancontext"
 "java/text"
 "java/security"
 "java/security/cert"
 "java/security/interfaces"
 "java/security/spec"
 "java/security/acl"
 "java/applet"
 "java/math"
 "java/sql"
 "java/rmi"
 "java/rmi/server"
 "java/rmi/activation"
 "java/rmi/dgc"
 "java/rmi/registry"
 "javax"
 "javax/accessibility"
 "javax/swing"
 "javax/swing/plaf"
 "javax/swing/plaf/basic"
 "javax/swing/plaf/metal"
 "javax/swing/border"
 "javax/swing/event"
 "javax/swing/text"
 "javax/swing/text/html"
 "javax/swing/text/html/parser"
 "javax/swing/undo"
 "javax/swing/tree"
 "javax/swing/filechooser"
 "javax/swing/table"
 "javax/swing/colorchooser"
 "javax/security"
 "javax/security/auth"
 "javax/security/auth/x500"
 "javax/naming"
 "javax/naming/directory"
 "javax/naming/spi"
 "javax/naming/event"
 "javax/naming/ldap"
 "javax/print"
 "javax/print/attribute"
 "javax/print/attribute/standard"
 "javax/rmi"
 "javax/rmi/CORBA"
 "javax/transaction"
 "javax/transaction/xa"
 "javax/sql"
 "javax/xml"
 "javax/xml/parsers"
 "javax/xml/transform"
 "javax/xml/transform/stream"
 "javax/xml/transform/sax"
 "javax/xml/transform/dom"
 "gnu"
 "gnu/java"
 #+(or) "gnu/java/awt"
 #+(or) "gnu/java/awt/peer"
 #+(or) "gnu/java/awt/peer/gtk"
 #+(or) "gnu/java/awt/image"
 "gnu/java/io"
 "gnu/java/io/encode"
 "gnu/java/io/decode"
 "gnu/java/util"
 "gnu/java/util/prefs"
 "gnu/java/net"
 "gnu/java/net/protocol"
 "gnu/java/net/protocol/jar"
 "gnu/java/net/protocol/file"
 "gnu/java/net/protocol/http"
 "gnu/java/net/content"
 "gnu/java/net/content/text"
 "gnu/java/lang"
 "gnu/java/lang/reflect"
 "gnu/java/security"
 "gnu/java/security/provider"
 "gnu/java/security/der"
 "gnu/java/security/util"
 "gnu/java/security/x509"
 "gnu/java/nio"
 "gnu/java/nio/charset"
 "gnu/java/nio/channels"
 "gnu/java/math"
 "gnu/java/text"
 "gnu/java/beans"
 "gnu/java/beans/editors"
 "gnu/java/beans/info"
 "gnu/java/rmi"
 "gnu/java/rmi/rmic"
 "gnu/java/rmi/server"
 "gnu/java/rmi/dgc"
 "gnu/java/rmi/registry"
 "gnu/java/locale"
 "gnu/classpath"
 "gnu/regexp"
 "gnu/xml"
 "gnu/xml/util"
 "gnu/xml/aelfred2"
 "gnu/xml/pipeline"
 "gnu/xml/dom"
 "gnu/javax"
 "gnu/javax/rmi"
 "gnu/javax/rmi/CORBA"
 "gnu/javax/swing"
 "gnu/javax/swing/plaf"
 "gnu/javax/swing/plaf/gtk"
 "gnu/test"
 "org"
 "org/w3c"
 "org/w3c/dom"
 "org/w3c/dom/events"
 "org/w3c/dom/views"
 "org/w3c/dom/traversal"
 "org/w3c/dom/css"
 "org/w3c/dom/stylesheets"
 "org/w3c/dom/html"
 "org/w3c/dom/ranges"
 "org/xml"
 "org/xml/sax"
 "org/xml/sax/helpers"
 "org/xml/sax/ext")
