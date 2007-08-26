(in-package :cloak)

(defun make-java-byte-array (initial-contents)
  (make-array (length initial-contents)
              :element-type '(signed-byte 8)
              :initial-contents initial-contents))

(defstatic |java/net/InetAddress.lookupInaddrAny()| ()
  (make-cloak-array "[B" (make-java-byte-array #(0 0 0 0))))

(defstatic |java/net/InetAddress.getHostByAddr([B)| (ipaddr)
  (make-cloak-string (get-host-by-address (co.data ipaddr))))

(defstatic |java/net/InetAddress.getHostByName(Ljava/lang/String;)| (hostname)
  (make-cloak-array "[[B"
    (vector
     (make-cloak-array "[B"
       (make-java-byte-array
        (get-host-by-name (get-string-value hostname)))))))
