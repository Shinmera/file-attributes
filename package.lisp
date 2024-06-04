(defpackage #:org.shirakumo.file-attributes
  (:use #:cl)
  (:shadow #:byte)
  ;; protocol.lisp
  (:export
   #:access-time
   #:modification-time
   #:creation-time
   #:group
   #:owner
   #:attributes
   #:*system*
   #:encode-attributes
   #:decode-attributes
   #:all-fields
   #:fields
   #:fields-access-time
   #:fields-modification-time
   #:fields-creation-time
   #:fields-group
   #:fields-owner
   #:fields-attributes))
