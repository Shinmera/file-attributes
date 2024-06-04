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
   #:stat-file
   #:stat-result
   #:st-access-time
   #:st-modification-time
   #:st-creation-time
   #:st-group
   #:st-owner
   #:st-attributes))
