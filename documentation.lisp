#|
 This file is a part of file-attributes
 (c) 2020 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.file-attributes)

(docs:define-docs
  (function access-time
    "Accesses the last time this file was accessed.

Signals an error if retrieving or setting the information is not
possible.")
  
  (function modification-time
    "Accesses the last time this file was modified.

Signals an error if retrieving or setting the information is not
possible.")
  
  (function creation-time
    "Accesses the time this file was created.

Signals an error if retrieving or setting the information is not
possible.")
  
  (function group
    "Accesses the owning group of this file.

The group is expressed as a positive integer.

Signals an error if retrieving or setting the information is not
possible.")
  
  (function owner
    "Accesses the owning user of this file.

The user is expressed as a positive integer.

Signals an error if retrieving or setting the information is not
possible.")
  
  (function attributes
    "Accesses the attributes of this file.

The attributes are expressed as a positive integer.

Signals an error if retrieving or setting the information is not
possible.

The contents of the file attributes are highly system specific and may
contain things such as user permissions or file kind information.

See ENCODE-PERMISSIONS
See DECODE-PERMISSIONS")

  (function encode-attributes
    "")

  (function decode-attributes
    ""))

