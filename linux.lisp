#|
 This file is a part of file-attributes
 (c) 2020 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.file-attributes)

(defconstant AT-FDCWD -100)
(defconstant STATX-BTIME #x00000800)

(cffi:defcfun (cstatx "statx") :int
  (dirfd :int)
  (path :string)
  (flags :int)
  (mask :unsigned-int)
  (statx :pointer))

(cffi:defcstruct statx-timestamp
  (sec :int64)
  (nsec :uint32))

(cffi:defcstruct statx
  (mask :uint32)
  (blksize :uint32)
  (attributes :uint64)
  (nlink :uint32)
  (uid :uint32)
  (gid :uint32)
  (mode :uint16)
  (ino :uint64)
  (size :uint64)
  (blocks :uint64)
  (attributes_mask :uint64)
  (atime (:struct statx-timestamp))
  (btime (:struct statx-timestamp))
  (ctime (:struct statx-timestamp))
  (mtime (:struct statx-timestamp))
  (rdev_major :uint32)
  (rdev_minor :uint32)
  (dev_major :uint32)
  (dev_minor :uint32))

(defun statx (path)
  (cffi:with-foreign-object (statx '(:struct statx))
    (if (= 0 (cstatx AT-FDCWD (enpath path) 0 STATX-BTIME statx))
        (cffi:mem-ref statx '(:struct statx))
        (error "Statx failed"))))

(define-implementation creation-time (file)
  (unix->universal (getf (getf (statx file) 'btime) 'sec)))
