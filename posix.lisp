(in-package #:org.shirakumo.file-attributes)

;; Linux 5.7.7 AMD64
#+linux
(cffi:defcstruct (stat :size 144 :conc-name stat-)
  (mode    :uint32 :offset 24)
  (uid     :uint32 :offset 28)
  (gid     :uint32 :offset 32)
  (size    :uint64 :offset 48)
  (atime   :uint64 :offset 72)
  (mtime   :uint64 :offset 88))

;; OS X 10.14
#+darwin
(cffi:defcstruct (stat :size 144 :conc-name stat-)
  (mode    :uint16 :offset  4)
  (uid     :uint32 :offset 16)
  (gid     :uint32 :offset 20)
  (atime   :uint64 :offset 32)
  (mtime   :uint64 :offset 48)
  (size    :uint64 :offset 96))

;; FreeBSD 12.1 AMD64
#+freebsd
(cffi:defcstruct (stat :size 224 :conc-name stat-)
  (mode    :uint16 :offset 24)
  (uid     :uint32 :offset 28)
  (gid     :uint32 :offset 32)
  (size    :uint64 :offset 112)
  (atime   :uint64 :offset 48)
  (mtime   :uint64 :offset 64))

;; OpenBSD 7.1 AMD64
#+openbsd
(cffi:defcstruct (stat :size 128 :conc-name stat-)
  (mode    :uint32 :offset  0)
  (uid     :uint32 :offset 20)
  (gid     :uint32 :offset 24)
  (size    :uint64 :offset 80)
  (atime   :uint64 :offset 32)
  (mtime   :uint64 :offset 48))

;; NX 17.5.4
#+nx
(cffi:defcstruct (stat :size 128 :conc-name stat-)
  (mode    :uint32 :offset 16)
  (uid     :uint32 :offset 24)
  (gid     :uint32 :offset 28)
  (size    :uint64 :offset 48)
  (atime   :uint64 :offset 72)
  (mtime   :uint64 :offset 88))

(cffi:defcfun (cgstat "stat") :int
  (path :string)
  (buffer :pointer))

(cffi:defcfun (cxstat "__xstat") :int
  (path :string)
  (buffer :pointer))

(cffi:defcfun (cutimes "utimes") :int
  (path :string)
  (times :pointer))

(cffi:defcfun (cchown "chown") :int
  (path :string)
  (owner :uint32)
  (group :uint32))

(cffi:defcfun (cchmod "chmod") :int
  (path :string)
  (mode :uint32))

(defun unix->universal (unix)
  (+ unix (encode-universal-time 0 0 0 1 1 1970 0)))

(defun universal->unix (universal)
  (- universal (encode-universal-time 0 0 0 1 1 1970 0)))

(defun cstat (path buffer)
  (cond ((cffi:foreign-symbol-pointer "stat")
         (cgstat path buffer))
        ((cffi:foreign-symbol-pointer "__xstat")
         (cxstat path buffer))
        (T
         1)))

(defmacro with-stat ((ptr path) &body body)
  `(cffi:with-foreign-object (,ptr '(:struct stat))
     (if (= 0 (cstat (enpath ,path) ,ptr))
         (progn ,@body)
         (error "Stat failed."))))

(defun stat (path)
  (with-stat (ptr path)
    (cffi:mem-ref ptr '(:struct stat))))

(defun utimes (path atime mtime)
  (cffi:with-foreign-object (ptr :long 4)
    (setf (cffi:mem-aref ptr :long 0) (universal->unix atime))
    (setf (cffi:mem-aref ptr :long 2) (universal->unix mtime))
    (unless (= 0 (cutimes (enpath path) ptr))
      (error "Utimes failed."))))

(defun chown (path uid gid)
  (cchown (enpath path) uid gid))

(defun chmod (path mode)
  (cchmod (enpath path) mode))

(define-implementation access-time (file)
  (with-stat (stat file)
    (unix->universal (stat-atime stat))))

(define-implementation (setf access-time) (value file)
  (utimes file value (modification-time file))
  value)

(define-implementation modification-time (file)
  (with-stat (stat file)
    (unix->universal (stat-mtime stat))))

(define-implementation (setf modification-time) (value file)
  (utimes file (access-time file) value)
  value)

(define-implementation group (file)
  (with-stat (stat file)
    (stat-gid stat)))

(define-implementation (setf group) (value file)
  (chown file (owner file) value)
  value)

(define-implementation owner (file)
  (with-stat (stat file)
    (stat-uid stat)))

(define-implementation (setf owner) (value file)
  (chown file value (group file))
  value)

(define-implementation attributes (file)
  (with-stat (stat file)
    (stat-mode stat)))

(define-implementation (setf attributes) (value file)
  (chmod file value))

(define-implementation all-fields (file)
  (with-stat (ptr file)
    (cffi:with-foreign-slots ((mode uid gid atime mtime) ptr (:struct stat))
      (make-fields :access-time (unix->universal atime)
                   :modification-time (unix->universal mtime)
                   :creation-time 0
                   :group gid
                   :owner uid
                   :attributes mode))))
