#|
 This file is a part of file-attributes
 (c) 2020 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.file-attributes)

(defconstant GENERIC-READ   #x80000000)
(defconstant GENERIC-WRITE  #x40000000)
(defconstant FILE-SHARE-ALL #x00000007)
(defconstant OPEN-EXISTING  3)
(defconstant FILE-ATTRIBUTE-NORMAL #x80)
(defconstant FILE-OBJECT #x1)
(defconstant OWNER-SECURITY-INFORMATION #x1)
(defconstant GROUP-SECURITY-INFORMATION #x2)

(cffi:defctype hfile :int)
(cffi:defctype byte :uint8)
(cffi:defctype word :uint16)
(cffi:defctype dword :uint32)

(cffi:defcstruct (filetime :conc-name filetime-)
  (low-date-time dword)
  (high-date-time dword))

(cffi:defcstruct (systemtime :conc-name systemtime-)
  (year word)
  (month word)
  (day-of-week word)
  (day word)
  (hour word)
  (minute word)
  (second word)
  (milliseconds word))

(cffi:defcstruct (sid :conc-name sid-)
  (revision byte)
  (authority-count byte)
  (identifier byte :count 6)
  (sub-authority dword))

(cffi:defcfun (filetime-to-systemtime "FileTimeToSystemTime") :bool
  (filetime :pointer)
  (systemtime :pointer))

(cffi:defcfun (systemtime-to-filetime "SystemTimeToFileTime") :bool
  (systemtime :pointer)
  (filetime :pointer))

(cffi:defcfun (get-file-time "GetFileTime") :bool
  (file hfile)
  (ctime :pointer)
  (atime :pointer)
  (mtime :pointer))

(cffi:defcfun (set-file-time "SetFileTime") :bool
  (file hfile)
  (ctime :pointer)
  (atime :pointer)
  (mtime :pointer))

(cffi:defcfun (create-file "CreateFileW") hfile
  (filename :pointer)
  (access dword)
  (share-mode dword)
  (security-attributes :pointer)
  (creation-disposition dword)
  (flags-and-attributes dword)
  (template hfile))

(cffi:defcfun (allocate-sid "AllocateAndInitializeSid") :bool
  (authority :pointer)
  (sub-authorities byte)
  (authority-0 dword)
  (authority-1 dword)
  (authority-2 dword)
  (authority-3 dword)
  (authority-4 dword)
  (authority-5 dword)
  (authority-6 dword)
  (authority-7 dword)
  (sid :pointer))

(cffi:defcfun (get-security-info "GetSecurityInfo") dword
  (handle hfile)
  (object-type :int)
  (security-info :int)
  (owner :pointer)
  (group :pointer)
  (dacl :pointer)
  (sacl :pointer)
  (security-descriptor :pointer))

(cffi:defcfun (set-security-info "SetSecurityInfo") dword
  (handle hfile)
  (object-type :int)
  (security-info :int)
  (owner :pointer)
  (group :pointer)
  (dacl :pointer)
  (sacl :pointer))

(cffi:defcfun (close-file "CloseHandle") :bool
  (object hfile))

(cffi:defcfun (local-free "LocalFree") :pointer
  (object :pointer))

(cffi:defcfun (get-file-attributes "GetFileAttributesW") dword
  (filename :pointer))

(defun open-file (path mode)
  ;; FIXME: ensure path is translated to pointer properly.
  (let ((handle (create-file (enpath path) mode FILE-SHARE-ALL (cffi:null-pointer)
                             OPEN-EXISTING FILE-ATTRIBUTE-NORMAL (cffi:null-pointer))))
    (if (/= -1 handle)
        handle
        (error "CreateFile failed."))))

(defmacro with-file ((file path mode) &body body)
  `(let ((,file (open-file ,path ,mode)))
     (unwind-protect (progn ,@body)
       (close-file ,file))))

(defun filetime->universal (filetime)
  (cffi:with-foreign-object (systemtime '(:struct systemtime))
    (if (filetime-to-systemtime filetime systemtime)
        (encode-universal-time
         (systemtime-second systemtime)
         (systemtime-minute systemtime)
         (systemtime-hour systemtime)
         (systemtime-day systemtime)
         (systemtime-month systemtime)
         (systemtime-year systemtime) 0)
        (error "Failed to convert filetime."))))

(defun universal->filetime (universal filetime)
  (cffi:with-foreign-object (systemtime '(:struct systemtime))
    (multiple-value-bind (s m h dd mm yy) (decode-universal-time universal 0)
      (setf (systemtime-second systemtime) s)
      (setf (systemtime-minute systemtime) m)
      (setf (systemtime-hour systemtime) h)
      (setf (systemtime-day systemtime) dd)
      (setf (systemtime-month systemtime) mm)
      (setf (systemtime-year systemtime) yy))
    (if (systemtime-to-filetime systemtime filetime)
        filetime
        (error "Failed to convert filetime."))))

(defmacro define-time-reader (name args)
  `(define-implementation ,name (file)
     (with-file (file file GENERIC-READ)
       (cffi:with-foreign-object (filetime '(:struct filetime))
         (if (get-file-time file ,@args)
             (filetime->universal filetime)
             (error "GetFileTime failed."))))))

(defmacro define-time-writer (name args)
  `(define-implementation (setf ,name) (value file)
     (with-file (file file GENERIC-WRITE)
       (cffi:with-foreign-object (filetime '(:struct filetime))
         (universal->filetime value filetime)
         (if (set-file-time file ,@args)
             value
             (error "SetFileTime failed."))))))

(define-time-reader access-time ((cffi:null-pointer) filetime (cffi:null-pointer)))
(define-time-writer access-time ((cffi:null-pointer) filetime (cffi:null-pointer)))
(define-time-reader modification-time ((cffi:null-pointer) (cffi:null-pointer) filetime))
(define-time-writer modification-time ((cffi:null-pointer) (cffi:null-pointer) filetime))
(define-time-reader creation-time (filetime (cffi:null-pointer) (cffi:null-pointer)))
(define-time-writer creation-time (filetime (cffi:null-pointer) (cffi:null-pointer)))

(define-implementation user (file)
  (with-file (file file GENERIC-READ)
    (cffi:with-foreign-objects ((user '(:struct sid))
                                (attribs :pointer))
      (cond ((= 0 (get-security-info handle FILE-OBJECT OWNER-SECURITY-INFORMATION user (cffi:null-pointer)
                                     (cffi:null-pointer) (cffi:null-pointer) attribs))
             (local-free attribs)
             (sid-sub-authority user))
            (T
             (error "GetSecurityInfo failed."))))))

(define-implementation (setf user) (user file)
  (with-file (file file GENERIC-WRITE)
    (cffi:with-foreign-objects ((user '(:struct sid))
                                (authority byte :count 6))
      (setf (cffi:mem-aref authority 'byte 6) 5)
      (unless (allocate-sid authority 1 user 0 0 0 0 0 0 0 user)
        (error "AllocateAndInitializeSid failed."))
      (unless (= 0 (set-security-info file FILE-OBJECT USER-SECURITY-INFORMATION user (cffi:null-pointer)
                                      (cffi:null-pointer) (cffi:null-pointer)))
        (error "SetSecurityInfo failed."))
      user)))

(define-implementation group (file)
  (with-file (file file GENERIC-READ)
    (cffi:with-foreign-objects ((group '(:struct sid))
                                (attribs :pointer))
      (cond ((= 0 (get-security-info file FILE-OBJECT GROUP-SECURITY-INFORMATION (cffi:null-pointer) group
                                     (cffi:null-pointer) (cffi:null-pointer) attribs))
             (local-free attribs)
             (sid-sub-authority group))
            (T
             (error "GetSecurityInfo failed."))))))

(define-implementation (setf group) (group file)
  (with-file (file file GENERIC-WRITE)
    (cffi:with-foreign-objects ((group '(:struct sid))
                                (authority byte :count 6))
      (setf (cffi:mem-aref authority 'byte 6) 5)
      (unless (allocate-sid authority 1 group 0 0 0 0 0 0 0 group)
        (error "AllocateAndInitializeSid failed."))
      (unless (= 0 (set-security-info file FILE-OBJECT GROUP-SECURITY-INFORMATION (cffi:null-pointer) group
                                      (cffi:null-pointer) (cffi:null-pointer)))
        (error "SetSecurityInfo failed."))
      group)))

(define-implementation attributes (file)
  (let ((attributes (get-file-attributes (enpath file))))
    (if (= attributes #xFFFFFFFF)
        (error "GetFileAttributes failed.")
        attributes)))

(define-implementation (setf attributes) (value file)
  (if (set-file-attributes (enpath file) value)
      value
      (error "SetFileAttributes failed.")))
