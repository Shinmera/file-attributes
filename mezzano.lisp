(in-package #:org.shirakumo.file-attributes)

(define-implementation access-time (file)
  (or (getf (mezzano.file-system:file-properties file) :access-date)
      (getf (mezzano.file-system:file-properties file) :write-date)))

(define-implementation (setf access-time) (value file)
  (mezzano.file-system:set-file-properties file :access-date value))

(define-implementation modification-time (file)
  (getf (mezzano.file-system:file-properties file) :write-date))

(define-implementation (setf modification-time) (value file)
  (mezzano.file-system:set-file-properties file :write-date value))

(define-implementation creation-time (file)
  (or (getf (mezzano.file-system:file-properties file) :creation-date)
      (getf (mezzano.file-system:file-properties file) :write-date)))

(define-implementation (setf creation-time) (value file)
  (mezzano.file-system:set-file-properties file :creation-date value))

(define-implementation group (file)
  (getf (mezzano.file-system:file-properties file) :guid 0))

(define-implementation (setf group) (value file)
  (mezzano.file-system:set-file-properties file :guid value))

(define-implementation owner (file)
  (or (getf (mezzano.file-system:file-properties file) :uid)
      (sxhash (getf (mezzano.file-system:file-properties file) :author ""))))

(define-implementation (setf owner) (value file)
  (mezzano.file-system:set-file-properties file :uid value))

(define-implementation attributes (file)
  (encode-attributes (mezzano.file-system:file-properties file) :mezzano))

(define-implementation (setf attributes) (value file)
  (apply #'mezzano.file-system:set-file-properties file (decode-attributes value :mezzano)))

(define-implementation stat-file (file)
  (let ((props (mezzano.file-system:file-properties file)))
    (make-stat-result :access-time (getf props :access-date)
                      :modification-time (getf props :write-date)
                      :creation-time (or (getf props :creation-date)
                                         (getf props :write-date))
                      :group (getf props :guid 0)
                      :owner (or (getf props :uid)
                                 (sxhash (getf props :author "")))
                      :attributes (encode-attributes props :mezzano))))
