#|
 This file is a part of file-attributes
 (c) 2020 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.file-attributes)

(defmacro define-implementable (name args)
  `(setf (fdefinition ',name)
         (lambda ,args
           (declare (ignore ,@args))
           (error "Not implemented"))))

(defmacro define-implementation (name args &body body)
  `(progn
     (fmakunbound ',name)
     (defun ,name ,args ,@body)))

(define-implementable access-time (file))
(define-implementable (setf access-time) (value file))
(define-implementable modification-time (file))
(define-implementable (setf modification-time) (value file))
(define-implementable creation-time (file))
(define-implementable (setf creation-time) (value file))
(define-implementable group (file))
(define-implementable (setf group) (value file))
(define-implementable owner (file))
(define-implementable (setf owner) (value file))
(define-implementable permissions (file))
(define-implementable (setf permissions) (value file))

(defun decode-permissions (permissions))
(defun encode-permissions (permissions))

(defun enpath (path)
  (etypecase path
    (string path)
    (stream (namestring (pathname path)))
    (pathname (namestring path))))
