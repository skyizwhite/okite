(defpackage #:okite/interfaces
  (:use #:cl)
  (:import-from #:closer-mop
                #:generic-function-methods)
  (:export #:unimplemented-generics
           #:ensure-implemented
           #:unimplemented-error
           #:unimplemented-error-generics))
(in-package #:okite/interfaces)

(defun unimplemented-generics (packages)
  "The generic functions exported from PACKAGES, a list of package designators,
that no method implements, as their names. A method defined next to the generic
function, a default, counts as an implementation."
  (let ((missing '()))
    (dolist (package packages)
      (do-external-symbols (symbol (or (find-package package)
                                       (error "No package named ~a" package)))
        (when (and (fboundp symbol)
                   (typep (fdefinition symbol) 'generic-function)
                   (null (generic-function-methods (fdefinition symbol))))
          (pushnew symbol missing))))
    (sort missing #'string< :key #'symbol-name)))

(define-condition unimplemented-error (error)
  ((generics :initarg :generics :reader unimplemented-error-generics))
  (:report (lambda (condition stream)
             (format stream "No method implements ~{~s~^, ~}"
                     (unimplemented-error-generics condition)))))

(defun ensure-implemented (packages)
  "Signal UNIMPLEMENTED-ERROR when a generic function exported from PACKAGES has
no method. Called where the implementations have been loaded, it finds a missing
one then rather than when it is first called."
  (let ((missing (unimplemented-generics packages)))
    (when missing
      (error 'unimplemented-error :generics missing))))
