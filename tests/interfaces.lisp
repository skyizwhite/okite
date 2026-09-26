(defpackage #:okite-test/interfaces
  (:use #:cl
        #:rove)
  (:import-from #:okite/interfaces
                #:unimplemented-generics
                #:ensure-implemented
                #:unimplemented-error
                #:unimplemented-error-generics))
(in-package #:okite-test/interfaces)

(defpackage #:okite-test/interfaces/port
  (:use #:cl)
  (:export #:implemented #:defaulted #:missing #:plain))
(in-package #:okite-test/interfaces/port)

(defgeneric implemented (x))
(defgeneric defaulted (x)
  (:method (x) x))
(defgeneric missing (x))
(defgeneric internal (x))
(defun plain (x) x)

(in-package #:okite-test/interfaces)

(defmethod okite-test/interfaces/port:implemented ((x integer)) x)

(deftest unimplemented-generics
  (ok (equal (unimplemented-generics '(#:okite-test/interfaces/port))
             '(okite-test/interfaces/port:missing))
      "a generic function without a method, exported; a default method counts, a plain function is not one"))

(deftest ensure-implemented
  (ok (null (ensure-implemented '())))
  (ok (equal (handler-case (ensure-implemented '(#:okite-test/interfaces/port))
               (unimplemented-error (e) (unimplemented-error-generics e)))
             '(okite-test/interfaces/port:missing))))

(deftest an-unknown-package
  (ok (signals (unimplemented-generics '(#:okite-test/no-such-package)))))
