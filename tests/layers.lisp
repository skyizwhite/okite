(defpackage #:okite-test/layers
  (:use #:cl
        #:rove)
  (:import-from #:okite/layers
                #:define-layers
                #:layer-violations
                #:ensure-layers
                #:violation-kind
                #:violation-file
                #:violation-layer
                #:violation-dependency
                #:violation-dependency-layer
                #:violation-error
                #:violation-error-violations))
(in-package #:okite-test/layers)

;;; tests/fixture/ is a system of five layers:
;;;   domain/entity, domain/rule -> domain/entity
;;;   usecases/create -> domain/entity, somelib
;;;   infra/store     -> domain/entity, usecases/create
;;;   web/page        -> usecases/create, somelib/extra
;;;   main            -> infra/store, web/page
;;; and two files that are not part of it: scripts/run, which does not start
;;; with a defpackage, and .hidden/junk, in a directory okite never walks.

(setup
  (asdf:load-asd (asdf:system-relative-pathname "okite-test" "tests/fixture/okite-fixture.asd")))

(define-layers lenient
  (:system "okite-fixture")
  (:ignore "scripts/")
  (:layers (:domain "domain/")
           (:usecases "usecases/")
           (:infra "infra/")
           (:web "web/")
           (:main "main"))
  (:allow (:usecases :domain)
          (:infra :domain :usecases)
          (:web :usecases)
          (:main :infra :web))
  (:libraries ("somelib" :usecases :web)))

(define-layers strict
  (:system "okite-fixture")
  (:ignore "scripts/")
  (:layers (:domain "domain/")
           (:usecases "usecases/")
           (:infra "infra/")
           (:web "web/")
           (:main "main"))
  (:allow (:usecases :domain)
          (:infra :domain)
          (:web :usecases)
          (:main :infra :web))
  (:libraries ("somelib" :usecases)))

(defun summary (violations)
  (mapcar (lambda (v)
            (list (violation-kind v) (violation-file v) (violation-layer v)
                  (violation-dependency v) (violation-dependency-layer v)))
          violations))

(deftest kept-layers
  (ok (null (layer-violations 'lenient)) "nothing is reported when every dependency is allowed"))

(deftest broken-layers
  (ok (equal (summary (layer-violations 'strict))
             '((:layer "okite-fixture/infra/store" :infra
                "okite-fixture/usecases/create" :usecases)
               (:library "okite-fixture/web/page" :web "somelib/extra" (:usecases))))
      "a layer outside :allow and a library outside its layers are reported, in file order"))

(deftest a-violation-reads-as-a-sentence
  (ok (equal (mapcar #'princ-to-string (layer-violations 'strict))
             '("okite-fixture/infra/store (infra) uses okite-fixture/usecases/create (usecases)"
               "okite-fixture/web/page (web) uses somelib/extra, which is for usecases"))))

(define-layers without-web
  (:system "okite-fixture")
  (:ignore "scripts/")
  (:layers (:domain "domain/")
           (:usecases "usecases/")
           (:infra "infra/")
           (:main "main"))
  (:allow (:usecases :domain)
          (:infra :domain :usecases)
          (:main :infra))
  (:anywhere "somelib"))

(deftest a-file-in-no-layer
  (ok (equal (summary (layer-violations 'without-web))
             '((:unplaced "okite-fixture/web/page" nil nil nil)))
      "the file is reported once, and main's use of it is not reported again"))

(define-layers scripts-included
  (:system "okite-fixture")
  (:layers (:domain "domain/")
           (:usecases "usecases/")
           (:infra "infra/")
           (:web "web/")
           (:main "main"))
  (:allow (:usecases :domain)
          (:infra :domain :usecases)
          (:web :usecases)
          (:main :infra :web))
  (:libraries ("somelib" :usecases :web)))

(deftest files-that-are-not-part-of-the-system
  (ok (equal (summary (layer-violations 'scripts-included))
             '((:unplaced "okite-fixture/scripts/run" nil nil nil)))
      "a file :ignore leaves out is looked at; one under a dot directory never is"))

(define-layers longest-wins
  (:system "okite-fixture")
  (:ignore "scripts/")
  (:layers (:domain "domain/")
           (:entities "domain/entity")
           (:usecases "usecases/")
           (:infra "infra/")
           (:web "web/")
           (:main "main"))
  (:allow (:usecases :entities)
          (:infra :entities :usecases)
          (:web :usecases)
          (:main :infra :web))
  (:anywhere "somelib"))

(deftest the-longest-pattern-wins
  (ok (equal (summary (layer-violations 'longest-wins))
             '((:layer "okite-fixture/domain/rule" :domain
                "okite-fixture/domain/entity" :entities)))
      "domain/entity is in :entities though domain/ matches it too"))

(define-layers no-somelib
  (:system "okite-fixture")
  (:ignore "scripts/")
  (:layers (:domain "domain/")
           (:usecases "usecases/")
           (:infra "infra/")
           (:web "web/")
           (:main "main"))
  (:allow (:usecases :domain)
          (:infra :domain :usecases)
          (:web :usecases)
          (:main :infra :web))
  (:forbid "somelib"))

(define-layers longest-library-wins
  (:system "okite-fixture")
  (:ignore "scripts/")
  (:layers (:domain "domain/")
           (:usecases "usecases/")
           (:infra "infra/")
           (:web "web/")
           (:main "main"))
  (:allow (:usecases :domain)
          (:infra :domain :usecases)
          (:web :usecases)
          (:main :infra :web))
  (:libraries ("somelib" :usecases :web)
              ("somelib/extra" :usecases)))

(deftest the-longest-library-name-wins
  (ok (equal (summary (layer-violations 'longest-library-wins))
             '((:library "okite-fixture/web/page" :web "somelib/extra" (:usecases))))
      "\"somelib/extra\" decides, though \"somelib\" covers it too and comes first"))

(deftest forbidden-systems
  (ok (equal (summary (layer-violations 'no-somelib))
             '((:forbidden "okite-fixture/usecases/create" :usecases "somelib" nil)
               (:forbidden "okite-fixture/web/page" :web "somelib/extra" nil)))))

(define-layers somelib-anywhere
  (:system "okite-fixture")
  (:ignore "scripts/")
  (:layers (:domain "domain/")
           (:usecases "usecases/")
           (:infra "infra/")
           (:web "web/")
           (:main "main"))
  (:allow (:usecases :domain)
          (:infra :domain :usecases)
          (:web :usecases)
          (:main :infra :web))
  (:anywhere "somelib"))

(deftest a-library-anywhere
  (ok (null (layer-violations 'somelib-anywhere)) "every layer may use it"))

(define-layers somelib-unlisted
  (:system "okite-fixture")
  (:ignore "scripts/")
  (:layers (:domain "domain/")
           (:usecases "usecases/")
           (:infra "infra/")
           (:web "web/")
           (:main "main"))
  (:allow (:usecases :domain)
          (:infra :domain :usecases)
          (:web :usecases)
          (:main :infra :web))
  (:libraries ("some" :usecases :web)))

(deftest an-unlisted-library
  (ok (equal (summary (layer-violations 'somelib-unlisted))
             '((:unlisted "okite-fixture/usecases/create" :usecases "somelib" nil)
               (:unlisted "okite-fixture/web/page" :web "somelib/extra" nil)
               (:unused nil nil "some" nil)))
      "a library nothing lists is reported where it is used, and so is a name that covers nothing")
  (ok (equal (mapcar #'princ-to-string (layer-violations 'somelib-unlisted))
             '("okite-fixture/usecases/create (usecases) uses somelib, which is not listed"
               "okite-fixture/web/page (web) uses somelib/extra, which is not listed"
               "\"some\" covers nothing a file uses"))))

(define-layers unused-names
  (:system "okite-fixture")
  (:ignore "scripts/")
  (:layers (:domain "domain/")
           (:usecases "usecases/")
           (:infra "infra/")
           (:web "web/")
           (:main "main"))
  (:allow (:usecases :domain)
          (:infra :domain :usecases)
          (:web :usecases)
          (:main :infra :web))
  (:libraries ("somelib" :usecases :web) ("otherlib" :infra))
  (:anywhere "thirdlib")
  (:forbid "forbiddenlib"))

(deftest names-that-cover-nothing
  (ok (equal (summary (layer-violations 'unused-names))
             '((:unused nil nil "otherlib" nil)
               (:unused nil nil "thirdlib" nil)))
      "in :libraries and :anywhere, but not in :forbid"))

(define-layers web-unplaced-with-its-library
  (:system "okite-fixture")
  (:ignore "scripts/")
  (:layers (:domain "domain/")
           (:usecases "usecases/")
           (:infra "infra/")
           (:main "main"))
  (:allow (:usecases :domain)
          (:infra :domain :usecases)
          (:main :infra))
  (:libraries ("somelib" :usecases)
              ("somelib/extra" :usecases)))

(deftest a-file-in-no-layer-still-uses-its-libraries
  (ok (equal (summary (layer-violations 'web-unplaced-with-its-library))
             '((:unplaced "okite-fixture/web/page" nil nil nil)))
      "somelib/extra, which only web/page uses, is not reported unused"))

(define-layers names-in-capitals
  (:system "okite-fixture")
  (:ignore "scripts/")
  (:layers (:domain "domain/")
           (:usecases "usecases/")
           (:infra "infra/")
           (:web "web/")
           (:main "main"))
  (:allow (:usecases :domain)
          (:infra :domain :usecases)
          (:web :usecases)
          (:main :infra :web))
  (:libraries ("SomeLib" :usecases :web))
  (:anywhere "SomeLib/Extra"))

(deftest names-are-read-downcased
  (ok (null (layer-violations 'names-in-capitals))
      "as the systems ASDF names are"))

(deftest ensure-layers-signals
  (ok (null (ensure-layers 'lenient)))
  (ok (signals (ensure-layers 'strict) 'violation-error))
  (ok (= 2 (length (handler-case (ensure-layers 'strict)
                     (violation-error (e) (violation-error-violations e))))))
  (ok (search "infra/store (infra) uses"
              (handler-case (ensure-layers 'strict)
                (violation-error (e) (princ-to-string e))))))

(deftest the-system-defaults-to-the-name
  (ok (equal (okite/layers::definition-system
              (okite/layers::make-definition 'my-app '((:layers (:a "a/")))))
             "my-app"))
  (ok (equal (okite/layers::definition-system
              (okite/layers::make-definition 'strict '((:system "my-app") (:layers (:a "a/")))))
             "my-app")))

(deftest what-a-name-covers
  (let ((covers-p (lambda (name dependency separators)
                    (okite/layers::covers-p name dependency separators))))
    (ok (funcall covers-p "lack" "lack" '(#\/ #\-)))
    (ok (funcall covers-p "lack" "lack/request" '(#\/ #\-)))
    (ok (funcall covers-p "lack" "lack-middleware-session" '(#\/ #\-)) "a library covers its extensions")
    (ng (funcall covers-p "lack" "lackey" '(#\/ #\-)))
    (ng (funcall covers-p "koya" "koya-server" '(#\/)) "a forbidden name does not")))

(deftest a-definition-is-checked-when-it-is-made
  (flet ((make (&rest clauses) (okite/layers::make-definition 'bad clauses)))
    (ok (signals (make '(:system "x"))) "no :layers")
    (ok (signals (make '(:system "x") '(:layers (:a "a/")) '(:allow (:a :b)))) "an unknown layer in :allow")
    (ok (signals (make '(:system "x") '(:layers (:a "a/")) '(:libraries ("lib" :b)))) "an unknown layer in :libraries")
    (ok (signals (make '(:system "x") '(:layers (:a "a/") (:a "b/")))) "a layer twice")
    (ok (signals (make '(:system "x") '(:layers (:a "a/") (:b "a/")))) "a pattern in two layers")
    (ok (signals (make '(:system "x") '(:layers (:a "a/")) '(:alow (:a)))) "an unknown clause")
    (ok (signals (make '(:system "x") '(:layers (:a "a/") (:b "b/")) '(:allow (:a :b) (:a :a))))
        "a layer given twice in :allow")
    (ok (signals (make '(:system "x") '(:layers (:a "")))) "an empty pattern")
    (ok (signals (make '(:system "x") '(:layers (:a "a/")) '(:ignore ""))) "an empty pattern in :ignore")
    (ok (signals (make '(:system "x") '(:layers (:a "a/")) '(:anywhere :lib))) "a name in :anywhere that is not a string")
    (ok (signals (make '(:system "x") '(:layers (:a "a/")) '(:libraries ("lib" :a)) '(:anywhere "lib")))
        "a library in :libraries and :anywhere")
    (ok (signals (make '(:system "x") '(:layers (:a "a/")) '(:libraries ("lib" :a) ("lib" :a))))
        "a library twice in :libraries")
    (ok (signals (make '(:system "x") '(:layers (:a "a/")) '(:libraries ("lib"))))
        "a library no layer is given")
    (ok (signals (make '(:system "x") '(:layers (:a "a/")) '(:libraries ("lib" :a)) '(:anywhere "LIB")))
        "a library in :libraries and :anywhere, whatever its case")))
