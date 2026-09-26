(defsystem "okite"
  :version "0.1.0"
  :description "Rules for what the files of a package-inferred system may depend on"
  :long-description #.(uiop:read-file-string
                       (uiop:subpathname *load-pathname* "README.md"))
  :author "Akira Tempaku"
  :maintainer "Akira Tempaku <paku@skyizwhite.dev>"
  :license "MIT"
  :class :package-inferred-system
  :pathname "src"
  :depends-on ("okite/main")
  :in-order-to ((test-op (test-op "okite-test"))))
