(defsystem "okite-test"
  :class :package-inferred-system
  :pathname "tests"
  :depends-on ("rove"
               "okite-test/layers"
               "okite-test/interfaces")
  :perform (test-op (o c) (symbol-call :rove :run c :style :dot)))
