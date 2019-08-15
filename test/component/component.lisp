(in-package :component-test)

(in-suite :component-test)

(test define-component/tag
  (finishes (define-component navbar () ()))
  (finishes (define-component navbar ()
              ()
              (:tag-option root-tag))))
