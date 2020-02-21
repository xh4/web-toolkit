(in-package :html-test)

(in-suite :html-test)

(test document
  (is-true (typep (document) 'document))
  (is-true (typep (document (html)) 'document))
  (signals error (document (p))))
