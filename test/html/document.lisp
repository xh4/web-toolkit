(in-package :html-test)

(in-suite :html-test)

(test document
  (signals error (document))
  (is (typep (document (html)) 'document))
  (is (typep (document (p)) 'document)))
