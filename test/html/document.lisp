(in-package :html-test)

(in-suite :html-test)

(test document
  (is (typep (document) 'document))
  (is (typep (document (html)) 'document))
  (is (typep (document (p)) 'document)))
