(in-package :css-test)

(defun what-to-implement ()
  (remove-duplicates
   (loop for (name . value) in (bootstrap-declarations)
      for index from 1
      append (let* ((symbol (multiple-value-bind (symbol scope)
                                (find-symbol (string-upcase name) :css)
                              symbol))
                    (function (when symbol
                                (ignore-errors
                                  (symbol-function symbol)))))
               (unless function
                 (list name))))
   :test 'equal))

(defmacro tokenize-string ((tokenizer string) &body body)
  `(with-input-from-string (stream ,string)
     (let ((,tokenizer (make-instance 'css::tokenizer :stream stream)))
       ,@body)))
