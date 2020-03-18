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
