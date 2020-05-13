(in-package :html-test)

(defun tokenize-string (string)
  (with-input-from-string (stream string)
    (let ((tokenizer (make-instance 'html::tokenizer :stream stream)))
      (loop for token = (html::tokenize tokenizer :errorp t)
            until (typep token 'html::end-of-file)
            collect token))))

