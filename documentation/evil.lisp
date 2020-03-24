(in-package :documentation)

;; https://github.com/sbcl/sbcl/blob/master/src/code/print.lisp#L400

(defun remove-package-names (form)
  (if (listp form)
      (map-tree
       (lambda (form)
         (when form
           (if (listp form)
               (if (and (symbolp (car form))
                        (symbol-package (car form))
                        (not (or (eq (symbol-package (car form))
                                     (find-package :cl))
                                 (eq (symbol-package (car form))
                                     (find-package :keyword)))))
                   (cons (make-symbol (symbol-name (car form))) (rest form))
                   form)
               form)))
       form)
      form))

;; (remove-package-names '(json:encode (json:array '(1 2 3))))

(defmacro evil (form &optional package)
  (let* ((form1 (remove-package-names form))
         (form-string (with-output-to-string (stream)
                        (let ((*print-case* :downcase)
                              (*print-gensym* nil)
                              (*print-right-margin* 70))
                          (pprint form1 stream))))
         (multiple-lines-p (find #\Newline (subseq form-string 1))))
    (when multiple-lines-p
      (setf form-string (concatenate 'string form-string (string #\Newline))))
    `(pre ,form-string
          (span :class "evaluates-to" "⇒")
          (let ((string (if ,package
                            (let ((*package* (find-package ,package)))
                              (with-output-to-string (stream)
                                (prin1 ,form stream)))
                            (with-output-to-string (stream)
                              (prin1 ,form stream)))))
            (setf string (cl-ppcre:regex-replace "\\s+([A-Z0-9]+)?\\>$" string ">"))))))
