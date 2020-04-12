(in-package :ant-design)

(defun antd-css-rules (css-pathname)
  (with-open-file (stream css-pathname)
    (let ((rules (parse-list-of-rules stream)))
      (loop for rule in rules
            when (typep rule 'qualified-rule)
            append (let ((selectors (split-selector-tokens (rule-prelude rule)))
                         (declarations (parse-list-of-declarations (rule-block rule))))
                     (list (apply 'rule
                                  selectors
                                  declarations)))))))

(defun search-antd-css-rules (css-pathname class)
  (let ((rules (antd-css-rules css-pathname)))
    (loop for rule in rules
          when (loop for selector in (rule-selector rule)
                     when (search class selector)
                     do (return t))
          collect rule)))

(defun transform-rule (rule)
  (let ((selector '()))
    (if (= 1 (length (rule-selector rule)))
        (setf selector (first (rule-selector rule)))
      (setf selector (rule-selector rule)))
    `(rule ,selector
       ,@(loop for declaration in (rule-declarations rule)
               collect `(property ,(declaration-name declaration)
                                  ,(css::serialize-tokens (declaration-value declaration)))))))

(defun print-rule (rule &optional (stream *standard-output*))
  (let ((form (transform-rule rule)))
    (let ((*print-pretty* t)
          (*print-case* :downcase))
      (pprint-logical-block (stream form :prefix "(" :suffix ")")
        (format stream "~S" (pprint-pop))
        (let ((selector (pprint-pop)))
          (typecase selector
            (string (format stream " ~S" selector))
            (list (format stream " '~S" selector))))
        (loop for declaration = (pprint-pop)
              while declaration
              do (progn
                   (pprint-indent :block 1 stream)
                   (pprint-newline :mandatory stream)
                   (format stream "~S" declaration)))))))

(defun split-selector-tokens (tokens)
  (let ((groups (split-sequence 'comma-token tokens
                                :key 'type-of)))
    (loop for group in groups
          for selector = (css::serialize-tokens group)
          collect (string-trim '(#\Space) selector))))

;; (antd-css-rules #p"c:/users/xh/desktop/antd.css")

(defun print-antd-rules-for-class ()
  (let ((rules (search-antd-css-rules #p"c:/users/xh/desktop/antd.css" ".ant-btn")))
    (loop for rule in rules
          for declarations = (rule-declarations rule)
          do (progn
               (print-rule rule)
               (format t "~%")))))

