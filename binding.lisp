(in-package :wt)

(defun parse-lambda-list-keywords (lambda-list)
  (multiple-value-bind (v1 v2 v3 keyword-parameters v5)
      (parse-ordinary-lambda-list lambda-list)
    (declare (ignore v1 v2 v3 v5))
    (loop for param in keyword-parameters
       ;; Keyword parameters is normalized into form ((keyword-name name) init suppliedp)
       collect (cadar param))))


;; (defun make-parameter-binding-form (parameter)
;;   (if (string-prefix-p "$" (symbol-name parameter))
;;       `(,parameter ,(match parameter
;;                       ('$body '(lack.request:request-content request))))
;;       `(,parameter (fetch-parameter request ,(make-keyword parameter)))))

;; (defun make-parameter-binding-forms (lambda-list)
;;   (let ((parameters (parse-lambda-list-keywords lambda-list)))
;;     (loop for parameter in parameters
;;        collect (make-parameter-binding-form parameter))))
