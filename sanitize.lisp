(in-package :wt)

(defun parameter-blank-p (value)
  (if (vectorp value) (emptyp value) (blankp value)))

(defmacro define-sanitizer (name lambda-list &rest forms)
  (let* ((function-name (intern (concat "SANITIZE-" (symbol-name name))))
         (required-parameters (parse-ordinary-lambda-list lambda-list))
         (parameter (car required-parameters)))
    (when (> (length required-parameters) 1)
      (error "A sanitizer can accept only one variable, got ~A" required-parameters))
    `(defun ,function-name ,lambda-list
       (when (not (parameter-blank-p ,parameter))
         ,@forms))))

(defun sanitize-form-p (form)
  (and (consp form)
       (symbolp (first form))
       (string-prefix-p "SANITIZE-" (symbol-name (first form)))))

(defun expand-sanitize-form (form)
  (let ((variable (second form)))
    `(setq ,variable ,form)))

(defun expand-sanitize-forms (tree)
  (map-tree
   (lambda (form)
     (if (sanitize-form-p form)
         (throw nil (expand-sanitize-form form))
         form))
   tree
   nil))

(define-sanitizer integer (value)
  (parse-integer value :junk-allowed t))

(define-sanitizer json (value)
  (if (vectorp value)
      (json:decode-json-from-source (flexi-streams:octets-to-string value))
      (json:decode-json-from-source value)))
