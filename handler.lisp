(in-package :wt)

(defvar *request* nil)

(defvar *response* nil)

(defclass handler ()
  ((bindings
    :initarg :bindings
    :initform nil
    :accessor handler-bindings)
   (function
    :initarg :function
    :initform (lambda ())
    :accessor handler-function)))

(defun option-form-p (form)
  (and (consp form) (keywordp (car form))))

(defun segment-handler-forms (forms)
  (let (option-forms
        lambda-forms
        option-form-finished)
    (loop for form in forms
       do
         (if (and (option-form-p form) (not option-form-finished))
             (appendf option-forms (list form))
             (progn
               (setf option-form-finished t)
               (appendf lambda-forms (list form))))
       finally
         (return (values option-forms lambda-forms)))))

(segment-handler-forms
 '((:bind abc)
   (:options)
   (+ 1 2 3)
   (/ 4 5 6)))

(defun parse-option-forms (forms)
  (let (bindings)
    (loop for form in forms
       do
         (case (car form)
           (:bind
               (loop for binding in (cdr form)
                  do
                    (cond
                      ((symbolp binding)
                       (appendf bindings `((:symbol ,binding))))
                      ((consp binding)
                       (let ((symbol (car binding)))
                         (appendf bindings `((:symbol ,symbol))))))))))
    bindings))

(defun assocdr-cases (name alist)
  (check-type name keyword)
  (loop for case-fn in '(snake-case camel-case param-case)
     for case = (funcall case-fn (symbol-name name))
     for value = (assoc case alist :test #'string-equal)
     when value
     do (return (cdr value))))

(defun make-binding-let-form (binding)
  (let ((symbol (getf binding :symbol)))
    `(,symbol (fetch-parameter ,(make-keyword symbol)))))

(parse-option-forms
 '((:bind
       foo
     (bar :from :query))))

(defmacro define-handler (name direct-superclasses &rest forms)
  ;; Check all super classes are sub class of handler
  (when (not (find 'handler direct-superclasses))
    (appendf direct-superclasses (list 'handler)))

  (multiple-value-bind (option-forms lambda-forms) (segment-handler-forms forms)
    (multiple-value-bind (bindings) (parse-option-forms option-forms)
      `(progn
         (defclass ,name ,direct-superclasses ())
         (defparameter ,name
           (make-instance ',name
                          :bindings ',bindings
                          :function (lambda ()
                                      (let (,@(loop for binding in bindings
                                                 collect (make-binding-let-form binding)))
                                        ,@lambda-forms))))))))

;; Note: 返回结果中包含 HANDLER 自身的 Class
;; TODO: 参数对于 Class 和 Instance 都适用
(defun handler-class-precedence-list (handler)
  (subtypep (type-of handler) 'handler)
  (let ((classes (class-precedence-list (class-of handler))))
    (remove-if-not
     (lambda (class)
       (subtypep class 'handler))
     classes)))

;; Note: 返回结果中包含 HANDLER 自身
;; TODO: 参数对于 Class 和 Instance 都适用
(defun handler-precedence-list (handler)
  (mapcan
   (lambda (handler-class)
     (let ((handler-name (class-name handler-class)))
       (when (boundp handler-name)
         (list (symbol-value handler-name)))))
   (handler-class-precedence-list handler)))

(defun call-with-request (handler request &optional (response *response*))
  (let ((*request* request)
        (*response* response))
    (let ((handlers (reverse (handler-precedence-list handler))))
      (loop for handler in handlers
         for function = (handler-function handler)
         do
           (funcall function)))
    *response*))
