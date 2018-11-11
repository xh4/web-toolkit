(in-package :wt)

(defvar *request* nil)

(defvar *request-headers* nil)

(defvar *request-query-parameters* nil)

(defvar *request-body-parameters* nil)

(defvar *response* nil)

(defclass handler ()
  ((bindings :initarg :bindings :initform nil :accessor handler-bindings)
   (thunk :initarg :thunk :initform (lambda ()) :accessor handler-thunk)))

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

(defun request-header (name)
  (assocdr-cases name *request-headers*))

(defun request-query-parameter (name)
  (assocdr-cases name *request-query-parameters*))

(defun request-body-parameter (name)
  (assocdr-cases name *request-body-parameters*))

(defun fetch-parameter (name)
  (or (request-header name)
      (request-query-parameter name)
      (request-body-parameter name)))

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
                          :thunk (lambda ()
                                   (let (,@(loop for binding in bindings
                                              collect (make-binding-let-form binding)))
                                     ,@lambda-forms))))))))

(defgeneric call-with-request (handler request)
  (:method (handler request)
    (let ((thunk (handler-thunk handler)))
      (let ((*request* request)
            (*request-headers* (request-headers request))
            (*request-query-parameters* (request-query-parameters request))
            (*request-body-parameters* (request-body-parameters request)))
        (funcall thunk)))))
