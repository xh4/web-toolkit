(in-package :http)

(defclass handler-class (standard-class)
  ((function
    :initarg :function
    :initform nil)
   (%function
    :initform nil
    :accessor handler-function)
   (function-lambda-list
    :initarg :function-lambda-list
    :initform nil
    :accessor handler-function-lambda-list)))

(defmethod validate-superclass ((class handler-class) (super-class standard-class))
  t)

(defun check-handler-function (function)
  (typecase function
    (null "Missing handler function body")
    (symbol (if (ignore-errors (symbol-function function))
                (check-handler-function (symbol-function function))
                (error "Symbol not associate with function")))
    ((or function cl-cont::funcallable/cc)
     (let ((function-lambda-list (function-lambda-list function)))
       (check-handler-function-lambda-list function-lambda-list)))
    (t (error "Bad handler function ~A" function))))

(defun check-handler-function-lambda-list (lambda-list)
  (when (> (length lambda-list) 2)
    (error "Bad handler function lambda list ~A" lambda-list)))

(defmethod shared-initialize :after ((class handler-class) slot-names &key function &allow-other-keys)
  (declare (ignore slot-names))
  (if function
      (progn (setf function (eval (car function)))
             (check-handler-function function)
             (let ((function-lambda-list (function-lambda-list function)))
               (setf (slot-value class '%function) function)
               (setf (slot-value class 'function-lambda-list) function-lambda-list)))
      (progn (setf (slot-value class '%function) nil
                   (slot-value class 'function-lambda-list) nil))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *request* nil)
  (defvar *response* nil)
  (defvar *handler* nil)
  (defvar *next-handlers* nil)

  (defmacro call-next-handler ()
    (with-gensyms (result)
      `(if *next-handlers*
           (let ((*handler* (first *next-handlers*))
                 (*next-handlers* (rest *next-handlers*)))
             (let ((,result (call-handler *handler* *request*)))
               (when (or (typep ,result 'response)
                         (typep ,result 'entity))
                 (setf *response* ,result))
               *response*))
           *response*)))

  (defmacro next-handler ()
    `(first *next-handlers*))

  (define-condition abort-handler () ())

  (defmacro abort-handler ()
    `(signal 'abort-handler)))

;; Mapping from handler names (class names of handlers) to handler instances
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *static-handlers* (make-hash-table)))

(defclass handler ()
  ()
  (:metaclass handler-class)
  (:function (lambda (request) (call-next-handler))))

(defmethod handler-function ((handler handler))
  (handler-function (class-of handler)))

(defmethod (setf handler-function) (function (handler handler))
  (check-handler-function function)
  (setf (handler-function (class-of handler)) function))

(defmethod handler-function-lambda-list ((handler handler))
  (handler-function-lambda-list (class-of handler)))

(defmethod (setf handler-function-lambda-list) (lambda-list (handler handler))
  (check-handler-function-lambda-list lambda-list)
  (setf (handler-function-lambda-list (class-of handler)) lambda-list))

(defmacro define-handler (handler-name superclasses slots &rest options)
  (unless (find 'application-handler superclasses)
    (appendf superclasses '(application-handler)))
  (let ((function (second (find :function options :key 'first)))
        (metaclass (second (find :metaclass options :key 'first)))
        (instanize (if-let ((option (find :instanize options :key 'first)))
                     (second option)
                     t)))
    (let ((options (remove-if (lambda (options)
                                (member (first options) '(:instanize)))
                              options)))
      (unless metaclass
        (rewrite-class-option options :metaclass handler-class))
      `(progn
         (eval-when (:compile-toplevel :load-toplevel :execute)
           (defclass ,handler-name ,superclasses
             ,slots
             ,@options)
           ,@(if instanize
               `((defvar ,handler-name
                   (make-instance ',handler-name))
                 (setf (gethash ',handler-name *static-handlers*) ,handler-name)
                 ,handler-name)
               `((remhash ',handler-name *static-handlers*)
                 (find-class ',handler-name))))))))

(defgeneric handler-class-precedence-list (handler-class)
  (:method ((handler-class handler-class))
    (compute-handler-class-precedence-list handler-class))
  (:method ((handler handler))
    (compute-handler-class-precedence-list (class-of handler))))

(defun compute-handler-class-precedence-list (handler-class)
  (let ((handler-classes (compute-class-precedence-list handler-class))
        (root-handler-class (find-class 'handler)))
    (remove-if-not
     (lambda (handler-class)
       (subclassp handler-class root-handler-class))
     handler-classes)))

(defun make-handler (handler-class)
  (make-instance handler-class))

(defun compute-handler-precedence-list (handler)
  (let ((handler-classes (handler-class-precedence-list handler)))
    (let ((handlers (loop for handler-class in handler-classes
                       for handler-instance = (gethash (class-name handler-class)
                                                       *static-handlers*)
                       when handler-instance collect handler-instance
                       else collect (make-handler handler-class))))
      (typecase handler
        (handler (cons handler (rest handlers)))
        (t handlers)))))

(defun invoke-handler (handler request)
  (check-type handler handler)
  (let ((*request* request)
        (*response* (make-instance 'response))
        (*next-handlers* (reverse (compute-handler-precedence-list handler))))
    (block nil
      (handler-bind ((abort-handler
                      (lambda (c)
                        (declare (ignore c))
                        (return *response*)))
                     (redirect
                      (lambda (c)
                        (with-slots (location status) c
                          (reply (status status))
                          (reply (header "Location" (case location
                                                      ;; TODO: handle referer missing condition
                                                      (:back (header-field-value
                                                              (find-header-field
                                                               "Referer"
                                                               request)))
                                                      (t location))
                                         "Content-Length" 0))
                          (return)))))
        (call-next-handler)))
    *response*))

(defun call-handler (handler request)
  (check-type handler handler)
  (when-let ((function (handler-function handler)))
    (let ((function-lambda-list (handler-function-lambda-list handler)))
      (cond
        ((= 0 (length function-lambda-list))
         (funcall function))
        ((= 1 (length function-lambda-list))
         (funcall function request))
        ((= 2 (length function-lambda-list))
         (funcall function handler request))))))

(defun handler-form (form)
  (typecase form
    (symbol form)
    (list (let ((object (eval form)))
            (typecase object
              (handler object)
              ((or function funcallable/cc)
               (let ((function object)
                     (handler (make-instance 'anonymous-handler)))
                 (check-handler-function function)
                 (setf (handler-function handler) function
                       (handler-function-lambda-list handler) (function-lambda-list function))
                 handler))
              (t "~A evaluate to ~A which is not a valid handler form" form object))))
    (t (error "Bad handler form ~A" form))))
