(in-package :http)

(defclass handler-class (standard-class)
  ((function
    :initarg :function
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
    ((or function cl-cont::funcallable/cc))
    (symbol (unless (ignore-errors (symbol-function function))
              (error "Symbol not associate with function")))
    (t (error "Wrong function value"))))

(defun check-handler-function-lambda-list (lambda-list)
  (when (> (length lambda-list) 2)
    (error "Bad handler function lambda list")))

(defmethod shared-initialize :around ((class handler-class) slot-names
                                      &rest args
                                      &key name direct-slots direct-superclasses location
                                        extra-initargs direct-default-initargs documentation
                                        function
                                        &allow-other-keys)
  ;; (format t "Shared-initialize :around (handler-class): ~A~%" args)
  (if function
      (progn (setf function (eval (car function)))
             (check-handler-function function)
             (let ((function-lambda-list (function-lambda-list function)))
               (check-handler-function-lambda-list function-lambda-list)
               (setf (slot-value class 'function) function)
               (setf (slot-value class 'function-lambda-list) function-lambda-list)))
      (progn (setf (slot-value class 'function) nil
                   (slot-value class 'function-lambda-list) nil)))
  (if (getf args :name)
      ;; First initialize
      (call-next-method class slot-names
                        :name name
                        :direct-slots direct-slots
                        :direct-superclasses direct-superclasses
                        :location location)
      ;; Rest initialize
      (call-next-method class slot-names
                        :direct-slots direct-slots
                        :direct-superclasses direct-superclasses
                        :extra-initargs extra-initargs
                        :direct-default-initargs direct-default-initargs
                        :documentation documentation
                        :location location)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (define-condition condition/next-handler () ())

  (defmacro next-handler ()
    `(restart-case (signal 'condition/next-handler)
       (restart/next-handler (handler) handler)))

  (define-condition condition/call-next-handler () ())

  (defmacro call-next-handler ()
    `(restart-case (signal 'condition/call-next-handler)
       (restart/call-next-handler (response) response)))

  (define-condition condition/abort-handler () ())

  (defmacro abort-handler ()
    `(signal 'condition/abort-handler)))

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
  (setf (handler-function (class-of handler)) function))

(defmethod handler-function-lambda-list ((handler handler))
  (handler-function-lambda-list (class-of handler)))

(defvar handler (make-instance 'handler))

(setf (gethash 'handler *static-handlers*) handler)

(defmacro define-handler (handler-name superclasses slots &rest options)
  (unless (find 'handler superclasses)
    (appendf superclasses '(handler)))
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

(defun compute-handler-precedence-list (handler)
  (let ((handler-classes (handler-class-precedence-list handler)))
    (let ((handlers (loop for handler-class in handler-classes
                       for handler-instance = (gethash (class-name handler-class)
                                                       *static-handlers*)
                       when handler-instance
                       collect handler-instance
                       else
                       collect (make-instance handler-class))))
      (typecase handler
        (handler (cons handler (rest handlers)))
        (t handlers)))))

(defun invoke-handler (handler request)
  (let ((*response* (make-instance 'response
                                   :header (make-instance 'header)))
        (next-handlers (reverse (compute-handler-precedence-list handler))))
    (block finish-handling
      (labels ((%next-handler ()
                 (first next-handlers))

               (%call-next-handler ()
                 (let ((handler (%next-handler)))
                   (when handler
                     (%call-handler handler))))

               (%call-handler (handler)
                 (setf next-handlers (rest next-handlers))
                 (handler-bind ((condition/next-handler
                                 (lambda (c)
                                   (declare (ignore c))
                                   (let ((next-handler (%next-handler)))
                                     (invoke-restart 'restart/next-handler next-handler))))
                                (condition/call-next-handler
                                 (lambda (c)
                                   (declare (ignore c))
                                   (%call-next-handler)
                                   (invoke-restart 'restart/call-next-handler *response*)))
                                (condition/abort-handler
                                 (lambda (c)
                                   (declare (ignore c))
                                   (return-from finish-handling)))

                                (condition/redirect
                                 (lambda (c)
                                   (with-slots (location status) c
                                     (reply (header "Location" (case location
                                                                 (:back (header-field-value
                                                                         (header-field request "Referer")))
                                                                 (t location))))
                                     (reply (status status)))
                                   (return-from finish-handling))))
                   (let ((result (call-handler handler request)))
                     (when (or (typep result 'response)
                               (typep result 'entity))
                       (setf *response* result))))))
        (%call-next-handler)))
    *response*))

(defun call-handler (handler request)
  (when-let ((function (handler-function handler)))
    (let ((function-lambda-list (handler-function-lambda-list handler)))
      (cond
        ((= 0 (length function-lambda-list))
         (funcall function))
        ((= 1 (length function-lambda-list))
         (funcall function request))
        ((= 2 (length function-lambda-list))
         (funcall function handler request))))))
