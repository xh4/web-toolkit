(in-package :http)

(defclass handler-class (standard-class) ())

(defmethod validate-superclass ((class handler-class) (super-class standard-class))
  t)

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
  `(signal 'condition/abort-handler))

;; Mapping from handler names (class names of handlers) to handler instances
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *static-handlers* (make-hash-table)))

(defclass handler () ()
  (:metaclass handler-class))

(defmethod handle ((handler handler) (request request))
  (call-next-handler))

(defvar handler (make-instance 'handler))

(setf (gethash 'handler *static-handlers*) handler)

(defmacro define-handler (handler-name superclasses slots &rest options)
  (unless (find 'handler superclasses)
    (appendf superclasses '(handler)))
  (let ((instanize (if-let ((option (find :instanize options :key 'first)))
                     (second option)
                     t)))
    (let ((options (remove-if (lambda (options)
                                (member (first options) '(:instanize)))
                              options)))
      (rewrite-class-option options :metaclass handler-class)
      `(progn
         (eval-when (:compile-toplevel :load-toplevel :execute)
           (defclass ,handler-name ,superclasses
             ,slots
             ,@options)
           ,@(when instanize
               `((defvar ,handler-name
                   (make-instance ',handler-name))
                 (setf (gethash ',handler-name *static-handlers*) ,handler-name))))))))

(defgeneric handle (handler object))

(defun compute-handler-class-precedence-list (handler)
  (let ((handler-class
         (typecase handler
           (symbol (find-class handler))
           (handler (class-of handler))
           (standard-class handler))))
    (let ((handler-classes (compute-class-precedence-list handler-class))
          (root-handler-class (find-class 'handler)))
      (remove-if-not
       (lambda (handler-class)
         (subclassp handler-class root-handler-class))
       handler-classes))))

;; (compute-handler-class-precedence-list 'your-handler)
;; (compute-handler-class-precedence-list your-handler)

(defun compute-handler-precedence-list (handler)
  (let ((handler-classes (compute-handler-class-precedence-list handler)))
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

;; (compute-handler-precedence-list your-handler)

(defun invoke-handler (handler request)
  (let ((next-handlers (reverse (compute-handler-precedence-list handler)))
        (*response* (make-instance 'response
                                   :header (make-instance 'header))))
    (block finish-handling
      (labels ((%next-handler ()
                 (first next-handlers))

               (%call-next-handler ()
                 (let ((handler (%next-handler)))
                   (when handler
                     (%call-handler handler))))

               (%call-handler (handler)
                 (when (find-method #'handle
                                    '()
                                    (list (class-of handler) (find-class 'request))
                                    nil)
                   (%call-handler-with-request handler))
                 (when (find-method #'handle
                                    '()
                                    (list (class-of handler) (find-class t))
                                    nil)
                   (%call-handler-with-request handler)))

               (%call-handler-with-request (handler)
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
                   (let ((result (handle handler request)))
                     (when (typep result 'response)
                       (setf *response* result))))))

        (%call-next-handler)))
    *response*))

(define-handler default-handler ()
  ())

(defmethod handle ((handler default-handler) request)
  (reply
   (status 200)
   (header "Content-Type" "text/plain")
   "Lisp Web Toolkit"))
