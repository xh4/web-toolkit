(in-package :http)

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
(defvar *handler-mapping-table* (make-hash-table))

(defclass handler () ())

(defmethod handle ((handler handler) (request request))
  (call-next-handler))

(defparameter handler (make-instance 'handler))

(setf (gethash 'handler *handler-mapping-table* ) handler)

(defmacro define-handler (handler-name super-handlers slots &rest handler-options)
  (unless (find 'handler super-handlers)
    (appendf super-handlers '(handler)))
  `(progn
     (eval-when (:compile-toplevel :load-toplevel :execute)
       (defclass ,handler-name ,super-handlers
         ,slots
         ,@handler-options))
     (eval-when (:compile-toplevel)
       (defvar ,handler-name
         (make-instance ',handler-name)))
     (eval-when (:load-toplevel :execute)
       (if (boundp ',handler-name)
           (setf ,handler-name (make-instance ',handler-name))
           (defvar ,handler-name
             (make-instance ',handler-name))))
     (setf (gethash ',handler-name *handler-mapping-table* ) ,handler-name)))

(defgeneric handle (handler thing))

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
                       for handler-instance = (gethash (class-name handler-class) *handler-mapping-table*)
                       when handler-instance
                       collect handler-instance
                       unless handler-instance
                       do (format t "Handler class ~A has no instance~%" handler-class))))
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
