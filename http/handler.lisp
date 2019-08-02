(in-package :http)

(define-condition next-handler () ())

(defmacro next-handler ()
  `(restart-case (signal 'next-handler)
     (%next-handler (handler) handler)))

(define-condition call-next-handler () ())

(defmacro call-next-handler ()
  `(restart-case (signal 'call-next-handler)
     (%call-next-handler (response) response)))

;; Mapping from handler names (class names of handlers) to handler instances
(defvar *handler-mapping-table* (make-hash-table))

(defclass handler () ())

(defmethod handle ((handler handler) (request request))
  (call-next-handler))

(defparameter handler (make-instance 'handler))

(setf (gethash 'handler *handler-mapping-table* ) handler)

(defmacro define-handler (name super-handlers slots &rest handler-options)
  (unless (find 'handler super-handlers)
    (appendf super-handlers '(handler)))
  `(progn
     (defclass ,name ,super-handlers
       ,slots
       ,@handler-options)
     (if (boundp ',name)
         (setf ,name (make-instance ',name))
         (defvar ,name
           (make-instance ',name)))
     (setf (gethash ',name *handler-mapping-table* ) ,name)))

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
  (let ((handlers (loop for handler-class in (compute-handler-class-precedence-list handler)
                     for handler-instance = (gethash (class-name handler-class) *handler-mapping-table*)
                     when handler-instance
                     collect handler-instance)))
    (typecase handler
      (handler (cons handler (rest handlers)))
      (t handlers))))

;; (compute-handler-precedence-list your-handler)

(defun invoke-handler (handler request)
  (let ((next-handlers (reverse (compute-handler-precedence-list handler)))
        (*response* (make-instance 'response
                                   :header (make-instance 'header))))
    (labels ((%next-handler ()
               (first next-handlers))

             (%call-next-handler ()
               (let ((handler (%next-handler)))
                 (when handler
                   (%call-handler handler))))

             (%call-handler (handler)
               (let ((method (find-method #'handle
                                          '()
                                          (list (class-of handler) (find-class 'request))
                                          nil)))
                 (when method
                   (%call-handler-method handler))))

             (%call-handler-method (handler)
               (setf next-handlers (rest next-handlers))
               (handler-bind ((next-handler (lambda (c)
                                              (declare (ignore c))
                                              (let ((next-handler (next-handler)))
                                                (break)
                                                (invoke-restart '%next-handler next-handler))))
                              (call-next-handler (lambda (c)
                                                   (declare (ignore c))
                                                   (call-next-handler)
                                                   (invoke-restart '%call-next-handler *response*))))
                 (let ((result (handle handler request)))
                   (when (typep result 'response)
                     (setf *response* result))))))

      (call-next-handler))
    *response*))
