(in-package :http)

;; Mapping from handler names (class names of handlers) to handler instances
(defvar *handler-mapping-table* (make-hash-table))

(defclass handler () ())

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
  (let ((handlers (reverse (compute-handler-precedence-list handler)))
        (*response* (make-instance 'response)))
    (loop for handler in handlers
       do
         (let ((method (find-method #'handle
                                    '()
                                    (list (class-of handler) (find-class 'request))
                                    nil)))
           (when method
             (let ((result (handle handler request)))
               (when (typep result 'response)
                 (setf *response* result))))))
    *response*))

(defmacro next-handler ()
  )

(defmacro call-next-handler ()
  )
