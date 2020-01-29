(in-package :websocket)

(defvar *handler-macros* '())

(defmacro define-handler-macro (name lambda-list &body body)
  (let ((macro (append (list name lambda-list) body)))
    `(eval-when (:compile-toplevel :load-toplevel :execute)
       (if-let ((pos (position ',name *handler-macros* :key 'first)))
         (setf (nth pos *handler-macros*) ',macro)
         (appendf *handler-macros* (list ',macro))))))

(defmacro make-handler (lambda-list &body body)
  `(lambda/cc ,lambda-list
     (macrolet (,@*handler-macros*)
       ,@body)))

(defun compute-class-precedence-list/0 (instance-or-class)
  (let ((class (cond
                 ((typep instance-or-class 'standard-class) instance-or-class)
                 (t (class-of instance-or-class)))))
    (loop for class in (compute-class-precedence-list class)
       when (or (typep class 'endpoint-class)
                (typep class 'session-class))
       collect class)))

(defun invoke-open-handler/1 (endpoint-class endpoint session)
  (when-let ((open-handler (endpoint-open-handler endpoint-class))
             (open-handler-code (endpoint-open-handler-code endpoint-class)))
    (let ((open-handler-lambda-list (first open-handler-code)))
      (let ((lambda-list-groups (multiple-value-list
                                 (parse-ordinary-lambda-list
                                  open-handler-lambda-list))))
        (let ((required-parameters (first lambda-list-groups)))
          (cond
            ((= 0 (length required-parameters)) (funcall open-handler))
            ((= 1 (length required-parameters)) (funcall open-handler session))
            ((= 2 (length required-parameters)) (funcall open-handler endpoint session))))))))

(defun invoke-open-handler (endpoint session)
  (let ((endpoint-classes (reverse
                           (compute-class-precedence-list/0 endpoint))))
    (loop for endpoint-class in endpoint-classes
       do (invoke-open-handler/1 endpoint-class endpoint session))))

(defun invoke-close-handler/1 (endpoint-class endpoint session code reason)
  (when-let ((close-handler (endpoint-close-handler endpoint-class))
             (close-handler-code (endpoint-close-handler-code endpoint-class)))
    (let ((close-handler-lambda-list (first close-handler-code)))
      (let ((lambda-list-groups (multiple-value-list
                                 (parse-ordinary-lambda-list
                                  close-handler-lambda-list))))
        (let ((required-parameters (first lambda-list-groups)))
          (cond
            ((= 0 (length required-parameters)) (funcall close-handler))
            ((= 1 (length required-parameters)) (funcall close-handler session))
            ((= 2 (length required-parameters)) (funcall close-handler endpoint session))))))))

(defun invoke-close-handler (endpoint session code reason)
  (let ((endpoint-classes (reverse
                  (compute-class-precedence-list/0 endpoint))))
    (loop for endpoint-class in endpoint-classes
       do (invoke-close-handler/1 endpoint-class endpoint session code reason))))

(defun invoke-error-handler/1 (endpoint-class endpoint session error)
  (when-let ((error-handler (endpoint-error-handler endpoint-class))
             (error-handler-code (endpoint-error-handler-code endpoint-class)))
    (let ((error-handler-lambda-list (first error-handler-code)))
      (let ((lambda-list-groups (multiple-value-list
                                 (parse-ordinary-lambda-list
                                  error-handler-lambda-list))))
        (let ((required-parameters (first lambda-list-groups)))
          (cond
            ((= 0 (length required-parameters)) (funcall error-handler))
            ((= 1 (length required-parameters)) (funcall error-handler error))
            ((= 2 (length required-parameters)) (funcall error-handler session error))
            ((= 3 (length required-parameters)) (funcall error-handler endpoint session error))))))))

(defun invoke-error-handler (endpoint session error)
  (let ((endpoint-classes (reverse
                  (compute-class-precedence-list/0 endpoint))))
    (loop for endpoint-class in endpoint-classes
       do (invoke-error-handler/1 endpoint-class endpoint session error))))

(defun invoke-message-handler/1 (session-class session message)
  (when-let ((message-handler (session-message-handler session-class))
             (message-handler-code (session-message-handler-code session-class)))
    (let ((message-handler-lambda-list (first message-handler-code)))
      (let ((lambda-list-groups (multiple-value-list
                                 (parse-ordinary-lambda-list
                                  message-handler-lambda-list))))
        (let ((required-parameters (first lambda-list-groups)))
          (cond
            ((= 0 (length required-parameters)) (funcall message-handler))
            ((= 1 (length required-parameters)) (funcall message-handler message))
            ((= 2 (length required-parameters)) (funcall message-handler session message))))))))

(defun invoke-message-handler (session message)
  (let ((session-classes (reverse
                          (compute-class-precedence-list/0 session))))
    (loop for session-class in session-classes
       do (invoke-message-handler/1 session-class session message))))
