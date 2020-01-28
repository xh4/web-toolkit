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

(defun make-handler-slot-definitions (type lambda-list body)
  (let ((handler-slot-name (case type
                             (:open 'open-handler)
                             (:close 'close-handler)
                             (:error 'error-handler)
                             (:message 'message-handler)))
        (handler-code-slot-name (case type
                                  (:open 'open-handler-code)
                                  (:close 'close-handler-code)
                                  (:error 'error-handler-code)
                                  (:message 'message-handler-code))))
    (let ((handler-accessor-name
           (intern (case type
                     ((or :open :close :error)
                      (format nil "ENDPOINT-~A" handler-slot-name))
                     (:message
                      (format nil "SESSION-~A" handler-slot-name)))))
          (handler-code-accessor-name
           (intern (case type
                     ((or :open :close :error)
                      (format nil "ENDPOINT-~A" handler-code-slot-name))
                     (:message
                      (format nil "SESSION-~A" handler-code-slot-name))))))
      `((,handler-slot-name
         :initarg ,(make-keyword handler-slot-name)
         :initform (make-handler ,lambda-list ,@body)
         :accessor ,handler-accessor-name
         :allocation :class)
        (,handler-code-slot-name
         :initarg ,(make-keyword handler-code-slot-name)
         :initform '(,lambda-list ,@body)
         :accessor ,handler-code-accessor-name
         :allocation :class)))))
