(in-package :websocket)

(defclass endpoint ()
  ((session-class
    :initarg :session-class
    :initform 'session
    :accessor endpoint-session-class
    :allocation :class)
   (open-handler
    :initarg :open-handler
    :initform nil
    :accessor endpoint-open-handler
    :allocation :class)
   (open-handler-code
    :initarg :open-handler-code
    :initform nil
    :accessor endpoint-open-handler-code
    :allocation :class)
   (close-handler
    :initarg :close-handler
    :initform nil
    :accessor endpoint-close-handler
    :allocation :class)
   (close-handler-code
    :initarg :close-handler-code
    :initform nil
    :accessor endpoint-close-handler-code
    :allocation :class)
   (error-handler
    :initarg :error-handler
    :initform nil
    :accessor endpoint-error-handler
    :allocation :class)
   (error-handler-code
    :initarg :error-handler-code
    :initform nil
    :accessor endpoint-error-handler-code
    :allocation :class)))

(defun make-session-class-slot-definition (session-class)
  `(session-class
    :initarg :session-class
    :initform ,session-class
    :accessor endpoint-session-class
    :allocation :class))

(defmacro define-endpoint (endpoint-name superclasses slots &rest options)
  (let ((superclasses (if (find 'endpoint superclasses)
                          superclasses
                          (append superclasses (list 'endpoint)))))
    (let ((session-class (or (second (find :session-class options :key 'car))
                             'session))
          (on-open (rest (find :on-open options :key 'car)))
          (on-close (rest (find :on-close options :key 'car)))
          (on-error (rest (find :on-error options :key 'car))))
      (let ((slots (append slots
                           (list (make-session-class-slot-definition session-class))
                           (make-handler-slot-definitions
                            :open
                            (first on-open)
                            (rest on-open))
                           (make-handler-slot-definitions
                            :close
                            (first on-close)
                            (rest on-close))
                           (make-handler-slot-definitions
                            :error
                            (first on-error)
                            (rest on-error))))
            (options (remove-if (lambda (option)
                                  (member (first option) '(:session-class
                                                           :on-open
                                                           :on-close
                                                           :on-error)))
                                options)))
        (with-gensyms (s/endpoint s/request)
          `(progn
             (eval-when (:compile-toplevel :load-toplevel :execute)
               (defclass ,endpoint-name ,superclasses ,slots ,@options)
               (defvar ,endpoint-name (make-instance ',endpoint-name)))
             (eval-when (:load-toplevel :execute)
               (setf (endpoint-session-class ,endpoint-name) ,session-class
                     (endpoint-open-handler ,endpoint-name) (make-handler
                                                                ,(first on-open)
                                                              ,@(rest on-open))
                     (endpoint-open-handler-code ,endpoint-name) '(,(first on-open)
                                                                   ,@(rest on-open))
                     (endpoint-close-handler ,endpoint-name) (make-handler
                                                                 ,(first on-close)
                                                               ,@(rest on-close))
                     (endpoint-close-handler-code ,endpoint-name) '(,(first on-close)
                                                                    ,@(rest on-close))
                     (endpoint-error-handler ,endpoint-name) (make-handler
                                                                 ,(first on-error)
                                                               ,@(rest on-error))
                     (endpoint-error-handler-code ,endpoint-name) '(,(first on-error)
                                                                    ,@(rest on-error))))
             (defmethod http:handle ((,s/endpoint ,endpoint-name) (,s/request request))
               (handle-user-endpoint-request ,s/endpoint ,s/request))
             (eval-when (:execute)
               ,endpoint-name)))))))
