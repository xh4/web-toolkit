(in-package :http)

(defclass server-class (standard-class) ())

(defmethod validate-superclass ((class server-class) (super-class standard-class))
  t)

(defclass server ()
  ((handler
    :initarg :handler
    :initform nil
    :accessor server-handler)
   (listener
    :initarg :listener
    :initform nil
    :accessor server-listener)
   (started-p
    :initarg :started-p
    :initform nil
    :accessor server-started-p))
  (:metaclass server-class))

(defun make-handler-slot-definition (handler-option)
  `(handler
    :initarg :handler
    :initform ,handler-option
    :accessor server-handler))

(defun make-listener-slot-definition (listener-option)
  `(listener
    :initarg :listener
    :initform ,listener-option
    :accessor server-listener))

(defmacro define-server (server-name superclasses slots &rest options)
  (let ((instanize (let ((option (find :instanize options :key 'first)))
                     (if (> (length option) 1)
                         (second option)
                         t)))
        (server-name-boundp (boundp server-name)))
    `(progn
       (eval-when (:compile-toplevel :load-toplevel :execute)
         (define-server-class ,server-name ,superclasses ,slots ,@options))
       (eval-when (:load-toplevel :execute)
         (when (and ,instanize (boundp ',server-name))
           (update-server-instance ,server-name ,options)))
       (eval-when (:compile-toplevel :load-toplevel :execute)
         (if ,instanize
             (define-server-instance ,server-name)
             (find-class ',server-name))))))

(defmacro define-server-class (server-name superclasses slots &rest options)
  (unless (find 'server superclasses)
    (appendf superclasses `(server)))
  (let ((handler (second (find :handler options :key 'first)))
        (listener (second (find :listener options :key 'first))))
    (appendf slots `(,(make-handler-slot-definition handler)
                     ,(make-listener-slot-definition listener)))
    (let ((class-options (remove-if
                          (lambda (option)
                            (member (first option) '(:handler :listener :instanize)))
                          options)))
      (rewrite-class-option class-options :metaclass server-class)
      `(defclass ,server-name ,superclasses ,slots ,@class-options))))

(defmacro define-server-instance (server-name)
  `(progn
     (defvar ,server-name (make-instance ',server-name))
     (loop for listener in (ensure-list (server-listener ,server-name))
        do (setf (listener-server listener) ,server-name))
     ,server-name))

(defmacro update-server-instance (server-name options)
  (let ((handler (second (find :handler options :key 'first)))
        (listener (second (find :listener options :key 'first))))
    `(update-server ,server-name :handler ,handler :listener ,listener)))

(defun update-server (server &key handler listener)
  (setf (server-handler server) handler)
  (let ((current-listeners (ensure-list (server-listener server)))
        (target-listeners (ensure-list listener))
        (listeners-to-add '())
        (listeners-to-remove '()))
    (loop for listener in target-listeners
       unless (find-if (lambda (current-listener)
                         (and (= (listener-port listener)
                                 (listener-port current-listener))
                              (equal (listener-address listener)
                                     (listener-address current-listener))))
                       current-listeners)
       do (appendf listeners-to-add (list listener)))
    (loop for listener in current-listeners
       unless (find-if (lambda (target-listener)
                         (and (= (listener-port listener)
                                 (listener-port target-listener))
                              (equal (listener-address listener)
                                     (listener-address target-listener))))
                       target-listeners)
       do (appendf listeners-to-remove (list listener)))
    (loop for listener in listeners-to-add
       do (add-listener server listener))
    (loop for listener in listeners-to-remove
       do (remove-listener server listener))))

(defun pprint-server (server stream)
  (let ((*print-pretty* t))
    (pprint-logical-block (stream nil)
      (pprint-indent :block (indent-relative-to-object-name server 1) stream)
      (pprint-newline :mandatory stream)
      (write-string "Handler:" stream)
      (pprint-indent :block (indent-relative-to-object-name server 3) stream)
      (pprint-newline :mandatory stream)
      (format stream "~A" (server-handler server))

      (pprint-indent :block (indent-relative-to-object-name server 1) stream)
      (pprint-newline :mandatory stream)
      (write-string "Listeners:" stream)
      (loop for listener in (ensure-list (server-listener server))
         do
           (pprint-indent :block (indent-relative-to-object-name server 3) stream)
           (pprint-newline :mandatory stream)
           (format stream "~A" listener))
      (pprint-indent :block (indent-relative-to-object-name server -2) stream)
      (pprint-newline :mandatory stream))))

(defmethod print-object ((server server) stream)
  (print-unreadable-object (server stream :type t :identity t)
    (if *print-pretty*
        (pprint-server server stream))))

(defmethod initialize-instance :after ((server server) &key))

(defgeneric start-server (server &key))

(defmethod start-server ((server server) &key)
  (loop for listener in (ensure-list (server-listener server))
     do
       (handler-case
           (start-listener listener)
         (error (e)
           ;; (stop-server server)
           (error e)))
     finally
       (setf (server-started-p server) t))
  server)

(defgeneric stop-server (server &key))

(defmethod stop-server ((server server) &key)
  (loop for listener in (ensure-list (server-listener server))
     do
       (stop-listener listener)
     finally
       (setf (server-started-p server) nil))
  server)

(defgeneric add-listener (server listener))

(defmethod add-listener ((server server) (listener listener))
  (setf (listener-server listener) server)
  (when (server-started-p server)
    (start-listener listener))
  (let ((listeners (append (ensure-list (server-listener server))
                           (list listener))))
    (setf (server-listener server) listeners)))

(defgeneric remove-listener (server listener))

(defmethod remove-listener ((server server) (listener listener))
  (when (server-started-p server)
    (stop-listener listener))
  (setf (listener-server listener) nil)
  (setf (server-listener server)
        (remove listener (ensure-list (server-listener server)))))

(defmethod (setf server-handler) (handler (server server))
  (setf (slot-value server 'handler) handler))
