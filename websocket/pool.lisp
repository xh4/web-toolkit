(in-package :websocket)

(defclass session-pool ()
  ((sessions
    :initarg :sessions
    :initform nil
    :accessor session-pool-sessions)))

(defgeneric add-session (session-pool session)
  (:method ((session-pool session-pool) session)
    (pushnew session (session-pool-sessions session-pool))
    session))

(defgeneric remove-session (session-pool session)
  (:method ((session-pool session-pool) session)
    (let ((sessions (session-pool-sessions session-pool)))
      (setf (session-pool-sessions session-pool)
            (remove session sessions)))
    session))

(defmacro define-session-pool (session-pool-name superclasses slots &rest options)
  (let ((superclasses (if (find 'session-pool superclasses)
                          superclasses
                          (append superclasses (list 'session-pool))))
        (instance-symbol (second (find :instance options :key 'first)))
        (options (remove-if (lambda (option)
                              (member (first option) '(:instance)))
                            options)))
    `(eval-when (:compile-toplevel :load-toplevel :execute)
       (defclass ,session-pool-name ,superclasses
         ,slots
         ,@options)
       (defvar ,instance-symbol (make-instance ',session-pool-name)))))

(defmethod on-open :before (endpoint session)
  (when-let ((pool (slot-value session 'pool)))
    (add-session pool session)))

(defmethod on-close :after (endpoint session code &optional reason)
  (when-let ((pool (slot-value session 'pool)))
    (remove-session pool session)))

(defmethod on-error :after (endpoint session error)
  (when-let ((pool (slot-value session 'pool)))
    (remove-session pool session)))

(defmethod send-text ((session-pool session-pool) text &key)
  (loop for session in (session-pool-sessions session-pool)
     do (send-text session text)))

(defmethod send-binary ((session-pool session-pool) data &key)
  (loop for session in (session-pool-sessions session-pool)
     do (send-binary session data)))
