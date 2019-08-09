(in-package :http)

(defclass server ()
  ((listeners
    :initarg :listeners
    :initform nil
    :accessor server-listeners)
   (handler
    :initarg :handler
    :initform nil
    :accessor server-handler)
   (started-p
    :initarg :started-p
    :initform nil
    :accessor server-started-p)))

(defmacro define-server (name &key listeners handler)
  `(let ((target-listeners ,listeners)
         (handler ,handler))
     (if (boundp ',name)
         ;; Update
         (let ((server ,name))
           (setf (server-handler server) handler)
           (let ((current-listeners (server-listeners server))
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
             (format t "Listeners to add: ~A~%" listeners-to-add)
             (loop for listener in listeners-to-add
                do (add-listener server listener))
             (format t "Listeners to remove: ~A~%" listeners-to-remove)
             (loop for listener in listeners-to-remove
                do (remove-listener server listener))))
         ;; New
         (progn
           (defvar ,name
             (make-instance 'server
                            :handler handler))
           (loop for listener in target-listeners
              do
                (add-listener ,name listener))))
     ,name))

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
      (loop for listener in (server-listeners server)
         do
           (pprint-indent :block (indent-relative-to-object-name server 3) stream)
           (pprint-newline :mandatory stream)
           (format stream "~A" listener))
      (pprint-indent :block (indent-relative-to-object-name server -2) stream)
      (pprint-newline :mandatory stream))))

(defmethod print-object ((server server) stream)
  (print-unreadable-object (server stream :type t :identity t)
    (pprint-server server stream)))

(defmethod initialize-instance :after ((server server) &key)
  )

(defgeneric start-server (server &key))

(defmethod start-server ((server server) &key)
  (loop for listener in (server-listeners server)
     do
       (handler-case
           (start-listener listener)
         (error (e)
           (stop-server server)
           (error e)))
     finally
       (setf (server-started-p server) t)))

(defgeneric stop-server (server &key))

(defmethod stop-server ((server server) &key)
  (loop for listener in (server-listeners server)
     do
       (stop-listener listener)
     finally
       (setf (server-started-p server) nil)))

(defgeneric add-listener (server listener))

(defmethod add-listener ((server server) (listener listener))
  (setf (listener-server listener) server)
  (when (server-started-p server)
    (start-listener listener))
  (appendf (server-listeners server) (list listener)))

(defgeneric remove-listener (server listener))

(defmethod remove-listener ((server server) (listener listener))
  (when (server-started-p server)
    (stop-listener listener))
  (setf (listener-server listener) nil)
  (setf (server-listeners server)
        (remove listener (server-listeners server))))

(defmethod (setf server-handler) (handler (server server))
  (setf (slot-value server 'handler) handler))
