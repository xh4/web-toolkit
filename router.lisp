(in-package :wt)

(defclass route (routes:route)
  ((mapping :initarg :mapping :initform nil :documentation "method -> handler")))

(defun normalize-path (path)
  "Add leading slash and remove tailing slash."
  (setf path (string-trim '(#\/) path))
  (if (string-prefix-p "/" path)
      path
      (concat "/" path)))

(defun make-route (mapper method template handler)
  (multiple-value-bind (route bindings) (routes:match mapper template)
    (declare (ignore bindings))
    (if route
        (let ((mapping (slot-value route 'mapping)))
          (push handler mapping)
          (push method mapping)
          (setf (slot-value route 'mapping) mapping))
        (let ((route (make-instance 'route
                                    :mapping `(,method ,handler)
                                    :template (routes:parse-template
                                               (normalize-path template)))))
          (routes:connect mapper route)))))

(defun match-route (router request-path request-method)
  (let ((mapper (router-mapper router)))
    (multiple-value-bind (route bindings) (routes:match mapper request-path)
      (when route
        (let ((route-mapping (slot-value route 'mapping)))
          (loop for (method handler) on route-mapping by #'cddr
             when (or (eq method :any)
                      (eq method request-method))
             do (return (values handler bindings))))))))

(defun match-route-with-request (router request)
  (let ((request-path (normalize-path (request-path request)))
        (request-method (request-method request)))
    (match-route router request-path request-method)))

(defclass router ()
  ((mapper :initarg :mapper :reader router-mapper)))

(defmacro make-router (&rest forms)
  `(let ((mapper (make-instance 'routes:mapper)))
     ,@(loop for form in forms
          collect (match form
                    ((list method template handler)
                     `(make-route mapper ,method ,template ,(if (symbolp handler)
                                                                `(intern (symbol-name ',handler))
                                                                handler)))))
     (make-instance 'router :mapper mapper)))

;; (make-router
;;  (:get "/" foo)
;;  (:post "/" (lambda (req)
;;               fwef)))
