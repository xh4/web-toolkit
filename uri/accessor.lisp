(in-package :uri)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun parse-lambda-list-keywords (lambda-list)
    (let ((keyword-specs (fourth
                          (multiple-value-list
                           (parse-ordinary-lambda-list lambda-list)))))
      (loop for ((keyword-name name)) in keyword-specs
         collect keyword-name))))

(defmacro define-uri-reader (component &body body)
  (let* ((component-symbol (or (and (listp component) (car component))
                               component))
         (lambda-list (and (listp component) (rest component)))
         (lambda-list-keys (parse-lambda-list-keywords lambda-list))
         (accessor-name (intern (format nil "URI-~A"
                                        (symbol-name component-symbol)))))
    `(defgeneric ,accessor-name (uri ,@(loop for key in lambda-list-keys
                                          collect (intern (symbol-name key))
                                          into keys
                                          finally (return
                                                    (when keys
                                                      (append '(&key) keys)))))
       (:method ((uri string) ,@lambda-list)
         (,accessor-name (uri uri)
                         ,@(loop for key in lambda-list-keys
                              append (list key
                                           (intern (symbol-name key))))))
       (:method ((uri uri) ,@lambda-list)
         ,@(typecase component
             (symbol `((slot-value uri ',component-symbol)))
             (list `((let ((,component-symbol (slot-value uri ',component-symbol)))
                       ,@body))))))))

(defmacro define-uri-writer (component)
  (let ((accessor-name (intern (format nil "URI-~A" (symbol-name component))))
        (checker-name (intern (format nil "CHECK-~A" (symbol-name component)))))
    (with-gensyms (checked-value)
      `(defgeneric (setf ,accessor-name) (value uri)
         (:method (value (uri uri))
           (let ((,checked-value (,checker-name value)))
             (setf (slot-value uri 'string) nil)
             (setf (slot-value uri ',component) ,checked-value)))))))

(define-uri-reader scheme)

(define-uri-writer scheme)

(define-uri-reader (userinfo &key (decode t))
  (if decode
      (percent-decode userinfo)
      userinfo))

(define-uri-writer userinfo)

(define-uri-reader (host &key (decode t))
  (if decode
      (percent-decode host)
      host))

(define-uri-writer host)

(define-uri-reader port)

(define-uri-writer port)

(define-uri-reader (path &key (decode t))
  (if decode
      (percent-decode path)
      path))

(define-uri-writer path)

(define-uri-reader (query &key (type) (decode nil))
    (case type
      (:alist (uri-query-alist query))
      (:plist (uri-query-plist query))
      (:hash-table (uri-query-hash-table query))
      (t (if decode
             (percent-decode query)
             query))))

(define-uri-writer query)

(define-uri-reader (fragment &key (decode t))
    (if decode
        (percent-decode fragment)
        fragment))

(define-uri-writer fragment)
