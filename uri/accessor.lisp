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
           (let ((,checked-value (handler-bind
                                     ((error
                                       (lambda (e)
                                         (error 'uri-component-error
                                                :component ,(make-keyword component)
                                                :value value
                                                :message (format nil "~A" e)))))
                                   (,checker-name value))))
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

(define-uri-reader (query &key (type :alist) (decode nil))
  (case type
    (:alist (uri-query-alist query))
    (:hash-table (uri-query-hash-table query))
    ((or nil :string) (if decode
                            (percent-decode query)
                            query))
    (t (error "Bad type ~A" type))))

(define-uri-writer query)

(define-uri-reader (fragment &key (decode t))
  (if decode
      (percent-decode fragment)
      fragment))

(define-uri-writer fragment)

(defun update-uri (uri &key (scheme nil scheme-present-p)
                       (userinfo nil userinfo-present-p)
                       (host nil host-present-p)
                       (port nil port-present-p)
                       (path nil path-present-p)
                       (query nil query-present-p)
                       (fragment nil fragment-present-p))
  (let ((uri (uri uri)))
    (handler-bind ((uri-component-error
                    (lambda (c)
                      (change-class c 'update-uri-error :uri uri))))
      (when scheme-present-p
        (setf (uri-scheme uri) scheme))
      (when userinfo-present-p
        (setf (uri-userinfo uri) userinfo))
      (when host-present-p
        (setf (uri-host uri) host))
      (when port-present-p
        (setf (uri-port uri) port))
      (when path-present-p
        (setf (uri-path uri) path))
      (when query-present-p
        (setf (uri-query uri) query))
      (when fragment-present-p
        (setf (uri-fragment uri) fragment)))
    uri))
