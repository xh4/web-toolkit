(in-package :html)

(defmacro define-parser-insertion-mode (name &body body)
  (let ((function-name (intern (format nil "PROCESS-TOKEN-IN-~A-INSERTION-MODE" name))))
    `(defun ,function-name (parser)
       ,@(unless body '((declare (ignore parser))))
       (symbol-macrolet ((token (slot-value parser 'current-token))
                         (next-token nil)
                         (stack-of-open-elements (slot-value parser 'stack-of-open-elements)))
         (macrolet ()
           (flet ()
             ,@(if body
                   body
                 `((error "Parser ~A not implemented" ',name)))))))))

(define-parser-insertion-mode initial)
(define-parser-insertion-mode before-html)
(define-parser-insertion-mode before-head)
(define-parser-insertion-mode in-head)
(define-parser-insertion-mode in-head-noscript)
(define-parser-insertion-mode after-head)
(define-parser-insertion-mode in-body)
(define-parser-insertion-mode text)
(define-parser-insertion-mode in-table)
(define-parser-insertion-mode in-table-text)
(define-parser-insertion-mode in-caption)
(define-parser-insertion-mode in-column-group)
(define-parser-insertion-mode in-table-body)
(define-parser-insertion-mode in-row)
(define-parser-insertion-mode in-cell)
(define-parser-insertion-mode in-select)
(define-parser-insertion-mode in-select-in-table)
(define-parser-insertion-mode in-template)
(define-parser-insertion-mode after-body)
(define-parser-insertion-mode in-frameset)
(define-parser-insertion-mode after-frameset)
(define-parser-insertion-mode after-after-body)
(define-parser-insertion-mode after-after-frameset)

(defclass parser ()
  ((current-token
    :initform nil)
   (stack-of-open-elements
    :initform nil)))


(defgeneric parse (source &key))

(defmethod parse (source &key)
  (let ((document nil))
    (labels ((transform (node)
               (when node
                 (let ((parent (typecase node
                                 (plump-dom:element
                                  (when-let* ((tag-name (plump:tag-name node))
                                              (constructor (constructor tag-name)))
                                    (construct constructor)))
                                 (plump-dom:text-node
                                  (make-instance 'text :data (plump-dom:text node))))))
                   (when (and parent
                              (typep node 'plump-dom:element))
                     (let ((children (mapcar #'transform
                                             (coerce (plump:children node) 'list))))
                       (loop for child in children
                          do (append-child parent child)))
                     (let ((attributes (plump:attributes node)))
                       (loop for name being the hash-keys of attributes
                          using (hash-value value)
                          do (dom:set-attribute parent name value))))
                   parent))))
      (let ((root (plump:parse source)))
        (when (and (typep root 'plump-dom:root)
                   (plusp (length (plump:children root))))
          (loop for child across (plump:children root)
             if (typep child 'plump-dom:doctype)
             do (setf document (make-instance 'document))
             else if (typep child 'plump-dom:element)
             do (when-let ((element (transform child)))
                  (if document
                      (append-child document element)
                      (return element)))
             finally (return document)))))))
