(in-package :html)

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
