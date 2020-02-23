(in-package :html)

(define-condition event ()
  ((type
    :initarg :type
    :initform nil
    :accessor event-type)
   (node
    :initarg :node
    :initform nil
    :accessor event-node)))

(defun traverse (node)
  (when (null node) (return-from traverse nil))
  (let ((stack nil))
    (loop for node in (reverse (ensure-list node))
       do (push `(:enter . ,node) stack))
    (loop for (action . node) = (pop stack)
       while node
       do
         (case action
           (:enter (typecase node
                     (text (signal 'event :type :enter :node node))
                     ((or element document) (signal 'event :type :enter :node node)
                      (push `(:leave . ,node) stack)
                      (loop for child in (reverse (dom:children node))
                         do (push `(:enter . ,child) stack)))))
           (:leave (typecase node
                     (element
                      (signal 'event :type :leave :node node))))))))

(defun write-char (char &optional (stream *standard-output*) &key)
  (cl:write-char char stream))

(defun write-string (string &optional (stream *standard-output*) &key)
  (cl:write-string string stream))

(defun write-text (text &optional (stream *standard-output*) &key)
  (let ((parent (dom:parent text)))
    (etypecase parent
      (normal-element (write-normal-text text stream))
      (escapable-raw-text-element (write-escapable-raw-text text stream)))))

(defun write-normal-text (text &optional (stream *standard-output*) &key)
  (loop for char across (dom:data text)
     do (cond
          ((char= char #\<) (write-string "&lt;" stream))
          (t (write-char char stream)))))

(defun write-escapable-raw-text (text &optional (stream *standard-output*) &key)
  (loop for char across (dom:data text)
     do (cond ((char= char #\Tab) (write-string "&#9;" stream))
              ((char= char #\Newline) (write-string "&#10;" stream))
              ((char= char #\Return) (write-string "&#13;" stream))
              ((char= char #\&) (write-string "&amp;" stream))
              ((char= char #\<) (write-string "&lt;" stream))
              ((char= char #\>) (write-string "&gt;" stream))
              ((char= char #\") (write-string "&quot;" stream))
              (t (write-char char stream)))))

(defun write-element-start-tag (element &optional (stream *standard-output*) &key)
  (write-string "<" stream)
  (write-string (dom:tag-name element) stream)
  (loop for name in (dom:get-attribute-names element)
     for value = (dom:get-attribute element name)
     do (write-attribute name value stream))
  (write-string ">" stream))

(defun write-attribute (name value &optional (stream *standard-output*) &key)
  (when value
    (write-char #\space stream)
    (write-string name stream)
    (unless (eq t value)
      (write-char #\= stream)
      (write-double-quoted-attribute-value value stream))))

(defun write-double-quoted-attribute-value (value stream)
  (write-char #\" stream)
  (loop for char across value
     do (cond
          ((char= char #\") (write-string "&quot;" stream))
          (t (write-char char stream))))
  (write-char #\" stream))

(defun write-element-end-tag (element &optional (stream *standard-output*) &key)
  (unless (typep element 'void-element)
    (write-string "</" stream)
    (write-string (dom:tag-name element) stream)
    (write-string ">" stream)))

(defun write-doctype (document &optional (stream *standard-output*) &key)
  (declare (ignore document))
  (write-string "<!DOCTYPE html>" stream))

(defun serialize (root &optional (stream *standard-output* stream-present-p))
  (unless stream-present-p (setf stream (make-string-output-stream)))
  (handler-bind
      ((event
        (lambda (event)
          (with-slots (type node) event
            (case type
              (:enter (typecase node
                        (document (write-doctype node stream))
                        (element (write-element-start-tag node stream))
                        (text (write-text node stream))))
              (:leave (unless (typep node 'void-element)
                        (typecase node
                          (element (write-element-end-tag node stream))))))))))
    (traverse root))
  (unless stream-present-p
    (get-output-stream-string stream)))
