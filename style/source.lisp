(in-package :style)

(defclass source-file () ())

(defclass local-source-file (source-file)
  ((path
    :initarg :path
    :initform nil
    :accessor file-path)))

(defclass remote-source-file (source-file)
  ((url
    :initarg :url
    :initform nil
    :accessor file-url)))

(defclass css-source-file (source-file) ())

(defclass local-css-source-file (css-source-file local-source-file) ())

(defclass remote-css-source-file (css-source-file remote-source-file) ())

(defmacro define-css-source-file (name location)
  (let ((uri (uri:uri location)))
    (cond
      ((member (uri:uri-scheme uri) '("http" "https") :test 'equal)
       `(defparameter ,name (make-instance 'remote-css-source-file
                                           :url ,location)))
      ((null (uri:uri-scheme uri))
       `(defparameter ,name (make-instance 'local-css-source-file
                                           :path ,location)))
      (t (error "Unable to handle location ~A" location)))))
