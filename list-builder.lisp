(in-package :wt.list)

(defclass list-group ()
  ())

(defclass list-segment ()
  ())

(defclass list-item ()
  ())

(defgeneric list-group-skeleton (list-group &key &allow-other-keys))

(defgeneric list-group-search (list-group &key &allow-other-keys))

(defgeneric list-group-item (list-group &key &allow-other-keys))

(defmethod list-group-skeleton (list-group &key &allow-other-keys)
  (let ((classes (class-precedence-list (class-of list-group))))
    (let ((class-names (loop for class in classes
                          when (subtypep class (find-class 'list-group))
                          collect (class-name class))))
      `(:div :class ,(format nil "~{~(~a~)~^ ~}" class-names)
             ))))

(defmethod list-group-search (list-group &key &allow-other-keys)
  `(:div :class "list-group-search"
         ))

(defmethod list-group-item (list-group &key &allow-other-keys)
  `(:div :class "list-group-item"
         ))
