(in-package :style)

(defclass style ()
  ((declarations
    :initarg :declarations
    :initform nil
    :accessor style-declarations)))

(defmethod style-declarations ((nothing null)))

(defun style (&rest objects)
  (loop with target = nil
     with declarations = nil
     for object in objects
     do (typecase object
          (declaration (if target
                           (if (slot-value target 'style)
                               (appendf (style-declarations
                                         (slot-value target 'style))
                                        (list object))
                               (setf (slot-value target 'style)
                                     (make-instance 'style
                                                    :declarations (list object))))
                           (appendf declarations (list object))))
          (null)
          (t (setf target object)))
     finally (unless target
               (return (make-instance 'style
                                      :declarations declarations)))))

(defun merge-style (style-1 style-2)
  (make-instance 'style
                 :declarations (append (style-declarations style-1)
                                       (style-declarations style-2))))
