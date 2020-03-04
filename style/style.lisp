(in-package :style)

(defclass style ()
  ((declarations
    :initarg :declarations
    :initform nil
    :accessor style-declarations)))

(defvar *styling-target* nil)

(defun style (&rest objects)
  (loop with target = *styling-target*
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
          (t (setf target object)))
     finally (return declarations)))
