(in-package :component)

(defgeneric component-style (component)
  (:method ((component component))
    (when-let ((styler (component-style (class-of component))))
      (funcall styler))))
