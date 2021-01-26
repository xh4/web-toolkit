(in-package :svg)

(defclass element (dom:element)
  ((dom:namespace
    :initform dom:svg-namespace
    :allocation :class)))

(defmethod print-object ((element element) stream)
  (print-unreadable-object (element stream :type t :identity t)
    (format stream "~S" (dom:tag-name element))
    (let ((children-count (dom:length (dom:child-nodes element))))
      (cond
        ((= 1 children-count)
         (let ((child (dom:first-child element)))
           (if (typep child 'dom:text)
               (let ((data (dom:data child)))
                 (if (> (length data) 30)
                     (format stream " {~S}"
                             (concatenate 'string (subseq data 0 30) "..."))
                     (format stream " {~S}" data)))
               (format stream " {~A}" children-count))))
        ((> children-count 1)
         (format stream " {~A}" children-count))))))

(defclass graphics-element (element) ())

(defclass geometry-element (graphics-element) ())

(defclass container-element (element) ())

(defclass structural-element (element) ())

(defclass shape-element (element) ())

(defclass renderable-element (element) ())

(defclass never-rendered-element (element) ())

(defclass descriptive-element (element) ())

(defclass graphics-referencing-element (element) ())

(defclass structurally-external-element (element) ())

(defclass text-content-element (element) ())

(defclass text-content-child-element (element) ())

(defclass animation-element (element) ())

(defclass paint-server-element (element) ())

(defun segment-attributes-children (form)
  (let* ((body (loop for rest on form by #'cddr
                  unless (and (keywordp (car rest)) (cdr rest))
                  return rest))
         (attributes (ldiff form body)))
    (values attributes body)))

(defmacro define-svg-element (element-name superclasses slots &rest options)
  (unless (find 'element superclasses)
    (appendf superclasses '(element)))
  `(progn
     (defclass ,element-name ,superclasses ,slots ,@options)

     (defun ,element-name (&rest arguments)
       (let* ((local-name (string-downcase (symbol-name ',element-name)))
              (element (make-instance ',element-name :local-name local-name)))
         (multiple-value-bind (attributes children)
             (segment-attributes-children arguments)
           (loop for (_name _value) on attributes by #'cddr
              for name = (cl-change-case:camel-case (symbol-name _name))
              for value = (if (eq _value t)
                              ""
                              (typecase _value
                                (null nil)
                                (string _value)
                                (list (format nil "~{~A~^ ~}" _value))
                                (t (format nil "~A" _value))))
              when value
              do (dom:set-attribute element name value))
           (loop for child in (flatten children)
              do
                (typecase child
                  (string (dom:append-child element (make-instance 'dom:text
                                                                   :data child)))
                  ((or element text) (dom:append-child element child))
                  (t (dom:append-child element (make-instance 'dom:text
                                                              :data (format nil "~A" child)))))))
         element))

     (defmethod print-object ((element ,element-name) stream)
       (print-unreadable-object (element stream :type t :identity t)
         (let ((children-count (dom:length (dom:child-nodes element))))
           (cond
             ((= 1 children-count)
              (let ((child (dom:first-child element)))
                (if (typep child 'dom:text)
                    (let ((data (dom:data child)))
                      (if (> (length data) 30)
                          (format stream " {~S}"
                                  (concatenate 'string (subseq data 0 30) "..."))
                          (format stream " {~S}" data)))
                    (format stream " {~A}" children-count))))
             ((> children-count 1)
              (format stream " {~A}" children-count))))))))

(define-svg-element svg (container-element
                         renderable-element
                         structural-element) ())

(define-svg-element g (container-element
                       renderable-element
                       structural-element) ())

(define-svg-element defs (container-element
                          never-rendered-element
                          structural-element) ())

(define-svg-element desc (descriptive-element
                          never-rendered-element) ())

(define-svg-element metadata (descriptive-element
                              never-rendered-element) ())

(define-svg-element title (descriptive-element
                           never-rendered-element) ())

(define-svg-element symbol (container-element
                            structural-element) ())

(define-svg-element use (graphics-referencing-element
                         renderable-element
                         structural-element
                         structurally-external-element) ())

(define-svg-element switch (container-element
                            renderable-element) ())

(define-svg-element path (graphics-element
                          renderable-element
                          shape-element) ())

(define-svg-element rect (graphics-element
                          renderable-element
                          shape-element) ())

(define-svg-element circle (graphics-element
                            renderable-element
                            shape-element) ())

(define-svg-element ellipse (graphics-element
                             renderable-element
                             shape-element) ())

(define-svg-element line (graphics-element
                          renderable-element
                          shape-element) ())

(define-svg-element polyline (graphics-element
                              renderable-element
                              shape-element) ())

(define-svg-element polygon (graphics-element
                             renderable-element
                             shape-element) ())

(define-svg-element text (graphics-element
                          renderable-element
                          text-content-element) ())

(define-svg-element tspan (graphics-element
                           renderable-element
                           text-content-element
                           text-content-child-element) ())

(define-svg-element text-path (graphics-element
                               renderable-element
                               text-content-element
                               text-content-child-element) ())

(define-svg-element image (graphics-element
                           graphics-referencing-element
                           renderable-element
                           structurally-external-element) ())

(define-svg-element foreign-object (graphics-element
                                    renderable-element
                                    structurally-external-element) ())

(define-svg-element marker (container-element
                            never-rendered-element) ())

(define-svg-element a (container-element
                       renderable-element) ())

(define-svg-element view () ())

(define-svg-element style (never-rendered-element) ())
