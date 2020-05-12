(in-package :svg)

(defclass element (dom:element)
  ())

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

(defmacro define-svg-element (name superclasses slots &rest options)
  (unless (find 'element superclasses)
    (appendf superclasses '(element)))
  `(defclass ,name ,superclasses ,slots ,@options))

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

(define-svg-element marker (container-element
                            never-rendered-element) ())

(define-svg-element a (container-element
                       renderable-element) ())

(define-svg-element view () ())
