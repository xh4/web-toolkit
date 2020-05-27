(in-package :cl-user)

(defpackage :svg
  (:nicknames :wt.svg)
  (:use :cl :alexandria)
  (:shadow :use :symbol :switch)
  (:export
   :element
   :graphics-element
   :geometry-element
   :container-element
   :structural-element
   :shape-element
   :renderable-element
   :never-rendered-element
   :descriptive-element
   :graphics-referencing-element
   :structurally-external-element
   :text-content-element
   :text-content-child-element
   :animation-element
   :paint-server-element
   :svg
   :g
   :defs
   :desc
   :metadata
   :title
   :symbol
   :use
   :switch
   :path
   :rect
   :circle
   :ellipse
   :line
   :polyline
   :polygon
   :text
   :tspan
   :text-path
   :image
   :marker
   :a
   :view))
