(in-package :cl-user)

(defpackage :style
  (:nicknames :wt.style)
  (:use :cl :alexandria)
  (:shadow :length :float :declaration :rem :time :position)
  (:export
   ;; length
   :em :ex :ch :rem
   :vw :vh :vmin :vmax
   :cm :mm :q :in :pt :pc :px
   ;; angle
   :deg :grad :rad :turn
   ;; time
   :s :ms
   ;; frequency
   :hz :khz
   ;; resolution
   :dpi :dpcm :dppx
   ;; percentage
   :%
   ;; color
   :color :opacity :rgb :rgba
   ;; box
   :margin-top :margin-right :margin-left :margin-bottom :margin
   :padding-top :padding-right :padding-left :padding-bottom :padding
   ;; sizing
   :width :height :min-width :min-height
   ;; display
   :display
   ;; position
   :position :top :right :bottom :left
   ;; logical
   :float :clear
   ;; overflow
   :overflow-x :overflow-y :overflow :text-overflow
   ;; text
   :white-space :word-break :line-break :word-wrap :text-align
   ;; flexbox
   :flex :flex-direction :flex-wrap :flex-flow :order
   :flex-grow :flex-shrink :flex-basis
   :justify-content :align-items :align-self :align-content)
  (:import-from :closer-mop
                :validate-superclass))
