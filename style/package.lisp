(in-package :cl-user)

(defpackage :style
  (:nicknames :wt.style)
  (:use :cl :alexandria)
  (:shadow :length :float :declaration :rem :time :position)
  (:export :em :ex :ch :rem
           :vw :vh :vmin :vmax
           :cm :mm :q :in :pt :pc :px
           :deg :grad :rad :turn
           :s :ms
           :hz :khz
           :dpi :dpcm :dppx
           :%
           :color :opacity :rgb :rgba
           :margin-top
           :margin-right
           :margin-left
           :margin-bottom
           :margin
           :padding-top
           :padding-right
           :padding-left
           :padding-bottom
           :padding)
  (:import-from :closer-mop
                :validate-superclass))
