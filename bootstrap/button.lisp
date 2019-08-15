(in-package :bootstrap)

(define-component button ()
  (;; :primary, :secondary, :success, :danger
   ;; :warning, :info, :light, :dark
   ;; :link
   (style
    :initarg :style
    :initform nil)
   (link
    :initarg :link
    :initform nil)
   (outline-p
    :initarg :outline-p
    :initform nil)
   ;; :large, :small
   (size
    :initarg :size
    :initform nil)
   (block-p
    :initarg :block-p
    :initform nil)
   (disabled-p
    :initarg :disabled-p
    :initform nil))
  (:tag-option tag :initform :button)
  (:class-option class :default "btn"))

(define-render button (tag class style link size
                           outline-p block-p disabled-p
                           children)
  ;; link
  (when link
    (setf tag :a))
  ;; outline-p
  (when outline-p
    (appendf class '(btn-outline)))
  ;; style
  (when (member style '(:primary :secondary :success :danger
                        :warning :info :light :dark :link))
    (appendf class (list
                    (concatenate 'string "btn-"
                                 (if outline-p
                                     "outline-"
                                     "")
                                 (string-downcase
                                  (symbol-name style))))))
  ;; size
  (case size
    (:large (appendf class (list "btn-lg")))
    (:small (appendf class (list "btn-sm"))))
  ;; block-p
  (when block-p
    (appendf class (list "btn-block")))
  ;; disabled-p
  (when disabled-p
    (appendf class (list "disabled")))

  (tag :class class
       :disabled disabled-p
       :role (when (eq tag :a)
               "button")
       :href (when (eq tag :a)
               (or link ""))
       :tabindex (when disabled-p -1)
       :aria-disabled disabled-p
       @children))
