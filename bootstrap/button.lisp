(in-package :bootstrap)

(com:define-component button (com:button)
  (;; :PRIMARY, :SECONDARY, :SUCCESS, :DANGER, :WARNING, :INFO, :LIGHT, :DARK
   ;; :LINK
   (style
    :initarg :style
    :initform :primary
    :accessor button-style)
   ;; https://getbootstrap.com/docs/4.3/components/buttons/#button-tags
   (link
    :initarg :link
    :initform nil
    :accessor button-link)
   ;; https://getbootstrap.com/docs/4.3/components/buttons/#outline-buttons
   (outline-p
    :initarg :outline-p
    :initform nil
    :accessor button-outline-p)
   ;; :LARGE, :SMALL
   (size
    :initarg :size
    :initform nil
    :accessor button-size)
   (block-p
    :initarg :block-p
    :initform nil
    :accessor button-block-p)
   (active-p
    :initarg :active-p
    :initform nil
    :accessor button-active-p)
   (disabled-p
    :initarg :disabled-p
    :initform nil
    :accessor button-disabled-p)))

(defmethod com:expand ((button button))
  (let (tag
        (class '("btn"))
        (other-attrs '()))
    (with-slots (style link outline-p size block-p active-p disabled-p) button
      ;; button or a?
      (if link
          (progn
            (setf tag 'html:a)
            (alexandria:appendf other-attrs (list :href (format nil "~A" link)
                                                  :role "button")))
          (setf tag 'html:button))
      ;; style & outline-p
      (let ((style-class "btn"))
        (when (and outline-p (not (eq style :link)))
          (setf style-class "btn-outline"))
        (when (member style '(:primary :secondary :success :danger
                              :warning :info :light :dark :link))
          (setf style-class (concatenate 'string style-class "-" (string-downcase
                                                                  (symbol-name style)))))
        (unless (equal style-class "btn")
          (alexandria:appendf class (list style-class))))
      ;; size
      (cond
        ((eq size :large) (alexandria:appendf class (list "btn-lg"))))
      (cond
        ((eq size :small) (alexandria:appendf class (list "btn-sm"))))
      ;; block-p
      (when block-p
        (alexandria:appendf class (list "btn-block")))
      ;; active-p
      (when active-p
        (alexandria:appendf class (list "active")))
      ;; disabled-p
      (when disabled-p
        (alexandria:appendf other-attrs (list :disabled t)))
      `(,tag :class ,(format nil "~{~A~^ ~}" class)
             ,@other-attrs
             ,@(com:component-children button)))))
