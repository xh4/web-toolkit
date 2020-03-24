(in-package :documentation)

(define-component article (addressable)
  ((chapter
    :initform nil))
  (:render
   (lambda (article)
     (with-slots (no title children) article
       (div
        (h3
         :class "heading"
         :id (id article)
         (span :class "secno" (format nil "~{~A~^.~}" no)) " "
         (span :class "content" title) " "
         (a :class "self-link" :href (format nil "#~A" (id article))))
        children)))))

(defmethod id ((article article))
  (format nil "~A/~A" (id (slot-value article 'chapter)) (slot-value article 'id)))
