(in-package :documentation)

(define-component chapter (addressable)
  ()
  (:render
   (lambda (chapter)
     (loop for child in children
        when (or (typep child 'article)
                 (typep child 'symbol/o))
        do (setf (slot-value child 'chapter) chapter))
     (with-slots (no title id) chapter
       (div
        (h2
         :class "heading"
         :id id
         (span :class "secno" (format nil "~{~A~^.~}" no)) " "
         (span :class "content" title) " "
         (a :class "self-link" :href (format nil "#~A" id)))
        children)))))
