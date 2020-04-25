(in-package :javascript-test)

(define-condition node-mismatch (error)
  ((left
    :initarg :left
    :initform nil)
   (right
    :initarg :right
    :initform nil)
   (slot
    :initarg :slot
    :initform nil)
   (message
    :initarg :message
    :initform nil))
  (:report
   (lambda (condition stream)
     (with-slots (left right slot message) condition
       (format stream "Left: ~A~%" (type-of left))
       (format stream "Right: ~A~%" (type-of right))
       (when (typep left 'node)
           (format stream "Location: ~A~%" (slot-value left 'location)))
       (format stream "Slot: ~A~%" slot)
       (format stream "~%~A" message)))))

(defun assert-node-equal (left right)
  (unless (and (subtypep (type-of left) (type-of right))
               (subtypep (type-of right) (type-of left)))
    (error 'node-mismatch :left left :right right
           :message "Left node type not equal to right node"))
  (let* ((left-slots (class-slots (class-of left)))
         (left-slot-names (sort (mapcar 'slot-definition-name left-slots)
                                'string< :key 'symbol-name)))
    (loop for slot-name in left-slot-names
          for left-slot-value = (slot-value left slot-name)
          for right-slot-value = (slot-value right slot-name)
          unless (member slot-name '(range location start end
                                           line-number line-start))
          do (cond
              ((and (typep left-slot-value 'node)
                    (typep right-slot-value 'node))
               (assert-node-equal left-slot-value right-slot-value))
              ((and (typep left-slot-value 'list)
                    (not (typep right-slot-value 'list)))
               (error 'node-mismatch :left left :right right :slot slot-name
                      :message "Left slot is a LIST while right slot is not"))
              ((and (typep right-slot-value 'list)
                    (not (typep left-slot-value 'list)))
               (error 'node-mismatch :left left :right right :slot slot-name
                      :message "Right slot is a LIST while left slot is not"))
              ((and (typep left-slot-value 'list)
                    (typep right-slot-value 'list))
               (unless (= (length left-slot-value)
                          (length right-slot-value))
                 (error 'node-mismatch :left left :right right :slot slot-name
                      :message (format nil "Left slot list length (~A) not equal to right slot (~A)" (length left-slot-value) (length right-slot-value))))
               (loop for left in left-slot-value
                     for right in right-slot-value
                     do (assert-node-equal left right)))
              (t (unless (equal left-slot-value right-slot-value)
                   (error 'node-mismatch :left left :right right :slot slot-name
                      :message (format nil "Left slot (~A) not equal to right slot (~A)"
                                       left-slot-value right-slot-value))))))))

(defun test-parse-and-serialize (source)
  (finishes
   (let ((root-1 (parse source)))
    (let ((new-source (serialize root-1)))
      (let ((root-2 (parse new-source)))
        (assert-node-equal root-1 root-2))))))
