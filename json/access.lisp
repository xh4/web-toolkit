(in-package :json)

(defun get (value &rest accessors)
  (let ((current-value value)
        (found-p t))
    (labels ((fail (value accessor)
               (error "Can't get ~A in ~A" accessor value))
             (get1 (value accessor)
               (typecase value
                 (list (typecase accessor
                         (integer (if (> accessor (1- (length value)))
                                      (setf found-p nil
                                            current-value nil)
                                      (setf found-p t
                                            current-value (nth accessor value))))
                         (t (fail value accessor))))
                 (object (typecase accessor
                           ((or string symbol)
                            (with-slots (pairs) value
                              (multiple-value-bind (value0 present-p)
                                  (gethash (lisp-name-to-object-key accessor) pairs)
                                (setf found-p present-p
                                      current-value value0))))
                           (t (get1 value (format nil "~A" accessor)))))
                 (t (fail value accessor))))
             (get2 (value accessors)
               (if (null accessors)
                   (values current-value found-p)
                   (get2 (get1 value (car accessors)) (cdr accessors)))))
      (get2 value (alexandria:flatten accessors)))))
