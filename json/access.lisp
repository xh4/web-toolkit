(in-package :json)

(defun get (value &rest accessors)
  (let ((current-value value)
        (found-p t))
    (labels ((fail (value accessor)
               (error "Can't get ~A in ~A" accessor value))
             (get1 (value accessor)
               (typecase value
                 (cl:null nil)
                 (array (let ((list (value value)))
                          (typecase accessor
                            (integer (if (> accessor (1- (length list)))
                                         (setf found-p nil
                                               current-value nil)
                                         (setf found-p t
                                               current-value (nth accessor list))))
                            (t (fail value accessor)))))
                 (object (typecase accessor
                           ((or string symbol)
                            (with-slots (pairs) value
                              (if-let ((cons (assoc accessor pairs :test 'equal)))
                                (setf found-p t
                                      current-value (cdr cons))
                                (setf found-p nil
                                      current-value nil))))
                           (t (get1 value (format nil "~A" accessor)))))
                 (t (fail value accessor))))
             (get2 (value accessors)
               (if (cl:null accessors)
                   (values
                    (typecase current-value
                      (array (value current-value))
                      (null nil)
                      (t current-value))
                    found-p)
                   (get2 (get1 value (car accessors)) (cdr accessors)))))
      (get2 value (alexandria:flatten accessors)))))
