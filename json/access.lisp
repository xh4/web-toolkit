(in-package :json)

(defun get (object &rest accessors)
  (let ((current-value object))
    (labels ((fail (value accessor)
               (error "Can't get ~A in ~A" accessor value))
             (get1 (value accessor)
               (typecase value
                 (cl:null nil)
                 (array (let ((list (value value)))
                          (typecase accessor
                            (integer (if (> accessor (1- (length list)))
                                         (setf current-value nil)
                                         (setf current-value (nth accessor list))))
                            (t (fail value accessor)))))
                 (object (typecase accessor
                           ((or string symbol)
                            (with-slots (properties) value
                              (if-let ((cons (assoc accessor properties :test 'equal)))
                                (setf current-value (cdr cons))
                                (setf current-value nil))))
                           (t (get1 value (format nil "~A" accessor)))))
                 (t (fail value accessor))))
             (get2 (value accessors)
               (if (cl:null accessors)
                   (typecase current-value
                     ((or json:array json:true json:false json:null)
                      (values (value current-value) current-value))
                     (cl:null nil)
                     (t (values current-value t)))
                   (get2 (get1 value (car accessors)) (cdr accessors)))))
      (get2 object (flatten accessors)))))

(defun (setf get) (value object &rest accessors)
  (cond
    ((eq value t) (setf value true))
    ((eq value nil) (setf value false))
    ((listp value) (setf value (make-instance 'array :value value)))
    ((stringp value))
    ((vectorp value) (setf value (make-instance 'array :value (coerce value 'list)))))
  (check-type value (or true false null array object number string))
  (labels ((set1 (thing accessor)
             (typecase thing
               (array (unless (integerp accessor)
                        (error "Can't set ~A to ~A, accessor: ~A" value thing accessor))
                      (when (>= accessor (length (value thing)))
                        (error "Can't set ~A to ~A, accessor ~A out of index" value thing accessor))
                      (setf (nth accessor (value thing)) value))
               (object (with-slots (properties) thing
                         (loop for (name . nil) in properties
                            for index from 0
                            when (equal accessor name)
                            do (return (setf (cdr (nth index properties)) value))
                            finally
                              (appendf properties (list (cons accessor value)))
                              (return value))))
               (t (error "Can't set ~A to ~A, accessor: ~A" value thing accessor))))
           (set2 (thing accessors)
             (if (= 1 (length accessors))
                 (set1 thing (first accessors))
                 (let ((accessor (first accessors)))
                   (typecase thing
                     (array (let ((list (value thing)))
                              (set2 (nth accessor list) (rest accessors))))
                     (object (with-slots (properties) thing
                               (loop for (name . value) in properties
                                  when (equal name accessor)
                                  do (return (set2 value (rest accessors)))
                                  finally (error "Object name ~A not found" accessor))))
                     (t (error "Can't access ~A using accessor ~A" thing accessor)))))))
    (set2 object (flatten accessors))))
