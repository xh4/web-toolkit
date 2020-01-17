(in-package :uri)

(define-parser .element ()
  (lambda (input)
    (if (maxpc::input-empty-p input)
        (values input nil nil)
        (values (maxpc::input-rest input) (maxpc::input-first input) t))))

(define-parser .satisfies (test &optional (parser (.element)))
  (lambda (input)
    (multiple-value-bind (rest value match-p) (parse parser input)
      (if (and match-p (funcall test value))
          (values rest value match-p)
          (values input nil nil)))))

(define-parser .or (&rest parsers)
  (lambda (input)
    (loop for parser in parsers
       for branch-stack = nil
       for (rest value match-p) = (with-parser-stack (stack)
                                    (prog1
                                        (multiple-value-list
                                         (parse parser input))
                                      (setf branch-stack stack)))
       when match-p
       do (loop for parser in (reverse branch-stack)
             do (push parser *parser-stack*))
       and
       return (values rest value match-p))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmacro .test ((test &rest arguments) &optional (parser '(.element))
                   &aux (value-sym (gensym "value")))
    `(.satisfies (lambda (,value-sym)
                   (funcall ,test ,value-sym ,@arguments))
                 ,parser)))

(define-parser .eq (x &optional (parser (.element)))
  (.test ('eq x) parser))

(define-parser .seq (&rest parsers)
  (lambda (input)
    (loop for parser in parsers
       for (rest value match-p) = (multiple-value-list
                                   (parse parser input))
       unless match-p return nil
       collect value into results
       do (setf input rest)
       finally (return (values input
                               results
                               t)))))

(define-parser .seq/s (&rest parsers)
  (lambda (input)
    (multiple-value-bind (rest value match-p)
        (parse (apply '.seq parsers) input)
      (if match-p
          (values
           rest
           (apply 'concatenate 'string
                  (loop for part in value
                     when part
                     collect (format nil "~A" part)))
           t)
          (values input nil nil)))))

(define-parser .any (parser)
  (lambda (input)
    (let (rest value match-p)
      (loop do (setf (values rest value match-p) (parse parser input))
         if match-p do (setf input rest)
         else return (values input list (not (null list)))
         when match-p collect value into list))))

(define-parser .any/s (parser)
  (lambda (input)
    (multiple-value-bind (rest value match-p)
        (parse (.any parser) input)
      (if match-p
          (values
           rest
           (apply 'concatenate 'string
                  (loop for part in value
                     when part
                     collect (format nil "~A" part)))
           t)
          (values input nil nil)))))

(define-parser .and (&rest parsers)
  (lambda (input)
    (let (rest value match-p)
      (loop for parser in parsers do
           (setf (values rest value match-p) (parse parser input))
         unless match-p return nil
         finally (return (values rest value match-p))))))

(define-parser .maybe (parser)
  (.or parser (.seq)))

(define-parser .some (parser)
  (.and parser (.any parser)))

(define-parser .some/s (parser)
  (lambda (input)
    (multiple-value-bind (rest value match-p)
        (parse (.some parser) input)
      (if match-p
          (values
           rest
           (apply 'concatenate 'string
                  (loop for part in value
                     when part
                     collect (format nil "~A" part)))
           t)
          (values input nil nil)))))

(define-parser .end  ()
  (lambda (input)
    (if (maxpc::input-empty-p input)
        (values input nil t)
        (values input nil nil))))

(define-parser .not (parser)
  (lambda (input)
    (let ((result (parse parser input)))
      (if (third result)
          (values input nil nil)
          (values input t t)))))

(define-parser .n (n parser)
  (if (equal n 0)
      (.not parser)
      (lambda (original-input)
        (let ((input original-input))
          (loop repeat n
             for (rest value match-p) = (multiple-value-list
                                         (parse parser input))
             if match-p
             collect value into values
             and do (setf input rest)
             else do (return (values original-input nil nil))
             finally (return (values rest values t)))))))

(define-parser .n/s (n parser)
  (lambda (input)
    (multiple-value-bind (rest values match-p)
        (parse (.n n parser) input)
      (if match-p
          (values rest (format nil "~{~A~}" values) t)
          (values input nil nil)))))

(define-parser .s (string)
  (lambda (original-input)
    (let ((input original-input))
      (loop for char across string
         for element = (maxpc.input:input-first input)
         if (equal char element)
         do (setf input (maxpc.input:input-rest input))
         else do (return (values original-input nil nil))
         finally (return (values input string t))))))
