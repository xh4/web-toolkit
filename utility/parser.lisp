(in-package :utility)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass parser ()
    ((function
      :initarg :function
      :initform nil
      :accessor parser-function)
     (delegation
      :initarg :delegation
      :initform nil
      :accessor parser-delegation)
     (arguments
      :initarg :arguments
      :initform nil
      :accessor parser-arguments)
     (bindings
      :initarg :bindings
      :initform nil
      :accessor parser-bindings)
     (code
      :initarg :code
      :initform nil
      :accessor parser-code)
     (match-p
      :initarg :match-p
      :initform nil
      :accessor parser-match-p)
     (value
      :initarg :value
      :initform nil
      :accessor parser-value))))

(defun parse (parser input)
  (typecase input
    (string (funcall parser (maxpc.input:make-input input)))
    (t (funcall parser input))))

(defvar *parser-stack* nil)

(defmacro define-parser (name arguments &body body)
  `(defun ,name ,arguments ,@body))

(defmacro define-traced-parser (name arguments &body body)
  `(defun ,name ,arguments
     (let ((parser ,@body))
       (lambda (input)
         (multiple-value-bind (input value match-p)
             (funcall parser input)
           (push (cons ',name value) *parser-stack*)
           (values input value match-p))))))

(defmacro with-parser-stack ((var) &body body)
  `(let ((*parser-stack* '()))
     (symbol-macrolet ((,var *parser-stack*))
       ,@body)))

(defun parser-match-all-p (parser input)
  (multiple-value-bind (input value match-p)
      (parse parser input)
    (declare (ignore value))
    (and match-p
         (maxpc.input:input-empty-p input))))

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
       return (values rest value match-p)
       finally (return (values input nil nil)))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmacro .test ((test &rest arguments) &optional (parser '(.element))
                   &aux (value-sym (gensym "value")))
    `(.satisfies (lambda (,value-sym)
                   (funcall ,test ,value-sym ,@arguments))
                 ,parser)))

(define-parser .eq (x &optional (parser (.element)))
  (.test ('eq x) parser))

(define-parser .seq (&rest parsers)
  (lambda (original-input)
    (loop with input = original-input
       for parser in parsers
       for (rest value match-p) = (multiple-value-list
                                   (parse parser input))
       unless match-p return (values original-input nil nil)
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
         else return (values input list t)
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
    (loop for parser in parsers
       for (rest value match-p) = (multiple-value-list
                                   (parse parser input))
       unless match-p
       return (values input nil nil)
       else do (setf input rest)
       finally (return (values rest value t)))))

(define-parser .maybe (parser)
  (.or parser (.seq)))

(define-parser .some (parser)
  (lambda (input)
    (let (rest value match-p)
      (loop do (setf (values rest value match-p) (parse parser input))
         if match-p do (setf input rest)
         else return (values input list (not (null list)))
         when match-p collect value into list))))

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

(define-parser .end ()
  (lambda (input)
    (if (maxpc::input-empty-p input)
        (values input nil t)
        (values input nil nil))))

(define-parser .not (parser)
  (lambda (input)
    (let ((result (multiple-value-list (parse parser input))))
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

(define-parser .m (m parser)
  (if (equal m 0)
      (.not parser)
      (lambda (original-input)
        (let ((input original-input))
          (loop repeat (1+ m)
             for (rest value match-p) = (multiple-value-list
                                         (parse parser input))
             if match-p
             collect value into values
             and do (setf input rest)
             finally (if (> (length values) m)
                         (return (values original-input nil nil))
                         (return (values input values t))))))))

(define-parser .m/s (m parser)
  (lambda (input)
    (multiple-value-bind (rest values match-p)
        (parse (.m m parser) input)
      (if match-p
          (values rest (format nil "~{~A~}" values) t)
          (values input nil nil)))))

(define-parser .s (string)
  (lambda (original-input)
    (let ((input original-input))
      (loop for char across string
         do (if (maxpc::input-empty-p input)
                (return (values original-input nil nil))
                (let ((element (maxpc.input:input-first input)))
                  (if (equal char element)
                      (setf input (maxpc.input:input-rest input))
                      (return (values original-input nil nil)))))
         finally (return (values input string t))))))

(defun alpha-p (char)
  (or
   (char<= #\a char #\z)
   (char<= #\A char #\Z)))

(define-parser .alpha ()
  (.satisfies 'alpha-p))

(defun digit-p (char)
  (char<= #\0 char #\9))

(define-parser .digit ()
  (.satisfies 'digit-p))

(defun hexdig-p (char)
  (or (digit-p char)
      (char<= #\a char #\f)
      (char<= #\A char #\F)))

(define-parser .hexdig ()
  (.satisfies 'hexdig-p))
