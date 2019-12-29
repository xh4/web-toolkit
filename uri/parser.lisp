(in-package :uri)

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
    :accessor parser-value)))

(defgeneric parse (parser input)
  (:method (parser input)
    (multiple-value-prog1
        (multiple-value-bind (input value match-p)
            (cond
              ((parser-function parser) (funcall (parser-function parser) input))
              ((parser-delegation parser) (parse (parser-delegation parser) input)))
          (setf (parser-match-p parser) match-p
                (parser-value parser) value)
          (values input value match-p))
      (maybe-trace-parser parser))))

(defmacro define-parser (name arguments &body body)
  (with-gensyms (parser bindings)
    (multiple-value-bind (required-arguments)
        (parse-ordinary-lambda-list arguments)
      (let ((inner (eval `(apply (lambda ,arguments ,@body)
                                 ',(loop repeat (length required-arguments)
                                      collect nil)))))
        `(eval-when (:compile-toplevel :load-toplevel :execute)
           (defclass ,name (parser)
             ((arguments
               :initform ',arguments)
              (code
               :initform ',body)))
           (defun ,name (&rest ,bindings)
             (let ((,parser (make-instance ',name
                                           :bindings ,bindings)))
               ,@(typecase inner
                   (function `((let ((function (apply
                                                (lambda ,arguments
                                                  ,@body)
                                                ,bindings)))
                                 (setf (parser-function ,parser) function))))
                   (parser `((let ((delegation (apply
                                                (lambda ,arguments
                                                  ,@body)
                                                ,bindings)))
                               (setf (parser-delegation ,parser) delegation)))))
               ,parser))
           (find-class ',name))))))

(defvar *parser-stack* nil)

(defvar *parser-tracing-names* nil)

(defmacro with-parser-stack ((var &key trace) &body body)
  `(let ((*parser-stack* (list))
         (*parser-tracing-names* (or ,trace *parser-tracing-names*)))
     (symbol-macrolet ((,var *parser-stack*))
       ,@body)))

(defun maybe-trace-parser (parser)
  (when (find (class-name (class-of parser)) *parser-tracing-names*)
    (push parser *parser-stack*)))
