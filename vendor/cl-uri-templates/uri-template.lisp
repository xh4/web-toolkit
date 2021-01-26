;;  cl-uri-templates
;;  Extensive URI-Templates implementation in Common-Lisp.
;;
;;  Copyright 2009 Thomas de Grivel <billitch@gmail.com>
;;  Copyright (c) 2007, 2008, 2009 Vladimir Sedach
;;
;;  This software is provided "AS IS".
;;  Please see COPYING for details.

(in-package #:cl-uri-templates)


(declaim (optimize (debug 3)))


(deftype uri-template ()
  '(or string
    (cons (eql 'uri-template) list)))


(define-condition invalid-uri-warning (warning)
  ((message :initarg :message
            :initform "Invalid URI"
            :reader message-of))
  (:report (lambda (condition stream)
             (write-string (message-of condition) stream))))


(define-condition invalid-expansion-error (reader-error)
  ((message :initarg :message
            :initform "Invalid URI-Template expansion"
            :reader message-of))
  (:report (lambda (condition stream)
             (write-string (message-of condition) stream))))


(define-condition invalid-op-error (invalid-expansion-error) ())
(define-condition invalid-op-vars-error (invalid-expansion-error) ())
(define-condition invalid-arg-error (invalid-expansion-error) ())
(define-condition invalid-var-error (invalid-expansion-error) ())


(defmacro check-uri (test condition fmt &rest arguments)
  `(assert ,test () ',condition :message (format nil ,fmt ,@arguments)))


(defmacro define-reader (name &key allowed-char valid-next-char eat-next-char
                         valid-result (condition-type 'invalid-expansion-error))
  "Reads string from STREAM of characters satisfying ALLOWED-CHAR-SPEC.
   Next char in stream will satisfy VALID-NEXT-CHAR-SPEC and
   is eaten when EAT-NEXT-CHAR is non-nil.
   The *-CHAR-SPEC can be :
    - a string of allowed characters
    - a single allowed character
    - a symbol which functional value will be used as a predicate
    - a lambda expression to be used as a predicate"
  (assert allowed-char () "You must supply valid characters with :ALLOWED.")
  (flet ((char-predicate (spec char-var)
           (typecase spec
             (symbol `(,spec ,char-var))
             (string `(find ,char-var ,spec))
             (character `(char= ,char-var ,spec))
             (t `(funcall ,spec ,char-var)))))
    (let ((stream (gensym "STREAM"))
          (next-char (gensym "NEXT-CHAR"))
          (result (gensym "RESULT"))
          (char (gensym "CHAR")))
      `(defun ,name (,stream)
         (declare (type stream ,stream))
         (let* ((,next-char nil)
                (,result (coerce (loop
                                    for ,char = (read-char ,stream)
                                    while ,(char-predicate allowed-char char)
                                    collect ,char
                                    finally
                                      (setf ,next-char ,char)
                                      ,@(unless eat-next-char
                                                `((unread-char ,char ,stream))))
                                 'string)))
           ,@(when valid-next-char
                   `((check-uri ,(char-predicate valid-next-char next-char)
                                ,condition-type
                                "Next char ~S is invalid after ~S."
                                ,next-char ,result)))
           ,@(when valid-result
                   `((funcall ,valid-result ,result)))
           ,result)))))


(define-reader read-op
    :allowed-char alpha-char-p
    :valid-next-char #\|
    :valid-result (lambda (result)
                    (> (length result) 0))
    :condition-type invalid-op-error
    :eat-next-char t)


(defmacro define-constant (name value &key (test 'equal))
  (unless (and (constantp name)
               (funcall test value (symbol-value name)))
    `(eval-when (:compile-toplevel)
       (defconstant ,name ,value))))


(define-reader read-arg
    :allowed-char (lambda (char)
                    (declare (type character char))
                    (or (uri-unreserved-char-p char)
                        (uri-reserved-char-p char)
                        (char= char #\%)))
    :valid-next-char #\|
    :condition-type invalid-op-error
    :eat-next-char t)


(define-reader read-varname
    :allowed-char (lambda (char)
                    (declare (type character char))
                    (or (alphanumericp char)
                        (find char "._-")))
    :valid-next-char "=,}"
    :valid-result (lambda (result)
                    (check-uri (and (> (length result) 0)
                                       (alphanumericp (char result 0)))
                            invalid-var-error
                            "Invalid variable name : ~S. ~
                             Variable names must start with alphanum."
                            result))
    :condition-type invalid-var-error)


(define-reader read-vardefault
    :allowed-char (lambda (char)
                    (declare (type character char))
                    (or (uri-unreserved-char-p char)
                        (char= char #\%)))
    :valid-next-char ",}"
    :condition-type invalid-var-error)


(defun eat-char (stream char)
  (when (char= char (peek-char nil stream))
    (read-char stream)))


(defun read-var (stream)
  (declare (type stream stream))
  (let* ((name (intern (string-upcase (read-varname stream)))))
    (if (eat-char stream #\=)
        `(uri-template-var ,name
                           ,(read-vardefault stream))
        `(uri-template-var ,name))))


(defun read-vars (stream)
  (declare (type stream stream))
  (loop
     for var = (read-var stream)
     for next-char = (read-char stream)
     collect var
     while (char= #\, next-char)
     finally (check-uri (char= #\} next-char)
                        invalid-var-error
                        "Invalid URI expansion vars near ~S"
                        var)))


(defun intern-op (name)
  (multiple-value-bind (symbol type)
      (find-symbol (concatenate 'string "-" (string-upcase name))
                   'cl-uri-templates.operators)
    (check-uri (and symbol
                    (eq type :external))
               invalid-op-error
               "~A is not a recognized operator for a URI-Template."
               name)
    symbol))


(defun read-operator (stream)
  (declare (type stream stream))
  (check-uri (char= #\- (read-char stream))
             invalid-op-error
             "operator must start with '-'.")
  (let ((op (intern-op (read-op stream)))
        (arg (read-arg stream))
        (vars (read-vars stream)))
    (check-op-arity op (length vars))
    (cons op (cons arg vars))))


(defun read-expansion (stream)
  (declare (type stream stream))
  (if (char= #\- (peek-char nil stream))
      (read-operator stream)
      (read-var stream)))


(defun read-uri-template (stream &optional recursive-p)
  (declare (type stream stream))
  (let ((*readtable* (copy-readtable nil))
        (token-accumulator ())
        (string-accumulator ()))
    (flet ((collect-string ()
             (when string-accumulator
               (push (coerce (reverse string-accumulator) 'string)
                     token-accumulator)
               (setf string-accumulator ()))))
      (loop
         for next-char = (read-char stream nil nil recursive-p)
         until (member next-char '(nil #\Space \#Tab #\Newline #\)))
         do (case next-char
              (#\{ (collect-string)
                   (push (read-expansion stream)
                         token-accumulator))
              (#\})
              (t (push next-char string-accumulator)))
         finally
           (if next-char
               (unread-char next-char stream))
           (collect-string))
      (reverse token-accumulator))))


(defun enable-uri-template-syntax ()
  (set-dispatch-macro-character #\# #\U
    (lambda (stream subchar arg)
      (declare (ignore subchar arg))
      `(macrolet ((uri-template-var (var &optional default)
                    `(handler-case ,var
                       (unbound-variable ()
                     ,default))))
         (uri-template ,@(read-uri-template stream t)))))
  (values))


(defun parse-uri-template (str)
  (declare (type string str))
  (with-input-from-string (stream str)
    (cons 'uri-template
          (read-uri-template stream))))
