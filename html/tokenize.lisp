(in-package :html)

(defclass token ()
  ((value
    :initarg :value
    :initform nil)))

(defclass doctype (token) ())

(defclass start-tag (token)
  ((tag-name
    :initarg :tag-name
    :initform nil)
   (attributes
    :initarg :attributes
    :initform nil)))

(defclass end-tag (token)
  ((tag-name
    :initarg :tag-name
    :initform nil)))

(defclass comment (token)
  ((data
    :initarg :data
    :initform nil)))

(defclass character (token) ())

(defclass end-of-file (token) ())

(defclass attribute ()
  ((name
    :initarg :name
    :initform nil)
   (value
    :initarg :value
    :initform nil)))

(define-condition on-token ()
  ((token
    :initarg :token
    :initform nil)))

(defun ascii-alpha-p (char)
  (or (ascii-upper-alpha-p char)
      (ascii-lower-alpha-p char)))

(defun ascii-upper-alpha-p (char)
  (and char
       (char<= #\A char #\Z)))

(defun ascii-lower-alpha-p (char)
  (and char
       (char<= #\a char #\z)))

(defmacro define-tokenizer-state (name &body body)
  `(defun ,name (tokenizer)
     ,@(unless body '((declare (ignore tokenizer))))
     (symbol-macrolet ((next-input-character (next-input-character tokenizer))
                       (current-input-character (current-input-character tokenizer))
                       (end-of-file (make-instance 'end-of-file))
                       (current-tag-token (slot-value tokenizer 'current-tag-token))
                       (return-state (slot-value tokenizer 'return-state))
                       (current-attribute (slot-value tokenizer 'current-attribute))
                       (current-comment-token (slot-value tokenizer 'current-comment)))
       (macrolet ((switch-to (state)
                    `(setf (slot-value tokenizer 'state) ,state))
                  (reconsume-in (state)
                    `(progn
                       (reconsume tokenizer)
                       (setf (slot-value tokenizer 'state) ,state))))
         (flet (,@(loop for error-name in *parse-errors*
                    collect `(,error-name () (error ',error-name)))
                (emit (token)
                  (signal 'on-token :token token)))
           ,@(if body
                 body
               `((error "Tokenizer ~A not implemented" ',name))))))))

(define-tokenizer-state data-state
  (case next-input-character
    (#\&
     (setq return-state 'data-state)
     (switch-to 'character-reference-state))
    (#\<
     (switch-to 'tag-open-state))
    (#\null
     (unexpected-null-character)
     (emit current-input-character))
    ((nil)
     (emit end-of-file))
    (t
     (emit current-input-character))))

(define-tokenizer-state rcdata-state)
(define-tokenizer-state rawtext-state)
(define-tokenizer-state script-data-state)
(define-tokenizer-state plaintext-state)

(define-tokenizer-state tag-open-state
  (let ((char next-input-character))
    (cond 
     ((eq #\! char)
      (switch-to 'markup-declaration-open-state))
     ((eq #\/ char)
      (switch-to 'end-tag-open-state))
     ((ascii-alpha-p char)
      (let ((token (make-instance 'start-tag :tag-name "")))
        (setf current-tag-token token))
      (reconsume-in 'tag-name-state))
     ((eq #\? char)
      (unexpected-question-mark-instead-of-tag-name)
      (let ((token (make-instance 'comment :data "")))
        (setf current-comment-token token)
        (reconsume-in 'bogus-comment-state)))
     ((null char)
      (eof-before-tag-name)
      (emit #\<)
      (emit end-of-file))
     (t
      (invalid-first-character-of-tag-name)
      (emit #\<)
      (reconsume-in 'data-state)))))

(define-tokenizer-state end-tag-open-state
  (let ((char next-input-character))
    (cond
     ((ascii-alpha-p char)
      (let ((token (make-instance 'end-tag :tag-name "")))
        (setf current-tag-token token))
      (reconsume-in 'tag-name-state))
     ((eq #\> char)
      (missing-end-tag-name)
      (switch-to 'data-state))
     ((null char)
      (eof-before-tag-name)
      (emit #\<)
      (emit #\/)
      (emit end-of-file))
     (t
      (invalid-first-character-of-tag-name)
      (let ((token (make-instance 'comment :data "")))
        (setf current-comment-token token))
      (reconsume-in 'bogus-comment-state)))))

(define-tokenizer-state tag-name-state
  (let ((char next-input-character))
    (cond
     ((or (eq #\tab char)
          (eq #\newline char)
          (eq #\page char)
          (eq #\space char))
      (switch-to 'before-attribute-name-state))
     ((eq #\/ char)
      (switch-to 'self-closing-start-tag-state))
     ((eq #\> char)
      (switch-to 'data-state)
      (emit current-tag-token))
     ((ascii-upper-alpha-p char)
      (with-slots (tag-name) current-tag-token
        (setf tag-name (concatenate 'string tag-name
                                    (string (char-downcase current-input-character))))))
     ((eq #\null char)
      (unexpected-null-character)
      (with-slots (tag-name) current-tag-token
        (setf tag-name (concatenate 'string tag-name
                                    (string #\replacement-character)))))
     ((null char)
      (eof-in-tag)
      (emit end-of-file))
     (t
      (with-slots (tag-name) current-tag-token
        (setf tag-name (concatenate 'string tag-name
                                    (string current-input-character))))))))

(define-tokenizer-state rcdata-less-than-sign-state)
(define-tokenizer-state rcdata-end-tag-open-state)
(define-tokenizer-state rcdata-end-tag-name-state)
(define-tokenizer-state rawtext-less-than-sign-state)
(define-tokenizer-state rawtext-end-tag-open-state)
(define-tokenizer-state rawtext-end-tag-name-state)
(define-tokenizer-state script-data-less-than-sign-state)
(define-tokenizer-state script-data-end-tag-open-state)
(define-tokenizer-state script-data-end-tag-name-state)
(define-tokenizer-state script-data-escape-start-state)
(define-tokenizer-state script-data-escape-start-dash-state)
(define-tokenizer-state script-data-escaped-state)
(define-tokenizer-state script-data-escaped-dash-state)
(define-tokenizer-state script-data-escaped-dash-dash-state)
(define-tokenizer-state script-data-escape-start-dash-state)
(define-tokenizer-state script-data-escaped-end-tag-open-state)
(define-tokenizer-state script-data-escaped-end-tag-name-state)
(define-tokenizer-state script-data-double-escape-start-state)
(define-tokenizer-state script-data-double-escaped-state)
(define-tokenizer-state script-data-double-escaped-dash-state)
(define-tokenizer-state script-data-double-escaped-dash-dash-state)
(define-tokenizer-state script-data-double-escaped-less-than-sign-state)
(define-tokenizer-state script-data-double-escaped-state)

(define-tokenizer-state before-attribute-name-state
  (case next-input-character
    ((#\tab #\newline #\page #\space))
    ((#\/ #\> nil)
     (reconsume-in 'after-attribute-name-state))
    (#\=
     (unexpected-equals-sign-before-attribute-name)
     (let ((attribute (make-instance 'attribute
                                     :name (string current-input-character)
                                     :value "")))
       (appendf (slot-value current-tag-token 'attributes) (list attribute))
       (setf current-attribute attribute))
     (switch-to 'attribute-name-state))
    (t
     (let ((attribute (make-instance 'attribute :name "" :value "")))
       (appendf (slot-value current-tag-token 'attributes) (list attribute))
       (setf current-attribute attribute))
     (reconsume-in 'attribute-name-state))))

(define-tokenizer-state attribute-name-state
  (let ((char next-input-character))
    (cond
     ((or (eq #\tab char) (eq #\newline char)
          (eq #\page char) (eq #\space char)
          (eq #\/ char) (eq #\> char) (null char))
      (reconsume-in 'after-attribute-name-state))
     ((eq #\= char)
      (switch-to 'before-attribute-value-state))
     ((ascii-upper-alpha-p char)
      (setf (slot-value current-attribute 'name)
           (concatenate 'string (slot-value current-attribute 'name)
                        (string (char-downcase current-input-character)))))
     ((eq #\null char)
      (setf (slot-value current-attribute 'name)
            (concatenate 'string (slot-value current-attribute 'name)
                         (string #\replacement-character))))
     ((or (eq #\" char) (eq #\' char) (eq #\< char))
      (unexpected-character-in-attribute-name)
      (setf (slot-value current-attribute 'name)
            (concatenate 'string (slot-value current-attribute 'name)
                         (string current-input-character))))
     (t
      (setf (slot-value current-attribute 'name)
            (concatenate 'string (slot-value current-attribute 'name)
                         (string current-input-character)))))))

(define-tokenizer-state after-attribute-name-state
  (case next-input-character
    ((#\tab #\newline #\page #\space))
    (#\/
     (switch-to 'self-closing-start-tag-state))
    (#\=
     (switch-to 'before-attribute-value-state))
    (#\>
     (switch-to 'data-state)
     (emit current-tag-token))
    ((nil)
     (eof-in-tag)
     (emit end-of-file))
    (t
     (let ((attribute (make-instance 'attribute :name "" :value "")))
       (appendf (slot-value current-tag-token 'attributes) (list attribute))
       (setf current-attribute attribute))
     (reconsume-in 'attribute-name-state))))

(define-tokenizer-state before-attribute-value-state
  (case next-input-character
    ((#\tab #\newline #\page #\space))
    (#\"
     (switch-to 'attribute-value-double-quoted-state))
    (#\'
     (switch-to 'attribute-value-single-quoted-state))
    (#\>
     (missing-attribute-value)
     (switch-to 'data-state)
     (emit current-tag-token))
    (t
     (reconsume-in 'attribute-value-unquoted-state))))

(define-tokenizer-state attribute-value-double-quoted-state
  (case next-input-character
    (#\"
     (switch-to 'after-attribute-value-quoted-state))
    (#\&
     (setf return-state 'attribute-value-double-quoted-state)
     (switch-to 'character-reference-state))
    (#\null
     (unexpected-null-character)
     (setf (slot-value current-attribute 'value)
           (concatenate 'string (slot-value current-attribute 'value)
                        (string #\replacement-character))))
    ((nil)
     (eof-in-tag)
     (emit end-of-file))
    (t
     (setf (slot-value current-attribute 'value)
           (concatenate 'string (slot-value current-attribute 'value)
                        (string current-input-character))))))

(define-tokenizer-state attribute-value-single-quoted-state
  (case next-input-character
    (#\'
     (switch-to 'after-attribute-value-quoted-state))
    (#\&
     (setf return-state 'attribute-value-single-quoted-state)
     (switch-to 'character-reference-state))
    (#\null
     (unexpected-null-character)
     (setf (slot-value current-attribute 'value)
           (concatenate 'string (slot-value current-attribute 'value)
                        (string #\replacement-character))))
    ((nil)
     (eof-in-tag)
     (emit end-of-file))
    (t
     (setf (slot-value current-attribute 'value)
           (concatenate 'string (slot-value current-attribute 'value)
                        (string current-input-character))))))

(define-tokenizer-state attribute-value-unquoted-state
  (case next-input-character
    ((#\tab #\newline #\page #\space)
     (switch-to 'before-attribute-name-state))
    (#\&
     (setf return-state 'attribute-value-unquoted-state)
     (switch-to 'character-reference-state))
    (#\>
     (switch-to 'data-state)
     (emit current-tag-token))
    (#\null
     (unexpected-null-character)
     (setf (slot-value current-attribute 'value)
           (concatenate 'string (slot-value current-attribute 'value)
                        (string #\replacement-character))))
    ((#\" #\' #\< #\= #\`)
     (unexpected-character-in-unquoted-attribute-value)
     (setf (slot-value current-attribute 'value)
           (concatenate 'string (slot-value current-attribute 'value)
                        (string current-input-character))))
    ((nil)
     (eof-in-tag)
     (emit end-of-file))
    (t
     (setf (slot-value current-attribute 'value)
           (concatenate 'string (slot-value current-attribute 'value)
                        (string current-input-character))))))

(define-tokenizer-state after-attribute-value-quoted-state
  (case next-input-character
    ((#\tab #\newline #\page #\space)
     (switch-to 'before-attribute-name-state))
    (#\/
     (switch-to 'self-closing-start-tag-state))
    (#\>
     (switch-to 'data-state)
     (emit current-tag-token))
    ((nil)
     (eof-in-tag)
     (emit end-of-file))
    (t
     (missing-whitespace-between-attributes)
     (reconsume-in 'before-attribute-name-state))))

(define-tokenizer-state self-closing-start-tag-state
  (case next-input-character
    (#\>
     (setf (slot-value current-tag-token 'self-closing-flag) t)
     (switch-to 'data-state)
     (emit current-tag-token))
    ((nil)
     (eof-in-tag)
     (emit end-of-file))
    (t
     (unexpected-solidus-in-tag)
     (reconsume-in 'before-attribute-name-state))))

(define-tokenizer-state bogus-comment-state
  )

(define-tokenizer-state markup-declaration-open-state)
(define-tokenizer-state comment-start-state)
(define-tokenizer-state comment-start-dash-state)
(define-tokenizer-state comment-state)
(define-tokenizer-state comment-less-than-sign-state)
(define-tokenizer-state comment-less-than-sign-bang-state)
(define-tokenizer-state comment-less-than-sign-bang-dash-state)
(define-tokenizer-state comment-less-than-sign-bang-dash-dash-state)
(define-tokenizer-state comment-end-dash-state)
(define-tokenizer-state comment-end-state)
(define-tokenizer-state comment-end-bang-state)
(define-tokenizer-state doctype-state)
(define-tokenizer-state before-doctype-name-state)
(define-tokenizer-state doctype-name-state)
(define-tokenizer-state after-doctype-name-state)
(define-tokenizer-state after-doctype-public-keyword-state)
(define-tokenizer-state before-doctype-public-identifier-state)
(define-tokenizer-state doctype-public-identifier-double-quoted-state)
(define-tokenizer-state doctype-public-identifier-single-quoted-state)
(define-tokenizer-state after-doctype-public-identifier-state)
(define-tokenizer-state between-doctype-public-and-system-identifiers-state)
(define-tokenizer-state after-doctype-system-keyword-state)
(define-tokenizer-state before-doctype-system-identifier-state)
(define-tokenizer-state doctype-system-identifier-double-quoted-state)
(define-tokenizer-state doctype-system-identifier-single-quoted-state)
(define-tokenizer-state after-doctype-system-identifier-state)
(define-tokenizer-state bogus-doctype-state)
(define-tokenizer-state cdata-section-state)
(define-tokenizer-state cdata-section-bracket-state)
(define-tokenizer-state cdata-section-end-state)
(define-tokenizer-state character-reference-state)
(define-tokenizer-state named-character-reference-state)
(define-tokenizer-state ambiguous-ampersand-state)
(define-tokenizer-state numeric-character-reference-state)
(define-tokenizer-state hexadecimal-character-reference-start-state)
(define-tokenizer-state decimal-character-reference-start-state)
(define-tokenizer-state hexadecimal-character-reference-state)
(define-tokenizer-state decimal-character-reference-state)
(define-tokenizer-state numeric-character-reference-end-state)

(defclass tokenizer ()
  ((state
    :initarg :state
    :initform 'data-state)
   (stream
    :initarg :stream
    :initform nil)
   (current-input-character
    :initform nil
    :reader current-input-character)
   (current-tag-token :initform nil)
   (current-attribute :initform nil)))

(defun next-input-character (tokenizer)
  (with-slots (current-input-character stream) tokenizer
    (setf current-input-character (read-char stream nil nil))))

(defun reconsume (tokenizer)
  (with-slots (current-input-character stream) tokenizer
    (unread-char current-input-character stream)
    (setf current-input-character nil)))

(defgeneric tokenize (source)
  (:method ((source string))
   (with-input-from-string (stream source)
     (tokenize stream)))
  (:method ((stream stream))
   (let ((tokenizer (make-instance 'tokenizer :stream stream)))
     (loop with tokens = '()
           do (handler-bind ((on-token (lambda (c)
                                         (let ((token (slot-value c 'token)))
                                           (format t "~A~%" token)
                                           (if (typep token 'end-of-file)
                                               (loop-finish)
                                             (push token tokens))))))
                (funcall (slot-value tokenizer 'state) tokenizer))
           finally (return (reverse tokens))))))