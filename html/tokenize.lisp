(in-package :html)

(defclass token ()
  ((value
    :initarg :value
    :initform nil)))

(defclass doctype (token)
  ((force-quirks-flag
    :initarg :force-quirks-flag
    :initform nil)))

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

(defmacro append-char (place char)
  `(setf ,place
         (concatenate 'string (string ,char))))

(defmacro define-tokenizer-state (name &body body)
  `(defun ,name (tokenizer)
     ,@(unless body '((declare (ignore tokenizer))))
     (symbol-macrolet ((next-input-character (next-input-character tokenizer))
                       (current-input-character (current-input-character tokenizer))
                       (end-of-file (make-instance 'end-of-file))
                       (current-tag-token (slot-value tokenizer 'current-tag-token))
                       (return-state (slot-value tokenizer 'return-state))
                       (current-attribute (slot-value tokenizer 'current-attribute))
                       (current-comment-token (slot-value tokenizer 'current-comment))
                       (temporary-buffer (slot-value tokenizer 'temporary-buffer)))
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

(define-tokenizer-state rcdata-state
  (case next-input-character
    (#\&
     (setf return-state 'rcdata-state)
     (switch-to 'character-reference-state))
    (#\<
     (switch-to 'rcdata-less-than-sign-state))
    (#\null
     (unexpected-null-character)
     (emit #\replacement-character))
    ((nil)
     (emit end-of-file))
    (t
     (emit current-input-character))))

(define-tokenizer-state rawtext-state
  (case next-input-character
    (#\<
     (switch-to 'rawtext-less-than-sign-state))
    (#\null
     (unexpected-null-character)
     (emit #\replacement-character))
    ((nil)
     (emit end-of-file))
    (emit current-input-character)))

(define-tokenizer-state script-data-state
  (case next-input-character
    (#\<
     (switch-to 'script-data-less-than-sign-state))
    (#\null
     (unexpected-null-character)
     (emit #\replacement-character))
    ((nil)
     (emit end-of-file))
    (t
     (emit current-input-character))))

(define-tokenizer-state plaintext-state
  (case next-input-character
    (#\null
     (unexpected-null-character)
     (emit #\replacement-character))
    ((nil)
     (emit end-of-file))
    (t
     (emit current-input-character))))

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
        (append-char tag-name (char-downcase current-input-character))))
     ((eq #\null char)
      (unexpected-null-character)
      (with-slots (tag-name) current-tag-token
        (append-char tag-name #\replacement-character)))
     ((null char)
      (eof-in-tag)
      (emit end-of-file))
     (t
      (with-slots (tag-name) current-tag-token
        (append-char tag-name current-input-character))))))

(define-tokenizer-state rcdata-less-than-sign-state
  (case next-input-character
    (#\/
     (setf temporary-buffer "")
     (switch-to 'rcdata-end-tag-open-state))
    (t
     (emit #\<)
     (reconsume-in 'rcdata-state))))

(define-tokenizer-state rcdata-end-tag-open-state
  (let ((char next-input-character))
    (cond
     ((ascii-alpha-p char)
      (let ((token (make-instance 'end-tag :tag-name "")))
        (setf current-tag-token token)
        (reconsume-in 'rcdata-end-tag-name-state)))
     (t
      (emit #\<)
      (emit #\/)
      (reconsume-in 'rcdata-state)))))

(define-tokenizer-state rcdata-end-tag-name-state
  (let ((char next-input-character))
    (cond
     ((or (eq #\tab char) (eq #\newline char)
          (eq #\page char) (eq #\space char))
      (if (appropriate-end-tag-token-p current-tag-token)
          (switch-to 'before-attribute-name-state)
        (progn
          (emit #\<)
          (emit #\/)
          (loop for char across temporary-buffer do (emit char))
          (reconsume-in 'rcdata-state))))
     ((eq #\/ char)
      (if (appropriate-end-tag-token-p current-tag-token)
          (switch-to 'self-closing-start-tag-state)
        (progn
          (emit #\<)
          (emit #\/)
          (loop for char across temporary-buffer do (emit char))
          (reconsume-in 'rcdata-state))))
     ((eq #\> char)
      (if (appropriate-end-tag-token-p current-tag-token)
          (progn
            (switch-to 'data-state)
            (emit current-tag-token))
        (progn
          (emit #\<)
          (emit #\/)
          (loop for char across temporary-buffer do (emit char))
          (reconsume-in 'rcdata-state))))
     ((ascii-upper-alpha-p char)
      (append-char (slot-value current-tag-token 'tag-name)
                   (char-downcase current-input-character))
      (append-char temporary-buffer current-input-character))
     ((ascii-lower-alpha-p char)
      (append-char (slot-value current-tag-token 'tag-name)
                   current-input-character)
      (append-char temporary-buffer current-input-character))
     (t
      (emit #\<)
      (emit #\/)
      (loop for char across temporary-buffer do (emit char))
      (reconsume-in 'rcdata-state)))))

(define-tokenizer-state rawtext-less-than-sign-state
  (case next-input-character
    (#\/
     (setf temporary-buffer "")
     (switch-to 'rawtext-end-tag-open-state))
    (t
     (emit #\<)
     (reconsume-in 'rawtext-state))))

(define-tokenizer-state rawtext-end-tag-open-state
  (let ((char next-input-character))
    (cond
     ((ascii-alpha-p char)
      (let ((token (make-instance 'end-tag :tag-name "")))
        (setf current-tag-token token)
        (reconsume-in 'rawtext-end-tag-name-state)))
     (t
      (emit #\<)
      (emit #\/)
      (reconsume-in 'rawtext-state)))))

(define-tokenizer-state rawtext-end-tag-name-state
  (let ((char next-input-character))
    (cond
     ((or (eq #\tab char) (eq #\newline char)
          (eq #\page char) (eq #\space char))
      (if (appropriate-end-tag-token-p current-tag-token)
          (switch-to 'before-attribute-name-state)
        (progn
          (emit #\<)
          (emit #\/)
          (loop for char across temporary-buffer do (emit char))
          (reconsume-in 'rawtext-state))))
     ((eq #\/ char)
      (if (appropriate-end-tag-token-p current-tag-token)
          (switch-to 'self-closing-start-tag-state)
        (progn
          (emit #\<)
          (emit #\/)
          (loop for char across temporary-buffer do (emit char))
          (reconsume-in 'rawtext-state))))
     ((eq #\> char)
      (if (appropriate-end-tag-token-p current-tag-token)
          (progn
            (switch-to 'data-state)
            (emit current-tag-token))
        (progn
          (emit #\<)
          (emit #\/)
          (loop for char across temporary-buffer do (emit char))
          (reconsume-in 'rawtext-state))))
     ((ascii-upper-alpha-p char)
      (append-char (slot-value current-tag-token 'tag-name)
                   (char-downcase current-input-character))
      (append-char temporary-buffer current-input-character))
     ((ascii-lower-alpha-p char)
      (append-char (slot-value current-tag-token 'tag-name)
                   current-input-character)
      (append-char temporary-buffer current-input-character))
     (t
      (emit #\<)
      (emit #\/)
      (loop for char across temporary-buffer do (emit char))
      (reconsume-in 'rawtext-state)))))

(define-tokenizer-state script-data-less-than-sign-state
  (case next-input-character
    (#\/
     (setf temporary-buffer "")
     (switch-to 'script-data-end-tag-open-state))
    (#\!
     (switch-to 'script-data-escape-start-state)
     (emit #\<)
     (emit #\!))
    (t
     (emit #\<)
     (reconsume-in 'script-data-state))))

(define-tokenizer-state script-data-end-tag-open-state
  (let ((char next-input-character))
    (cond
     ((ascii-alpha-p char)
      (let ((token (make-instance 'end-tag :tag-name "")))
        (setf current-tag-token token)
        (reconsume 'script-data-end-tag-name-state)))
     (t
      (emit #\<)
      (emit #\/)
      (reconsume-in 'script-data-state)))))

(define-tokenizer-state script-data-end-tag-name-state
  (let ((char next-input-character))
    (cond
     ((or (eq #\tab char) (eq #\newline char)
          (eq #\page char) (eq #\space char))
      (if (appropriate-end-tag-token-p current-tag-token)
          (switch-to 'before-attribute-name-state)
        (progn
          (emit #\<)
          (emit #\/)
          (loop for char across temporary-buffer do (emit char))
          (reconsume-in 'script-data-state))))
     ((eq #\/ char)
      (if (appropriate-end-tag-token-p current-tag-token)
          (switch-to 'self-closing-start-tag-state)
        (progn
          (emit #\<)
          (emit #\/)
          (loop for char across temporary-buffer do (emit char))
          (reconsume-in 'script-data-state))))
     ((eq #\> char)
      (if (appropriate-end-tag-token-p current-tag-token)
          (progn
            (switch-to 'data-state)
            (emit current-tag-token))
        (progn
          (emit #\<)
          (emit #\/)
          (loop for char across temporary-buffer do (emit char))
          (reconsume-in 'script-data-state))))
     ((ascii-upper-alpha-p char)
      (append-char (slot-value current-tag-token 'tag-name)
                   (char-downcase current-input-character))
      (append-char temporary-buffer current-input-character))
     ((ascii-lower-alpha-p char)
      (append-char (slot-value current-tag-token 'tag-name)
                   current-input-character)
      (append-char temporary-buffer current-input-character))
     (t
      (emit #\<)
      (emit #\/)
      (loop for char across temporary-buffer do (emit char))
      (reconsume-in 'script-data-state)))))

(define-tokenizer-state script-data-escape-start-state
  (case next-input-character
    (#\-
     (switch-to 'script-data-escape-start-dash-state)
     (emit #\-))
    (t
     (reconsume-in 'script-data-state))))

(define-tokenizer-state script-data-escape-start-dash-state
  (case next-input-character
    (#\-
     (switch-to 'script-data-escaped-dash-dash-state)
     (emit #\-))
    (t
     (reconsume-in 'script-data-state))))

(define-tokenizer-state script-data-escaped-state
  (case next-input-character
    (#\-
     (switch-to 'script-data-escaped-dash-state)
     (emit #\-))
    (#\<
     (switch-to 'script-data-escaped-less-than-sign-state))
    (#\null
     (unexpected-null-character)
     (emit #\replacement-character))
    ((nil)
     (eof-in-script-html-comment-like-text)
     (emit end-of-file))
    (t
     (emit current-input-character))))

(define-tokenizer-state script-data-escaped-dash-state
  (case next-input-character
    (#\-
     (switch-to 'script-data-escaped-dash-dash-state)
     (emit #\-))
    (#\<
     (switch-to 'script-data-escaped-less-than-sign-state))
    (#\null
     (unexpected-null-character)
     (switch-to 'script-data-escaped-state)
     (emit #\replacement-character))
    ((nil)
     (eof-in-script-html-comment-like-text)
     (emit end-of-file))
    (t
     (switch-to 'script-data-escaped-state)
     (emit current-input-character))))

(define-tokenizer-state script-data-escaped-dash-dash-state
  (case next-input-character
    (#\-
     (emit #\-))
    (#\<
     (switch-to 'script-data-escaped-less-than-sign-state))
    (#\>
     (switch-to 'script-data-state)
     (emit #\>))
    (#\null
     (unexpected-null-character)
     (switch-to 'script-data-escaped-state)
     (emit #\replacement-character))
    ((nil)
     (eof-in-script-html-comment-like-text)
     (emit end-of-file))
    (t
     (switch-to 'script-data-escaped-state)
     (emit current-input-character))))

(define-tokenizer-state script-data-escaped-less-than-sign-state
  (let ((char next-input-character))
    (cond
     ((eq #\/ char)
      (setf temporary-buffer "")
      (switch-to 'script-data-escaped-end-tag-open-state))
     ((ascii-alpha-p char)
      (setf temporary-buffer "")
      (emit #\<)
      (reconsume-in 'script-data-double-escape-start-state))
     (t
      (emit #\<)
      (reconsume-in 'script-data-escaped-state)))))

(define-tokenizer-state script-data-escaped-end-tag-open-state
  (let ((char next-input-character))
    (cond
     ((ascii-alpha-p char)
      (let ((token (make-instance 'end-tag :tag-name "")))
        (setf current-tag-token token)
        (reconsume-in 'script-data-escaped-end-tag-name-state)))
     (t
      (emit #\<)
      (emit #\/)
      (reconsume-in 'script-data-escaped-state)))))

(define-tokenizer-state script-data-escaped-end-tag-name-state
  (let ((char next-input-character))
    (cond
     ((or (eq #\tab char) (eq #\newline char)
          (eq #\page char) (eq #\space char))
      (if (appropriate-end-tag-token-p current-tag-token)
          (switch-to 'before-attribute-name-state)
        (progn
          (emit #\<)
          (emit #\/)
          (loop for char across temporary-buffer do (emit char))
          (reconsume-in 'script-data-escaped-state))))
     ((eq #\/ char)
      (if (appropriate-end-tag-token-p current-tag-token)
          (switch-to 'self-closing-start-tag-state)
        (progn
          (emit #\<)
          (emit #\/)
          (loop for char across temporary-buffer do (emit char))
          (reconsume-in 'script-data-escaped-state))))
     ((eq #\> char)
      (if (appropriate-end-tag-token-p current-tag-token)
          (progn
            (switch-to 'data-state)
            (emit current-tag-token))
        (progn
          (emit #\<)
          (emit #\/)
          (loop for char across temporary-buffer do (emit char))
          (reconsume-in 'script-data-escaped-state))))
     ((ascii-upper-alpha-p char)
      (append-char (slot-value current-tag-token 'tag-name)
                   (char-downcase current-input-character))
      (append-char temporary-buffer current-input-character))
     ((ascii-lower-alpha-p char)
      (append-char (slot-value current-tag-token 'tag-name)
                   current-input-character)
      (append-char temporary-buffer current-input-character))
     (t
      (emit #\<)
      (emit #\/)
      (loop for char across temporary-buffer do (emit char))
      (reconsume-in 'script-data-escaped-state)))))

(define-tokenizer-state script-data-double-escape-start-state
  (let ((char next-input-character))
    (cond
     ((or (eq #\tab char) (eq #\newline char)
          (eq #\page char) (eq #\space char)
          (eq #\/ char) (eq #\> char))
      (if (equal temporary-buffer "script")
          (switch-to 'script-data-double-escaped-state)
        (switch-to 'script-data-escaped-state))
      (emit current-input-character))
     ((ascii-upper-alpha-p char)
      (append-char temporary-buffer (char-downcase current-input-character))
      (emit current-input-character))
     ((ascii-lower-alpha-p char)
      (append-char temporary-buffer current-input-character)
      (emit current-input-character))
     (t
      (reconsume-in 'script-data-escaped-state)))))

(define-tokenizer-state script-data-double-escaped-state
  (case next-input-character
    (#\-
     (switch-to 'script-data-double-escaped-dash-state)
     (emit #\-))
    (#\<
     (switch-to 'script-data-escaped-less-than-sign-state)
     (emit #\<))
    (#\null
     (unexpected-null-character)
     (emit #\replacement-character))
    ((nil)
     (eof-in-script-html-comment-like-text)
     (emit end-of-file))
    (t
     (emit current-input-character))))

(define-tokenizer-state script-data-double-escaped-dash-state
  (case next-input-character
    (#\-
     (switch-to 'script-data-double-escaped-dash-dash-state)
     (emit #\-))
    (#\<
     (switch-to 'script-data-escaped-less-than-sign-state)
     (emit #\<))
    (#\null
     (unexpected-null-character)
     (switch-to 'script-data-double-escaped-state)
     (emit #\replacement-character))
    ((nil)
     (eof-in-script-html-comment-like-text)
     (emit end-of-file))
    (t
     (switch-to 'script-data-double-escaped-state)
     (emit current-input-character))))

(define-tokenizer-state script-data-double-escaped-dash-dash-state
  (case next-input-character
    (#\-
     (emit #\-))
    (#\<
     (switch-to 'script-data-escaped-less-than-sign-state)
     (emit #\<))
    (#\>
     (switch-to 'script-data-state)
     (emit #\>))
    (#\null
     (unexpected-null-character)
     (switch-to 'script-data-double-escaped-state)
     (emit #\replacement-character))
    ((nil)
     (eof-in-script-html-comment-like-text)
     (emit end-of-file))
    (t
     (switch-to 'script-data-double-escaped-state)
     (emit current-input-character))))

(define-tokenizer-state script-data-double-escaped-less-than-sign-state
  (case next-input-character
    (#\/
     (setf temporary-buffer "")
     (switch-to 'script-data-double-escape-end-state)
     (emit #\/))
    (t
     (reconsume-in 'script-data-double-escaped-state))))

(define-tokenizer-state script-data-double-escape-end-state
  (let ((char next-input-character))
    (cond
     ((or (eq #\tab char) (eq #\newline char)
          (eq #\page char) (eq #\space char)
          (eq #\/ char) (eq #\> char))
      (if (equal temporary-buffer "script")
          (switch-to 'script-data-escaped-state)
        (switch-to 'script-data-double-escaped-state))
      (emit current-input-character))
     ((ascii-upper-alpha-p char)
      (append-char temporary-buffer (char-downcase current-input-character))
      (emit current-input-character))
     ((ascii-lower-alpha-p char)
      (append-char temporary-buffer current-input-character)
      (emit current-input-character))
     (t
      (reconsume-in 'script-data-double-escaped-state)))))

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
      (append-char (slot-value current-attribute 'name)
                   (char-downcase current-input-character)))
     ((eq #\null char)
      (append-char (slot-value current-attribute 'name)
                   #\replacement-character))
     ((or (eq #\" char) (eq #\' char) (eq #\< char))
      (unexpected-character-in-attribute-name)
      (append-char (slot-value current-attribute 'name)
                   current-input-character))
     (t
      (append-char (slot-value current-attribute 'name)
                   current-input-character)))))

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
     (append-char (slot-value current-attribute 'value)
                  #\replacement-character))
    ((nil)
     (eof-in-tag)
     (emit end-of-file))
    (t
     (append-char (slot-value current-attribute 'value)
                  current-input-character))))

(define-tokenizer-state attribute-value-single-quoted-state
  (case next-input-character
    (#\'
     (switch-to 'after-attribute-value-quoted-state))
    (#\&
     (setf return-state 'attribute-value-single-quoted-state)
     (switch-to 'character-reference-state))
    (#\null
     (unexpected-null-character)
     (append-char (slot-value current-attribute 'value)
                  #\replacement-character))
    ((nil)
     (eof-in-tag)
     (emit end-of-file))
    (t
     (append-char (slot-value current-attribute 'value)
                  current-input-character))))

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
     (append-char (slot-value current-attribute 'value)
                  #\replacement-character))
    ((#\" #\' #\< #\= #\`)
     (unexpected-character-in-unquoted-attribute-value)
     (append-char (slot-value current-attribute 'value)
                  current-input-character))
    ((nil)
     (eof-in-tag)
     (emit end-of-file))
    (t
     (append-char (slot-value current-attribute 'value)
                  current-input-character))))

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
  (case next-input-character
    (#\>
     (switch-to 'data-state)
     (emit current-comment-token))
    ((nil)
     (emit current-comment-token)
     (emit end-of-file))
    (#\null
     (unexpected-null-character)
     (append-char (slot-value current-comment-token 'data)
                  #\replacement-character))
    (t
     (append-char (slot-value current-comment-token 'data)
                  current-input-character))))

(define-tokenizer-state markup-declaration-open-state)

(define-tokenizer-state comment-start-state
  (case next-input-character
    (#\-
     (switch-to 'comment-start-dash-state))
    (#\>
     (abrupt-closing-of-empty-comment)
     (switch-to 'data-state)
     (emit current-comment-token))
    (t
     (reconsume-in 'comment-state))))

(define-tokenizer-state comment-start-dash-state
  (case next-input-character
    (#\-
     (switch-to 'comment-end-state))
    (#\>
     (abrupt-closing-of-empty-comment)
     (switch-to 'data-state)
     (emit current-comment-token))
    ((nil)
     (eof-in-comment)
     (emit current-comment-token)
     (emit end-of-file))
    (t
     (append-char (slot-value current-comment-token 'data) #\-)
     (reconsume-in 'comment-state))))

(define-tokenizer-state comment-state
  (case next-input-character
    (#\<
     (append-char (slot-value current-comment-token 'data) current-input-character)
     (switch-to 'comment-less-than-sign-state))
    (#\-
     (switch-to 'comment-end-dash-state))
    (#\null
     (unexpected-null-character)
     (append-char (slot-value current-comment-token 'data) #\replacement-character))
    ((nil)
     (eof-in-comment)
     (emit current-comment-token)
     (emit end-of-file))
    (t
     (append-char (slot-value current-comment-token 'data) current-input-character))))

(define-tokenizer-state comment-less-than-sign-state
  (case next-input-character
    (#\!
     (append-char (slot-value current-comment-token 'data) current-input-character)
     (switch-to 'comment-less-than-sign-bang-state))
    (#\<
     (append-char (slot-value current-comment-token 'data) current-input-character))
    (t
     (reconsume-in 'comment-state))))

(define-tokenizer-state comment-less-than-sign-bang-state
  (case next-input-character
    (#\-
     (switch-to 'comment-less-than-sign-bang-dash-state))
    (t
     (reconsume-in 'comment-state))))

(define-tokenizer-state comment-less-than-sign-bang-dash-state
  (case next-input-character
    (#\-
     (switch-to 'comment-less-than-sign-bang-dash-dash-state))
    (t
     (reconsume-in 'comment-end-dash-state))))

(define-tokenizer-state comment-less-than-sign-bang-dash-dash-state
  (case next-input-character
    ((#\- nil)
     (switch-to 'comment-end-state))
    (t
     (nested-comment)
     (reconsume-in 'comment-end-state))))

(define-tokenizer-state comment-end-dash-state
  (case next-input-character
    ((#\-)
     (switch-to 'comment-end-state))
    ((nil)
     (eof-in-comment)
     (emit current-comment-token)
     (emit end-of-file))
    (t
     (append-char (slot-value current-comment-token 'data) #\-)
     (reconsume-in comment-state))))

(define-tokenizer-state comment-end-state
  (case next-input-character
    (#\>
     (switch-to 'data-state)
     (emit current-comment-token))
    (#\!
     (switch-to 'comment-end-bang-state))
    (#\-
     (append-char (slot-value current-comment-token 'data) #\-))
    ((nil)
     (eof-in-comment)
     (emit current-comment-token)
     (emit end-of-file))
    (t
     (append-char (slot-value current-comment-token 'data) #\-)
     (append-char (slot-value current-comment-token 'data) #\-))))

(define-tokenizer-state comment-end-bang-state
  (case next-input-character
    (#\-
     (append-char (slot-value current-comment-token 'data) #\-)
     (append-char (slot-value current-comment-token 'data) #\-)
     (append-char (slot-value current-comment-token 'data) #\!)
     (switch-to 'comment-end-dash-state))
    (#\>
     (incorrectly-closed-comment)
     (switch-to 'data-state)
     (emit current-comment-token))
    ((nil)
     (eof-in-comment)
     (emit current-comment-token)
     (emit end-of-file))
    (t
     (append-char (slot-value current-comment-token 'data) #\-)
     (append-char (slot-value current-comment-token 'data) #\-)
     (append-char (slot-value current-comment-token 'data) #\!)
     (reconsume-in 'comment-state))))

(define-tokenizer-state doctype-state
  (case next-input-character
    ((#\tab #\newline #\page #\space)
     (switch-to 'before-doctype-name-state))
    (#\>
     (reconsume-in 'before-doctype-name-state))
    ((nil)
     (eof-in-doctype)
     (let ((token (make-instance 'doctype :force-quirks-flag :on)))
       (emit token)
       (emit end-of-file)))
    (t
     (missing-whitespace-before-doctype-name)
     (reconsume-in 'before-doctype-name-state))))

(define-tokenizer-state before-doctype-name-state
  (let ((char next-input-character))
    (cond
     ((or (eq #\tab char) (eq #\newline char)
          (eq #\page char) (eq #\space char)))
     ((ascii-upper-alpha-p char)
      (let ((token (make-instance 'doctype
                                  :name (string (char-downcase current-input-character)))))
        (switch-to 'doctype-name-state)))
     ((eq #\null char)
      (unexpected-null-character)
      (let ((token (make-instance 'doctype
                                  :name (string #\replacement-character))))
        (switch-to 'doctype-name-state)))
     ((eq #\> char)
      (missing-doctype-name)
      (let ((token (make-instance 'doctype :force-quirks-flag :on)))
        (switch-to 'data-state)
        (emit token)))
     ((null char)
      (eof-in-doctype)
      (let ((token (make-instance 'doctype :force-quirks-flag :on)))
        (emit token)
        (emit end-of-file)))
     (t
      (let ((token (make-instance 'doctype :name (string current-input-character))))
        (switch-to 'doctype-name-state))))))

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
   (current-attribute :initform nil)
   (temporary-buffer :initform nil)))

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