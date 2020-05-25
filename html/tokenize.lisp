(in-package :html)

;; https://html.spec.whatwg.org/multipage/parsing.html#tokenization

(defparameter stream nil)
(defparameter state nil)
(defparameter return-state nil)
(defparameter lookahead-buffer nil)
(defparameter current-input-character nil)
(defparameter current-tag-token nil)
(defparameter current-doctype-token nil)
(defparameter current-attribute nil)
(defparameter current-comment-token nil)
(defparameter character-reference-code nil)
(defparameter last-start-tag-token nil)
(defparameter temporary-buffer nil)
(defparameter pending-tokens nil)

(define-symbol-macro next-input-character (consume))

(defclass token () ())

(defclass doctype-token (token)
  ((name
    :initarg :name
    :initform nil)
   (public-identifier
    :initarg :public-identifier
    :initform nil)
   (system-identifier
    :initarg :system-identifier
    :initform nil)
   (force-quirks-flag
    :initarg :force-quirks-flag
    :initform nil)))

(defmethod print-object ((doctype-token doctype-token) stream)
  (print-unreadable-object (doctype-token stream :type t)
    (with-slots (name public-identifier system-identifier) doctype-token
      (when name
        (format stream " NAME: ~S" name))
      (when public-identifier
        (format stream " PUBLIC-IDENTIFIER: ~S" public-identifier))
      (when system-identifier
        (format stream " SYSTEM-IDENTIFIER: ~S" system-identifier)))))

(defclass start-tag (token)
  ((tag-name
    :initarg :tag-name
    :initform nil)
   (attributes
    :initarg :attributes
    :initform nil)
   (self-closing-flag
    :initform nil)))

(defmethod print-object ((start-tag start-tag) stream)
  (with-slots (tag-name attributes) start-tag
    (write-char #\< stream)
    (write-string (string-upcase tag-name) stream)
    (when attributes
      (loop for attribute in attributes
            do (format stream " ~A=~S"
                       (slot-value attribute 'name)
                       (slot-value attribute 'value))))
    (write-char #\> stream)))

(defclass end-tag (token)
  ((tag-name
    :initarg :tag-name
    :initform nil)))

(defmethod print-object ((end-tag end-tag) stream)
  (write-char #\< stream)
  (write-char #\/ stream)
  (write-string (string-upcase (slot-value end-tag 'tag-name)) stream)
  (write-char #\> stream))

(defclass comment-token (token)
  ((data
    :initarg :data
    :initform nil)))

(defmethod print-object ((comment-token comment-token) stream)
  (format stream "<!--~A-->" (slot-value comment-token 'data)))

(defclass end-of-file (token) ())

(defvar end-of-file (make-instance 'end-of-file))

(defmethod print-object ((end-of-file end-of-file) stream)
  (print-unreadable-object (end-of-file stream :type t)))

(defclass attribute ()
  ((name
    :initarg :name
    :initform nil
    :reader attribute-name)
   (value
    :initarg :value
    :initform nil
    :reader attribute-value)))

(declaim (inline consume))
(defun consume (&optional (n 1))
  (loop for i from 1 upto n
        for char = (or (pop lookahead-buffer)
                       (read-char stream nil nil))
        do (progn
             (setf current-input-character char)
             (when (= i n)
               (return char)))))

(declaim (inline reconsume))
(defun reconsume ()
  (push current-input-character lookahead-buffer))

(declaim (inline next-few-characters))
(defun next-few-characters (n)
  (loop repeat (- n (length lookahead-buffer))
        for char = (read-char stream nil nil)
        while char
        do (appendf lookahead-buffer (list char)))
  (if (> n (length lookahead-buffer))
      (coerce lookahead-buffer 'string)
    (coerce (subseq lookahead-buffer 0 n) 'string)))

(declaim (inline ascii-alpha-p ascii-upper-alpha-p
                 ascii-lower-alpha-p ascii-digit-p
                 ascii-upper-hex-digit-p ascii-lower-hex-digit-p
                 ascii-hex-digit-p ascii-alphanumeric-p
                 surrogate-p noncharacter-p c0-control-p
                 control-p whitespace-p))

(defun ascii-upper-alpha-p (char)
  (and char
       (char<= #\A char #\Z)))

(defun ascii-lower-alpha-p (char)
  (and char
       (char<= #\a char #\z)))

(defun ascii-alpha-p (char)
  (or (ascii-upper-alpha-p char)
      (ascii-lower-alpha-p char)))

(defun ascii-digit-p (char)
  (and char
       (char<= #\0 char #\9)))

(defun ascii-upper-hex-digit-p (char)
  (and char
       (char<= #\A char #\F)))

(defun ascii-lower-hex-digit-p (char)
  (and char
       (char<= #\f char #\f)))

(defun ascii-hex-digit-p (char)
  (or (ascii-upper-hex-digit-p char)
      (ascii-lower-hex-digit-p char)))

(defun ascii-alphanumeric-p (char)
  (or (ascii-digit-p char)
      (ascii-alpha-p char)))

(defun surrogate-p (code)
  (and code
       (<= #xD800 code #xDFFF)))

(defun noncharacter-p (code)
  (and code
       (<= #xFDD0 code #xFDEF)
       (find code '(#xFFFE #xFFFF #x1FFFE #x1FFFF #x2FFFE #x2FFFF
                           #x3FFFE #x3FFFF #x4FFFE #x4FFFF #x5FFFE
                           #x5FFFF #x6FFFE #x6FFFF #x7FFFE #x7FFFF
                           #x8FFFE #x8FFFF #x9FFFE #x9FFFF #xAFFFE
                           #xAFFFF #xBFFFE #xBFFFF #xCFFFE #xCFFFF
                           #xDFFFE #xDFFFF #xEFFFE #xEFFFF #xFFFFE
                           #xFFFFF #x10FFFE #x10FFFF))))

(defun c0-control-p (code)
  (and code
       (<= #x0000 code #x001F)))

(defun control-p (code)
  (and code
       (or (c0-control-p code)
           (<= #x007F code #x009F))))

(defun whitespace-p (code)
  (and code
       (or (= #x0009 code)
           (= #x000A code)
           (= #x000C code)
           (= #x000D code)
           (= #x0020 code))))

(defmacro append-char (place char)
  `(vector-push-extend ,char ,place))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *tokenizer-states* '()))

(defmacro define-tokenizer-state (name &body body)
  (if-let ((i (position name *tokenizer-states* :key 'first)))
      (setf (nth i *tokenizer-states*) `(,name ,@body))
    (appendf *tokenizer-states* `((,name ,@body))))
  nil)

(define-tokenizer-state data-state
  (case next-input-character
    (#\&
     (setq return-state 'data-state)
     (switch-state 'character-reference-state))
    (#\<
     (switch-state 'tag-open-state))
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
     (switch-state 'character-reference-state))
    (#\<
     (switch-state 'rcdata-less-than-sign-state))
    (#\null
     (unexpected-null-character)
     (emit +replacement-character+))
    ((nil)
     (emit end-of-file))
    (t
     (emit current-input-character))))

(define-tokenizer-state rawtext-state
  (case next-input-character
    (#\<
     (switch-state 'rawtext-less-than-sign-state))
    (#\null
     (unexpected-null-character)
     (emit +replacement-character+))
    ((nil)
     (emit end-of-file))
    (t
     (emit current-input-character))))

(define-tokenizer-state script-data-state
  (case next-input-character
    (#\<
     (switch-state 'script-data-less-than-sign-state))
    (#\null
     (unexpected-null-character)
     (emit +replacement-character+))
    ((nil)
     (emit end-of-file))
    (t
     (emit current-input-character))))

(define-tokenizer-state plaintext-state
  (case next-input-character
    (#\null
     (unexpected-null-character)
     (emit +replacement-character+))
    ((nil)
     (emit end-of-file))
    (t
     (emit current-input-character))))

(define-tokenizer-state tag-open-state
  (let ((char next-input-character))
    (cond
     ((eq #\! char)
      (switch-state 'markup-declaration-open-state))
     ((eq #\/ char)
      (switch-state 'end-tag-open-state))
     ((ascii-alpha-p char)
      (let ((token (make-instance 'start-tag :tag-name (make-adjustable-string))))
        (setf current-tag-token token))
      (reconsume-in 'tag-name-state))
     ((eq #\? char)
      (unexpected-question-mark-instead-of-tag-name)
      (let ((token (make-instance 'comment-token :data (make-adjustable-string))))
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
      (let ((token (make-instance 'end-tag :tag-name (make-adjustable-string))))
        (setf current-tag-token token))
      (reconsume-in 'tag-name-state))
     ((eq #\> char)
      (missing-end-tag-name)
      (switch-state 'data-state))
     ((null char)
      (eof-before-tag-name)
      (emit #\<)
      (emit #\/)
      (emit end-of-file))
     (t
      (invalid-first-character-of-tag-name)
      (let ((token (make-instance 'comment-token :data (make-adjustable-string))))
        (setf current-comment-token token))
      (reconsume-in 'bogus-comment-state)))))

(define-tokenizer-state tag-name-state
  (let ((char next-input-character))
    (cond
     ((or (eq #\tab char)
          (eq #\newline char)
          (eq #\page char)
          (eq #\space char))
      (switch-state 'before-attribute-name-state))
     ((eq #\/ char)
      (switch-state 'self-closing-start-tag-state))
     ((eq #\> char)
      (switch-state 'data-state)
      (emit current-tag-token))
     ((ascii-upper-alpha-p char)
      (with-slots (tag-name) current-tag-token
        (append-char tag-name (char-downcase current-input-character))))
     ((eq #\null char)
      (unexpected-null-character)
      (with-slots (tag-name) current-tag-token
        (append-char tag-name +replacement-character+)))
     ((null char)
      (eof-in-tag)
      (emit end-of-file))
     (t
      (with-slots (tag-name) current-tag-token
        (append-char tag-name current-input-character))))))

(define-tokenizer-state rcdata-less-than-sign-state
  (case next-input-character
    (#\/
     (setf temporary-buffer (make-adjustable-string))
     (switch-state 'rcdata-end-tag-open-state))
    (t
     (emit #\<)
     (reconsume-in 'rcdata-state))))

(define-tokenizer-state rcdata-end-tag-open-state
  (let ((char next-input-character))
    (cond
     ((ascii-alpha-p char)
      (let ((token (make-instance 'end-tag :tag-name (make-adjustable-string))))
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
          (switch-state 'before-attribute-name-state)
        (progn
          (emit #\<)
          (emit #\/)
          (loop for char across temporary-buffer do (emit char))
          (reconsume-in 'rcdata-state))))
     ((eq #\/ char)
      (if (appropriate-end-tag-token-p current-tag-token)
          (switch-state 'self-closing-start-tag-state)
        (progn
          (emit #\<)
          (emit #\/)
          (loop for char across temporary-buffer do (emit char))
          (reconsume-in 'rcdata-state))))
     ((eq #\> char)
      (if (appropriate-end-tag-token-p current-tag-token)
          (progn
            (switch-state 'data-state)
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
     (setf temporary-buffer (make-adjustable-string))
     (switch-state 'rawtext-end-tag-open-state))
    (t
     (emit #\<)
     (reconsume-in 'rawtext-state))))

(define-tokenizer-state rawtext-end-tag-open-state
  (let ((char next-input-character))
    (cond
     ((ascii-alpha-p char)
      (let ((token (make-instance 'end-tag :tag-name (make-adjustable-string))))
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
          (switch-state 'before-attribute-name-state)
        (progn
          (emit #\<)
          (emit #\/)
          (loop for char across temporary-buffer do (emit char))
          (reconsume-in 'rawtext-state))))
     ((eq #\/ char)
      (if (appropriate-end-tag-token-p current-tag-token)
          (switch-state 'self-closing-start-tag-state)
        (progn
          (emit #\<)
          (emit #\/)
          (loop for char across temporary-buffer do (emit char))
          (reconsume-in 'rawtext-state))))
     ((eq #\> char)
      (if (appropriate-end-tag-token-p current-tag-token)
          (progn
            (switch-state 'data-state)
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
     (setf temporary-buffer (make-adjustable-string))
     (switch-state 'script-data-end-tag-open-state))
    (#\!
     (switch-state 'script-data-escape-start-state)
     (emit #\<)
     (emit #\!))
    (t
     (emit #\<)
     (reconsume-in 'script-data-state))))

(define-tokenizer-state script-data-end-tag-open-state
  (let ((char next-input-character))
    (cond
     ((ascii-alpha-p char)
      (let ((token (make-instance 'end-tag :tag-name (make-adjustable-string))))
        (setf current-tag-token token)
        (reconsume-in 'script-data-end-tag-name-state)))
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
          (switch-state 'before-attribute-name-state)
        (progn
          (emit #\<)
          (emit #\/)
          (loop for char across temporary-buffer do (emit char))
          (reconsume-in 'script-data-state))))
     ((eq #\/ char)
      (if (appropriate-end-tag-token-p current-tag-token)
          (switch-state 'self-closing-start-tag-state)
        (progn
          (emit #\<)
          (emit #\/)
          (loop for char across temporary-buffer do (emit char))
          (reconsume-in 'script-data-state))))
     ((eq #\> char)
      (if (appropriate-end-tag-token-p current-tag-token)
          (progn
            (switch-state 'data-state)
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
     (switch-state 'script-data-escape-start-dash-state)
     (emit #\-))
    (t
     (reconsume-in 'script-data-state))))

(define-tokenizer-state script-data-escape-start-dash-state
  (case next-input-character
    (#\-
     (switch-state 'script-data-escaped-dash-dash-state)
     (emit #\-))
    (t
     (reconsume-in 'script-data-state))))

(define-tokenizer-state script-data-escaped-state
  (case next-input-character
    (#\-
     (switch-state 'script-data-escaped-dash-state)
     (emit #\-))
    (#\<
     (switch-state 'script-data-escaped-less-than-sign-state))
    (#\null
     (unexpected-null-character)
     (emit +replacement-character+))
    ((nil)
     (eof-in-script-html-comment-like-text)
     (emit end-of-file))
    (t
     (emit current-input-character))))

(define-tokenizer-state script-data-escaped-dash-state
  (case next-input-character
    (#\-
     (switch-state 'script-data-escaped-dash-dash-state)
     (emit #\-))
    (#\<
     (switch-state 'script-data-escaped-less-than-sign-state))
    (#\null
     (unexpected-null-character)
     (switch-state 'script-data-escaped-state)
     (emit +replacement-character+))
    ((nil)
     (eof-in-script-html-comment-like-text)
     (emit end-of-file))
    (t
     (switch-state 'script-data-escaped-state)
     (emit current-input-character))))

(define-tokenizer-state script-data-escaped-dash-dash-state
  (case next-input-character
    (#\-
     (emit #\-))
    (#\<
     (switch-state 'script-data-escaped-less-than-sign-state))
    (#\>
     (switch-state 'script-data-state)
     (emit #\>))
    (#\null
     (unexpected-null-character)
     (switch-state 'script-data-escaped-state)
     (emit +replacement-character+))
    ((nil)
     (eof-in-script-html-comment-like-text)
     (emit end-of-file))
    (t
     (switch-state 'script-data-escaped-state)
     (emit current-input-character))))

(define-tokenizer-state script-data-escaped-less-than-sign-state
  (let ((char next-input-character))
    (cond
     ((eq #\/ char)
      (setf temporary-buffer (make-adjustable-string))
      (switch-state 'script-data-escaped-end-tag-open-state))
     ((ascii-alpha-p char)
      (setf temporary-buffer (make-adjustable-string))
      (emit #\<)
      (reconsume-in 'script-data-double-escape-start-state))
     (t
      (emit #\<)
      (reconsume-in 'script-data-escaped-state)))))

(define-tokenizer-state script-data-escaped-end-tag-open-state
  (let ((char next-input-character))
    (cond
     ((ascii-alpha-p char)
      (let ((token (make-instance 'end-tag :tag-name (make-adjustable-string))))
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
          (switch-state 'before-attribute-name-state)
        (progn
          (emit #\<)
          (emit #\/)
          (loop for char across temporary-buffer do (emit char))
          (reconsume-in 'script-data-escaped-state))))
     ((eq #\/ char)
      (if (appropriate-end-tag-token-p current-tag-token)
          (switch-state 'self-closing-start-tag-state)
        (progn
          (emit #\<)
          (emit #\/)
          (loop for char across temporary-buffer do (emit char))
          (reconsume-in 'script-data-escaped-state))))
     ((eq #\> char)
      (if (appropriate-end-tag-token-p current-tag-token)
          (progn
            (switch-state 'data-state)
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
          (switch-state 'script-data-double-escaped-state)
        (switch-state 'script-data-escaped-state))
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
     (switch-state 'script-data-double-escaped-dash-state)
     (emit #\-))
    (#\<
     (switch-state 'script-data-escaped-less-than-sign-state)
     (emit #\<))
    (#\null
     (unexpected-null-character)
     (emit +replacement-character+))
    ((nil)
     (eof-in-script-html-comment-like-text)
     (emit end-of-file))
    (t
     (emit current-input-character))))

(define-tokenizer-state script-data-double-escaped-dash-state
  (case next-input-character
    (#\-
     (switch-state 'script-data-double-escaped-dash-dash-state)
     (emit #\-))
    (#\<
     (switch-state 'script-data-escaped-less-than-sign-state)
     (emit #\<))
    (#\null
     (unexpected-null-character)
     (switch-state 'script-data-double-escaped-state)
     (emit +replacement-character+))
    ((nil)
     (eof-in-script-html-comment-like-text)
     (emit end-of-file))
    (t
     (switch-state 'script-data-double-escaped-state)
     (emit current-input-character))))

(define-tokenizer-state script-data-double-escaped-dash-dash-state
  (case next-input-character
    (#\-
     (emit #\-))
    (#\<
     (switch-state 'script-data-escaped-less-than-sign-state)
     (emit #\<))
    (#\>
     (switch-state 'script-data-state)
     (emit #\>))
    (#\null
     (unexpected-null-character)
     (switch-state 'script-data-double-escaped-state)
     (emit +replacement-character+))
    ((nil)
     (eof-in-script-html-comment-like-text)
     (emit end-of-file))
    (t
     (switch-state 'script-data-double-escaped-state)
     (emit current-input-character))))

(define-tokenizer-state script-data-double-escaped-less-than-sign-state
  (case next-input-character
    (#\/
     (setf temporary-buffer (make-adjustable-string))
     (switch-state 'script-data-double-escape-end-state)
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
          (switch-state 'script-data-escaped-state)
        (switch-state 'script-data-double-escaped-state))
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
                                     :name (make-adjustable-string
                                            (string current-input-character))
                                     :value (make-adjustable-string))))
       (appendf (slot-value current-tag-token 'attributes) (list attribute))
       (setf current-attribute attribute))
     (switch-state 'attribute-name-state))
    (t
     (let ((attribute (make-instance 'attribute
                                     :name (make-adjustable-string)
                                     :value (make-adjustable-string))))
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
      (switch-state 'before-attribute-value-state))
     ((ascii-upper-alpha-p char)
      (append-char (slot-value current-attribute 'name)
                   (char-downcase current-input-character)))
     ((eq #\null char)
      (append-char (slot-value current-attribute 'name)
                   +replacement-character+))
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
     (switch-state 'self-closing-start-tag-state))
    (#\=
     (switch-state 'before-attribute-value-state))
    (#\>
     (switch-state 'data-state)
     (emit current-tag-token))
    ((nil)
     (eof-in-tag)
     (emit end-of-file))
    (t
     (let ((attribute (make-instance 'attribute
                                     :name (make-adjustable-string)
                                     :value (make-adjustable-string))))
       (appendf (slot-value current-tag-token 'attributes) (list attribute))
       (setf current-attribute attribute))
     (reconsume-in 'attribute-name-state))))

(define-tokenizer-state before-attribute-value-state
  (case next-input-character
    ((#\tab #\newline #\page #\space))
    (#\"
     (switch-state 'attribute-value-double-quoted-state))
    (#\'
     (switch-state 'attribute-value-single-quoted-state))
    (#\>
     (missing-attribute-value)
     (switch-state 'data-state)
     (emit current-tag-token))
    (t
     (reconsume-in 'attribute-value-unquoted-state))))

(define-tokenizer-state attribute-value-double-quoted-state
  (case next-input-character
    (#\"
     (switch-state 'after-attribute-value-quoted-state))
    (#\&
     (setf return-state 'attribute-value-double-quoted-state)
     (switch-state 'character-reference-state))
    (#\null
     (unexpected-null-character)
     (append-char (slot-value current-attribute 'value)
                  +replacement-character+))
    ((nil)
     (eof-in-tag)
     (emit end-of-file))
    (t
     (append-char (slot-value current-attribute 'value)
                  current-input-character))))

(define-tokenizer-state attribute-value-single-quoted-state
  (case next-input-character
    (#\'
     (switch-state 'after-attribute-value-quoted-state))
    (#\&
     (setf return-state 'attribute-value-single-quoted-state)
     (switch-state 'character-reference-state))
    (#\null
     (unexpected-null-character)
     (append-char (slot-value current-attribute 'value)
                  +replacement-character+))
    ((nil)
     (eof-in-tag)
     (emit end-of-file))
    (t
     (append-char (slot-value current-attribute 'value)
                  current-input-character))))

(define-tokenizer-state attribute-value-unquoted-state
  (case next-input-character
    ((#\tab #\newline #\page #\space)
     (switch-state 'before-attribute-name-state))
    (#\&
     (setf return-state 'attribute-value-unquoted-state)
     (switch-state 'character-reference-state))
    (#\>
     (switch-state 'data-state)
     (emit current-tag-token))
    (#\null
     (unexpected-null-character)
     (append-char (slot-value current-attribute 'value)
                  +replacement-character+))
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
     (switch-state 'before-attribute-name-state))
    (#\/
     (switch-state 'self-closing-start-tag-state))
    (#\>
     (switch-state 'data-state)
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
     (switch-state 'data-state)
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
     (switch-state 'data-state)
     (emit current-comment-token))
    ((nil)
     (emit current-comment-token)
     (emit end-of-file))
    (#\null
     (unexpected-null-character)
     (append-char (slot-value current-comment-token 'data)
                  +replacement-character+))
    (t
     (append-char (slot-value current-comment-token 'data)
                  current-input-character))))

(define-tokenizer-state markup-declaration-open-state
  (cond
   ((equal "--" (next-few-characters 2))
    (consume 2)
    (let ((token (make-instance 'comment-token :data (make-adjustable-string))))
      (setf current-comment-token token)
      (switch-state 'comment-start-state)))
   ((string-equal "DOCTYPE" (next-few-characters 7))
    (consume 7)
    (switch-state 'doctype-state))
   ((equal "[CDATA[" (next-few-characters 7))
    (consume 7)
    ;; TODO
    (let ((token (make-instance 'comment-token :data (make-adjustable-string "[CDATA["))))
      (setf current-comment-token token)
      (switch-state 'bogus-comment-state)))
   (t
    (incorrectly-opened-comment)
    (let ((token (make-instance 'comment-token :data (make-adjustable-string))))
      (setf current-comment-token token)
      (switch-state 'bogus-comment-state)))))

(define-tokenizer-state comment-start-state
  (case next-input-character
    (#\-
     (switch-state 'comment-start-dash-state))
    (#\>
     (abrupt-closing-of-empty-comment)
     (switch-state 'data-state)
     (emit current-comment-token))
    (t
     (reconsume-in 'comment-state))))

(define-tokenizer-state comment-start-dash-state
  (case next-input-character
    (#\-
     (switch-state 'comment-end-state))
    (#\>
     (abrupt-closing-of-empty-comment)
     (switch-state 'data-state)
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
     (switch-state 'comment-less-than-sign-state))
    (#\-
     (switch-state 'comment-end-dash-state))
    (#\null
     (unexpected-null-character)
     (append-char (slot-value current-comment-token 'data) +replacement-character+))
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
     (switch-state 'comment-less-than-sign-bang-state))
    (#\<
     (append-char (slot-value current-comment-token 'data) current-input-character))
    (t
     (reconsume-in 'comment-state))))

(define-tokenizer-state comment-less-than-sign-bang-state
  (case next-input-character
    (#\-
     (switch-state 'comment-less-than-sign-bang-dash-state))
    (t
     (reconsume-in 'comment-state))))

(define-tokenizer-state comment-less-than-sign-bang-dash-state
  (case next-input-character
    (#\-
     (switch-state 'comment-less-than-sign-bang-dash-dash-state))
    (t
     (reconsume-in 'comment-end-dash-state))))

(define-tokenizer-state comment-less-than-sign-bang-dash-dash-state
  (case next-input-character
    ((#\- nil)
     (switch-state 'comment-end-state))
    (t
     (nested-comment)
     (reconsume-in 'comment-end-state))))

(define-tokenizer-state comment-end-dash-state
  (case next-input-character
    ((#\-)
     (switch-state 'comment-end-state))
    ((nil)
     (eof-in-comment)
     (emit current-comment-token)
     (emit end-of-file))
    (t
     (append-char (slot-value current-comment-token 'data) #\-)
     (reconsume-in 'comment-state))))

(define-tokenizer-state comment-end-state
  (case next-input-character
    (#\>
     (switch-state 'data-state)
     (emit current-comment-token))
    (#\!
     (switch-state 'comment-end-bang-state))
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
     (switch-state 'comment-end-dash-state))
    (#\>
     (incorrectly-closed-comment)
     (switch-state 'data-state)
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
     (switch-state 'before-doctype-name-state))
    (#\>
     (reconsume-in 'before-doctype-name-state))
    ((nil)
     (eof-in-doctype)
     (let ((token (make-instance 'doctype-token :force-quirks-flag :on)))
       (setf current-doctype-token token)
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
      (let ((token (make-instance 'doctype-token
                                  :name (make-adjustable-string
                                         (string
                                          (char-downcase
                                           current-input-character))))))
        (setf current-doctype-token token)
        (switch-state 'doctype-name-state)))
     ((eq #\null char)
      (unexpected-null-character)
      (let ((token (make-instance 'doctype-token
                                  :name (make-adjustable-string
                                         (string +replacement-character+)))))
        (setf current-doctype-token token)
        (switch-state 'doctype-name-state)))
     ((eq #\> char)
      (missing-doctype-name)
      (let ((token (make-instance 'doctype-token :force-quirks-flag :on)))
        (setf current-doctype-token token)
        (switch-state 'data-state)
        (emit token)))
     ((null char)
      (eof-in-doctype)
      (let ((token (make-instance 'doctype-token :force-quirks-flag :on)))
        (setf current-doctype-token token)
        (emit token)
        (emit end-of-file)))
     (t
      (let ((token (make-instance 'doctype-token :name (make-adjustable-string
                                                        (string current-input-character)))))
        (setf current-doctype-token token)
        (switch-state 'doctype-name-state))))))

(define-tokenizer-state doctype-name-state
  (let ((char next-input-character))
    (cond
     ((or (eq #\tab char) (eq #\newline char)
          (eq #\page char) (eq #\space char))
      (switch-state 'after-doctype-name-state))
     ((eq #\> char)
      (switch-state 'data-state)
      (emit current-doctype-token))
     ((ascii-upper-alpha-p char)
      (append-char (slot-value current-doctype-token 'name)
                   (char-downcase current-input-character)))
     ((eq #\null char)
      (unexpected-null-character)
      (append-char (slot-value current-doctype-token 'name)
                   +replacement-character+))
     ((null char)
      (eof-in-doctype)
      (setf (slot-value current-doctype-token 'force-quirks-flag) :on)
      (emit end-of-file))
     (t
      (append-char (slot-value current-doctype-token 'name)
                   current-input-character)))))

(define-tokenizer-state after-doctype-name-state
  (let ((char next-input-character))
    (cond
     ((or (eq #\tab char) (eq #\newline char)
          (eq #\page char) (eq #\space char)))
     ((eq #\> char)
      (switch-state 'data-state)
      (emit current-doctype-token))
     ((null char)
      (eof-in-doctype)
      (setf (slot-value current-doctype-token 'force-quirks-flag) :on)
      (emit end-of-file))
     (t
      (cond
       ((string-equal "PUBLIC" (next-few-characters 6))
        (consume 6)
        (switch-state 'after-doctype-public-keyword-state))
       ((string-equal "SYSTEM" (next-few-characters 6))
        (consume 6)
        (switch-state 'after-doctype-system-keyword-state))
       (t
        (invalid-character-sequence-after-doctype-name)
        (setf (slot-value current-doctype-token 'force-quirks-flag) :on)
        (reconsume-in 'bogus-doctype-state)))))))

(define-tokenizer-state after-doctype-public-keyword-state
  (case next-input-character
    ((#\tab #\newline #\page #\space)
     (switch-state 'before-doctype-public-identifier-state))
    (#\"
     (missing-whitespace-after-doctype-public-keyword)
     (setf (slot-value current-doctype-token 'public-identifier)
           (make-adjustable-string))
     (switch-state 'doctype-public-identifier-double-quoted-state))
    (#\'
     (missing-whitespace-after-doctype-public-keyword)
     (setf (slot-value current-doctype-token 'public-identifier)
           (make-adjustable-string))
     (switch-state 'doctype-public-identifier-single-quoted-state))
    (#\>
     (missing-doctype-public-identifier)
     (setf (slot-value current-doctype-token 'force-quirks-flag) :on)
     (switch-state 'data-state)
     (emit current-doctype-token))
    ((nil)
     (eof-in-doctype)
     (setf (slot-value current-doctype-token 'force-quirks-flag) :on)
     (emit end-of-file))
    (t
     (missing-quote-before-doctype-public-identifier)
     (setf (slot-value current-doctype-token 'force-quirks-flag) :on)
     (reconsume-in 'bogus-doctype-state))))

(define-tokenizer-state before-doctype-public-identifier-state
  (case next-input-character
    ((#\tab #\newline #\page #\space))
    (#\"
     (setf (slot-value current-doctype-token 'public-identifier)
           (make-adjustable-string))
     (switch-state 'doctype-public-identifier-double-quoted-state))
    (#\'
     (setf (slot-value current-doctype-token 'public-identifier)
           (make-adjustable-string))
     (switch-state 'doctype-public-identifier-single-quoted-state))
    (#\>
     (missing-doctype-public-identifier)
     (setf (slot-value current-doctype-token 'force-quirks-flag) :on)
     (switch-state 'data-state)
     (emit current-doctype-token))
    ((nil)
     (eof-in-doctype)
     (setf (slot-value current-doctype-token 'force-quirks-flag) :on)
     (emit current-doctype-token)
     (emit end-of-file))
    (t
     (missing-quote-before-doctype-public-identifier)
     (setf (slot-value current-doctype-token 'force-quirks-flag) :on)
     (reconsume-in 'bogus-doctype-state))))

(define-tokenizer-state doctype-public-identifier-double-quoted-state
  (case next-input-character
    (#\"
     (switch-state 'after-doctype-public-identifier-state))
    (#\null
     (unexpected-null-character)
     (append-char (slot-value current-doctype-token 'public-identifier)
                  +replacement-character+))
    (#\>
     (abrupt-doctype-public-identifier)
     (setf (slot-value current-doctype-token 'force-quirks-flag) :on)
     (switch-state 'data-state)
     (emit current-doctype-token))
    ((nil)
     (eof-in-doctype)
     (setf (slot-value current-doctype-token 'force-quirks-flag) :on)
     (emit current-doctype-token)
     (emit end-of-file))
    (t
     (append-char (slot-value current-doctype-token 'public-identifier)
                  current-input-character))))

(define-tokenizer-state doctype-public-identifier-single-quoted-state
  (case next-input-character
    (#\'
     (switch-state 'after-doctype-public-identifier-state))
    (#\null
     (unexpected-null-character)
     (append-char (slot-value current-doctype-token 'public-identifier)
                  +replacement-character+))
    (#\>
     (abrupt-doctype-public-identifier)
     (setf (slot-value current-doctype-token 'force-quirks-flag) :on)
     (switch-state 'data-state)
     (emit current-doctype-token))
    ((nil)
     (eof-in-doctype)
     (setf (slot-value current-doctype-token 'force-quirks-flag) :on)
     (emit current-doctype-token)
     (emit end-of-file))
    (t
     (append-char (slot-value current-doctype-token 'public-identifier)
                  current-input-character))))

(define-tokenizer-state after-doctype-public-identifier-state
  (case next-input-character
    ((#\tab #\newline #\page #\space)
     (switch-state 'between-doctype-public-and-system-identifiers-state))
    (#\>
     (switch-state 'data-state)
     (emit current-doctype-token))
    (#\"
     (missing-whitespace-between-doctype-public-and-system-identifiers)
     (setf (slot-value current-doctype-token 'system-identifier)
           (make-adjustable-string))
     (switch-state 'doctype-system-identifier-double-quoted-state))
    (#\'
     (missing-whitespace-between-doctype-public-and-system-identifiers)
     (setf (slot-value current-doctype-token 'system-identifier)
           (make-adjustable-string))
     (switch-state 'doctype-system-identifier-single-quoted-state))
    ((nil)
     (eof-in-doctype)
     (setf (slot-value current-doctype-token 'force-quirks-flag) :on)
     (emit current-doctype-token)
     (emit end-of-file))
    (t
     (missing-quote-before-doctype-system-identifier)
     (setf (slot-value current-doctype-token 'force-quirks-flag) :on)
     (reconsume-in 'bogus-doctype-state))))

(define-tokenizer-state between-doctype-public-and-system-identifiers-state
  (case next-input-character
    ((#\tab #\newline #\page #\space))
    (#\>
     (switch-state 'data-state)
     (emit current-doctype-token))
    (#\"
     (setf (slot-value current-doctype-token 'system-identifier)
           (make-adjustable-string))
     (switch-state 'doctype-system-identifier-double-quoted-state))
    (#\'
     (setf (slot-value current-doctype-token 'system-identifier)
           (make-adjustable-string))
     (switch-state 'doctype-system-identifier-single-quoted-state))
    ((nil)
     (eof-in-doctype)
     (setf (slot-value current-doctype-token 'force-quirks-flag) :on)
     (emit current-doctype-token)
     (emit end-of-file))
    (t
     (missing-quote-before-doctype-system-identifier)
     (setf (slot-value current-doctype-token 'force-quirks-flag) :on)
     (reconsume-in 'bogus-doctype-state))))

(define-tokenizer-state after-doctype-system-keyword-state
  (case next-input-character
    ((#\tab #\newline #\page #\space)
     (switch-state 'before-doctype-system-identifier-state))
    (#\"
     (missing-whitespace-after-doctype-system-keyword)
     (setf (slot-value current-doctype-token 'system-identifier)
           (make-adjustable-string))
     (switch-state 'doctype-system-identifier-double-quoted-state))
    (#\'
     (missing-whitespace-after-doctype-system-keyword)
     (setf (slot-value current-doctype-token 'system-identifier)
           (make-adjustable-string))
     (switch-state 'doctype-system-identifier-single-quoted-state))
    (#\>
     (missing-doctype-system-identifier)
     (setf (slot-value current-doctype-token 'force-quirks-flag) :on)
     (switch-state 'data-state)
     (emit current-doctype-token))
    ((nil)
     (eof-in-doctype)
     (setf (slot-value current-doctype-token 'force-quirks-flag) :on)
     (emit current-doctype-token)
     (emit end-of-file))
    (t
     (missing-quote-before-doctype-system-identifier)
     (setf (slot-value current-doctype-token 'force-quirks-flag) :on)
     (reconsume-in 'bogus-doctype-state))))

(define-tokenizer-state before-doctype-system-identifier-state
  (case next-input-character
    ((#\tab #\newline #\page #\space))
    (#\"
     (setf (slot-value current-doctype-token 'system-identifier)
           (make-adjustable-string))
     (switch-state 'doctype-system-identifier-double-quoted-state))
    (#\'
     (setf (slot-value current-doctype-token 'system-identifier)
           (make-adjustable-string))
     (switch-state 'doctype-system-identifier-single-quoted-state))
    (#\>
     (missing-doctype-system-identifier)
     (setf (slot-value current-doctype-token 'force-quirks-flag) :on)
     (switch-state 'data-state)
     (emit current-doctype-token))
    ((nil)
     (eof-in-doctype)
     (setf (slot-value current-doctype-token 'force-quirks-flag) :on)
     (emit current-doctype-token)
     (emit end-of-file))
    (t
     (missing-quote-before-doctype-system-identifier)
     (setf (slot-value current-doctype-token 'force-quirks-flag) :on)
     (reconsume-in 'bogus-doctype-state))))

(define-tokenizer-state doctype-system-identifier-double-quoted-state
  (case next-input-character
    (#\"
     (switch-state 'after-doctype-system-identifier-state))
    (#\null
     (unexpected-null-character)
     (append-char (slot-value current-doctype-token 'system-identifier)
                  +replacement-character+))
    (#\>
     (abrupt-doctype-system-identifier)
     (setf (slot-value current-doctype-token 'force-quirks-flag) :on)
     (switch-state 'data-state)
     (emit current-doctype-token))
    ((nil)
     (eof-in-doctype)
     (setf (slot-value current-doctype-token 'force-quirks-flag) :on)
     (emit current-doctype-token)
     (emit end-of-file))
    (t
     (append-char (slot-value current-doctype-token 'system-identifier)
                  current-input-character))))

(define-tokenizer-state doctype-system-identifier-single-quoted-state
  (case next-input-character
    (#\'
     (switch-state 'after-doctype-system-identifier-state))
    (#\null
     (unexpected-null-character)
     (append-char (slot-value current-doctype-token 'system-identifier)
                  +replacement-character+))
    (#\>
     (abrupt-doctype-system-identifier)
     (setf (slot-value current-doctype-token 'force-quirks-flag) :on)
     (switch-state 'data-state)
     (emit current-doctype-token))
    ((nil)
     (eof-in-doctype)
     (setf (slot-value current-doctype-token 'force-quirks-flag) :on)
     (emit current-doctype-token)
     (emit end-of-file))
    (t
     (append-char (slot-value current-doctype-token 'system-identifier)
                  current-input-character))))

(define-tokenizer-state after-doctype-system-identifier-state
  (case next-input-character
    ((#\tab #\newline #\page #\space))
    (#\>
     (switch-state 'data-state)
     (emit current-doctype-token))
    ((nil)
     (eof-in-doctype)
     (setf (slot-value current-doctype-token 'force-quirks-flag) :on)
     (emit current-doctype-token)
     (emit end-of-file))
    (t
     (unexpected-character-after-doctype-system-identifier)
     (reconsume-in 'bogus-doctype-state))))

(define-tokenizer-state bogus-doctype-state
  (case next-input-character
    ((#\tab #\newline #\page #\space))
    (#\>
     (switch-state 'data-state)
     (emit current-doctype-token))
    (#\null
     (unexpected-null-character))
    ((nil)
     (emit current-doctype-token)
     (emit end-of-file))))

(define-tokenizer-state cdata-section-state
  (case next-input-character
    (#\]
     (switch-state 'cdata-section-bracket-state))
    ((nil)
     (eof-in-cdata)
     (emit end-of-file))
    (t
     (emit current-input-character))))

(define-tokenizer-state cdata-section-bracket-state
  (case next-input-character
    (#\]
     (switch-state 'cdata-section-end-state))
    (t
     (emit #\])
     (reconsume-in 'cdata-section-state))))

(define-tokenizer-state cdata-section-end-state
  (case next-input-character
    (#\]
     (emit #\]))
    (#\>
     (switch-state 'data-state))
    (t
     (emit #\])
     (reconsume-in 'cdata-section-state))))

(define-tokenizer-state character-reference-state
  (setf temporary-buffer (make-adjustable-string))
  (append-char temporary-buffer #\&)
  (let ((char next-input-character))
    (cond
     ((ascii-alphanumeric-p char)
      (reconsume-in 'named-character-reference-state))
     ((eq #\# char)
      (append-char temporary-buffer current-input-character)
      (switch-state 'numeric-character-reference-state))
     (t
      (loop for char across temporary-buffer do (emit char))
      (reconsume-in return-state)))))

(define-tokenizer-state named-character-reference-state
  (let ((match-p)
        (last-character-matched)
        (codepoints))
    (loop with last-name-matched
          for i from 1
          for name = (concatenate 'string temporary-buffer (next-few-characters i))
          for result = (lookup-named-character-reference name)
          when (and result (= (length name) (+ (length temporary-buffer) i)))
          do (progn
               (setf last-character-matched (char name (1- (length name))))
               (when (listp result)
                 (setf match-p t
                       last-name-matched name
                       codepoints result)))
          else do (if last-name-matched
                      (progn (consume (1- (length last-name-matched))) (loop-finish))
                    (progn (consume (1- (length name))) (loop-finish)))
          finally (if last-name-matched
                      (setf temporary-buffer last-name-matched)
                    (setf temporary-buffer (subseq name 0 (1- (length name))))))
    (if match-p
        (if (and (or (eq return-state 'attribute-value-double-quoted-state)
                     (eq return-state 'attribute-value-single-quoted-state)
                     (eq return-state 'attribute-value-unquoted-state))
                 (not (eq #\; last-character-matched))
                 (let ((char next-input-character))
                   (or (eq #\= char)
                       (ascii-alphanumeric-p char))))
            (progn
              (loop for char across temporary-buffer do (emit char))
              (switch-state return-state))
          (progn
            (unless (eq #\; last-character-matched)
              (missing-semicolon-after-character-reference))
            (setf temporary-buffer (make-adjustable-string))
            (loop for code in codepoints
                  do (append-char temporary-buffer (code-char code)))
            (loop for char across temporary-buffer do (emit char))
            (switch-state return-state)))
      (progn
        (loop for char across temporary-buffer do (emit char))
        (switch-state 'ambiguous-ampersand-state)))))

(define-tokenizer-state ambiguous-ampersand-state
  (let ((char next-input-character))
    (cond
     ((ascii-alphanumeric-p char)
      (if (or (eq return-state 'attribute-value-double-quoted-state)
              (eq return-state 'attribute-value-single-quoted-state)
              (eq return-state 'attribute-value-unquoted-state))
          (append-char (slot-value current-attribute 'value) current-input-character)
        (emit current-input-character)))
     ((eq #\; char)
      (unknown-named-character-reference)
      (reconsume-in return-state))
     (t
      (reconsume-in return-state)))))

(define-tokenizer-state numeric-character-reference-state
  (setf character-reference-code 0)
  (case next-input-character
    ((#\x #\X)
     (append-char temporary-buffer current-input-character)
     (switch-state 'hexadecimal-character-reference-state))
    (t
     (reconsume-in 'decimal-character-reference-state))))

(define-tokenizer-state hexadecimal-character-reference-start-state
  (let ((char next-input-character))
    (cond
     ((ascii-hex-digit-p char)
      (reconsume-in 'hexadecimal-character-reference-state))
     (t
      (absence-of-digits-in-numeric-character-reference)
      (loop for char across temporary-buffer do (emit char))
      (reconsume-in return-state)))))

(define-tokenizer-state decimal-character-reference-start-state
  (let ((char next-input-character))
    (cond
     ((ascii-digit-p char)
      (reconsume-in 'decimal-character-reference-sta))
     (t
      (absence-of-digits-in-numeric-character-reference)
      (loop for char across temporary-buffer do (emit char))
      (reconsume-in return-state)))))

(define-tokenizer-state hexadecimal-character-reference-state
  (let ((char next-input-character))
    (cond
     ((ascii-digit-p char)
      (setf character-reference-code (* 16 character-reference-code)
            character-reference-code (+ (- (char-code current-input-character) #x0030)
                                        character-reference-code)))
     ((ascii-upper-hex-digit-p char)
      (setf character-reference-code (* 16 character-reference-code)
            character-reference-code (+ (- (char-code current-input-character) #x0037)
                                        character-reference-code)))
     ((ascii-lower-hex-digit-p char)
      (setf character-reference-code (* 16 character-reference-code)
            character-reference-code (+ (- (char-code current-input-character) #x0057)
                                        character-reference-code)))
     ((eq #\; char)
      (switch-state 'numeric-character-reference-end-state))
     (t
      (missing-semicolon-after-character-reference)
      (reconsume-in 'numeric-character-reference-end-state)))))

(define-tokenizer-state decimal-character-reference-state
  (let ((char next-input-character))
    (cond
     ((ascii-digit-p char)
      (setf character-reference-code (* 10 character-reference-code)
            character-reference-code (+ (- (char-code current-input-character) #x0030)
                                        character-reference-code)))
     ((eq #\; char)
      (switch-state 'numeric-character-reference-end-state))
     (t
      (missing-semicolon-after-character-reference)
      (reconsume-in 'numeric-character-reference-end-state)))))

(define-tokenizer-state numeric-character-reference-end-state
  (cond
   ((= #x00 character-reference-code)
    (null-character-reference)
    (setf character-reference-code #xFFFD))
   ((> character-reference-code #X10FFFF)
    (character-reference-outside-unicode-range)
    (setf character-reference-code #XFFFD))
   ((surrogate-p character-reference-code)
    (surrogate-character-reference)
    (setf character-reference-code #XFFFD))
   ((noncharacter-p character-reference-code)
    (noncharacter-character-reference))
   ((or (= #x0D character-reference-code)
        (and (control-p character-reference-code)
             (not (whitespace-p character-reference-code))))
    (control-character-reference)
    (let ((code-point (cdr (assoc character-reference-code
                                  '((0x80 . 0x20AC)
                                    (0x82 . 0x201A)
                                    (0x83 . 0x0192)
                                    (0x84 . 0x201E)
                                    (0x85 . 0x2026)
                                    (0x86 . 0x2020)
                                    (0x87 . 0x2021)
                                    (0x88 . 0x02C6)
                                    (0x89 . 0x2030)
                                    (0x8A . 0x0160)
                                    (0x8B . 0x2039)
                                    (0x8C . 0x0152)
                                    (0x8E . 0x017D)
                                    (0x91 . 0x2018)
                                    (0x92 . 0x2019)
                                    (0x93 . 0x201C)
                                    (0x94 . 0x201D)
                                    (0x95 . 0x2022)
                                    (0x96 . 0x2013)
                                    (0x97 . 0x2014)
                                    (0x98 . 0x02DC)
                                    (0x99 . 0x2122)
                                    (0x9A . 0x0161)
                                    (0x9B . 0x203A)
                                    (0x9C . 0x0153)
                                    (0x9E . 0x017E)
                                    (0x9F . 0x0178))))))
      (when code-point (setf character-reference-code code-point)))))
  (setf temporary-buffer (make-adjustable-string))
  (append-char temporary-buffer (code-char character-reference-code))
  (loop for char across temporary-buffer do (emit char))
  (switch-state return-state))