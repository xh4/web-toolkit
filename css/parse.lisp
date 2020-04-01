(in-package :css)

;; https://www.w3.org/TR/css-syntax-3/#tokenizing-and-parsing

(define-condition parse-error (error) ())

(defclass tokenizer ()
  ((stream
    :initarg :stream
    :initform nil)
   (current-input-code-point
    :initform nil
    :reader current-input-code-point)))

(defun whitespacep (char)
  (or (eq #\Newline char)
      (eq #\Tab char)
      (eq #\Space char)))

(defun consume-token (tokenizer)
  (consume-comments tokenizer)
  (let ((char (consume-code-point tokenizer)))
    (cond
      ((whitespace-p char) (consume-whitespace tokenizer) :whitespace)
      ((eq #\" char) (consume-string tokenizer)))))

(defun consume-code-point (tokenizer)
  (with-slots (stream current-input-code-point) tokenizer
    (let ((char (read-char stream nil nil)))
      (setf current-input-code-point char))))

(defun next-input-code-point (tokenizer)
  (with-slots (stream) tokenizer
    (peek-char t stream nil nil)))

(defun reconsume-current-input-code-point (tokenizer)
  (with-slots (stream current-input-code-point) tokenizer
    (when current-input-code-point
      (unread-char current-input-code-point stream))))

(defun consume-comments (tokenizer)
  (let ((char (consume-code-point tokenizer)))
    (if (eq #\/ char)
        (if (eq #\* (next-input-code-point tokenizer))
            (progn
              (consume-code-point tokenizer)
              (loop with a = nil
                 with b = nil
                 for char = (consume-code-point tokenizer)
                 do (when (and (not char) (or (not a) (not b)))
                      (error "Unexpected end of input while consume comments"))
                 while char
                 do (cond
                      ((eq #\* char) (setf a char))
                      ((eq #\/ char) (setf b char))
                      (t (setf a nil b nil)))
                 until (and a b)
                 finally (return-from consume-comments nil)))
            (reconsume-current-input-code-point tokenizer))
        (reconsume-current-input-code-point tokenizer))))

(defun consume-whitespace (tokenizer)
  (loop for count from 0
     while (whitespacep (next-input-code-point tokenizer))
     do (consume-code-point tokenizer)))

(defun consume-string (tokenizer)
  (let ((ending-code-point (current-input-code-point tokenizer))
        (string ""))
    (loop for char = (consume-code-point tokenizer)
       do (cond
            ((eq ending-code-point char)
             (return-from consume-string `(:string ,string)))
            ((null char)
             (error "End of input white consume string"))
            ((eq #\Newline char)
             (reconsume-current-input-code-point tokenizer)
             (return-from consume-string :bad-string))
            ((eq #\/ char)
             (let ((next-char (next-input-code-point tokenizer)))
               (cond
                 ((null next-char))
                 ((eq #\Newline char) (consume-code-point tokenizer))
                 (t (let ((char (consume-escaped-code-point tokenizer)))
                      (setf string (concatenate 'string string (string char))))))))
            (t (setf string (concatenate 'string string (string char))))))))

(defun hex-digit-p (char)
  (or (char<= #\0 char #\9)
      (char<= #\a char #\f)
      (char<= #\A char #\F)))

(defun consume-hex-digit (tokenizer)
  (let ((char (next-input-code-point tokenizer)))
    (when (and char (hex-digit-p char))
      (consume-code-point tokenizer))))

(defun consume-escaped-code-point (tokenizer)
  (let ((char (consume-code-point tokenizer)))
    (cond
      ((hex-digit-p char)
       (let ((hexdigits (string char)))
         (loop for n upto 5
            for hexdigit = (consume-hex-digit tokenizer)
            while hexdigit
            do (setf hexdigits (concatenate 'string hexdigits (string hexdigit))))
         (when (whitespacep (next-input-code-point tokenizer))
           (consume-code-point tokenizer))
         (let ((number (parse-integer hexdigits :radix 16)))
           (if (or (zerop number)
                   (<= #xD800 number #xDFFF)
                   (> number #x10FFFF))
               #\Replacement-Character
               (code-char number)))))
      ((null char) #\Replacement-Character)
      (t char))))
