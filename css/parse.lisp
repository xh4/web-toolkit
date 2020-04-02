(in-package :css)

;; https://www.w3.org/TR/css-syntax-3/#tokenizing-and-parsing

(define-condition parse-error (error) ())

(defclass tokenizer ()
  ((stream
    :initarg :stream
    :initform nil)
   (buffer
    :initform (make-array 3))
   (current-input-code-point
    :initform nil
    :reader current-input-code-point)))

(defun uppercase-letter-p (char)
  (char<= #\A char #\Z))

(defun lowercase-letter-p (char)
  (char<= #\a char #\z))

(defun letter-p (char)
  (or (uppercase-letter-p char)
      (lowercase-letter-p char)))

(defun digit-p (char)
  (char<= #\0 char #\9))

(defun hex-digit-p (char)
  (or (digit-p char)
      (char<= #\a char #\f)
      (char<= #\A char #\F)))

(defun non-ascii-code-point-p (char)
  (char>= char #\U+0080))

(defun name-start-code-point-p (char)
  (or (letter-p char)
      (non-ascii-code-point-p char)
      (char= #\_ char)))

(defun name-code-point-p (char)
  (or (name-start-code-point-p char)
      (digit-p char)
      (char= #\- char)))

(defun valid-escape-p (two-chars)
  (and (char= #\\ (char two-chars 0))
       (not (char= #\Newline (char two-chars 1)))))

(defun start-with-a-valid-escape-p (tokenizer)
  (let ((two-chars (concatenate 'string
                                (string (current-input-code-point tokenizer))
                                (string (next-input-code-point tokenizer)))))
    (valid-escape-p two-chars)))

(defun identifier-start-p (three-chars)
  (cond
   ((char= #\- (char three-chars 0)) (or (name-start-code-point-p (char three-chars 1))
                                         (char= #\- (char three-chars 1))
                                         (valid-escape-p (subseq three-chars 1))))
   ((name-start-code-point-p (char three-chars 0)) t)
   ((char= #\\ (char three-chars 0)) (valid-escape-p (subseq three-chars 0 2)))))

(defun start-an-identifier-p (tokenizer)
  (let ((three-chars (concatenate 'string
                                  (string (current-input-code-point tokenizer))
                                  (next-2-input-code-points tokenizer))))
    (identifier-start-p three-chars)))

(defun number-start-p (three-chars)
  (cond
   ((or (char= #\+ (char three-chars 0))
        (char= #\- (char three-chars 0)))
    (or (digit-p (char three-chars 1))
        (and (char= #\. (char three-chars 1))
             (digit-p (char three-chars 2)))))
   ((char= #\. (char three-chars 0))
    (digit-p (char three-chars 1)))
   ((digit-p (char three-chars 0)) t)))

(defun start-a-number-p (tokenizer)
  (let ((three-chars (concatenate 'string
                                  (string (current-input-code-point tokenizer))
                                  (next-2-input-code-points tokenizer))))
    (number-start-p three-chars)))

(defun consume-token (tokenizer)
  (consume-comments tokenizer)
  (let ((char (consume-code-point tokenizer)))
    (cond
     ((whitespace-p char) (consume-whitespace tokenizer) :whitespace)
     ((char= #\" char) (consume-string tokenizer))
     ((char= #\# char) (if (or (name-code-point-p (next-input-code-point tokenizer))
                               (valid-escape-p (next-2-input-code-points tokenizer)))
                           (let ((type (when (start-an-identifier-p
                                              (next-3-input-code-points tokenizer))
                                         :id))
                                 (name (consume-name tokenizer)))
                             `(:hash ,type ,name))
                         `(:delim ,char)))
     ((char= #\' char) (consume-string tokenizer))
     ((char= #\( char) :left-parenthesis)
     ((char= #\) char) :right-parenthesis)
     ((char= #\+ char) (if (start-a-number-p tokenizer)
                           (progn
                             (reconsume-current-input-code-point tokenizer)
                             (consume-number tokenizer))
                         `(:delim ,char)))
     ((char= #\, char) :comma)
     ((char= #\- char) (if (start-a-number-p tokenizer)
                           (progn
                             (reconsume-current-input-code-point tokenizer)
                             (consume-number tokenizer))
                         (if (start-an-identifier-p tokenizer)
                             (progn
                               (reconsume-current-input-code-point tokenizer)
                               (consume-ident-like tokenizer))
                           `(:delim ,char))))
     ((char= #\. char) (if (start-a-number-p tokenizer)
                           (progn
                             (reconsume-current-input-code-point tokenizer)
                             (consume-number tokenizer))
                         `(:delim ,char)))
     ((char= #\: char) :colon)
     ((char= #\; char) :semicolon)
     ((char= #\< char) `(:delim ,char))
     ((char= #\@ char) (if (identifier-start-p
                            (next-3-input-code-points tokenizer))
                           (let ((name (consume-name tokenizer)))
                             `(:at-keyword ,name))
                         `(:delim ,char)))
     ((char= #\[ char) :left-square-bracket)
     ((char= #\\ char) (if (start-with-a-valid-escape-p tokenizer)
                           (progn
                             (reconsume-current-input-code-point tokenizer)
                             (consume-ident-like tokenizer))
                         `(:delim ,char)))
     ((char= #\] char) :right-square-bracket)
     ((char= #\{ char) :left-curly-bracket)
     ((char= #\} char) :right-curly-bracket)
     ((digit-p char)
      (reconsume-current-input-code-point tokenizer)
      (consume-number tokenizer))
     ((name-start-code-point-p char)
      (reconsume-current-input-code-point tokenizer)
      (consume-ident-like tokenizer))
     ((null char))
     (t `(:delim ,char)))))

(defun consume-code-point (tokenizer)
  (with-slots (stream buffer current-input-code-point) tokenizer
    (if-let (char (aref buffer 0))
        (prog1
            char
          (setf current-input-code-point char)
          (setf (aref buffer 0) (aref buffer 1)
                (aref buffer 1) (aref buffer 2)
                (aref buffer 2) nil))
      (setf current-input-code-point (read-char stream nil nil)))))

(defun next-input-code-point (tokenizer)
  (with-slots (stream buffer) tokenizer
    (or
     (aref buffer 0)
     (progn
       (read-sequence buffer stream)
       (aref buffer 0)))))

(defun next-2-input-code-points (tokenizer)
  (with-slots (stream buffer) tokenizer
    (if (aref buffer 1)
        (coerce (subseq buffer 0 2) 'string)
      (progn
        (if (aref buffer 0)
            (read-sequence buffer stream :start 1)
          (read-sequence buffer stream))
        (coerce (subseq buffer 0 2) 'string)))))

(defun next-3-input-code-points (tokenizer)
  (with-slots (stream buffer) tokenizer
    (if (aref buffer 2)
        (coerce (subseq buffer 0 3) 'string)
      (progn
        (cond
         ((aref buffer 1) (read-sequence buffer stream :start 2))
         ((aref buffer 0) (read-sequence buffer stream :start 1))
         (t (read-sequence buffer stream )))
        (coerce (subseq buffer 0 3) 'string)))))

(defun reconsume-current-input-code-point (tokenizer)
  (with-slots (stream buffer current-input-code-point) tokenizer
    (when current-input-code-point
      (cond
       ((aref buffer 2)
        (unread-char (aref buffer 0) stream)
        (setf (aref buffer 2) (aref buffer 1)
              (aref buffer 1) (aref buffer 0)
              (aref buffer 0) current-input-code-point))
       ((aref buffer 1)
        (unread-char (aref buffer 0) stream)
        (setf (aref buffer 1) (aref buffer 0)
              (aref buffer 0) current-input-code-point))
       ((aref buffer 0)
        (unread-char (aref buffer 0) stream)
        (setf (aref buffer 0) current-input-code-point))
       (t (unread-char current-input-code-point stream))))))

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
        while (whitespace-p (next-input-code-point tokenizer))
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
        (when (whitespace-p (next-input-code-point tokenizer))
          (consume-code-point tokenizer))
        (let ((number (parse-integer hexdigits :radix 16)))
          (if (or (zerop number)
                  (<= #xD800 number #xDFFF)
                  (> number #x10FFFF))
              #\Replacement-Character
            (code-char number)))))
     ((null char) #\Replacement-Character)
     (t char))))

(defun consume-name (tokenizer)
  (let ((result ""))
    (loop for char = (consume-code-point tokenizer)
          do (cond
              ((name-code-point-p char)
               (setf result (concatenate 'string
                                         result
                                         (string char))))
              ((start-with-a-valid-escape-p tokenizer)
               (let ((char (consume-escaped-code-point tokenizer)))
                 (setf result (concatenate 'string
                                         result
                                         (string char)))))
              (t (reconsume-current-input-code-point tokenizer)
                 (return result))))))

(defun consume-number (tokenizer)
  (let ((type :integer)
        (repr ""))
    ))

(defun consume-ident-like (tokenizer)
  )

