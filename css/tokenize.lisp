(in-package :css)

;; https://www.w3.org/TR/css-syntax-3/#tokenizing-and-parsing

(define-condition parse-error (error) ())

(defstruct ident-token (value))

(define-serialize-method (ident-token stream)
  (format stream "~A" (ident-token-value ident-token)))

(defstruct function-token (value))

(define-serialize-method (function-token stream)
  (format stream "~A" (function-token-value function-token)))

(defstruct at-keyword-token (value))

(define-serialize-method (at-keyword-token stream)
  (format stream "@~A" (at-keyword-token-value at-keyword-token)))

(defstruct hash-token (type) (name))

(define-serialize-method (hash-token stream)
  (format stream "#~A" (hash-token-name hash-token)))

(defstruct string-token (value))

(define-serialize-method (string-token stream)
  (format stream "~S" (string-token-value string-token)))

(defstruct bad-string-token (value))

(defstruct url-token (value))

(define-serialize-method (url-token stream)
  (format stream "url(~A)" (url-token-value url-token)))

(defstruct bad-url-token (value))

(defstruct delim-token (value))

(define-serialize-method (delim-token stream)
  (format stream "~A" (delim-token-value delim-token)))

(defstruct number-token (value))

(define-serialize-method (number-token stream)
  (format stream "~A" (number-token-value number-token)))

(defstruct percentage-token (value))

(define-serialize-method (percentage-token stream)
  (format stream "~A%" (percentage-token-value percentage-token)))

(defstruct dimension-token (value))

(defstruct whitespace-token)

(define-serialize-method (whitespace-token stream)
  (format stream " "))

(defstruct cdo-token)

(defstruct cdc-token)

(defstruct colon-token)

(define-serialize-method (colon-token stream)
  (format stream ":"))

(defstruct semicolon-token)

(define-serialize-method (semicolon-token stream)
  (format stream ";"))

(defstruct comma-token)

(define-serialize-method (comma-token stream)
  (format stream ","))

(defstruct left-square-bracket-token)

(define-serialize-method (left-square-bracket-token stream)
  (format stream "["))

(defstruct right-square-bracket-token)

(define-serialize-method (right-square-bracket-token stream)
  (format stream "]"))

(defstruct left-parenthesis-token)

(define-serialize-method (left-parenthesis-token stream)
  (format stream "("))

(defstruct right-parenthesis-token)

(define-serialize-method (right-parenthesis-token stream)
  (format stream ")"))

(defstruct left-curly-bracket-token)

(define-serialize-method (left-curly-bracket-token stream)
  (format stream "{"))

(defstruct right-curly-bracket-token)

(define-serialize-method (right-curly-bracket-token stream)
  (format stream "}"))

(defclass tokenizer ()
  ((stream
    :initarg :stream
    :initform nil)
   (buffer
    :initform (make-array 4 :initial-element nil))
   (current-input-code-point
    :initform nil
    :reader current-input-code-point)))

(defmacro define-code-point-predicate (name (char) &body body)
  `(defun ,name (,char)
     (when ,char
       ,@body)))

(defconstant +replacement-character+
  #+lispworks #\Replacement-Character #-lispworks #\Replacement_Character)

(define-code-point-predicate uppercase-letter-p (char)
  (char<= #\A char #\Z))

(define-code-point-predicate lowercase-letter-p (char)
  (char<= #\a char #\z))

(define-code-point-predicate letter-p (char)
  (or (uppercase-letter-p char)
      (lowercase-letter-p char)))

(define-code-point-predicate digit-p (char)
  (char<= #\0 char #\9))

(define-code-point-predicate hex-digit-p (char)
  (or (digit-p char)
      (char<= #\a char #\f)
      (char<= #\A char #\F)))

(define-code-point-predicate non-ascii-code-point-p (char)
  (char>= char #\U+0080))

(define-code-point-predicate name-start-code-point-p (char)
  (or (letter-p char)
      (non-ascii-code-point-p char)
      (eq #\_ char)))

(define-code-point-predicate name-code-point-p (char)
  (or (name-start-code-point-p char)
      (digit-p char)
      (eq #\- char)))

(define-code-point-predicate non-printable-code-point-p (char)
  (or (char<= #\Null char #\Backspace)
      (eq #\VT char)
      (char<= #\SO char #\US)
      (eq #\Delete char)))

(defun valid-escape-p (two-chars)
  (and (eq #\\ (aref two-chars 0))
       (not (eq #\Newline (aref two-chars 1)))))

(defun start-with-a-valid-escape-p (tokenizer)
  (let ((two-chars (concatenate 'string
                                (string (current-input-code-point tokenizer))
                                (string (next-input-code-point tokenizer)))))
    (valid-escape-p two-chars)))

(defun identifier-start-p (three-chars)
  (cond
   ((eq #\- (aref three-chars 0)) (or (name-start-code-point-p (aref three-chars 1))
                                      (eq #\- (aref three-chars 1))
                                      (valid-escape-p (subseq three-chars 1))))
   ((name-start-code-point-p (aref three-chars 0)) t)
   ((eq #\\ (aref three-chars 0)) (valid-escape-p (subseq three-chars 0 2)))))

(defun start-an-identifier-p (tokenizer)
  (let ((three-chars (concatenate 'vector
                                  (make-array 1 :initial-element (current-input-code-point tokenizer))
                                  (next-2-input-code-points tokenizer))))
    (identifier-start-p three-chars)))

(defun number-start-p (three-chars)
  (cond
   ((or (eq #\+ (aref three-chars 0))
        (eq #\- (aref three-chars 0)))
    (or (digit-p (aref three-chars 1))
        (and (eq #\. (aref three-chars 1))
             (digit-p (aref three-chars 2)))))
   ((eq #\. (aref three-chars 0))
    (digit-p (aref three-chars 1)))
   ((digit-p (aref three-chars 0)) t)))

(defun start-a-number-p (tokenizer)
  (let ((three-chars (concatenate 'vector
                                  (make-array 1 :initial-element (current-input-code-point tokenizer))
                                  (next-2-input-code-points tokenizer))))
    (number-start-p three-chars)))

(defmacro define-consume-function (name (tokenizer) &body body)
  `(defun ,name (,tokenizer) ,@body))

(define-consume-function consume-token (tokenizer)
  (consume-comments tokenizer)
  (let ((char (consume-code-point tokenizer)))
    (cond
     ((whitespace-p char) (consume-whitespace tokenizer) (make-whitespace-token))
     ((eq #\" char) (consume-string-token tokenizer))
     ((eq #\# char) (if (or (name-code-point-p (next-input-code-point tokenizer))
                            (valid-escape-p (next-2-input-code-points tokenizer)))
                        (let ((type (when (start-an-identifier-p tokenizer)
                                      :id))
                              (name (consume-name tokenizer)))
                          (make-hash-token :type type :name name))
                      (make-delim-token :value char)))
     ((eq #\' char) (consume-string-token tokenizer))
     ((eq #\( char) (make-left-parenthesis-token))
     ((eq #\) char) (make-right-parenthesis-token))
     ((eq #\+ char) (if (start-a-number-p tokenizer)
                        (progn
                          (reconsume-current-input-code-point tokenizer)
                          (consume-numeric-token tokenizer))
                      (make-delim-token :value char)))
     ((eq #\, char) (make-comma-token))
     ((eq #\- char) (if (start-a-number-p tokenizer)
                        (progn
                          (reconsume-current-input-code-point tokenizer)
                          (consume-numeric-token tokenizer))
                      (if (start-an-identifier-p tokenizer)
                          (progn
                            (reconsume-current-input-code-point tokenizer)
                            (consume-ident-like-token tokenizer))
                        (make-delim-token :value char))))
     ((eq #\. char) (if (start-a-number-p tokenizer)
                        (progn
                          (reconsume-current-input-code-point tokenizer)
                          (consume-numeric-token tokenizer))
                      (make-delim-token :value char)))
     ((eq #\: char) (make-colon-token))
     ((eq #\; char) (make-semicolon-token))
     ((eq #\< char) (make-delim-token :value char))
     ((eq #\@ char) (if (identifier-start-p
                         (next-3-input-code-points tokenizer))
                        (let ((name (consume-name tokenizer)))
                          (make-at-keyword-token :value name))
                      (make-delim-token :value char)))
     ((eq #\[ char) (make-left-square-bracket-token))
     ((eq #\\ char) (if (start-with-a-valid-escape-p tokenizer)
                        (progn
                          (reconsume-current-input-code-point tokenizer)
                          (consume-ident-like-token tokenizer))
                      (make-delim-token :value char)))
     ((eq #\] char) (make-right-square-bracket-token))
     ((eq #\{ char) (make-left-curly-bracket-token))
     ((eq #\} char) (make-right-curly-bracket-token))
     ((digit-p char)
      (reconsume-current-input-code-point tokenizer)
      (consume-numeric-token tokenizer))
     ((name-start-code-point-p char)
      (reconsume-current-input-code-point tokenizer)
      (consume-ident-like-token tokenizer))
     ((null char) nil)
     (t (make-delim-token :value char)))))

(define-consume-function consume-code-point (tokenizer)
  (with-slots (stream buffer current-input-code-point) tokenizer
    (unless (aref buffer 0)
      (read-sequence buffer stream :start 0 :end 4))
    (prog1
        (aref buffer 0)
      (setf current-input-code-point (aref buffer 0))
      (setf (aref buffer 0) (aref buffer 1)
            (aref buffer 1) (aref buffer 2)
            (aref buffer 2) (aref buffer 3)
            (aref buffer 3) nil))))

(defun next-input-code-point (tokenizer)
  (with-slots (stream buffer) tokenizer
    (or
     (aref buffer 0)
     (progn
       (read-sequence buffer stream :start 0 :end 3)
       (aref buffer 0)))))

(defun next-2-input-code-points (tokenizer)
  (with-slots (stream buffer) tokenizer
    (if (aref buffer 1)
        (subseq buffer 0 2)
      (progn
        (if (aref buffer 0)
            (read-sequence buffer stream :start 1 :end 3)
          (read-sequence buffer stream :start 0 :end 3))
        (subseq buffer 0 2)))))

(defun next-3-input-code-points (tokenizer)
  (with-slots (stream buffer) tokenizer
    (if (aref buffer 2)
        (subseq buffer 0 3)
      (progn
        (cond
         ((aref buffer 1) (read-sequence buffer stream :start 2 :end 3))
         ((aref buffer 0) (read-sequence buffer stream :start 1 :end 3))
         (t (read-sequence buffer stream :start 0 :end 3)))
        (subseq buffer 0 3)))))

(defun reconsume-current-input-code-point (tokenizer)
  (with-slots (stream buffer current-input-code-point) tokenizer
    (setf (aref buffer 3) (aref buffer 2)
          (aref buffer 2) (aref buffer 1)
          (aref buffer 1) (aref buffer 0)
          (aref buffer 0) current-input-code-point)))

(define-consume-function consume-comments (tokenizer)
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

(define-consume-function consume-whitespace (tokenizer)
  (loop for count from 0
        while (whitespace-p (next-input-code-point tokenizer))
        do (consume-code-point tokenizer)))

(define-consume-function consume-string-token (tokenizer)
  (let ((ending-code-point (current-input-code-point tokenizer))
        (string ""))
    (loop for char = (consume-code-point tokenizer)
          do (cond
              ((eq ending-code-point char)
               (return (make-string-token :value string)))
              ((null char)
               (error "End of input white consume string"))
              ((eq #\Newline char)
               (reconsume-current-input-code-point tokenizer)
               (return (make-bad-string-token)))
              ((eq #\/ char)
               (let ((next-char (next-input-code-point tokenizer)))
                 (cond
                  ((null next-char))
                  ((eq #\Newline next-char) (consume-code-point tokenizer))
                  (t (let ((char (consume-escaped-code-point tokenizer)))
                       (setf string (concatenate 'string string (string char))))))))
              (t (setf string (concatenate 'string string (string char))))))))

(define-consume-function consume-hex-digit (tokenizer)
  (let ((char (next-input-code-point tokenizer)))
    (when (and char (hex-digit-p char))
      (consume-code-point tokenizer))))

(define-consume-function consume-escaped-code-point (tokenizer)
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
              +replacement-character+
            (code-char number)))))
     ((null char) +replacement-character+)
     (t char))))

(define-consume-function consume-name (tokenizer)
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

(define-consume-function consume-numeric-token (tokenizer)
  (let ((type :integer)
        (repr ""))
    (let ((char (next-input-code-point tokenizer)))
      (when (or (eq #\+ char)
                (eq #\- char))
        (consume-code-point tokenizer)
        (setf repr (concatenate 'string repr (string char)))))
    (loop for char = (next-input-code-point tokenizer)
          while (digit-p char)
          do (progn
               (consume-code-point tokenizer)
               (setf repr (concatenate 'string repr (string char)))))
    (let ((two-chars (next-2-input-code-points tokenizer)))
      (when (and (eq #\. (aref two-chars 0))
                 (digit-p (aref two-chars 1)))
        (loop repeat 2 do
              (setf repr (concatenate 'string repr
                                      (string (consume-code-point tokenizer)))))
        (setf type :number)
        (loop for char = (next-input-code-point tokenizer)
              while (digit-p char)
              do (progn
                   (consume-code-point tokenizer)
                   (setf repr (concatenate 'string repr (string char)))))))
    (let ((three-chars (next-3-input-code-points tokenizer))
          (count 0))
      (when (and (or (eq #\e (aref three-chars 0))
                     (eq #\E (aref three-chars 0)))
                 (or (and (digit-p (aref three-chars 1))
                          (setf count 2))
                     (and (or (eq #\- (aref three-chars 1))
                              (eq #\+ (aref three-chars 1)))
                          (digit-p (aref three-chars 2))
                          (setf count 3))))
        (loop repeat count do
              (setf repr (concatenate 'string repr
                                      (string (consume-code-point tokenizer)))))
        (setf type :number)
        (loop for char = (next-input-code-point tokenizer)
              while (digit-p char)
              do (progn
                   (consume-code-point tokenizer)
                   (setf repr (concatenate 'string repr (string char)))))))
    (let ((value (convert-string-to-number repr)))
      (make-number-token :value value))))

(defun convert-string-to-number (string)
  (let (s i f d t_ e has-dot has-e)
    (let ((index 0)
          (ending (1- (cl:length string))))
      ;; sign
      (cond
       ((eq #\- (char string index)) (setf s -1) (incf index))
       ((eq #\+ (char string index)) (setf s 1) (incf index))
       (t (setf s 1)))
      ;; integer part
      (loop with start = index
            for end from index upto ending
            while (digit-p (char string end))
            do (continue)
            finally (if (plusp (- end start))
                        (setf i (parse-integer (subseq string start end))
                              index end)
                      (setf i 0 index end)))
      ;; decimal point
      (when (and (<= index ending)
                 (eq #\. (char string index)))
        (setf has-dot t)
        (incf index))
      ;; fractional part
      (loop with start = index
            for end from index upto ending
            while (digit-p (char string end))
            do (continue)
            finally (if (plusp (- end start))
                        (setf f (parse-integer (subseq string start end))
                              d (- end start)
                              index end)
                      (setf f 0 d 0 index end)))
      ;; exponent indicator
      (when (and (<= index ending)
                 (or (eq #\e (char string index))
                     (eq #\E (char string index))))
        (setf has-e t)
        (incf index))
      ;; exponent sign
      (if (<= index ending)
          (cond
           ((eq #\- (char string index)) (setf t_ -1) (incf index))
           ((eq #\+ (char string index)) (setf t_ 1) (incf index))
           (t (setf t_ 1)))
        (setf t_ 1))
      ;; exponent
      (loop with start = index
            for end from index upto ending
            while (digit-p (char string end))
            do (continue)
            finally (if (plusp (- end start))
                        (setf e (parse-integer (subseq string start end)))
                      (setf e 0))))
    (let ((number (* s
                     (+ i (* f (expt 10 (- d))))
                     (expt 10 (* t_ e)))))
      (if (or has-dot has-e)
          (coerce number 'single-float)
        number))))

(define-consume-function consume-ident-like-token (tokenizer)
  (let ((string (consume-name tokenizer)))
    (cond
     ((and (string= "url" string)
           (eq #\( (next-input-code-point tokenizer)))
      (consume-code-point tokenizer)
      (loop for two-chars = (next-2-input-code-points tokenizer)
            while (and (whitespace-p (aref two-chars 0))
                       (whitespace-p (aref two-chars 1)))
            do (consume-code-point tokenizer))
      (if (or (eq #\" (next-input-code-point tokenizer))
              (eq #\' (next-input-code-point tokenizer))
              (let ((two-chars (next-2-input-code-points tokenizer)))
                (and (whitespace-p (aref two-chars 0))
                     (eq #\" (aref two-chars 1))
                     (eq #\' (aref two-chars 1)))))
          (make-function-token :value string)
        (consume-url-token tokenizer)))
     ((eq #\( (next-input-code-point tokenizer))
      (consume-code-point tokenizer)
      (make-function-token :value string))
     (t (make-ident-token :value string)))))

(define-consume-function consume-url-token (tokenizer)
  (let ((url ""))
    (consume-whitespace tokenizer)
    (loop for char = (consume-code-point tokenizer)
          do (cond
              ((eq #\) char) (return (make-url-token :value url)))
              ((null char) (return (make-url-token :value url)))
              ((whitespace-p char)
               (consume-whitespace tokenizer)
               (let ((char (next-input-code-point tokenizer)))
                 (if (or (eq #\) char)
                         (null char))
                     (progn
                       (consume-code-point tokenizer)
                       (return (make-url-token :value url)))
                   (progn
                     (consume-remnants-of-bad-url tokenizer)
                     (return (make-bad-url-token))))))
              ((or (eq #\" char)
                   (eq #\' char)
                   (eq #\( char)
                   (non-printable-code-point-p char))
               (consume-remnants-of-bad-url tokenizer)
               (return (make-bad-url-token)))
              ((eq #\\ char)
               (if (start-with-a-valid-escape-p tokenizer)
                   (let ((char (consume-escaped-code-point tokenizer)))
                     (setf url (concatenate 'string url (string char))))
                 (progn
                   (consume-remnants-of-bad-url tokenizer)
                   (return (make-bad-url-token)))))
              (t (setf url (concatenate 'string url (string char))))))))

(define-consume-function consume-remnants-of-bad-url (tokenizer)
  (loop for char = (consume-code-point tokenizer)
        do (cond
            ((or (eq #\) char)
                 (null char)) (return-from consume-remnants-of-bad-url))
            ((start-with-a-valid-escape-p tokenizer)
             (consume-escaped-code-point tokenizer)))))

(defun serialize-tokens (tokens &optional stream)
  (let ((string-stream-p (null stream)))
    (when string-stream-p (setf stream (make-string-output-stream)))
    (loop for token in tokens do (serialize token stream))
    (when string-stream-p
      (get-output-stream-string stream))))

(defgeneric tokenize (source)
  (:method ((stream stream))
   (let ((tokenizer (make-instance 'tokenizer :stream stream)))
     (loop for token = (consume-token tokenizer)
           while token
           collect token)))
  (:method ((string string))
   (with-input-from-string (stream string)
     (tokenize stream)))
  (:method ((pathname pathname))
   (with-open-file (stream pathname)
     (tokenize stream))))
