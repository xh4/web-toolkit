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

(defmacro define-code-point-predicate (name (char) &body body)
  `(defun ,name (,char)
     (when ,char
       ,@body)))

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
  (or (<= #\Null char #\Backspace)
      (eq #\VT char)
      (<= #\SO char #\US)
      (eq #\Delete char)))

(defun valid-escape-p (two-chars)
  (and (eq #\\ (char two-chars 0))
       (not (eq #\Newline (char two-chars 1)))))

(defun start-with-a-valid-escape-p (tokenizer)
  (let ((two-chars (concatenate 'string
                                (string (current-input-code-point tokenizer))
                                (string (next-input-code-point tokenizer)))))
    (valid-escape-p two-chars)))

(defun identifier-start-p (three-chars)
  (cond
   ((eq #\- (char three-chars 0)) (or (name-start-code-point-p (char three-chars 1))
                                         (eq #\- (char three-chars 1))
                                         (valid-escape-p (subseq three-chars 1))))
   ((name-start-code-point-p (char three-chars 0)) t)
   ((eq #\\ (char three-chars 0)) (valid-escape-p (subseq three-chars 0 2)))))

(defun start-an-identifier-p (tokenizer)
  (let ((three-chars (concatenate 'string
                                  (string (current-input-code-point tokenizer))
                                  (next-2-input-code-points tokenizer))))
    (identifier-start-p three-chars)))

(defun number-start-p (three-chars)
  (cond
   ((or (eq #\+ (char three-chars 0))
        (eq #\- (char three-chars 0)))
    (or (digit-p (char three-chars 1))
        (and (eq #\. (char three-chars 1))
             (digit-p (char three-chars 2)))))
   ((eq #\. (char three-chars 0))
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
     ((eq #\" char) (consume-string-token tokenizer))
     ((eq #\# char) (if (or (name-code-point-p (next-input-code-point tokenizer))
                               (valid-escape-p (next-2-input-code-points tokenizer)))
                           (let ((type (when (start-an-identifier-p
                                              (next-3-input-code-points tokenizer))
                                         :id))
                                 (name (consume-name tokenizer)))
                             `(:hash ,type ,name))
                         `(:delim ,char)))
     ((eq #\' char) (consume-string-token tokenizer))
     ((eq #\( char) :left-parenthesis)
     ((eq #\) char) :right-parenthesis)
     ((eq #\+ char) (if (start-a-number-p tokenizer)
                           (progn
                             (reconsume-current-input-code-point tokenizer)
                             (consume-numeric-token tokenizer))
                         `(:delim ,char)))
     ((eq #\, char) :comma)
     ((eq #\- char) (if (start-a-number-p tokenizer)
                           (progn
                             (reconsume-current-input-code-point tokenizer)
                             (consume-numeric-token tokenizer))
                         (if (start-an-identifier-p tokenizer)
                             (progn
                               (reconsume-current-input-code-point tokenizer)
                               (consume-ident-like-token tokenizer))
                           `(:delim ,char))))
     ((eq #\. char) (if (start-a-number-p tokenizer)
                           (progn
                             (reconsume-current-input-code-point tokenizer)
                             (consume-numeric-token tokenizer))
                         `(:delim ,char)))
     ((eq #\: char) :colon)
     ((eq #\; char) :semicolon)
     ((eq #\< char) `(:delim ,char))
     ((eq #\@ char) (if (identifier-start-p
                            (next-3-input-code-points tokenizer))
                           (let ((name (consume-name tokenizer)))
                             `(:at-keyword ,name))
                         `(:delim ,char)))
     ((eq #\[ char) :left-square-bracket)
     ((eq #\\ char) (if (start-with-a-valid-escape-p tokenizer)
                           (progn
                             (reconsume-current-input-code-point tokenizer)
                             (consume-ident-like-token tokenizer))
                         `(:delim ,char)))
     ((eq #\] char) :right-square-bracket)
     ((eq #\{ char) :left-curly-bracket)
     ((eq #\} char) :right-curly-bracket)
     ((digit-p char)
      (reconsume-current-input-code-point tokenizer)
      (consume-numeric-token tokenizer))
     ((name-start-code-point-p char)
      (reconsume-current-input-code-point tokenizer)
      (consume-ident-like-token tokenizer))
     ((null char) nil)
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

(defun consume-string-token (tokenizer)
  (let ((ending-code-point (current-input-code-point tokenizer))
        (string ""))
    (loop for char = (consume-code-point tokenizer)
          do (cond
              ((eq ending-code-point char)
               (return-from consume-string-token `(:string ,string)))
              ((null char)
               (error "End of input white consume string"))
              ((eq #\Newline char)
               (reconsume-current-input-code-point tokenizer)
               (return-from consume-string-token :bad-string))
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

(defun consume-numeric-token (tokenizer)
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
      (when (and (eq #\. (char two-chars 0))
                 (digit-p (char two-chars 1)))
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
      (when (and (or (eq #\e (char three-chars 0))
                     (eq #\E (char three-chars 0)))
                 (or (and (digit-p (char three-chars 1))
                          (setf count 2))
                     (and (or (eq #\- (char three-chars 1))
                              (eq #\+ (char three-chars 1)))
                          (digit-p (char three-chars 2))
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
      `(,type ,value))))

(defun convert-string-to-number (string)
  (let (s i f d t_ e)
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
    (* s
       (+ i (* f (expt 10 (- d))))
       (expt 10 (* t_ e)))))

(defun consume-ident-like-token (tokenizer)
  (let ((string (consume-name tokenizer)))
    (cond
     ((and (string= "url" string)
           (eq #\( (next-input-code-point tokenizer)))
      (consume-code-point tokenizer)
      (loop for two-chars = (next-2-input-code-points tokenizer)
            while (and (whitespace-p (char two-chars 0))
                       (whitespace-p (char two-chars 1)))
            do (consume-code-point tokenizer))
      (if (or (eq #\" (next-input-code-point tokenizer))
              (eq #\' (next-input-code-point tokenizer))
              (let ((two-chars (next-2-input-code-points tokenizer)))
                (and (whitespace-p (char two-chars 0))
                     (eq #\" (char two-chars 1))
                     (eq #\' (char two-chars 1)))))
          `(:function ,string)
        (consume-url-token tokenizer)))
     ((eq #\( (next-input-code-point tokenizer))
      (consume-code-point tokenizer)
      `(:function ,string))
     (t `(:ident ,string)))))

(defun consume-url-token (tokenizer)
  (let ((url ""))
    (consume-whitespace tokenizer)
    (loop for char = (consume-code-point tokenizer)
          do (cond
              ((eq #\) char) `(:url ,url))
              ((null char) `(:url ,url))
              ((whitespace-p char)
               (consume-whitespace tokenizer)
               (let ((char (next-input-code-point tokenizer)))
                 (if (or (eq #\) char)
                         (null char))
                     (progn
                       (consume-code-point tokenizer)
                       (return-from consume-url-token `(:url ,url)))
                   (progn
                     (consume-remnants-of-bad-url tokenizer)
                     (return-from consume-url-token :bad-url)))))
              ((or (eq #\" char)
                   (eq #\' char)
                   (eq #\( char)
                   (non-printable-code-point-p char))
               (consume-remnants-of-bad-url tokenizer)
               (return-from consume-url-token :bad-url))
              ((eq #\\ char)
               (if (start-with-a-valid-escape-p tokenizer)
                   (let ((char (consume-escaped-code-point tokenizer)))
                     (setf url (concatenate 'string url (string char))))
                 (progn
                   (consume-remnants-of-bad-url tokenizer)
                   (return-from consume-url-token :bad-url))))
              (t (setf url (concatenate 'string url (string char))))))))

(defun consume-remnants-of-bad-url (tokenizer)
  (loop for char = (consume-code-point tokenizer)
        do (cond
            ((or (eq #\) char)
                 (null char)) (return-from consume-remnants-of-bad-url))
            ((start-with-a-valid-escape-p tokenizer)
             (consume-escaped-code-point tokenizer)))))

(defun test-tokenizer ()
  (with-input-from-string (stream ".foo {background: red;}")
    (let ((tokenizer (make-instance 'tokenizer :stream stream)))
      (loop for token = (consume-token tokenizer)
            while token
            do (format t "~A~%" token)))))