(in-package :javascript)

(defclass tokenizer ()
  ((scanner
    :initarg :scanner
    :initform nil)
   (reader
    :initarg :reader
    :initform nil)
   (buffer
    :initform nil)))

(defun make-tokenizer (source)
  (let ((scanner (make-instance 'scanner :source source))
        (reader (make-instance 'reader)))
    (make-instance 'tokenizer :scanner scanner :reader reader)))

(defun get-next-token (tokenizer)
  (with-slots (scanner reader buffer) tokenizer
    (scan-comments scanner)
    (unless (eof-p scanner)
      (let ((start-regex (with-slots (source index) scanner
                           (and (eq #\/ (char source index))
                                (regex-start-p reader)))))
        (let ((token (if start-regex
                         (scan-reg-exp scanner)
                       (lex scanner))))
          (push-token reader token)
          (push token buffer))))
    (pop buffer)))

(defgeneric tokenize (source)
  (:method ((source string))
   (let ((tokenizer (make-tokenizer source)))
    (loop for token = (get-next-token tokenizer)
          while (and token (not (typep token 'eof)))
          collect token)))
  (:method ((stream stream))
   (tokenize (alexandria::read-stream-content-into-string stream)))
  (:method ((pathname pathname))
   (tokenize (read-file-into-string pathname))))

(defclass reader ()
  ((values
    :initform nil)
   (curly
    :initform -1)
   (paren
    :initform -1)))

;; Determine if forward slash (/) is an operator or part of a regular expression
;; https://github.com/mozilla/sweet.js/wiki/design
(defun regex-start-p (reader)
  (with-slots (values curly paren) reader
    (let ((previous (car (last values))))
      (let ((regex-p (not (null previous))))
        (cond
         ((or (equal "this" previous)
              (equal "]" previous))
          (setf regex-p nil))
         ((equal ")" previous)
          (let ((keyword (nth (1- paren) values)))
            (setf regex-p (or (equal "if" keyword)
                              (equal "while" keyword)
                              (equal "for" keyword)
                              (equal "with" keyword)))))
         ((equal "}" previous)
          (setf regex-p nil)
          (cond
           ;; Anonymous function, e.g. function(){} /42
           ((and (> curly 3)
                 (equal "function" (nth (- curly 3) values)))
            (let ((check (nth (- curly 4) values)))
              (setf regex-p (if check (not (before-function-expression-p check)) nil))))
           ;; Named function, e.g. function f(){} /42/
           ((and (> curly 4)
                 (equal "function" (nth (- curly 4) values)))
            (let ((check (nth (- curly 5) values)))
              (setf regex-p (if check (not (before-function-expression-p check)) t)))))))
        regex-p))))

;; A function following one of those tokens is an expression
(defun before-function-expression-p (str)
  (member str '("(" "{" "[" "in" "typeof" "instanceof" "new"
                "return" "case" "delete" "throw" "void"
                ;; assignment operators
                "=" "+=" "-=" "*=" "**=" "/=" "%=" "<<=" ">>=" ">>>="
                "&=" "|=" "^=" ","
                ;; binary/unary operators
                "+" "-" "*" "**" "/" "%" "++" "--" "<<" ">>" ">>>" "&"
                "|" "^" "!" "~" "&&" "||" "?" ":" "===" "==" ">=" "<="
                "<" ">" "!=" "!==")
          :test 'equal))

(defun push-token (reader token)
  (with-slots (values curly paren) reader
    (if (or (typep token 'punctuator)
            (typep token 'keyword))
        (let ((value (token-value token)))
          (cond
           ((equal "{" value) (setf curly (length values)))
           ((equal "(" value) (setf paren (length values))))
          (appendf values (list value)))
      (appendf values (list nil)))))
