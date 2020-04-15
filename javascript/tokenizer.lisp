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

(defun tokenize (source)
  (let ((tokenizer (make-tokenizer source)))
    (loop for token = (get-next-token tokenizer)
          while token
          collect token)))

(defclass reader ()
  ((values
    :initform nil)
   (curly
    :initform -1)
   (paren
    :initform -1)))

(defun regex-start-p (reader)
  (with-slots (values curly paren) reader
    (let ((previous (first values)))
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
           ((equal "function" (nth (- curly 3) values))
            (let ((check (nth (- curly 4) values)))
              (setf regex-p (if check (not (before-function-expression-p check)) nil))))
           ((equal "function" (nth (- curly 3) values))
            (let ((check (nth (- curly 5) values)))
              (setf regex-p (if check (not (before-function-expression-p check)) t)))))))
        regex-p))))

(defun before-function-expression-p (str)
  (member str '("(" "{" "[" "in" "typeof" "instanceof" "new"
                "return" "case" "delete" "throw" "void" "=" "+="
                ;; assignment operators
                "-=" "*=" "**=" "/=" "%=" "<<=" ">>=" ">>>="
                "&=" "|=" "^=" ","
                ;; binary/unary operators
                "+" "-" "*" "**" "/" "%" "++" "--" "<<" ">>" ">>>" "&"
                "|" "^" "!" "~" "&&" "||" "?" ":" "===" "==" ">=" "<="
                "<" ">" "!=" "!==")
          :test 'equal))

(defun push-token (reader token)
  (with-slots (values curly paren) reader
    (if (or (eq :punctuator (token-type token))
            (eq :keyword (token-type token)))
        (progn
          (cond
           ((eq "{" (token-value token)) (setf curly (length values)))
           ((eq "(" (token-value token)) (setf paren (length values))))
          (appendf values (list (token-value token))))
      (appendf values (list nil)))))
