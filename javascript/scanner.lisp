(in-package :javascript)

(defclass scanner ()
  ((source
    :initarg :source
    :initform nil
    :accessor scanner-source)
   (index
    :initform 0
    :accessor scanner-index)
   (line-number
    :initform 0
    :reader scanner-line-number)
   (line-start
    :initform 0
    :reader scanner-line-start)
   (curly-stack
    :initform nil
    :reader scanner-curly-stack)
   (length
    :initform nil
    :reader scanner-length)))

(defmethod initialize-instance :after ((scanner scanner) &key)
  (check-type (slot-value scanner 'source) string)
  (with-slots (source length) scanner
    (setf length (length source))))

(defun eof-p (scanner)
  (with-slots (index length) scanner
    (>= index length)))

(defun lex (scanner)
  (with-slots (index line-number line-start source curly-stack) scanner
    (when (eof-p scanner)
      (return-from lex (make-token
                        :type :eof
                        :line-number line-number
                        :line-start line-start
                        :start index
                        :end index)))
    (let ((char (char source index)))
      (cond
       ((or (eq #\( char)
            (eq #\) char)
            (eq #\; char)) (scan-punctuator scanner))
       ((or (eq #\' char)
            (eq #\" char)) (scan-string-literal scanner))
       ((eq #\. char) (if (decimal-digit-p (char source (1+ index)))
                          (scan-numeric-literal scanner)
                        (scan-punctuator scanner)))
       ((decimal-digit-p char) (scan-numeric-literal scanner))
       ((or (eq #\` char)
            (and (eq #\} char) (equal "${" (nth curly-stack (1- (length curly-stack))))))
        (scan-template scanner))
       ((<= #xD800 (char-code char) #xDFFF) (if (identifier-start-p char)
                                                (scan-identifier scanner)
                                              (scan-punctuator scanner)))
       (t (scan-punctuator scanner))))))

;; https://tc39.github.io/ecma262/#sec-punctuators
(defun scan-punctuator (scanner)
  (with-slots (index source curly-stack line-number line-start) scanner
    (let ((start index)
          (char (char source index))
          (str))
      (cond
       ((or (eq #\( char)
            (eq #\{ char))
        (when (eq #\{ char)
          (push #\{ curly-stack))
        (incf index))
       ((eq #\. char)
        (incf index)
        (when (and (eq #\. (char source index))
                   (eq #\. (char source (1+ index))))
          (incf index 2)
          (setf str "...")))
       ((eq #\} char)
        (incf index)
        (pop curly-stack))
       ((or (eq #\) char)
            (eq #\; char)
            (eq #\, char)
            (eq #\[ char)
            (eq #\] char)
            (eq #\: char)
            (eq #\? char)
            (eq #\~ char))
        (incf index))
       (t (setf str (subseq source index (+ 4 index)))
          (cond
           ((equal ">>>=" str) (incf index 4))
           (t (setf str (subseq str 0 3))
              (cond
               ((or (equal "===" str) (equal "!==" str) (equal ">>>" str)
                    (equal "<<=" str) (equal ">>=" str) (equal "**=" str))
                (incf index 3))
               (t (setf str (subseq str 0 2))
                  (cond
                   ((or (equal "&&" str) (equal "||" str) (equal "==" str) (equal "!=" str)
                        (equal "+=" str)  (equal "-=" str) (equal "*=" str) (equal "/=" str)
                        (equal "++" str) (equal "--" str) (equal "<<" str) (equal ">>" str)
                        (equal "&=" str) (equal "|=" str) (equal "^=" str) (equal "%=" str)
                        (equal "<=" str) (equal ">=" str) (equal "=>" str) (equal "**" str))
                    (incf index 2))
                   (t (setf str (string (char source index)))
                      (when (search str "<>=!+-*%&|^/")
                        (incf index))))))))))
      (when (= index start)
        (error "Unexpected token"))
      (make-token
       :type :punctuator
       :value str
       :line-number line-number
       :line-start line-start
       :start start
       :end index))))

;; https://tc39.github.io/ecma262/#sec-literals-string-literals
(defun scan-string-literal (scanner)
  (with-slots (index source curly-stack line-number line-start) scanner
    (let ((start index)
          (quote (char source index)))
      (incf index)
      (loop with octal = nil
            with str = ""
            with ending = nil
            while (not (eof-p scanner))
            for char = (char source index)
            do (cond
                ((eq quote char) (setf ending t) (loop-finish))
                ((eq #\\ char)
                 (let ((char (char source index)))
                   (incf index)
                   (cond
                    ((or (null char)
                         (not (line-terminator-p char)))
                     (cond
                      ((eq #\u char)
                       (cond
                        ((eq #\{ (char source index))
                         (incf index)
                         (setf str (concatenate 'string str (scan-unicode-code-point-escape scanner))))
                        (t (let ((unescaped (scan-hex-escape scanner char)))
                             (unless unescaped
                               (error "Unexpected token"))
                             (setf str (concatenate 'string str unescaped))))))
                      ((eq #\x char)
                       (let ((unescaped (scan-hex-escape scanner char)))
                         (unless unescaped
                               (error "Unexpected token"))
                         (setf str (concatenate 'string str unescaped))))
                      ((eq #\n char) (setf str (concatenate 'string str (string #\Newline))))
                      ((eq #\r char) (setf str (concatenate 'string str (string #\Return))))
                      ((eq #\t char) (setf str (concatenate 'string str (string #\Tab))))
                      ((eq #\b char) (setf str (concatenate 'string str (string #\Backspace))))
                      ((eq #\f char) (setf str (concatenate 'string str (string #\Page))))
                      ((eq #\v char) (setf str (concatenate 'string str (string #\VT))))
                      ((or (eq #\8 char)
                           (eq #\9 char))
                       (setf str (concatenate 'string str (string char)))
                       (tolerate-unexpected-token scanner))
                      (t (cond
                          ((and char (octal-digit-p char))
                           )
                          (t (setf str (concatenate 'string str (string char))))))))
                    (t (incf line-number)
                       (when (and (eq #\Return char)
                                  (eq #\Newline (char source index)))
                         (incf index))
                       (setf line-start index)))))
                ((line-terminator-p char) (loop-finish))
                (t (setf str (concatenate 'string str (string char)))))
            do (incf index)
            finally
            (unless ending
              (setf index start)
              (error "Unexpected token"))
            (return (make-token
                     :type :string-literal
                     :value str
                     :octal octal
                     :line-number line-number
                     :line-start line-start
                     :start start
                     :end index))))))

;; https://tc39.github.io/ecma262/#sec-names-and-keywords
(defun scan-identifier (scanner)
  (with-slots (index source line-number line-start) scanner
    (let ((start index)
          (type))
      (let ((id (if (eq #\\ (char source index))
                    (get-complex-identifier scanner)
                  (get-identifier scanner))))
        (cond
         ((= 1 (length id)) (setf type :identifier))
         ((keyword-p id) (setf type :keyword))
         ((equal "null" id) (setf type :null-literal))
         ((or (equal "true" id)
              (equal "false" id))
          (setf type :boolean-literal))
         (t (setf type :identifier)))
        (when (and (not (eq :identifier type))
                   (not (= (+ start (length id)) index)))
          (let ((restore index))
            (setf index start)
            (tolerate-unexpected-token scanner)
            (setf index restore)))
        (make-token :type type
                    :value id
                    :line-number line-number
                    :line-start line-start
                    :start start
                    :end index)))))

(defun get-identifier (scanner)
  (with-slots (index source line-number line-start) scanner
    (let ((start index))
      (incf index)
      (loop while (not (eof-p scanner))
            for char = (char source index)
            do (cond
                ((eq #\\ char)
                 (setf index start)
                 (return-from get-identifier (get-complex-identifier scanner)))
                ((< #xD800 (char-code char) #xDFFF)
                 (setf index start)
                 (return-from get-identifier (get-complex-identifier scanner))))
            (if (identifier-part-p char)
                (incf index)
              (return)))
      (subseq start index))))

(defun get-complex-identifier (scanner)
  (with-slots (index source) scanner
    (let* ((char (char source index))
           (id (string char)))
      (incf index)
      (let ((char))
        (when (equal "\\" id)
          (when (not (eq #\u (char source index)))
            (error "Unexpected token"))
          (incf index)
          (when (eq #\{ (char source index))
            (setf index (1+ index)
                  char (scan-unicode-code-point-escape scanner))
            (progn
              (setf char (scan-hex-escape scanner #\u))
              (when (or (null char)
                        (eq #\\ char)
                        (not (identifier-start-p char)))
                (error "Unexpected token"))))
          (setf id (string char))))
      (loop while (not (eof-p scanner))
            for char = (char source index)
            do (when (not (identifier-part-p char))
                 (return))
            (setf id (concatenate 'string id (string char))
                  index (1+ index))
            (when (eq #\\ char)
              (setf id (subseq id 0 (1- (length id))))
              (when (not (eq #\u (char source index)))
                (error "Unexpected token"))
              (incf index)
              (when (eq #\{ (char source index))
                (setf index (1+ index)
                      char (scan-unicode-code-point-escape scanner))
                (progn
                  (setf char (scan-hex-escape scanner #\u))
                  (when (or (null char)
                            (eq #\\ char)
                            (not (identifier-start-p char)))
                    (error "Unexpected token"))))
              (setf id (concatenate 'string id (string char)))))
      id)))

(defun scan-numeric-literal (scanner)
  (with-slots (index source line-number line-start) scanner
    (let ((start index)
          (char (char source index))
          (num ""))
      (when  (not (eq #\. char))
        (setf num (string char))
        (incf index)
        (let ((char (char source index)))
          (when (equal "0" num)
            (cond
             ((or (eq #\x char) (eq #\X char))
              (incf index)
              (return-from scan-numeric-literal (scan-hex-literal scanner start)))
             ((or (eq #\b char) (eq #\B char))
              (incf index)
              (return-from scan-numeric-literal (scan-binary-literal scanner start)))
             ((or (eq #\o char) (eq #\O char))
              (incf index)
              (return-from scan-numeric-literal (scan-octal-literal scanner char start)))
             ((and (octal-digit-p char) (implicit-octal-literal-p scanner))
              (return-from scan-numeric-literal (scan-octal-literal scanner char start)))))
          (loop while (decimal-digit-p (char source index))
                do (setf num (concatenate 'string num (string (char source index))))
                do (incf index)
                finally (setf char (char source index)))))
      (when (eq #\. char)
        (setf num (concatenate 'string num (string (char source index))))
        (incf index)
        (loop while (decimal-digit-p (char source index))
              do (setf num (concatenate 'string num (string (char source index))))
              do (incf index)
              finally (setf char (char source index))))
      (when (or (eq #\e char) (eq #\E char))
        (setf num (concatenate 'string num (string (char source index))))
        (incf index)
        (setf char (char source index))
        (when (or (eq #\+ char) (eq #\- char))
          (setf num (concatenate 'string num (string (char source index))))
          (incf index))
        (if (decimal-digit-p (char source index))
            (loop while (decimal-digit-p (char source index))
                  do (setf num (concatenate 'string num (string (char source index))))
                  do (incf index)
                  finally (setf char (char source index)))
          (error "Unexpected token")))
      (when (identifier-start-p (char source index))
        (error "Unexpected token"))
      (make-token :type :numeric-literal
                  :value num
                  :line-number line-number
                  :line-start line-start
                  :start start
                  :end index))))

(defun scan-hex-literal (scanner start)
  (with-slots (index source line-number line-start) scanner
    (let ((num ""))
      (loop while (not (eof-p scanner))
            do (if (not (hex-digit-p (char source index)))
                 (return)
                 (setf num (concatenate 'string num (string (char source index)))
                       index (1+ index))))
      (when (= 0 (length num))
        (error "Unexpected token"))
      (when (identifier-start-p (char source index))
        (error "Unexpected token"))
      ;; TODO: parse number
      (make-token :type :numeric-literal
                  :value num
                  :line-number line-number
                  :line-start line-start
                  :start start
                  :end index))))

(defun scan-binary-literal (scanner start)
  (with-slots (index source line-number line-start) scanner
    (let ((num "")
          (char))
      (loop while (not (eof-p scanner))
            for char = (char source index)
            do (if (and (not (eq #\0 char))
                          (not (eq #\1 char)))
                 (return)
                 (setf num (concatenate 'string num (string (char source index)))
                       index (1+ index))))
      (when (= 0 (length num))
        (error "Unexpected token"))
      (when (not (eof-p scanner))
        (setf char (char source index))
        (when (or (identifier-start-p char)
                  (decimal-digit-p char))
          (error "Unexpected token")))
      ;; TODO: parse number
      (make-token :type :numeric-literal
                  :value num
                  :line-number line-number
                  :line-start line-start
                  :start start
                  :end index))))

(defun scan-octal-literal (scanner prefix start)
  (with-slots (index source line-number line-start) scanner
    (let ((num "")
          (octal nil))
      (if (octal-digit-p (char prefix 0))
          (setf octal t
                num (concatenate 'string "0" (char source index))
                index (1+ index))
        (incf index))
      (loop while (not (eof-p scanner))
            do (if (octal-digit-p (char source index))
                   (return)
                 (setf num (concatenate 'string num (string (char source index)))
                       index (1+ index))))
      (when (and (not octal)
                 (= 0 (length num)))
        (error "Unexpected token"))
      (when (or (identifier-start-p (char source index))
                (decimal-digit-p (char source index)))
        (error "Unexpected token"))
      ;; TODO: parse number 
      (make-token :type :numeric-literal
                  :value num
                  :octal octal
                  :line-number line-number
                  :line-start line-start
                  :start start
                  :end index))))

;; TODO: check this
(defun implicit-octal-literal-p (scanner)
  (with-slots (index length source) scanner
    (loop for i from (1+ index) upto (1- length)
          for char = (char source i)
          do (cond
              ((or (eq #\8 char)
                   (eq #\9 char)) (return))
              ((not (octal-digit-p char)) (return t)))
          finally (return t))))

;; ttps://tc39.github.io/ecma262/#sec-comments
(defun scan-comments (scanner)
  (with-slots (index source line-number line-start) scanner))

(defun skip-single-line-comment (scanner offset)
  (with-slots (index source line-number line-start) scanner))

(defun skip-multi-line-comment (scanner)
  (with-slots (index source line-number line-start) scanner))

(defun scan-reg-exp (scanner)
  (with-slots (index source line-number line-start) scanner))

(defun scan-reg-exp-body (scanner)
  (with-slots (index source line-number line-start) scanner))

(defun scan-reg-exp-flags (scanner)
  (with-slots (index source line-number line-start) scanner))

(defun test-reg-exp (scanner pattern flags))

;; https://tc39.github.io/ecma262/#sec-future-reserved-words
(defun future-reserved-word-p (id))

(defun strict-mode-reserved-word-p (id))

(defun restricted-word-p (id)
  (or (equal "eval" id) (equal "arguments" id)))

;; https://tc39.github.io/ecma262/#sec-keywords
(defun keyword-p (id)
  (member id '("id" "in" "do"
               "var" "for" "new" "try" "let"
               "this" "else" "case" "void" "with" "enum"
               "while" "break" "catch" "throw" "const" "yield"
               "clsss" "super" "return" "typeof" "delete" "switch"
               "export" "import" "default" "finally" "extends"
               "function" "continue" "debugger" "instanceof")
          :test 'equal))

(defun scan-hex-escape (scanner prefix)
  (with-slots (index source line-number line-start) scanner
    (let ((len (if (eq #\u prefix) 4 2))
          (code 0))
      (loop repeat len
            do (if (and (not (eof-p scanner))
                        (hex-digit-p (char source index)))
                   (setf code (+ (* code 16)
                                 (hex-value (char source index)))
                         index (1+ index))
                 (return)))
      (code-char code))))

(defun hex-value (char)
  (position (char-downcase char) "0123456789abcdef"))

(defun octal-value (char)
  (position char "01234567"))

(defun scan-unicode-code-point-escape (scanner)
  (with-slots (index source line-number line-start) scanner
    (let ((char (char source index))
          (code 0))
      (when (eq #\} char)
        (error "Unexpected token"))
      (loop while (not (eof-p scanner))
            for char = (char source index)
            do (incf index)
            do (if (not (hex-digit-p char))
                   (return)
                 (setf code (+ (* code 16) (hex-value char)))))
      (when (or (> code #x10FFFF) (not (eq #\} char)))
        (error "Unexpected token"))
      (code-char code))))

(defun octal-to-decimal (scanner char)
  (with-slots (index source) scanner
    (let ((octal (not (eq #\0 char)))
          (code (octal-value char)))
      (when (and (not (eof-p scanner))
                 (octal-digit-p (char source index)))
        (setf octal t
              code (+ (* code 8)
                      (octal-value (char source index)))
              index (1+ index))
        (when (and (>= (position char "0123") 0)
                   (not (eof-p scanner))
                   (octal-digit-p (char source index)))
          (setf code (+ (* code 8) (octal-value (char source index)))
                index (1+ index))))
      `(:code ,code :octal ,octal))))

(defun scan-template (scanner)
  (with-slots (index source line-number line-start) scanner))

(defun tolerate-unexpected-token (scanner)
  (with-slots (index source line-number line-start) scanner))