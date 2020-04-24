(in-package :javascript)

(defclass scanner ()
  ((source
    :initarg :source
    :initform nil)
   (index
    :initform 0)
   (line-number
    :initform 1)
   (line-start
    :initform 0)
   (curly-stack
    :initform nil)
   (length
    :initform nil)
   (module-p
    :initarg :module
    :initform nil)))

(defmethod initialize-instance :after ((scanner scanner) &key)
  (check-type (slot-value scanner 'source) string)
  (with-slots (source length index line-number line-start) scanner
    (setf length (length source)
          index 0
          line-number (if (> length 0) 1 0)
          line-start 0)))

(defun eof-p (scanner)
  (with-slots (index length) scanner
    (>= index length)))

(defun scanner-throw-error (description index line column)
  (error description index line column))

(defun scanner-throw-unexpected-token (scanner &optional message)
  (with-slots (index line-number line-start) scanner
    (scanner-throw-error (or message
                             "Unexpected token at index ~D line ~D column ~D")
                         index line-number (1+ (- index line-start)))))

(defun scanner-tolerate-unexpected-token (scanner)
  (declare (ignore scanner)))

(defun save-state (scanner)
  `(:index ,(slot-value scanner 'index)
    :line-number ,(slot-value scanner 'line-number)
    :line-start ,(slot-value scanner 'line-start)))

(defun restore-state (scanner state)
  (setf (slot-value scanner 'index) (getf state :index)
        (slot-value scanner 'line-number) (getf state :line-number)
        (slot-value scanner 'line-start) (getf state :line-start)))

(defun lex (scanner)
  (with-slots (index line-number line-start source curly-stack) scanner
    (when (eof-p scanner)
      (return-from lex (make-instance 'eof
                                      :line-number line-number
                                      :line-start line-start
                                      :start index
                                      :end index)))
    (let ((char (char-at source index)))
      (cond
       ((identifier-start-p char) (scan-identifier scanner))
       ((or (eq #\( char)
            (eq #\) char)
            (eq #\; char)) (scan-punctuator scanner))
       ((or (eq #\' char)
            (eq #\" char)) (scan-string-literal scanner))
       ((eq #\. char) (if (decimal-digit-p (char-at source (1+ index)))
                          (scan-numeric-literal scanner)
                        (scan-punctuator scanner)))
       ((decimal-digit-p char) (scan-numeric-literal scanner))
       ((or (eq #\` char)
            (and (eq #\} char) (equal "${" (first curly-stack))))
        (scan-template scanner))
       ((<= #xD800 (char-code char) #xDFFF) (if (identifier-start-p char)
                                                (scan-identifier scanner)
                                              (scan-punctuator scanner)))
       (t (scan-punctuator scanner))))))

(defun skip-single-line-comment (scanner offset)
  (declare (ignore offset))
  (with-slots (index source line-number line-start) scanner
    (loop while (not (eof-p scanner))
          for char = (char-at source index)
          do (incf index)
          (when (line-terminator-p char)
            (when (and (eq #\Return char)
                       (eq #\Newline (char-at source index)))
              (incf index))
            (incf line-number)
            (setf line-start index)
            (return)))))

(defun skip-multi-line-comment (scanner)
  (with-slots (index source line-number line-start) scanner
    (loop while (not (eof-p scanner))
          for char = (char-at source index)
          do (cond
              ((line-terminator-p char)
               (when (and (eq #\Return char)
                          (eq #\Newline (char-at source index)))
                 (incf index))
               (incf line-number)
               (incf index)
               (setf line-start index))
              ((eq #\* char)
               (when (eq #\/ (char-at source (1+ index)))
                 (incf index 2)
                 (return))
               (incf index))
              (t (incf index))))
    (scanner-tolerate-unexpected-token scanner)))

;; ttps://tc39.github.io/ecma262/#sec-comments
(defun scan-comments (scanner)
  (with-slots (index source line-number line-start) scanner
    (let ((start (= 0 index)))
      (loop while (not (eof-p scanner))
            for char = (char-at source index)
            do (cond
                ((whitespace-p char) (incf index))
                ((line-terminator-p char)
                 (incf index)
                 (when (and (eq #\Return char)
                            (eq #\Newline (char-at source index)))
                   (incf index))
                 (incf line-number)
                 (setf line-start index
                       start t))
                ((eq #\/ char)
                 (setf char (char-at source (1+ index)))
                 (cond
                  ((eq #\/ char)
                   (incf index 2)
                   (skip-single-line-comment scanner 2)
                   (setf start t))
                  ((eq #\* char)
                   (incf index 2)
                   (skip-multi-line-comment scanner))
                  (t (return))))
                ((and start (eq #\- char))
                 (if (and (eq #\- (char-at source (+ index 1)))
                          (eq #\> (char-at source (+ index 2))))
                     (progn
                       (incf index 3)
                       (skip-single-line-comment scanner 4))
                   (return)))
                (t (return)))))))

;; https://tc39.github.io/ecma262/#sec-future-reserved-words
(defun future-reserved-word-p (id)
  (member id '("enum" "export" "import" "super")
          :test 'equal))

(defun strict-mode-reserved-word-p (id)
  (member id '("implements" "interface" "package" "private"
               "protected" "public" "static" "yield" "let")
          :test 'equal))

(defun restricted-word-p (id)
  (member id '("eval" "arguments") :test 'equal))

;; https://tc39.github.io/ecma262/#sec-keywords
(defun keyword-p (id)
  (member id '("if" "in" "do"
               "var" "for" "new" "try" "let"
               "this" "else" "case" "void" "with" "enum"
               "while" "break" "catch" "throw" "const" "yield"
               "class" "super" "return" "typeof" "delete" "switch"
               "export" "import" "default" "finally" "extends"
               "function" "continue" "debugger" "instanceof")
          :test 'equal))

(defun scan-hex-escape (scanner prefix)
  (with-slots (index source line-number line-start) scanner
    (let ((len (if (eq #\u prefix) 4 2))
          (code 0))
      (loop repeat len
            do (if (and (not (eof-p scanner))
                        (hex-digit-p (char-at source index)))
                   (setf code (+ (* code 16)
                                 (hex-value (char-at source index)))
                         index (1+ index))
                 (return-from scan-hex-escape)))
      (code-char code))))

(defun scan-unicode-code-point-escape (scanner)
  (with-slots (index source line-number line-start) scanner
    (let ((char (char-at source index))
          (code 0))
      (when (eq #\} char)
        (scanner-throw-unexpected-token scanner))
      (loop while (not (eof-p scanner))
            for char = (char-at source index)
            do (incf index)
            do (if (not (hex-digit-p char))
                   (return)
                 (setf code (+ (* code 16) (hex-value char)))))
      (when (or (> code #x10FFFF) (not (eq #\} char)))
        (scanner-throw-unexpected-token scanner))
      (code-char code))))

(defun get-identifier (scanner)
  (with-slots (index source line-number line-start) scanner
    (let ((start index))
      (incf index)
      (loop while (not (eof-p scanner))
            for char = (char-at source index)
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
      (substring source start index))))

(defun get-complex-identifier (scanner)
  (with-slots (index source) scanner
    (let* ((char (char-at source index))
           (id (string char)))
      (incf index)
      (when (eq #\\ char)
          (when (not (eq #\u (char-at source index)))
            (scanner-throw-unexpected-token scanner))
          (incf index)
          (when (eq #\{ (char-at source index))
            (setf index (1+ index)
                  char (scan-unicode-code-point-escape scanner))
            (progn
              (setf char (scan-hex-escape scanner #\u))
              (when (or (null char)
                        (eq #\\ char)
                        (not (identifier-start-p char)))
                (scanner-throw-unexpected-token scanner))))
          (setf id (string char)))
      (loop while (not (eof-p scanner))
            for char = (char-at source index)
            do (when (not (identifier-part-p char))
                 (return))
            (setf id (concatenate 'string id (string char))
                  index (1+ index))
            (when (eq #\\ char)
              (setf id (substring id 0 (1- (length id))))
              (when (not (eq #\u (char-at source index)))
                (scanner-throw-unexpected-token scanner))
              (incf index)
              (if (eq #\{ (char-at source index))
                (setf index (1+ index)
                      char (scan-unicode-code-point-escape scanner))
                (progn
                  (setf char (scan-hex-escape scanner #\u))
                  (when (or (null char)
                            (eq #\\ char)
                            (not (identifier-start-p char)))
                    (scanner-throw-unexpected-token scanner))))
              (setf id (concatenate 'string id (string char)))))
      id)))

(defun octal-to-decimal (scanner char)
  (with-slots (index source) scanner
    (let ((octal (not (eq #\0 char)))
          (code (octal-value char)))
      (when (and (not (eof-p scanner))
                 (octal-digit-p (char-at source index)))
        (setf octal t
              code (+ (* code 8)
                      (octal-value (char-at source index)))
              index (1+ index))
        (when (and (cl:position char "0123")
                   (not (eof-p scanner))
                   (octal-digit-p (char-at source index)))
          (setf code (+ (* code 8) (octal-value (char-at source index)))
                index (1+ index))))
      `(:code ,code :octal ,octal))))

;; https://tc39.github.io/ecma262/#sec-names-and-keywords
(defun scan-identifier (scanner)
  (with-slots (index source line-number line-start) scanner
    (let ((start index)
          (type))
      (let ((id (if (eq #\\ (char-at source index))
                    (get-complex-identifier scanner)
                  (get-identifier scanner))))
        (cond
         ((= 1 (length id)) (setf type 'identifier))
         ((keyword-p id) (setf type 'keyword))
         ((equal "null" id) (setf type 'null-literal))
         ((or (equal "true" id)
              (equal "false" id))
          (setf type 'boolean-literal))
         (t (setf type 'identifier)))
        (when (and (not (eq 'identifier type))
                   (not (= (+ start (length id)) index)))
          (let ((restore index))
            (setf index start)
            (scanner-tolerate-unexpected-token scanner)
            (setf index restore)))
        (case type
          ((identifier keyword)
           (make-instance type
                          :name id
                          :line-number line-number
                          :line-start line-start
                          :start start
                          :end index))
          (t (make-instance type
                            :value id
                            :line-number line-number
                            :line-start line-start
                            :start start
                            :end index)))))))

;; https://tc39.github.io/ecma262/#sec-punctuators
(defun scan-punctuator (scanner)
  (with-slots (index source curly-stack line-number line-start) scanner
    (let ((start index)
          (char (char-at source index))
          (str (string (char-at source index))))
      (cond
       ((or (eq #\( char)
            (eq #\{ char))
        (when (eq #\{ char)
          (push #\{ curly-stack))
        (incf index))
       ((eq #\. char)
        (incf index)
        (when (and (eq #\. (char-at source index))
                   (eq #\. (char-at source (1+ index))))
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
       (t (setf str (substring source index (+ 4 index)))
          (cond
           ((equal ">>>=" str) (incf index 4))
           (t (setf str (substring str 0 3))
              (cond
               ((member str '("===" "!==" ">>>"
                              "<<=" ">>=" "**=")
                        :test 'equal)
                (incf index 3))
               (t (setf str (substring str 0 2))
                  (cond
                   ((member str '("&&" "||" "==" "!="
                                  "+=" "-=" "*=" "/="
                                  "++" "--" "<<" ">>"
                                  "<=""|=" "^=" "%="
                                  "&=" ">=" "=>" "**")
                            :test 'equal)
                    (incf index 2))
                   (t (setf str (substring str 0 1))
                      (when (search str "<>=!+-*%&|^/")
                        (incf index))))))))))
      (when (= index start)
        (scanner-throw-unexpected-token scanner))
      (make-instance 'punctuator
                     :value str
                     :line-number line-number
                     :line-start line-start
                     :start start
                     :end index))))

(defun scan-hex-literal (scanner start)
  (with-slots (index source line-number line-start) scanner
    (let ((num ""))
      (loop while (not (eof-p scanner))
            do (if (not (hex-digit-p (char-at source index)))
                   (return)
                 (setf num (concatenate 'string num (string (char-at source index)))
                       index (1+ index))))
      (when (= 0 (length num))
        (scanner-throw-unexpected-token scanner))
      (when (identifier-start-p (char-at source index))
        (scanner-throw-unexpected-token scanner))
      ;; TODO: parse number
      (make-instance 'numeric-literal
                     :value (concatenate 'string "0x" num)
                     :line-number line-number
                     :line-start line-start
                     :start start
                     :end index))))

(defun scan-binary-literal (scanner start)
  (with-slots (index source line-number line-start) scanner
    (let ((num "")
          (char))
      (loop while (not (eof-p scanner))
            for char = (char-at source index)
            do (if (and (not (eq #\0 char))
                        (not (eq #\1 char)))
                   (return)
                 (setf num (concatenate 'string num (string (char-at source index)))
                       index (1+ index))))
      (when (= 0 (length num))
        (scanner-throw-unexpected-token scanner))
      (when (not (eof-p scanner))
        (setf char (char-at source index))
        (when (or (identifier-start-p char)
                  (decimal-digit-p char))
          (scanner-throw-unexpected-token scanner)))
      ;; TODO: parse number
      (make-instance 'numeric-literal
                     :value (concatenate 'string "0b" num)
                     :line-number line-number
                     :line-start line-start
                     :start start
                     :end index))))

(defun scan-octal-literal (scanner prefix start)
  (with-slots (index source line-number line-start) scanner
    (let ((num "")
          (octal nil))
      (if (octal-digit-p prefix)
          (setf octal t
                num (concatenate 'string "0" (string (char-at source index)))
                index (1+ index))
        (incf index))
      (loop while (not (eof-p scanner))
            do (unless (octal-digit-p (char-at source index))
                 (return))
            (setf num (concatenate 'string num (string (char-at source index)))
                  index (1+ index)))
      (when (and (not octal)
                 (= 0 (length num)))
        (scanner-throw-unexpected-token scanner))
      (when (or (identifier-start-p (char-at source index))
                (decimal-digit-p (char-at source index)))
        (scanner-throw-unexpected-token scanner))
      ;; TODO: parse number
      (make-instance'numeric-literal
                    :value (if (eq #\o prefix)
                               (concatenate 'string "0o" num)
                             num)
                    :octal octal
                    :line-number line-number
                    :line-start line-start
                    :start start
                    :end index))))

;; TODO: check this
(defun implicit-octal-literal-p (scanner)
  (with-slots (index length source) scanner
    (loop for i from (1+ index) upto (1- length)
          for char = (char-at source i)
          do (cond
              ((or (eq #\8 char)
                   (eq #\9 char)) (return))
              ((not (octal-digit-p char)) (return t)))
          finally (return t))))

(defun scan-numeric-literal (scanner)
  (with-slots (index source line-number line-start) scanner
    (let ((start index)
          (char (char-at source index))
          (num ""))
      (when (not (eq #\. char))
        (setf num (string char))
        (incf index)
        (setf char (char-at source index))
        (when (equal "0" num)
          (cond
           ((or (eq #\x char) (eq #\X char))
            (incf index)
            (return-from scan-numeric-literal (scan-hex-literal scanner start)))
           ((or (eq #\b char) (eq #\B char))
            (incf index)
            (return-from scan-numeric-literal (scan-binary-literal scanner start)))
           ((or (eq #\o char) (eq #\O char))
            (return-from scan-numeric-literal (scan-octal-literal scanner char start)))
           ((and char
                 (octal-digit-p char)
                 (implicit-octal-literal-p scanner))
            (return-from scan-numeric-literal (scan-octal-literal scanner char start)))))
        (loop while (decimal-digit-p (char-at source index))
              do (setf num (concatenate 'string num (string (char-at source index))))
              do (incf index)
              finally (setf char (char-at source index))))
      (when (eq #\. char)
        (setf num (concatenate 'string num (string (char-at source index))))
        (incf index)
        (loop while (decimal-digit-p (char-at source index))
              do (setf num (concatenate 'string num (string (char-at source index))))
              do (incf index)
              finally (setf char (char-at source index))))
      (when (or (eq #\e char) (eq #\E char))
        (setf num (concatenate 'string num (string (char-at source index))))
        (incf index)
        (setf char (char-at source index))
        (when (or (eq #\+ char) (eq #\- char))
          (setf num (concatenate 'string num (string (char-at source index))))
          (incf index))
        (if (decimal-digit-p (char-at source index))
            (loop while (decimal-digit-p (char-at source index))
                  do (setf num (concatenate 'string num (string (char-at source index))))
                  do (incf index)
                  finally (setf char (char-at source index)))
          (scanner-throw-unexpected-token scanner)))
      (when (identifier-start-p (char-at source index))
        (scanner-throw-unexpected-token scanner))
      (make-instance 'numeric-literal
                     :value num
                     :line-number line-number
                     :line-start line-start
                     :start start
                     :end index))))

;; https://tc39.github.io/ecma262/#sec-literals-string-literals
(defun scan-string-literal (scanner)
  (with-slots (index source curly-stack line-number line-start) scanner
    (let ((start index)
          (quote (char-at source index)))
      (incf index)
      (loop with octal = nil
            with str = ""
            with ending = nil
            while (not (eof-p scanner))
            for char = (char-at source index)
            do (incf index)
            do (cond
                ((eq quote char) (setf ending t) (loop-finish))
                ((eq #\\ char)
                 (let ((char (char-at source index)))
                   (incf index)
                   (cond
                    ((or (null char)
                         (not (line-terminator-p char)))
                     (cond
                      ((eq #\u char)
                       (cond
                        ((eq #\{ (char-at source index))
                         (incf index)
                         (setf str (concatenate 'string str
                                                (string (scan-unicode-code-point-escape scanner)))))
                        (t (let ((unescaped (scan-hex-escape scanner char)))
                             (unless unescaped
                               (scanner-throw-unexpected-token scanner))
                             (setf str (concatenate 'string str (string unescaped)))))))
                      ((eq #\x char)
                       (let ((unescaped (scan-hex-escape scanner char)))
                         (unless unescaped
                           (scanner-throw-unexpected-token scanner))
                         (setf str (concatenate 'string str (string unescaped)))))
                      ((eq #\n char) (setf str (concatenate 'string str (string #\Newline))))
                      ((eq #\r char) (setf str (concatenate 'string str (string #\Return))))
                      ((eq #\t char) (setf str (concatenate 'string str (string #\Tab))))
                      ((eq #\b char) (setf str (concatenate 'string str (string #\Backspace))))
                      ((eq #\f char) (setf str (concatenate 'string str (string #\Page))))
                      ((eq #\v char) (setf str (concatenate 'string str (string #\VT))))
                      ((or (eq #\8 char)
                           (eq #\9 char))
                       (setf str (concatenate 'string str (string char)))
                       (scanner-tolerate-unexpected-token scanner))
                      (t (cond
                          ((and char (octal-digit-p char))
                           )
                          (t (setf str (concatenate 'string str (string char))))))))
                    (t (incf line-number)
                       (when (and (eq #\Return char)
                                  (eq #\Newline (char-at source index)))
                         (incf index))
                       (setf line-start index)))))
                ((line-terminator-p char) (loop-finish))
                (t (setf str (concatenate 'string str (string char)))))
            finally
            (unless ending
              (setf index start)
              (scanner-throw-unexpected-token scanner))
            (return (make-instance 'string-literal
                                   :value str
                                   :octal octal
                                   :line-number line-number
                                   :line-start line-start
                                   :start start
                                   :end index))))))

;; FIXME: token range incorrect
(defun scan-template (scanner)
  (with-slots (index source line-number line-start curly-stack) scanner
    (let* ((cooked "")
           (terminated)
           (start index)
           (head (eq #\` (char-at source start)))
           (tail)
           (raw-offset 2))
      (incf index)
      (loop while (not (eof-p scanner))
            for char = (char-at source index)
            do (incf index)
            (cond
             ((eq #\` char)
              (setf raw-offset 1
                    tail t
                    terminated t)
              (return))
             ((eq #\$ char)
              (when (eq #\{ (char-at source index))
                (push "${" curly-stack)
                (incf index)
                (setf terminated t)
                (return))
              (setf cooked (concatenate 'string cooked (string char))))
             ((eq #\\ char)
              (setf char (char-at source index)
                    index (1+ index))
              (if (not (line-terminator-p char))
                  (case char
                    (#\n (setf cooked (concatenate 'string cooked (string #\Newline))))
                    (#\r (setf cooked (concatenate 'string cooked (string #\Return))))
                    (#\t (setf cooked (concatenate 'string cooked (string #\Tab))))
                    (#\u (if (eq #\{ (char-at source index))
                             (setf index (1+ index)
                                   cooked (concatenate 'string cooked
                                                       (string (scan-unicode-code-point-escape scanner))))
                           (let ((restore index)
                                 (unescaped (scan-hex-escape scanner char)))
                             (if unescaped
                                 (setf cooked (concatenate 'string cooked
                                                           (string unescaped)))
                               (setf index restore
                                     cooked (concatenate 'string cooked
                                                         (string char)))))))
                    (#\x (let ((unescaped (scan-hex-escape scanner char)))
                           (unless unescaped
                             (scanner-throw-unexpected-token scanner))
                           (setf cooked (concatenate 'string cooked
                                                     (string unescaped)))))
                    (#\b (setf cooked (concatenate 'string cooked (string #\Backspace))))
                    (#\f (setf cooked (concatenate 'string cooked (string #\Page))))
                    (#\v (setf cooked (concatenate 'string cooked (string #\VT))))
                    (t (cond
                        ((eq #\0 char)
                         (when (decimal-digit-p (char-at source index))
                           (scanner-throw-unexpected-token scanner))
                         (setf cooked (concatenate 'string cooked (string #\Null))))
                        ((octal-digit-p char)
                         (scanner-throw-unexpected-token scanner))
                        (t (setf cooked (concatenate 'string cooked (string char)))))))
                (progn
                  (incf line-number)
                  (when (and (eq #\Return char)
                             (eq #\Newline (char-at source index)))
                    (incf index))
                  (setf line-start index))))
             ((line-terminator-p char)
              (incf line-number)
              (when (and (eq #\Return char)
                         (eq #\Newline (char-at source index)))
                (incf index))
              (setf line-start index)
              (setf cooked (concatenate 'string cooked (string #\Newline))))
             (t (setf cooked (concatenate 'string cooked (string char))))))
      (unless terminated
        (scanner-throw-unexpected-token scanner))
      (unless head
        (pop curly-stack))
      (make-instance 'template
                     :value (substring source (1+ start) (- index raw-offset))
                     :raw (substring source (1+ start) (- index raw-offset))
                     :cooked cooked
                     :tail tail
                     :line-number line-number
                     :line-start line-start
                     :start start
                     :end index))))

(defun test-reg-exp (scanner pattern flags)
  (declare (ignore scanner))
  `(:pattern ,pattern :flags ,flags))

(defun scan-reg-exp-body (scanner)
  (with-slots (index source line-number line-start) scanner
    (let ((char (char-at source index)))
      (assert (eq #\/ char))
      (let ((str (string (char-at source index)))
            (class-marker)
            (terminated))
        (incf index)
        (loop while (not (eof-p scanner))
              for char = (char-at source index)
              do (setf str (concatenate 'string str (string (char-at source index)))
                       index (1+ index))
              (cond
               ((eq #\\ char)
                (setf char (char-at source index)
                      index (1+ index))
                (when (line-terminator-p char)
                  (error "Unterminated regexp"))
                (setf str (concatenate 'string str (string char))))
               ((line-terminator-p char)
                (error "Unterminated regexp"))
               (class-marker
                (when (eq #\] char)
                  (setf class-marker nil)))
               (t (cond
                   ((eq #\/ char) (setf terminated t) (return))
                   ((eq #\[ char) (setf class-marker t))))))
        (unless terminated
          (error "Unterminated regexp"))
        (substring str 1 (1- (length str)))))))

(defun scan-reg-exp-flags (scanner)
  (with-slots (index source line-number line-start) scanner
    (let ((str "")
          (flags ""))
      (loop while (not (eof-p scanner))
            for char = (char-at source index)
            do (when (not (identifier-part-p char))
                 (return))
            (incf index)
            (if (and (eq #\\ char) (not (eof-p scanner)))
                (let ((char (char-at source index)))
                  (if (eq #\u char)
                      (progn
                        (incf index)
                        (let ((restore index))
                          (let ((char (scan-hex-escape scanner #\u)))
                            (if char
                                (progn
                                  (setf flags (concatenate 'string flags (string char))
                                        str (concatenate 'string str "\\u"))
                                  (loop for i from restore upto (1- index)
                                        do (setf str (concatenate 'string str
                                                                  (string (char-at source restore))))))
                              (progn
                                (setf index restore
                                      flags (concatenate 'string flags (string #\u))
                                      str (concatenate 'string str "\\u"))))
                            (scanner-tolerate-unexpected-token scanner))))
                    (progn
                      (setf str (concatenate 'string str "\\"))
                      (scanner-tolerate-unexpected-token scanner))))
              (setf flags (concatenate 'string flags (string char))
                    str (concatenate 'string str (string char)))))
      flags)))

(defun scan-reg-exp (scanner)
  (with-slots (index line-number line-start) scanner
    (let ((start index))
      (let ((pattern (scan-reg-exp-body scanner))
            (flags (scan-reg-exp-flags scanner)))
        (make-instance 'regular-expression-literal
                         :pattern pattern
                         :flags flags
                         :line-number line-number
                         :line-start line-start
                         :start start
                         :end index)))))
