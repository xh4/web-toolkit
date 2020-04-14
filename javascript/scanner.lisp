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

(defun skip-single-line-comment (scanner offset))

(defun skip-multi-line-comment (scanner))

(defun scan-comments (scanner))

(defun future-reserved-word-p (id))

(defun strict-mode-reserved-word-p (id))

(defun restricted-word-p (id))

(defun keyword-p (id))

(defun code-point-at (scanner i))

(defun scan-hex-escape (scanner prefix))

(defun scan-unicode-code-point-escape (scanner))

(defun get-identifier (scanner))

(defun get-complex-identifier (scanner))

(defun octal-to-decimal (scanner ch))

(defun scan-identifier (scanner))

(defun scan-hex-literal (scanner start))

(defun scan-binary-literal (scanner start))

(defun scan-octal-literal(scanner prefix start))

(defun implicit-octal-literal-p (scanner))

(defun scan-numeric-literal (scanner))

(defun scan-template (scanner))

(defun test-reg-exp (scanner pattern flags))

(defun scan-reg-exp-body (scanner))

(defun scan-reg-exp-flags (scanner))

(defun scan-reg-exp (scanner))

(defun tolerate-unexpected-token (scanner))