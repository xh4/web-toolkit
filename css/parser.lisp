(in-package :css)

(defun whitespace-p (char)
  (char= char #\space))

(define-parser .whitespace ()
  (.satisfies 'whitespace-p))

(define-parser .choose (&rest parsers)
  (lambda (input)
    (loop with parsers = parsers
       with rest = input
       with values = '()
       while parsers
       for (parser rest0 value) = (loop with final-value = nil
                             for parser in parsers
                             do (multiple-value-bind (rest value match-p)
                                    (parse parser rest)
                                  (when match-p
                                    (return `(,parser ,rest ,value)))))
       if value
       do (progn
            (appendf values (list value))
            (setf rest rest0)
            (setf parsers (remove parser parsers)))
       else do (if values
                   (return (values rest values t))
                   (return (values input nil nil)))
       finally (if values
                   (return (values rest values t))
                   (return (values input nil nil))))))

;; (parse (.choose (.digit) (.alpha)) "a1")
;; (parse (.choose (.digit) (.alpha)) "1a")

(define-parser .anyorder (&rest parsers)
  (lambda (input)
    (loop with parsers = parsers
       with rest = input
       with values = '()
       while parsers
       for (parser rest0 value) = (loop with final-value = nil
                                     for parser in parsers
                                     do (multiple-value-bind (rest value match-p)
                                            (parse parser rest)
                                          (when match-p
                                            (return `(,parser ,rest ,value)))))
       if value
       do (progn
            (appendf values (list value))
            (setf rest rest0)
            (setf parsers (remove parser parsers)))
       else do (return (values input nil nil))
       finally (if (and values (null parsers))
                   (return (values rest values t))
                   (return (values input nil nil))))))

;; (parse (.anyorder (.digit) (.alpha)) "a1")
;; (parse (.anyorder (.digit) (.alpha)) "1a")

(defun parse-url (string)
  (setf string (string-trim '(#\Space) string))
  (when (and (string-prefix-p "url(" string)
             (string-suffix-p ")" string))
    (uri:uri (subseq string 4 (1- (cl:length string))))))

(define-parser .oneof (&rest parsers)
  (lambda (input)
    (loop with results = '()
       for parser in parsers
       for result = (multiple-value-list (parse parser input))
       do (appendf results `(,result))
       finally (return
                 (loop with first-matched-result = nil
                    for (nil nil match-p) in results
                    for index from 0
                    when match-p
                    do (if first-matched-result
                           (return (values input nil nil))
                           (setf first-matched-result (nth index results)))
                    finally (if first-matched-result
                                (return (values-list first-matched-result))
                                (return (values input nil nil))))))))

(define-parser .k (keyword)
  (lambda (input)
    (multiple-value-bind (rest value match-p)
        (parse (.s (string-downcase (symbol-name keyword))) input)
      (if match-p
          (values rest keyword t)
          (values input nil nil)))))
