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
