(in-package :uri)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar no-break-space
    #-(or abcl lispworks) #\No-break_space
    #+(or abcl lispworks) (code-char 160)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar whitespace
    #.(remove-duplicates
       (coerce (list #\Space #\Tab #\Linefeed #\Return #\Newline #\Page
                     #\Vt                 ;Vertical tab.
                     no-break-space)
               'string))))

(declaim (inline whitespacep))
(defun whitespacep (char)
  (case (char-code char)
    (#.(map 'list #'char-code whitespace) t)))

(declaim (inline blankp))
(defun blankp (seq)
  "SEQ is either empty, or consists entirely of characters that
satisfy `whitespacep'."
  (every #'whitespacep seq))

(defgeneric percent-encode (string &key reserve)
  (:method ((string string) &key reserve)
    (let ((pct-encoded (mapcar
                        (lambda (char)
                          (if (and reserve (funcall reserve char))
                              (string char)
                              (percent-encode char)))
                        (coerce string 'list))))
      (apply #'concatenate 'string pct-encoded)))
  (:method ((char character) &key reserve)
    (declare (ignore reserve))
    (let ((octets (babel:string-to-octets (string char))))
      (let ((pct-encoded (mapcar
                          (lambda (n) (format nil "%~X" n))
                          (coerce octets 'list))))
        (apply #'concatenate 'string pct-encoded))))
  (:method ((o null) &key reserve) (declare (ignore reserve))))

(defgeneric percent-decode (object)
  (:method ((string string))
    (let ((buffer (make-array 10 :element-type '(unsigned-byte 8)
                              :adjustable t :fill-pointer 0)))
      (do ((i 0 (1+ i)))
          ((>= i (length string)) buffer)
        (let* ((x (char string i)))
          (if (char= #\% x)
              (if (>= (length string) (+ i 3))
                  (let ((hexdigs (subseq string (+ i 1) (+ i 3))))
                    (let ((value (parse-integer hexdigs :radix 16)))
                      (vector-push-extend value buffer)
                      (incf i 2)))
                  (error "Not enough"))
              (if (<= (char-code x) 255)
                  (vector-push-extend (char-code x) buffer)
                  (let ((octets (babel:string-to-octets (string x))))
                    (loop for octet across octets
                       do (vector-push-extend octet buffer)))))))
      (babel:octets-to-string buffer :encoding :utf-8)))
  (:method ((o null))))
