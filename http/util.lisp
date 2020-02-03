(in-package :http)

(defvar +crlf+
  (make-array 2 :element-type '(unsigned-byte 8)
              :initial-contents (mapcar 'char-code '(#\Return #\Linefeed)))
  "A 2-element array consisting of the character codes for a CRLF sequence.")

(defvar +day-names+
  #("Mon" "Tue" "Wed" "Thu" "Fri" "Sat" "Sun")
  "The three-character names of the seven days of the week - needed
for cookie date format.")

(defvar +month-names+
  #("Jan" "Feb" "Mar" "Apr" "May" "Jun" "Jul" "Aug" "Sep" "Oct" "Nov" "Dec")
  "The three-character names of the twelve months - needed for cookie
date format.")

(defun rfc-1123-date (&optional (time (get-universal-time)))
  "Generates a time string according to RFC 1123. Default is current time.
This can be used to send a 'Last-Modified' header - see
HANDLE-IF-MODIFIED-SINCE."
  (multiple-value-bind
        (second minute hour date month year day-of-week)
      (decode-universal-time time 0)
    (format nil "~A, ~2,'0d ~A ~4d ~2,'0d:~2,'0d:~2,'0d GMT"
            (svref +day-names+ day-of-week)
            date
            (svref +month-names+ (1- month))
            year
            hour
            minute
            second)))

(defun read-char (stream &optional (eof-error-p t) eof-value)
  (let ((char-code (read-byte stream eof-error-p eof-value)))
    (and char-code
         (code-char char-code))))

(defun read-line (stream)
  (with-output-to-string (line)
    (loop for char-seen-p = nil then t
       for char = (read-char stream nil)
       for is-cr-p = (and char (char= char #\Return))
       until (or (null char)
                 is-cr-p)
       do (write-char char line)
       finally (cond ((and (not char-seen-p)
                           (null char))
                      (return-from read-line nil))
                     (is-cr-p
                      (unless (eql (read-char stream) #\Linefeed)
                        ;; raise error here?
                        ))))))

(defmacro replace-class-option (name key &rest values)
  (with-gensyms (pos/s)
    `(if-let ((,pos/s (position ,key ,name :key 'first)))
       (setf (nth ,pos/s ,name) (list ,key ,@values))
       (appendf ,name (list (list ,key ,@values))))))

(defmacro rewrite-class-option (name key &rest values)
  (let ((option (cons key values)))
    (with-gensyms (pos/s)
      `(if-let ((,pos/s (position ,key ,name :key 'first)))
         (setf (nth ,pos/s ,name) ',option)
         (appendf ,name (list ',option))))))
