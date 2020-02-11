(in-package :runes-encoding)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter +buffer-byte+
    #+rune-is-utf-16 '(unsigned-byte 16)
    #-rune-is-utf-16 '(unsigned-byte 32)))

(define-condition encoding-error (simple-error) ())

(defun xerror (fmt &rest args)
  (error 'encoding-error :format-control fmt :format-arguments args))

;;;; ---------------------------------------------------------------------------
;;;; Encoding names
;;;;

(defvar *names* (make-hash-table :test #'eq))

(defun canon-name (string)
  (with-output-to-string (bag)
    (map nil (lambda (ch)
               (cond ((char= ch #\_) (write-char #\- bag))
                     (t (write-char (char-upcase ch) bag))))
         string)))

(defun canon-name-2 (string)
  (with-output-to-string (bag)
    (map nil (lambda (ch)
               (cond ((char= ch #\_))
                     ((char= ch #\-))
                     (t (write-char (char-upcase ch) bag))))
         string)))

(defmethod encoding-names ((encoding symbol))
  (gethash encoding *names*))

(defmethod (setf encoding-names) (new-value (encoding symbol))
  (setf (gethash encoding *names*) new-value))

(defun add-name (encoding name)
  (pushnew (canon-name name) (encoding-names encoding) :test #'string=))

(defun resolve-name (string)
  (cond ((symbolp string)
         string)
        (t
         (setq string (canon-name string))
         (or
          (block nil
            (maphash (lambda (x y) 
                       (when (member string y :test #'string=)
                         (return x)))
                     *names*)
            nil)
          (block nil
            (maphash (lambda (x y) 
                       (when (member string y 
                                     :test #'(lambda (x y)
                                               (string= (canon-name-2 x) 
                                                        (canon-name-2 y))))
                         (return x)))
                     *names*)
            nil)))))

;;;; ---------------------------------------------------------------------------
;;;;  Encodings
;;;;

(defvar *encodings* (make-hash-table :test #'eq))

(defmacro define-encoding (name init-form)
  `(progn
     (setf (gethash ',name *encodings*)
       (list nil (lambda () ,init-form)))
     ',name))

(defun find-encoding (name)
  (let ((x (gethash (resolve-name name) *encodings*)))
    (and x
         (or (first x)
             (setf (first x) (funcall (second x)))))))

(defclass encoding () ())

(defclass simple-8-bit-encoding (encoding)
  ((table :initarg :table)))

(defun make-simple-8-bit-encoding (&key charset)
  (make-instance 'simple-8-bit-encoding
    :table (coerce (to-unicode-table charset) '(simple-array #.+buffer-byte+ (256)))))

;;;;;;;

(defmacro fx-op (op &rest xs) 
  `(the fixnum (,op ,@(mapcar (lambda (x) `(the fixnum ,x)) xs))))
(defmacro fx-pred (op &rest xs) 
  `(,op ,@(mapcar (lambda (x) `(the fixnum ,x)) xs)))

(defmacro %+   (&rest xs) `(fx-op + ,@xs))
(defmacro %-   (&rest xs) `(fx-op - ,@xs))
(defmacro %*   (&rest xs) `(fx-op * ,@xs))
(defmacro %/   (&rest xs) `(fx-op floor ,@xs))
(defmacro %and (&rest xs) `(fx-op logand ,@xs))
(defmacro %ior (&rest xs) `(fx-op logior ,@xs))
(defmacro %xor (&rest xs) `(fx-op logxor ,@xs))
(defmacro %ash (&rest xs) `(fx-op ash ,@xs))
(defmacro %mod (&rest xs) `(fx-op mod ,@xs))

(defmacro %=  (&rest xs)  `(fx-pred = ,@xs))
(defmacro %<= (&rest xs)  `(fx-pred <= ,@xs))
(defmacro %>= (&rest xs)  `(fx-pred >= ,@xs))
(defmacro %<  (&rest xs)  `(fx-pred < ,@xs))
(defmacro %>  (&rest xs)  `(fx-pred > ,@xs))

;;; Decoders

;; The decoders share a common signature:
;;
;; DECODE input input-start input-end
;;        output output-start output-end
;;        eof-p
;; -> first-not-written ; first-not-read
;;
;; These decode functions should decode as much characters off `input'
;; into the `output' as possible and return the indexes to the first
;; not read and first not written element of `input' and `output'
;; respectively.  If there are not enough bytes in `input' to decode a
;; full character, decoding shold be abandomed; the caller has to
;; ensure that the remaining bytes of `input' are passed to the
;; decoder again with more bytes appended.
;;
;; `eof-p' now in turn indicates, if the given input sequence, is all
;; the producer does have and might be used to produce error messages
;; in case of incomplete codes or decided what to do.
;;
;; Decoders are expected to handle the various CR/NL conventions and
;; canonicalize each end of line into a single NL rune (#xA) in good
;; old Lisp tradition.
;;

;; TODO: change this to an encoding class, which then might carry
;; additional state. Stateless encodings could been represented by
;; keywords. e.g.
;;
;;  defmethod DECODE-SEQUENCE ((encoding (eql :utf-8)) ...)
;;

(defmethod decode-sequence ((encoding (eql :utf-16-big-endian))
                            in in-start in-end out out-start out-end eof?)
  ;; -> new wptr, new rptr
  (let ((wptr out-start)
        (rptr in-start))
    (loop
      (when (%= wptr out-end)
        (return))
      (when (>= (%+ rptr 1) in-end)
        (return))
      (let* ((hi (aref in rptr))
	     (lo (aref in (%+ 1 rptr)))
	     (x (logior (ash hi 8) lo)))
        (when (or (eql x #xFFFE) (eql x #xFFFF))
	  (xerror "not a valid code point: #x~X" x))
	(when (<= #xDC00 x #xDFFF)
	  (xerror "unexpected high surrogate: #x~X" x))
	(when (<= #xD800 x #xDBFF)
	  ;; seen low surrogate, look for high surrogate now
	  (when (>= (%+ rptr 3) in-end)
	    (return))
	  (let* ((hi2 (aref in (%+ 2 rptr)))
		 (lo2 (aref in (%+ 3 rptr)))
		 (y (logior (ash hi2 8) lo2)))
	    (unless (<= #xDC00 x #xDFFF)
	      (xerror "expected a high surrogate but found: #x~X" x))
	    #-rune-is-utf-16
	    (progn
	      (setf x (logior (ash (%- x #xd7c0) 10) (%and y #x3FF)))
	      (setf rptr (%+ 2 rptr))))
	  ;; end of surrogate handling
	  )
	(setf (aref out wptr) x)
        (setf rptr (%+ 2 rptr))
        (setf wptr (%+ 1 wptr))))
    (values wptr rptr)))

(defmethod decode-sequence ((encoding (eql :utf-16-little-endian))
                            in in-start in-end out out-start out-end eof?)
  ;; -> new wptr, new rptr
  (let ((wptr out-start)
        (rptr in-start))
    (loop
      (when (%= wptr out-end)
        (return))
      (when (>= (%+ rptr 1) in-end)
        (return))
      (let* ((lo (aref in rptr))
	     (hi (aref in (%+ 1 rptr)))
	     (x (logior (ash hi 8) lo)))
        (when (or (eql x #xFFFE) (eql x #xFFFF))
	  (xerror "not a valid code point: #x~X" x))
	(when (<= #xDC00 x #xDFFF)
	  (xerror "unexpected high surrogate: #x~X" x))
	(when (<= #xD800 x #xDBFF)
	  ;; seen low surrogate, look for high surrogate now
	  (when (>= (%+ rptr 3) in-end)
	    (return))
	  (let* ((lo2 (aref in (%+ 2 rptr)))
		 (hi2 (aref in (%+ 3 rptr)))
		 (y (logior (ash hi2 8) lo2)))
	    (unless (<= #xDC00 x #xDFFF)
	      (xerror "expected a high surrogate but found: #x~X" x))
	    #-rune-is-utf-16
	    (progn
	      (setf x (logior (ash (%- x #xd7c0) 10) (%and y #x3FF)))
	      (setf rptr (%+ 2 rptr))))
	  ;; end of surrogate handling
	  )
	(setf (aref out wptr) x)
        (setf rptr (%+ 2 rptr))
        (setf wptr (%+ 1 wptr))))
    (values wptr rptr)))

(defmethod decode-sequence ((encoding (eql :utf-8))
                            in in-start in-end out out-start out-end eof?)
  (declare (optimize (speed 3) (safety 0))
           (type (simple-array (unsigned-byte 8) (*)) in)
           (type (simple-array #.+buffer-byte+ (*))
		 out)
           (type fixnum in-start in-end out-start out-end))
  (let ((wptr out-start)
        (rptr in-start)
        byte0)
    (macrolet ((put (x)
                 `((lambda (x)
                     (when (or (<= #xD800 x #xDBFF)
			       (<= #xDC00 x #xDFFF))
		       (xerror "surrogate encoded in UTF-8: #x~X." x))
                     (cond ((or (%> x #x10FFFF)
				(eql x #xFFFE)
				(eql x #xFFFF))
                            (xerror "not a valid code point: #x~X" x))
			   #+rune-is-utf-16
		           ((%> x #xFFFF)
                            (setf (aref out (%+ 0 wptr)) (%+ #xD7C0 (ash x -10))
                                  (aref out (%+ 1 wptr)) (%ior #xDC00 (%and x #x3FF)))
                            (setf wptr (%+ wptr 2)))
                           (t
                            (setf (aref out wptr) x)
                            (setf wptr (%+ wptr 1)))))
                   ,x))
               (put1 (x)
                 `(progn
                    (setf (aref out wptr) ,x)
                    (setf wptr (%+ wptr 1)))))
      (loop
        (when (%= (+ wptr 1) out-end) (return))
        (when (%>= rptr in-end) (return))
        (setq byte0 (aref in rptr))
        (cond ((= byte0 #x0D)
               ;; CR handling
               ;; we need to know the following character
               (cond ((>= (%+ rptr 1) in-end)
                      ;; no characters in buffer
                      (cond (eof?
                             ;; at EOF, pass it as NL
                             (put #x0A)
                             (setf rptr (%+ rptr 1)))
                            (t
                             ;; demand more characters
                             (return))))
                     ((= (aref in (%+ rptr 1)) #x0A)
                      ;; we see CR NL, so forget this CR and the next NL will be
                      ;; inserted literally
                      (setf rptr (%+ rptr 1)))
                     (t
                      ;; singleton CR, pass it as NL
                      (put #x0A)
                      (setf rptr (%+ rptr 1)))))
                    
              ((%<= #|#b00000000|# byte0 #b01111111)
               (put1 byte0)
               (setf rptr (%+ rptr 1)))
            
              ((%<= #|#b10000000|# byte0 #b10111111)
               (xerror "Corrupted UTF-8 input (initial byte was #b~8,'0B)" byte0)
               (setf rptr (%+ rptr 1)))
            
              ((%<= #|#b11000000|# byte0 #b11011111)
               (cond ((<= (%+ rptr 2) in-end)
                      (put
                       (dpb (ldb (byte 5 0) byte0) (byte 5 6)
                            (dpb (ldb (byte 6 0) (aref in (%+ rptr 1))) (byte 6 0)
                                 0)))
                      (setf rptr (%+ rptr 2)))
                     (t
                      (return))))
            
              ((%<= #|#b11100000|# byte0 #b11101111)
               (cond ((<= (%+ rptr 3) in-end)
                      (put
                       (dpb (ldb (byte 4 0) byte0) (byte 4 12)
                            (dpb (ldb (byte 6 0) (aref in (%+ 1 rptr))) (byte 6 6)
                                 (dpb (ldb (byte 6 0) (aref in (%+ 2 rptr))) (byte 6 0)
                                      0))))
                      (setf rptr (%+ rptr 3)))
                     (t
                      (return))))
            
              ((%<= #|#b11110000|# byte0 #b11110111)
               (cond ((<= (%+ rptr 4) in-end)
                      (put
                       (dpb (ldb (byte 3 0) byte0) (byte 3 18)
                            (dpb (ldb (byte 6 0) (aref in (%+ 1 rptr))) (byte 6 12)
                                 (dpb (ldb (byte 6 0) (aref in (%+ 2 rptr))) (byte 6 6)
                                      (dpb (ldb (byte 6 0) (aref in (%+ 3 rptr))) (byte 6 0)
                                           0)))))
                      (setf rptr (%+ rptr 4)))
                     (t
                      (return))))
            
              ((%<= #|#b11111000|# byte0 #b11111011)
               (cond ((<= (%+ rptr 5) in-end)
                      (put
                       (dpb (ldb (byte 2 0) byte0) (byte 2 24)
                            (dpb (ldb (byte 6 0) (aref in (%+ 1 rptr))) (byte 6 18)
                                 (dpb (ldb (byte 6 0) (aref in (%+ 2 rptr))) (byte 6 12)
                                      (dpb (ldb (byte 6 0) (aref in (%+ 3 rptr))) (byte 6 6)
                                           (dpb (ldb (byte 6 0) (aref in (%+ 4 rptr))) (byte 6 0)
                                                0))))))
                      (setf rptr (%+ rptr 5)))
                     (t
                      (return))))
            
              ((%<= #|#b11111100|# byte0 #b11111101)
               (cond ((<= (%+ rptr 6) in-end)
                      (put
                       (dpb (ldb (byte 1 0) byte0) (byte 1 30)
                            (dpb (ldb (byte 6 0) (aref in (%+ 1 rptr))) (byte 6 24)
                                 (dpb (ldb (byte 6 0) (aref in (%+ 2 rptr))) (byte 6 18)
                                      (dpb (ldb (byte 6 0) (aref in (%+ 3 rptr))) (byte 6 12)
                                           (dpb (ldb (byte 6 0) (aref in (%+ 4 rptr))) (byte 6 6)
                                                (dpb (ldb (byte 6 0) (aref in (%+ 5 rptr))) (byte 6 0)
                                                     0)))))))
                      (setf rptr (%+ rptr 6)))
                     (t
                      (return))))
            
              (t
               (xerror "Corrupted UTF-8 input (initial byte was #b~8,'0B)" byte0)) ) )) 
    (values wptr rptr))  )

(defmethod encoding-p ((object (eql :utf-16-little-endian))) t)
(defmethod encoding-p ((object (eql :utf-16-big-endian))) t)
(defmethod encoding-p ((object (eql :utf-8))) t)

(defmethod encoding-p ((object encoding)) t)

(defmethod decode-sequence ((encoding simple-8-bit-encoding)
                            in in-start in-end
                            out out-start out-end 
                            eof?)
  (declare (optimize (speed 3) (safety 0))
           (type (simple-array (unsigned-byte 8) (*)) in)
           (type (simple-array #.+buffer-byte+ (*)) out)
           (type fixnum in-start in-end out-start out-end))
  (let ((wptr out-start)
        (rptr in-start)
        (byte 0)
        (table (slot-value encoding 'table))) 
    (declare (type fixnum wptr rptr)
             (type (unsigned-byte 8) byte)
             (type (simple-array #.+buffer-byte+ (*)) table))
    (loop
      (when (%= wptr out-end) (return))
      (when (%>= rptr in-end) (return))
      (setq byte (aref in rptr))
      (cond ((= byte #x0D)
             ;; CR handling
             ;; we need to know the following character
             (cond ((>= (%+ rptr 1) in-end)
                    ;; no characters in buffer
                    (cond (eof?
                           ;; at EOF, pass it as NL
                           (setf (aref out wptr) #x0A)
                           (setf wptr (%+ wptr 1))
                           (setf rptr (%+ rptr 1)))
                          (t
                           ;; demand more characters
                           (return))))
                   ((= (aref in (%+ rptr 1)) #x0A)
                    ;; we see CR NL, so forget this CR and the next NL will be
                    ;; inserted literally
                    (setf rptr (%+ rptr 1)))
                   (t
                    ;; singleton CR, pass it as NL
                    (setf (aref out wptr) #x0A)
                    (setf wptr (%+ wptr 1))
                    (setf rptr (%+ rptr 1)))))
                    
            (t
             (setf (aref out wptr) (aref table byte))
             (setf wptr (%+ wptr 1))
             (setf rptr (%+ rptr 1))) ))
    (values wptr rptr)))

;;;; ---------------------------------------------------------------------------
;;;;  Character sets
;;;;

(defvar *charsets* (make-hash-table :test #'eq))

(defclass 8-bit-charset ()
  ((name :initarg :name)
   (to-unicode-table 
    :initarg :to-unicode-table
    :reader to-unicode-table)))

(defmacro define-8-bit-charset (name &rest codes)
  (assert (= 256 (length codes)))
  `(progn
     (setf (gethash ',name *charsets*)
         (make-instance '8-bit-charset
           :name ',name
           :to-unicode-table
           ',(make-array 256 
                         :element-type '#.+buffer-byte+
                         :initial-contents codes)))
     ',name))

(defun find-charset (name)
  (or (gethash name *charsets*)
      (xerror "There is no character set named ~S." name)))
