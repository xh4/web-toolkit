(in-package :json)

(defvar *json-output* (make-synonym-stream '*standard-output*)
  "The default output stream for encoding operations.")

(define-condition unencodable-value-error (type-error)
  ((context :accessor unencodable-value-error-context :initarg :context))
  (:documentation
   "Signalled when a datum is passed to ENCODE-JSON (or another
encoder function) which actually cannot be encoded.")
  (:default-initargs :expected-type t)
  (:report
   (lambda (condition stream)
     (with-accessors ((datum type-error-datum)
                      (context unencodable-value-error-context))
         condition
       (format stream
               "Value ~S is not of a type which can be encoded~@[ by ~A~]."
               datum context)))))

(defun unencodable-value-error (value &optional context)
  "Signal an UNENCODABLE-VALUE-ERROR."
  (error 'unencodable-value-error :datum value :context context))

(defmacro with-substitute-printed-representation-restart ((object stream)
                                                          &body body)
  "Establish a SUBSTITUTE-PRINTED-REPRESENTATION restart for OBJECT
and execute BODY."
  `(restart-case (progn ,@body)
     (substitute-printed-representation ()
       (let ((repr (with-output-to-string (s)
                     (write ,object :stream s :escape nil)
                     nil)))
         (write-json-string repr ,stream)))))

(defvar *json-aggregate-context* nil
  "NIL outside of any aggregate environment, 'ARRAY or 'OBJECT within
the respective environments.")

(defvar *json-aggregate-first* t
  "T when the first member of a JSON Object or Array is encoded,
afterwards NIL.")

(defun next-aggregate-member (context stream)
  "Between two members of an Object or Array, print a comma separator."
  (if (not (eq context *json-aggregate-context*))
      (error "Member encoder used ~:[outside any~;in inappropriate~] ~
              aggregate environment"
             *json-aggregate-context*))
  (prog1 *json-aggregate-first*
    (unless *json-aggregate-first*
      (write-char #\, stream))
    (setq *json-aggregate-first* nil)))

(defmacro with-aggregate ((context begin-char end-char
                                   &optional (stream '*json-output*))
                          &body body)
  "Run BODY to encode a JSON aggregate type, delimited by BEGIN-CHAR
and END-CHAR."
  `(let ((*json-aggregate-context* ',context)
         (*json-aggregate-first* t))
     (declare (special *json-aggregate-context* *json-aggregate-first*))
     (write-char ,begin-char ,stream)
     (unwind-protect (progn ,@body)
       (write-char ,end-char ,stream))))

(defmacro with-array ((&optional (stream '*json-output*)) &body body)
  "Open a JSON Array, run BODY, then close the Array.  Inside the BODY,
AS-ARRAY-MEMBER or ENCODE-ARRAY-MEMBER should be called to encode
Members of the Array."
  `(with-aggregate (array #\[ #\] ,stream) ,@body))

(defmacro as-array-member ((&optional (stream '*json-output*))
                           &body body)
  "BODY should be a program which encodes exactly one JSON datum to
STREAM.  AS-ARRAY-MEMBER ensures that the datum is properly formatted
as a Member of an Array, i. e. separated by comma from any preceding
or following Member."
  `(progn
     (next-aggregate-member 'array ,stream)
     ,@body))

(defun encode-array-member (object &optional (stream *json-output*))
  "Encode OBJECT as the next Member of the innermost JSON Array opened
with WITH-ARRAY in the dynamic context.  OBJECT is encoded using the
ENCODE-JSON generic function, so it must be of a type for which an
ENCODE-JSON method is defined."
  (next-aggregate-member 'array stream)
  (encode-json object stream)
  object)

(defun stream-array-member-encoder (stream
                                    &optional (encoder #'encode-json))
  "Return a function which takes an argument and encodes it to STREAM
as a Member of an Array.  The encoding function is taken from the
value of ENCODER (default is #'ENCODE-JSON)."
  (lambda (object)
    (as-array-member (stream)
      (funcall encoder object stream))))

(defmacro with-object ((&optional (stream '*json-output*)) &body body)
  "Open a JSON Object, run BODY, then close the Object.  Inside the BODY,
AS-OBJECT-MEMBER or ENCODE-OBJECT-MEMBER should be called to encode
Members of the Object."
  `(with-aggregate (object #\{ #\} ,stream) ,@body))

(defmacro as-object-member ((key &optional (stream '*json-output*))
                            &body body)
  "BODY should be a program which writes exactly one JSON datum to
STREAM.  AS-OBJECT-MEMBER ensures that the datum is properly formatted
as a Member of an Object, i. e. preceded by the (encoded) KEY and
colon, and separated by comma from any preceding or following Member."
  `(progn
     (next-aggregate-member 'object ,stream)
     (let ((key (encode-json-to-string ,key)))
       (if (char= (aref key 0) #\")
           (progn (write-string key ,stream) nil)
           (encode-json key ,stream)))
     (write-char #\: ,stream)
     ,@body))

(defun encode-object-member (key value
                             &optional (stream *json-output*))
  "Encode KEY and VALUE as a Member pair of the innermost JSON Object
opened with WITH-OBJECT in the dynamic context.  KEY and VALUE are
encoded using the ENCODE-JSON generic function, so they both must be
of a type for which an ENCODE-JSON method is defined.  If KEY does not
encode to a String, its JSON representation (as a string) is encoded
over again."
  (as-object-member (key stream)
    (encode-json value stream))
  value)

(defun stream-object-member-encoder (stream
                                     &optional (encoder #'encode-json))
  "Return a function which takes two arguments and encodes them to
STREAM as a Member of an Object (String : Value pair)."
  (lambda (key value)
    (as-object-member (key stream)
      (funcall encoder value stream))))

(defun encode-json-list (s stream)
  "Write the JSON representation of the list S to STREAM (or to
*JSON-OUTPUT*).  If S is not encodable as a JSON Array, try to encode
it as an Object (per ENCODE-JSON-ALIST)."
  (restart-case
      (handler-bind ((unencodable-value-error
                      (lambda (e)
                        (error e)))
                     (type-error
                      (lambda (e)
                        (declare (ignore e))
                        (unencodable-value-error s 'encode-json))))
        (write-string
         (with-output-to-string (temp)
           (with-array (temp)
             (mapcar (stream-array-member-encoder temp) s)))
         stream)))
  (values))

(defun json-bool (value)
  "Intended for the JSON-EXPLICT-ENCODER. Converts a non-nil value
to a value (:true) that creates a json true value when used in the
explict encoder. Or (:false)."
  (if value
      '(:true)
      '(:false)))

(defun json-or-null (value)
  "Intended for the JSON-EXPLICT-ENCODER. Returns a non-nil value
as itself, or a nil value as a json null-value"
  (or value '(:null)))

(defparameter *json-list-encoder-fn* 'encode-json-list)

(defun use-guessing-encoder ()
  (setf *json-list-encoder-fn* 'encode-json-list-guessing-encoder))

(defun use-explicit-encoder ()
  (setf *json-list-encoder-fn* 'encode-json-list-explicit-encoder))

(defmacro with-local-encoder (&body body)
  `(let (*json-list-encoder-fn*)
     (declare (special *json-list-encoder-fn*))
     ,@body))

(defmacro with-guessing-encoder  (&body body)
  `(with-local-encoder (use-guessing-encoder)
                       ,@body))

(defmacro with-explicit-encoder  (&body body)
  `(with-local-encoder (use-explicit-encoder)
                       ,@body))

(defgeneric encode-json (object &optional stream)
  (:documentation "Write a JSON representation of OBJECT to STREAM and
return NIL."))

(defun encode-json-to-string (object)
  "Return the JSON representation of OBJECT as a string."
  (with-output-to-string (stream)
    (encode-json object stream)))

(defmethod encode-object (anything &optional (stream *json-output*))
  "If OBJECT is not handled by any specialized encoder signal an error
which the user can correct by choosing to encode the string which is
the printed representation of the OBJECT."
  (declare (ignore stream))
  (unencodable-value-error anything 'encode-json))

(defmethod encode-json ((nr number) &optional (stream *json-output*))
  "Write the JSON representation of the number NR to STREAM (or to
*JSON-OUTPUT*)."
  (write-json-number nr stream))

(defmethod encode-json ((s string) &optional (stream *json-output*))
  "Write the JSON representation of the string S to STREAM (or to
*JSON-OUTPUT*)."
  (write-json-string s stream))

(defmethod encode-json ((c character) &optional (stream *json-output*))
  "JSON does not define a character type, we encode characters as Strings."
  (encode-json (string c) stream))

(defmethod encode-json ((s symbol) &optional (stream *json-output*))
  "Write the JSON representation of the symbol S to STREAM (or to
*JSON-OUTPUT*).  If S is boolean, a boolean literal is written.
Otherwise, the name of S is passed to *LISP-IDENTIFIER-NAME-TO-JSON*
and the result is written as String."
  (let ((mapped (car (rassoc s +json-lisp-symbol-tokens+))))
    (if mapped
        (progn (write-string mapped stream) nil)
        (let ((s (funcall *lisp-identifier-name-to-json* (symbol-name s))))
          (write-json-string s stream)))))

(defmethod encode-json ((value null) &optional (stream *json-output*))
  (format stream "null"))

(defmethod encode-json ((value cl:null) &optional (stream *json-output*))
  (format stream "false"))

(defmethod encode-json ((array array) &optional (stream *json-output*))
  (with-slots (value) array
    (if value
        (encode-json value stream)
        (format stream "[]"))))

(defmethod encode-json ((object object) &optional (stream *json-output*))
  (let ((pairs (slot-value object 'pairs)))
    (with-object (stream)
      (loop for (name . value) in pairs
         do (as-object-member (name stream)
              (encode-json value stream))))))

(defmethod encode-json ((s list) &optional (stream *json-output*))
  "Write the JSON representation of the list S to STREAM (or to
*JSON-OUTPUT*), using one of the two rules specified by
first calling USE-GUESSING-ENCODER or USE-EXPLICIT-ENCODER.
The guessing encoder: If S is a list encode S as a JSON Array, if
S is a dotted list encode it as an Object (per ENCODE-JSON-ALIST).
The explicit decoder: If S is a list, the first symbol defines
the encoding:
If (car S) is 'TRUE return a JSON true value.
If (car S) is 'FALSE return a JSON false value.
If (car S) is 'NULL return a JSON null value.
If (car S) is 'JSON princ the strings in (cdr s) to stream
If (car S) is 'LIST or 'ARRAY encode (cdr S) as a a JSON Array.
If (car S) is 'OBJECT encode (cdr S) as A JSON Object,
interpreting (cdr S) either as an A-LIST or a P-LIST."
  (funcall *json-list-encoder-fn* s stream))

(defmethod encode-json ((s sequence) &optional (stream *json-output*))
  "Write the JSON representation (Array) of the sequence S (not an
alist) to STREAM (or to *JSON-OUTPUT*)."
  (with-array (stream)
    (map nil (stream-array-member-encoder stream) s)))

(defmethod encode-json ((h hash-table) &optional (stream *json-output*))
  "Write the JSON representation (Object) of the hash table H to
STREAM (or to *JSON-OUTPUT*)."
  (with-object (stream)
    (maphash (stream-object-member-encoder stream) h)))

(defun write-json-string (s stream)
  "Write a JSON representation (String) of S to STREAM."
  (write-char #\" stream)
  (if (stringp s)
      (write-json-chars s stream)
      (encode-json s stream))
  (write-char #\" stream)
  nil)

(defun write-json-chars (s stream)
  "Write JSON representations (chars or escape sequences) of
characters in string S to STREAM."
  (loop for ch across s
     for code = (char-code ch)
     with special
     if (setq special (car (rassoc ch +json-lisp-escaped-chars+)))
     do (write-char #\\ stream) (write-char special stream)
     else if (< #x1f code #x7f)
     do (write-char ch stream)
     else
     do (let ((special '#.(rassoc-if #'consp +json-lisp-escaped-chars+)))
          (destructuring-bind (esc . (width . radix)) special
            (format stream "\\~C~V,V,'0R" esc radix width code)))))

(eval-when (:compile-toplevel)
  (if (subtypep 'long-float 'single-float)
      ;; only one float type
      (pushnew :json-only-one-float-type *features*)
      ;; else -- we check here only for the case where there are two
      ;; float types, single- and double- --- we don't consider the
      ;; "only single and short" case.  Could be added if necessary.
      (progn
        (when (subtypep 'single-float 'short-float)
          (pushnew :json-single-float-is-subsumed *features*))
        (when (subtypep 'long-float 'double-float)
          (pushnew :json-double-float-is-subsumed *features*)))))

(defun write-json-number (nr stream)
  "Write the JSON representation of the number NR to STREAM."
  (typecase nr
    (integer (format stream "~d" nr))
    (real (let ((*read-default-float-format*
                 (etypecase nr
                   (short-float 'short-float)
                   (rational 'single-float)
                   #-(or json-single-float-is-subsumed
                         json-only-one-float-type)
                   (single-float 'single-float)
                   #-(or json-double-float-is-subsumed
                         json-only-one-float-type)
                   (double-float 'double-float)
                   #-json-only-one-float-type
                   (long-float 'long-float))))
            (format stream "~f" nr)))
    (t (unencodable-value-error nr 'write-json-number))))

(defun encode (value &optional target &key)
  (typecase target
    (cl:null (encode-json-to-string value))
    (stream (encode-json value target))
    ((or string pathname)
     (with-open-file (stream target
                             :direction :output
                             :if-exists :supersede
                             :if-does-not-exist :create)
       (encode-json value stream)))
    (t (error "unknown target"))))
