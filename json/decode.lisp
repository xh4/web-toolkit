(in-package :wt.json)

(defun accumulator-get-object ()
  "Return a CLOS object, using keys and values accumulated so far in
the list accumulator as slot names and values, respectively. Create a OBJECT with slots interned in *JSON-SYMBOLS-PACKAGE*."
  (let ((bindings (cdr json::*accumulator*)))
    (let ((object (make-instance 'fluid-object)))
      (loop for (key . value) in bindings
         do (setf (slot-value object key) value))
      object)))

(defun set-decoder-clos-semantics ()
  "Set the decoder semantics to the following:
  * Strings and Numbers are decoded naturally, reals becoming floats.
  * The literal name true is decoded to T, false and null to NIL.
  * Arrays are decoded to sequences of the type LIST.
  * Objects are decoded to CLOS objects.  Object keys are converted by
the function *JSON-IDENTIFIER-NAME-TO-LISP*.  Otherwise, a
OBJECT is constructed whose slot names are interned in
*JSON-SYMBOLS-PACKAGE*."
  (json::set-custom-vars
   :integer #'json::parse-number
   :real #'json::parse-number
   :boolean #'json::json-boolean-to-lisp
   :beginning-of-array #'json::init-accumulator
   :array-member #'json::accumulator-add
   :end-of-array #'json::accumulator-get-sequence
   :array-type 'list
   :beginning-of-object #'json::init-accumulator
   :object-key #'json::accumulator-add-key
   :object-value #'json::accumulator-add-value
   :end-of-object #'accumulator-get-object
   :beginning-of-string #'json::init-string-stream-accumulator
   :string-char #'json::string-stream-accumulator-add
   :end-of-string #'json::string-stream-accumulator-get
   :aggregate-scope (union json::*aggregate-scope-variables*
                           '(json::*accumulator* json::*accumulator-last*))
   :object-scope (union json::*object-scope-variables*
                        '(json::*json-array-type*))
   :internal-decoder #'json::decode-json))

(defmacro with-decoder-clos-semantics (&body body)
  "Execute BODY in a dynamic environement where the decoder semantics
is such as set by SET-DECODER-WT-CLOS-SEMANTICS."
  `(json::with-shadowed-custom-vars
     (set-decoder-clos-semantics)
     ,@body))

(defun decode-json (source &key)
  (with-decoder-clos-semantics
    (typecase source
      (string (json:decode-json-from-string source))
      (pathname (with-open-file (stream source)
                  (json:decode-json-from-source stream)))
      (stream (json:decode-json-from-source source))
      (t (error "unknown source")))))
