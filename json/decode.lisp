(in-package :json)

(defun accumulator-add-key (key)
  "Add a cons whose CAR is KEY to the end of the list accumulator."
  (setq cl-json::*accumulator-last*
        (setf (cdr cl-json::*accumulator-last*) (cons (cons key nil) nil))))

(defun accumulator-get-object ()
  "Return a CLOS object, using keys and values accumulated so far in
the list accumulator as slot names and values, respectively. Create a OBJECT with slots interned in *JSON-SYMBOLS-PACKAGE*."
  (let ((bindings (cdr cl-json::*accumulator*)))
    (alist-object bindings)))

(defun set-decoder-clos-semantics ()
  "Set the decoder semantics to the following:
  * Strings and Numbers are decoded naturally, reals becoming floats.
  * The literal name true is decoded to T, false and null to NIL.
  * Arrays are decoded to sequences of the type LIST.
  * Objects are decoded to CLOS objects.  Object keys are converted by
the function *JSON-IDENTIFIER-NAME-TO-LISP*.  Otherwise, a
OBJECT is constructed whose slot names are interned in
*JSON-SYMBOLS-PACKAGE*."
  (cl-json::set-custom-vars
   :integer #'cl-json::parse-number
   :real #'cl-json::parse-number
   :boolean #'cl-json::json-boolean-to-lisp
   :beginning-of-array #'cl-json::init-accumulator
   :array-member #'cl-json::accumulator-add
   :end-of-array #'cl-json::accumulator-get-sequence
   :array-type 'list
   :beginning-of-object #'cl-json::init-accumulator
   :object-key #'accumulator-add-key
   :object-value #'cl-json::accumulator-add-value
   :end-of-object #'accumulator-get-object
   :beginning-of-string #'cl-json::init-string-stream-accumulator
   :string-char #'cl-json::string-stream-accumulator-add
   :end-of-string #'cl-json::string-stream-accumulator-get
   :aggregate-scope (union cl-json::*aggregate-scope-variables*
                           '(cl-json::*accumulator* cl-json::*accumulator-last*))
   :object-scope (union cl-json::*object-scope-variables*
                        '(cl-json::*json-array-type*))
   :internal-decoder #'cl-json::decode-json))

(defmacro with-decoder-clos-semantics (&body body)
  "Execute BODY in a dynamic environement where the decoder semantics
is such as set by SET-DECODER-WT-CLOS-SEMANTICS."
  `(cl-json::with-shadowed-custom-vars
     (set-decoder-clos-semantics)
     ,@body))

(defun decode (source &key)
  (with-decoder-clos-semantics
    (typecase source
      (string (cl-json:decode-json-from-string source))
      (pathname (with-open-file (stream source)
                  (cl-json:decode-json-from-source stream)))
      (stream (cl-json:decode-json-from-source source))
      (null nil)
      (t (error "unknown source")))))

(defun decode-json (&rest args)
  (apply #'decode args))
