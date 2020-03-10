(in-package :html)

(defclass element (dom:element)
  ((title
    :initarg :title
    :initform nil
    :accessor element-title)
   (lang
    :initarg :lang
    :initform nil
    :accessor element-lang)
   (translate
    :initarg :translate
    :initform nil
    :accessor element-translate)
   (dir
    :initarg :dir
    :initform nil
    :accessor element-dir)
   (style
    :initarg :style
    :initform nil
    :accessor element-style)))

(defmethod print-object ((element element) stream)
  (print-unreadable-object (element stream :type t :identity t)))

(defclass void-element (element) ())

(defclass template-element (element) ())

(defclass raw-text-element (element) ())

(defclass escapable-raw-text-element (element) ())

(defclass foreign-element (element) ())

(defclass normal-element (element) ())

(defclass custom-element (element) ())

(defclass element-constructor (constructor)
  ((element-class
    :initarg :element-class
    :initform nil
    :accessor constructor-element-class)))

(defun segment-attributes-children (form)
  (let* ((body (loop for rest on form by #'cddr
                  unless (and (keywordp (car rest)) (cdr rest))
                  return rest))
         (attributes (ldiff form body)))
    (values attributes body)))

;; TODO: check text
(defmethod construct ((constructor element-constructor) &rest arguments)
  (let* ((element-class (constructor-element-class constructor))
         (element-tag-name (string-downcase (symbol-name element-class)))
         (element (make-instance element-class :tag-name element-tag-name)))
    (multiple-value-bind (attributes children)
        (segment-attributes-children arguments)
      (loop for (_name _value) on attributes by #'cddr
         for name = (string-downcase (symbol-name _name))
         for value = (if (eq _value t)
                         ""
                         (typecase _value
                           (null nil)
                           (string _value)
                           (list (format nil "~{~A~^ ~}" _value))
                           (style (setf (slot-value element 'style) value) nil)
                           (t (format nil "~A" _value))))
         when value
         do (dom:set-attribute element name value))
      (loop for child in (flatten children)
         do
           (typecase child
             (string (dom:append-child element (text child)))
             (element (dom:append-child element child))
             (style (setf (slot-value element 'style)
                          (css:merge-style (slot-value element 'style) child))))))
    element))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmacro define-element (element-symbol superclasses)
    (unless superclasses (setf superclasses '(normal-element)))
    (let* ((constructor-name (format nil "~A-CONSTRUCTOR" (symbol-name element-symbol)))
           (constructor-symbol (intern constructor-name)))
      `(progn
         (defclass ,constructor-symbol (element-constructor)
           ((element-class :initform ',element-symbol)))

         (defparameter ,element-symbol (make-instance ',constructor-symbol))

         (defclass ,element-symbol ,superclasses ())

         (defun ,element-symbol (&rest arguments)
           (apply 'construct ,element-symbol arguments))))))

;; Main root
(define-element html ())

;; Document metadata
(define-element base (void-element))
(define-element head ())
(define-element link (void-element))
(define-element meta (void-element))
(define-element style (raw-text-element))
(define-element title (escapable-raw-text-element))

;; Sectioning root
(define-element body ())

;; Content sectioning
(define-element address ())
(define-element article ())
(define-element aside ())
(define-element footer ())
(define-element header ())
(define-element h1 ())
(define-element h2 ())
(define-element h3 ())
(define-element h4 ())
(define-element h5 ())
(define-element h6 ())
(define-element hgroup ())
(define-element main ())
(define-element nav ())
(define-element section ())

;; Text content
(define-element blockquote ())
(define-element dd ())
(define-element div ())
(define-element dl ())
(define-element dt ())
(define-element figcaption ())
(define-element figure ())
(define-element hr (void-element))
(define-element li ())
(define-element ol ())
(define-element p ())
(define-element pre ())
(define-element ul ())

;; Inline text semantics
(define-element a ())
(define-element abbr ())
(define-element b ())
(define-element bdi ())
(define-element bdo ())
(define-element br (void-element))
(define-element cite ())
(define-element code ())
(define-element data ())
(define-element dfn ())
(define-element em ())
(define-element i ())
(define-element kbd ())
(define-element mark ())
(define-element q ())
(define-element rb ())
(define-element rp ())
(define-element rt ())
(define-element rtc ())
(define-element ruby ())
(define-element s ())
(define-element samp ())
(define-element small ())
(define-element span ())
(define-element strong ())
(define-element sub ())
(define-element sup ())
;; (define-element time ())
(define-element u ())
(define-element var ())
(define-element wbr (void-element))

;; Image and multimedia
(define-element area (void-element))
(define-element audio ())
(define-element img (void-element))
;; (define-element map ())
(define-element track (void-element))
(define-element video ())

;; Embedded content
(define-element embed (void-element))
(define-element iframe ())
(define-element object ())
(define-element param (void-element))
(define-element picture ())
(define-element source (void-element))

;; Scripting
(define-element canvas ())
(define-element noscript ())
(define-element script (raw-text-element))

;; Demarcating edits
(define-element del ())
(define-element ins ())

;; Table content
(define-element caption ())
(define-element col (void-element))
(define-element colgroup ())
(define-element table ())
(define-element tbody ())
(define-element td ())
(define-element tfoot ())
(define-element th ())
(define-element thead ())
(define-element tr ())

;; Forms
(define-element button ())
(define-element datalist ())
(define-element fieldset ())
(define-element form ())
(define-element input (void-element))
(define-element label ())
(define-element legend ())
(define-element meter ())
(define-element optgroup ())
(define-element option ())
(define-element output ())
(define-element progress ())
(define-element select ())
(define-element textarea (escapable-raw-text-element))

;; Interactive elements
(define-element details ())
(define-element diglog ())
(define-element menu ())
(define-element menuitem ())
(define-element summary ())

;; Web Components
(define-element slot ())
(define-element template (template-element))
