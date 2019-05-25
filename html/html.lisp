(in-package :html)

(defclass constructor () ())

(defmethod print-object ((constructor constructor) stream)
  (print-unreadable-object (constructor stream :type t)))

(defgeneric construct (constructor &rest arguments))



(defclass document (rune-dom::document) ())

(defparameter *document* (make-instance 'document))

(defclass document-constructor (constructor) ())

(defparameter document (make-instance 'document-constructor))

(defun document (child)
  (let ((document (rune-dom:create-document child)))
    (change-class document 'document)))



(defclass text (rune-dom::text) ())

(defclass text-constructor (constructor) ())

(defparameter text (make-instance 'text-constructor))

(defun text (data)
  (construct text data))

(defmethod construct ((constructor text-constructor) &rest arguments)
  (let ((data (first arguments)))
    (make-instance 'text
                   :data data
                   :owner *document*)))



(defclass element (rune-dom::element) ())

(defmethod print-object ((element element) stream)
  (print-unreadable-object (element stream :type t :identity t)))

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

(defmethod construct ((constructor element-constructor) &rest arguments)
  (let* ((element-class (constructor-element-class constructor))
         (element-tag-name (string-downcase (symbol-name element-class)))
         (element (dom:create-element *document* element-tag-name)))
    (change-class element element-class)
    (multiple-value-bind (attributes children)
        (segment-attributes-children arguments)
      (loop for (_name _value) on attributes by #'cddr
         for name = (string-downcase (symbol-name _name))
         for value = (typecase _value
                       (string _value)
                       (t (format nil "~A" _value)))
         do (dom:set-attribute element name value))
      (loop for child in (flatten children)
         do (dom:append-child element child)))
    element))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmacro define-element (element-symbol)
    (let* ((constructor-name (format nil "~A-CONSTRUCTOR" (symbol-name element-symbol)))
           (constructor-symbol (intern constructor-name)))
      `(progn
         (defclass ,constructor-symbol (element-constructor)
           ((element-class :initform ',element-symbol)))

         (defparameter ,element-symbol (make-instance ',constructor-symbol))

         (defclass ,element-symbol (element) ())

         (defun ,element-symbol (&rest arguments)
           (apply 'construct ,element-symbol arguments))

         (export ',element-symbol)))))

;; Main root
(define-element html)

;; Document metadata
(define-element base)
(define-element head)
(define-element link)
(define-element meta)
(define-element style)
(define-element title)

;; Sectioning root
(define-element body)

;; Content sectioning
(define-element address)
(define-element article)
(define-element aside)
(define-element footer)
(define-element header)
(define-element h1)
(define-element h2)
(define-element h3)
(define-element h4)
(define-element h5)
(define-element h6)
(define-element hgroup)
(define-element main)
(define-element nav)
(define-element section)

;; Text content
(define-element blockquote)
(define-element dd)
(define-element div)
(define-element dl)
(define-element dt)
(define-element figcaption)
(define-element figure)
(define-element hr)
(define-element li)
(define-element main)
(define-element ol)
(define-element p)
(define-element pre)
(define-element ul)

;; Inline text semantics
(define-element a)
(define-element abbr)
(define-element b)
(define-element bdi)
(define-element bdo)
(define-element br)
(define-element cite)
(define-element code)
(define-element data)
(define-element dfn)
(define-element em)
(define-element i)
(define-element kbd)
(define-element mark)
(define-element q)
(define-element rb)
(define-element rp)
(define-element rt)
(define-element rtc)
(define-element ruby)
(define-element s)
(define-element samp)
(define-element smail)
(define-element span)
(define-element strong)
(define-element sub)
(define-element sup)
(define-element time)
(define-element u)
(define-element var)
(define-element wbr)

;; Image and multimedia
(define-element area)
(define-element audio)
(define-element img)
(define-element map)
(define-element track)
(define-element video)

;; Embedded content
(define-element embed)
(define-element iframe)
(define-element object)
(define-element param)
(define-element picture)
(define-element source)

;; Scripting
(define-element canvas)
(define-element noscript)
(define-element script)

;; Demarcating edits
(define-element del)
(define-element ins)

;; Table content
(define-element caption)
(define-element col)
(define-element colgroup)
(define-element table)
(define-element tbody)
(define-element td)
(define-element tfoot)
(define-element th)
(define-element thead)
(define-element tr)

;; Forms
(define-element button)
(define-element datalist)
(define-element fieldset)
(define-element form)
(define-element input)
(define-element label)
(define-element legend)
(define-element meter)
(define-element optgroup)
(define-element option)
(define-element output)
(define-element progress)
(define-element select)
(define-element textarea)

;; Interactive elements
(define-element details)
(define-element diglog)
(define-element menu)
(define-element menuitem)
(define-element summary)

;; Web Components
(define-element slot)
(define-element template)
