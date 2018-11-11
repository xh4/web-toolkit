(in-package :wt.html)

;; 参考
;;
;; 12.1 Writing HTML documents
;; https://html.spec.whatwg.org/multipage/syntax.html#writing
;;
;; 12.3 Serializing HTML fragments
;; https://html.spec.whatwg.org/multipage/parsing.html#serialising-html-fragments

(defvar *html-output-stream* *standard-output*)

(defparameter *html-indent* 0)

(defparameter *html-stack* nil)

(defparameter *html-queue* nil)

(defparameter *html-indent-size* 2)

(defun element-form-p (form)
  (and (consp form) (keywordp (car form))))

(defun segment-element-form (form)
  (let* ((tag (car form))
         (body (loop for rest on (cdr form) by #'cddr
                  unless (and (keywordp (car rest)) (cdr rest))
                  return rest))
         (attributes (ldiff (cdr form) body)))
    (values tag attributes body)))

(defun format-attributes (attributes)
  (let ((seen '()))
    (labels ((seen? (name)
               (declare (optimize speed)
                        (symbol name))
               (prog1 (member name seen)
                 (push name seen)))
             (prepare-attribute-value (value)
               (cond ((keywordp value) (string-downcase value))
                     ((stringp value) value)
                     ((eql value t) "")
                     (t (princ-to-string value))))
             (format-attribute (name value)
               (unless (or (null value) (eq :null value) (seen? name))
                 (let ((value (prepare-attribute-value value)))
                   (format *html-output-stream* " ~(~a~)=\"~a\"" name
                           (escape-common-double-quoted-attribute-value value))))))
      (declare (inline seen?))
      (doplist (name value attributes)
        (format-attribute name value)))))

(defun format-newline ()
  (write-char #\Newline *html-output-stream*))

(defun format-indent (&optional (addition 0))
  (loop for i from 1 upto (+ *html-indent* addition)
     do (loop for j from 1 upto *html-indent-size*
           do (write-char #\Space *html-output-stream*))))

(defun prepare-open-tag (tag)
  (declare (ignore tag))
  (when (not (emptyp *html-queue*))
    (format-newline))
  (format-indent))

(defun format-open-tag (tag attributes)
  (case tag
    (:!doctype
     (format *html-output-stream* "<!DOCTYPE html>"))
    (t
     (format *html-output-stream* "<~(~A~)" tag)
     (format-attributes attributes)
     (write-char #\> *html-output-stream*))))

(defun enter-element (tag attributes)
  (prepare-open-tag tag)
  (format-open-tag tag attributes)

  (when (not (eq tag :!doctype))
    (incf *html-indent*))
  (push tag *html-queue*)
  (push tag *html-stack*))

(defun prepare-close-tag (tag)
  (when (and (not (eq tag :!doctype))
             (not (member tag *void-elements*))
             (not (member tag *raw-text-elements*))
             (not (member tag *escapable-raw-text-elements*)))
    (format-newline)
    (format-indent -1)))

(defun format-close-tag (tag)
  (when (and (not (eq tag :!doctype))
             (not (member tag *void-elements*)))
    (format *html-output-stream* "</~(~A~)>" tag)))

(defun leave-element (tag)
  (prepare-close-tag tag)
  (format-close-tag tag)
  (decf *html-indent*)
  (pop *html-stack*))

(defun fill-text-content (form)
  (when (not (member (car *html-stack*) *preformatted-elements*))
    (format-newline)
    (format-indent))
  (let ((value (cond
                 ((stringp form) form)
                 ((or (null form) (eq :null form)) "")
                 (t (princ-to-string form)))))
    (cond
      ((member (car *html-stack*) *raw-text-elements*)
       (escape-raw-text value *html-output-stream*))
      ((member (car *html-stack*) *escapable-raw-text-elements*)
       (escape-escapable-raw-text value *html-output-stream*))
      (t (escape-element-content value *html-output-stream*)))))

(defvar *enter-element-hook* nil)
(defvar *leave-element-hook* nil)
(defvar *text-content-hook* nil)

(defun traverse-html (form)
  (when (null form) (return-from traverse-html nil))
  (let ((stack nil))
    (push form stack)
    (loop for form = (pop stack)
       while form
       do
         (cond
           ((atom form)
            (when *text-content-hook*
              (funcall *text-content-hook* form)))
           ((and (consp form) (eq (car form) :leave))
            (when *leave-element-hook*
              (funcall *leave-element-hook* (cdr form))))
           ((element-form-p form)
            (multiple-value-bind (tag attributes body)
                (segment-element-form form)
              (when *enter-element-hook*
                (funcall *enter-element-hook* tag attributes))
              (push `(:leave . ,tag) stack)
              (loop for form in (reverse body)
                 when form
                 do (push form stack))))
           (t (loop for form in (reverse form)
                 when form
                 do (push form stack)))))))

(defun html (stream form)
  (let ((*html-indent* 0)
        (*html-stack* nil)
        (*html-queue* nil)
        (*html-output-stream* stream)
        (*enter-element-hook* #'enter-element)
        (*leave-element-hook* #'leave-element)
        (*text-content-hook* #'fill-text-content))
    (traverse-html form)))

(defun html-string (form)
  (with-output-to-string (stream)
    (html stream form)))
