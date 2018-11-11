;;;; uri-template.lisp
;;;;
;;;; This file is part of the cl-routes library, released under Lisp-LGPL.
;;;; See file COPYING for details.
;;;;
;;;; Author: Moskvitin Andrey <archimag@gmail.com>
;;;;
;;;; Unification and substitutions based on code from AIMA by Peter Norvig

(in-package #:routes)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; unify-template
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass uri-component-template ()
  ((spec :initarg :spec :accessor template-data)))

;;; template-variables

(defgeneric template-variables (tmpl)
  (:method (tmpl)
    nil)
  (:method ((tmpl cons))
    (concatenate 'list
                 (template-variables (car tmpl))
                 (template-variables (cdr tmpl))))
  (:method ((tmpl uri-component-template))
    (template-variables (template-data tmpl))))

;;; variable-template

(defclass variable-template (uri-component-template) ())

(defun make-variable-template (spec)
  (make-instance 'variable-template
                 :spec spec))

(defmethod template-variables ((tmpl variable-template))
  (list (template-data tmpl)))

(defun variable-p (x)
  (typep x 'variable-template))

;;; concat template

(defclass concat-template (uri-component-template) ())

(defun make-concat-template (spec)
  (cond
    ((eql spec nil) nil)
    ((cdr spec) (make-instance 'concat-template :spec spec))
    (t (car spec))))

;;;  template

(defclass wildcard-template (uri-component-template) ())

(defun make-wildcard-template (spec)
  (make-instance 'wildcard-template :spec spec))

(defmethod template-variables ((tmpl wildcard-template))
  (list (template-data tmpl)))

;;; custom template

(defclass custom-template ()
  ((parse-fun :initarg :parse :initform nil :reader variable-parse-fun)))

(defclass custom-variable-template (custom-template variable-template) ())

(defclass custom-wildcard-template (custom-template wildcard-template) ())

;;; or-template

(defclass or-template (uri-component-template) ())

(defun make-or-template (spec)
  (labels ((not-string-position (tmpl &optional (init 0))
             (cond
               ((null tmpl) nil)
               ((and (consp tmpl)
                     (or (stringp (car tmpl))
                         (typep (car tmpl) 'route)))
                (not-string-position (cdr tmpl) (1+ init)))
               (t init)))
           (wildcard-position (tmpl &optional (init 0))
             (cond
               ((null tmpl) nil)
               ((and (consp tmpl)
                     (typep (car tmpl) 'wildcard-template))
                init)
               ((consp tmpl)
                (wildcard-position (cdr tmpl) (1+ init)))
               (t nil)))
           (less (x y
                    &aux
                    (posx (not-string-position x))
                    (posy (not-string-position y)))
             (cond
               ((and (not posx)
                     (not posy))
                t)
               ((and posx
                     (not posy)) 
                nil)
               ((and (not posx)
                     posy)
                t)
               ((or (not (= posx 0))
                    (not (= posy 0)))
                (if (= posx posy)
                    (string< (car x) (car y))
                    (> posx posy)))
               (t (let ((wildcard-pos-x (wildcard-position x))
                        (wildcard-pos-y (wildcard-position y)))
                    (cond
                      ((and (not wildcard-pos-x)
                            (not wildcard-pos-y))
                       (cond
                         ((> (length (template-variables x))
                             (length (template-variables y)))
                          t)
                         ((< (length (template-variables x))
                             (length (template-variables y)))
                          nil)
                         (t (flet ((concat-count (tmpl)
                                     (iter (for item in tmpl)
                                           (when (typep item 'concat-template)
                                             (counting item)))))
                              (> (concat-count x)
                                 (concat-count y)))
                            )))
                      ((and (not wildcard-pos-x)
                            wildcard-pos-y)
                       t)
                      ((and wildcard-pos-x
                            (not wildcard-pos-y))
                       nil)
                      (t t)))))))
    (make-instance 'or-template
                   :spec (sort (copy-seq spec)
                               #'less))))

;;; make-unify-template

(defparameter *make-template-map*
  '((variable . make-variable-template)
    (concat . make-concat-template)
    (or . make-or-template)
    (wildcard . make-wildcard-template)))

(defun make-unify-template (type spec)
  (funcall (cdr (assoc type *make-template-map*))
           spec))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; bindings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconstant +fail+ nil "Indicates unification failure")

(defvar +no-bindings+ '((nil))
  "Indicates unification success, with no variables.")

(defun get-binding (var bindings)
  "Find a (variable . value) pair in a binding list."
  (assoc var bindings))

(defun lookup (var bindings)
  "Get the value part (for var) from a binding list."
  (cdr (get-binding var bindings)))

(defun extend-bindings (var val bindings)
  "Add a (var . value) pair to a binding list."
  (acons var
         val
         (if (eq bindings +no-bindings+)
             nil
             bindings)))

(defun occurs-in-p (var x bindings)
  "Does var occur anywhere inside x?"
  (cond ((eq var x) t)
        ((and (variable-p x) (get-binding x bindings))
         (occurs-in-p var (lookup x bindings) bindings))
        ((consp x) (or (occurs-in-p var (first x) bindings)
                       (occurs-in-p var (rest x) bindings)))
        (t nil)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; uri-template-equal
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric uri-template-equal (a b)
  (:documentation "Return T if template a and template are equal"))

(defmethod uri-template-equal (a b)
  "Default implementation"
  (equal a b))

(defmethod uri-template-equal ((a uri-component-template) (b uri-component-template))
  (and (eql (type-of a)
            (type-of b))
       (uri-template-equal (template-data a)
                           (template-data b))))

(defmethod uri-template-equal ((a wildcard-template) b)
  nil)

(defmethod uri-template-equal (a (b wildcard-template))
  nil)

(defmethod uri-template-equal ((a cons) (b cons))
  (and (uri-template-equal (car a)
                           (car b))
       (uri-template-equal (cdr a) 
                           (cdr b))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; merge-uri-templates
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric merge-uri-templates (a b)
  (:documentation "Merge the templates A and B into one template"))

(defmethod merge-uri-templates (a b)
  (if (uri-template-equal a b)
      a
      (make-unify-template 'or (list a b))))

(defmethod merge-uri-templates ((a cons) (b cons))
  (if (uri-template-equal (car a) (car b))
      (cons (car a)
            (merge-uri-templates (cdr a)
                                 (cdr b)))
      (make-unify-template 'or (list a b))))


(defmethod merge-uri-templates (a (b or-template))
  (merge-uri-templates b a))

(defmethod merge-uri-templates ((a or-template) b)
  (make-unify-template 'or
                       (iter (for part in (template-data a))
                             (with x = nil)
                             (if (not x)
                                 (cond ((uri-template-equal part b) (setq x part))
                                       ((and (consp part)
                                             (consp b)
                                             (uri-template-equal (car part) (car b)))
                                        (setq x (merge-uri-templates part b)))
                                       (t (collect part into left)))
                                 (collect part into right))
                             (finally (return (if x
                                                  (concatenate 'list left (list x) right)
                                                  (cons b left)))))))

(defmethod merge-uri-templates ((a or-template) (b or-template))
  (error "not implemented"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; apply-bindings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric apply-bindings (tmpl bindings))

(defmethod apply-bindings ((tmpl (eql nil)) bindings)
  (declare (ignore bindings))
  nil)

(defmethod apply-bindings ((tmpl string) bindings)
  (declare (ignore bindings))
  tmpl)

(defmethod apply-bindings ((tmpl cons) bindings)
  (let ((first (apply-bindings (car tmpl)
                               bindings))
        (rem (apply-bindings (cdr tmpl) bindings)))
    (if (consp first)
        (concatenate 'list
                     first
                     rem)
        (cons first
              rem))))

(defmethod apply-bindings ((var variable-template) bindings)
  (or (lookup (template-data var) bindings)
      var))

(defmethod apply-bindings ((var wildcard-template) bindings)
  (or (lookup (template-data var) bindings)
      var))

;; optimize required
(defmethod apply-bindings ((tmpl concat-template) bindings)
  (labels ((simplify (spec)
             (cond
               ((= (length spec) 1) spec)
               ((and (stringp (first spec))
                     (stringp (second spec)))
                (simplify (cons (concatenate 'string
                                             (first spec)
                                             (second spec))
                                (cddr spec))))
               (t (cons (car spec)
                        (simplify (cdr spec)))))))
    (let ((spec (simplify (apply-bindings (template-data tmpl)
                                          bindings))))
      (if (cdr spec)
          (make-unify-template 'concat spec)
          (car spec)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; unify/impl
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric unify/impl (a b bindings))

(defun unify (x y &optional (bindings +no-bindings+))
  (if bindings
      (unify/impl x y bindings)))

;;; unify/impl default implementation

(defmethod unify/impl (a b bindings)
  (if (equal a b) bindings +fail+))


;;; unify/impl for wildcard

(defmethod unify/impl ((tmpl wildcard-template) x bindings &aux (var (template-data tmpl)))
  (cond ((get-binding var bindings)
         (unify (lookup var bindings) x bindings))
        ((and (variable-p x) (get-binding x bindings))
         (unify var (lookup x bindings) bindings))
        ((occurs-in-p var x bindings)
         +fail+)
        (t (extend-bindings var x bindings))))


;;; unify/impl for cons

(defmethod unify/impl ((a cons) (b cons) bindings)
  (let ((wildcard-p-1 (typep (first a) 'wildcard-template))
        (wildcard-p-2 (typep (first b) 'wildcard-template)))
    (cond
      ((and wildcard-p-1 wildcard-p-2) nil)
      (wildcard-p-1 (let ((len (- (length b)
                                  (length (cdr a)))))
                      (if (not (minusp len))
                          (unify (cdr a)
                                 (subseq b len)
                                 (unify (first a)
                                        (subseq b 0 len)
                                        bindings)))))
      (wildcard-p-2 (unify/impl b a bindings))
      (t (unify (rest a)
                (rest b)
                (unify (first a)
                       (first b)
                       bindings))))))

;;; unify/impl for strings

(defmethod unify/impl ((a string) (b string) bindings)
  (if (string= a b) bindings +fail+))

;;; unify/impl variable-template

(defmethod unify/impl (a (b variable-template) bindings)
  (unify b a bindings))

(defmethod unify/impl ((tmpl variable-template) x bindings &aux (var (template-data tmpl)))
  (cond ((get-binding var bindings)
         (unify (lookup var bindings) x bindings))
        ((and (variable-p x) (get-binding x bindings))
         (unify var (lookup x bindings) bindings))
        ((occurs-in-p var x bindings)
         +fail+)
        (t (extend-bindings var x bindings))))

;;; unify/impl custom-template

;; (defmethod unify/impl ((tmpl custom-variable-template) x bindings)
;;   (let ((parsed-value (ignore-errors
;;                         (funcall (variable-parse-fun tmpl)
;;                                  x))))
;;     (if parsed-value
;;         (call-next-method tmpl
;;                           parsed-value
;;                           bindings)
;;         +fail+)))

(defmethod unify/impl ((tmpl custom-template) x bindings)
  (let ((parsed-value (ignore-errors
                        (funcall (variable-parse-fun tmpl)
                                 x))))
    (if parsed-value
        (call-next-method tmpl
                          parsed-value
                          bindings)
        +fail+)))



;;; unify/impl concat-template

(defmethod unify/impl (a (b concat-template) bindings)
  (unify b a bindings))

(defmethod unify/impl ((tmpl concat-template) (str string) bindings)
  (flet ((remove-prefix (str prefix)
           (let ((prefix-length (length prefix)))
             (if (and (> (length str) prefix-length)
                      (string= (subseq str 0 prefix-length) prefix))
                 (subseq str prefix-length)))))
    (let* ((spec (template-data tmpl))
           (first-spec (car spec)))
      (typecase first-spec
        (string (let ((no-prefix-str (remove-prefix str first-spec)))
                  (if no-prefix-str
                      (unify no-prefix-str
                             (make-unify-template 'concat (cdr spec))
                             bindings)
                      +fail+)))
        (uri-component-template (let* ((second-spec (second spec))
                                       (pos (search second-spec str)))
                                  (if pos
                                      (unify (make-unify-template 'concat (cddr spec))
                                             (if (> (length str)
                                                    (+ (length second-spec) pos))
                                                 (subseq str (+ (length second-spec) pos)))
                                             (unify first-spec
                                                    (subseq str 0 pos)
                                                    bindings))
                                      +fail+)))
        (t +fail+)))))

;; unify/impl for or-template

(defmethod unify/impl (a (b or-template) bindings)
  (unify b a bindings))

(defmethod unify/impl ((tmpl or-template) x bindings)
  (iter (for variant in (template-data tmpl))
        (let ((result (unify variant x bindings)))
          (finding result such-that result))))

;;; default unify/impl    

(defmethod unify/impl (x y bindings)
  (if (eql x y) bindings +fail+))

