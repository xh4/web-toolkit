;;;; routes.lisp
;;;;
;;;; This file is part of the cl-routes library, released under Lisp-LGPL.
;;;; See file COPYING for details.
;;;;
;;;; Author: Moskvitin Andrey <archimag@gmail.com>


(in-package #:routes)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; macros
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro intern-keyword (name)
  `(intern (
	    #+allegro
	    ,(ecase excl:*current-case-mode*
	       (:case-sensitive-lower 'string-downcase)
	       (:case-insensitive-upper 'string-upcase))
	    #-allegro
	    ,'string-upcase
	    ,name) :keyword))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; interface
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass base-route () ())

(defgeneric route-template (route)
  (:documentation "Template URI of ROUTE"))

(defgeneric route-name (route)
  (:documentation "Route name")
  (:method (route)
    "ROUTE"))

(defgeneric route-check-conditions (route bindings)
  (:documentation "Used for check the additional conditions when comparing the ROUTE with the request.")
  (:method (route bindings)
    t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; common route
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass route (base-route)
  ((template :initarg :template :reader route-template)))

;;; make-route

(defun parse-path (str &optional varspecs)
  (flet ((mkvar (name &optional wildcard)
           (let* ((spec (intern-keyword name))
                  (parse-fun (getf varspecs spec )))
             (if parse-fun
                 (make-instance (if wildcard
                                    'custom-wildcard-template
                                    'custom-variable-template)
                                :spec spec
                                :parse parse-fun)
                 (make-unify-template (if wildcard
                                          'wildcard
                                          'variable)
                                      spec)))))
    (if (> (length str) 0)
        (if (char= (char str 0)
                   #\*)
            (list (mkvar (intern-keyword (subseq str 1)) t))
            (let ((pos (position #\: str)))
              (if pos
                  (let ((rest (if (char= (elt str (1+ pos)) #\()
                                  (let ((end (position #\) str :start (1+ pos))))
                                    (if end
                                        (cons (mkvar (subseq str (+ pos 2) end))
                                              (if (< (1+ end) (length str))
                                                  (handler-case
                                                      (parse-path (subseq str (1+ end)) varspecs)
                                                    (condition () (error "Bad format of the ~S" str)))))
                                        (error "Bad format of the ~S" str)))
                                  (list (mkvar (subseq str (1+ pos)))))))
                    (if (> pos 0)
                        (cons (subseq str 0 pos) rest)
                        rest))
                  (list str))))
        '(""))))

(defun parse-template (tmpl &optional varspecs)
  (iter (for path in (split-sequence #\/ (string-left-trim "/" tmpl)))
        (collect (let ((spec (routes::parse-path (string-trim " " path) varspecs)))
                   (if (cdr spec)
                       (make-unify-template 'concat spec)
                       (car spec))))))

(defun make-route (tmpl &optional varspecs)
  (make-instance 'route
                 :template (parse-template tmpl varspecs)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; proxy route
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass proxy-route (base-route)
  ((target :initarg :target :reader proxy-route-target)))

(defmethod route-template ((proxy proxy-route))
  (route-template (proxy-route-target proxy)))

(defmethod route-name ((proxy proxy-route))
  (route-name (proxy-route-target proxy)))

(defmethod route-check-conditions ((proxy proxy-route) bindings)
  (route-check-conditions (proxy-route-target proxy) bindings))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; route-variables
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun route-variables (route)
  (template-variables (route-template route)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; unify/impl for route
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod unify/impl ((b base-route) (a variable-template) bindings)
  (unify a b bindings))
  
(defmethod unify/impl ((a variable-template) (route base-route) bindings)
  (if (route-check-conditions route bindings)
      (call-next-method a
                        route
                        bindings)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; generate-url
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun generate-url (route bindings)
  (format nil
          "~{~A~^/~}"
          (apply-bindings (route-template route)
                          bindings)))
  
