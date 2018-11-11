;;;; core.lisp
;;;;
;;;; This file is part of the cl-routes library, released under Lisp-LGPL.
;;;; See file COPYING for details.
;;;;
;;;; Author: Moskvitin Andrey <archimag@gmail.com>

(defpackage #:routes.test
  (:use #:cl #:lift #:routes #:iter)
  (:export #:run-routes-tests))

(in-package :routes.test)

(deftestsuite routes-test () ())

(defun run-routes-tests (&optional (suite 'routes-test))
  (run-tests :suite suite
             :report-pathname nil))

(defun symbol-form (tmpl)
  (typecase tmpl
    (cons (iter (for i in tmpl)
                (collect (symbol-form i))))
    (custom-variable-template
     (list (type-of tmpl)
           (template-data tmpl)
           (variable-parse-fun tmpl)))
    (uri-component-template
     (cons (type-of tmpl)
           (symbol-form (template-data tmpl))))
    (otherwise tmpl)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; parse-template-test
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftestsuite parse-template-test (routes-test) ())

;;;; simple template

(addtest (parse-template-test)
  parse-simple-template-1
  (ensure-same '("foo")
               (parse-template "foo")))

(addtest (parse-template-test)
  parse-simple-template-2
  (ensure-same '("foo" "bar")
               (parse-template "foo/bar")))

(addtest (parse-template-test)
  parse-simple-template-3
  (ensure-same '("foo" "bar")
               (parse-template "/foo/bar")))

(addtest (parse-template-test)
  parse-simple-template-4
  (ensure-same '("foo" "bar" "baz")
               (parse-template "/foo/bar/baz")))

(addtest (parse-template-test)
  parse-simple-template-5
  (ensure-same '("foo" "bar" "baz" "")
               (parse-template "/foo/bar/baz/")))

(addtest (parse-template-test)
  parse-simple-template-6
  (ensure-same '("foo" "bar" "baz" )
               (parse-template "/ foo/bar /baz ")))

;;;; variable-template

(addtest (parse-template-test)
  parse-variable-template-1
  (ensure-same '((variable-template . :foo))
               (symbol-form (parse-template ":foo"))))

(addtest (parse-template-test)
  parse-variable-template-2
  (ensure-same '("foo" (variable-template . :x) "bar" (variable-template . :y))
               (symbol-form (parse-template "/foo/:x/bar/:y"))))

(addtest (parse-template-test)
  parse-variable-template-3
  (ensure-same '("foo" (variable-template . :x) "bar" (variable-template . :y))
               (symbol-form (parse-template "/foo/:(x)/bar/:y"))))

;;;; wildcard-template

(addtest (parse-template-test)
  parse-wildcard-template-1
  (ensure-same '((wildcard-template . :foo))
               (symbol-form (parse-template "*foo"))))

(addtest (parse-template-test)
  parse-wildcard-template-2
  (ensure-same '("foo" (wildcard-template . :x) "bar")
               (symbol-form (parse-template "/foo/*x/bar"))))

(addtest (parse-template-test)
  parse-wildcard-template-3
  (ensure-same '("foo" "bar" (wildcard-template . :x))
               (symbol-form (parse-template "/foo/bar/*x"))))

;;;; custom-variable-template

(addtest (parse-template-test)
  parse-custom-variable-template-1
  (ensure-same '((custom-variable-template :foo parse-integer))
               (symbol-form (parse-template ":foo" '(:foo parse-integer)))))

(addtest (parse-template-test)
  parse-custom-variable-template-2
  (ensure-same '("foo" "bar" (custom-variable-template :baz parse-integer))
               (symbol-form (parse-template "foo/bar/:baz" '(:baz parse-integer)))))

(addtest (parse-template-test)
  parse-custom-variable-template-3
  (ensure-same '((custom-variable-template :foo parse-integer)
                 (custom-variable-template :bar string-upcase))
               (symbol-form (parse-template ":foo/:bar" '(:foo parse-integer :bar string-upcase)))))

(addtest (parse-template-test)
  parse-custom-variable-template-1
  (ensure-same '((custom-variable-template :foo parse-integer))
               (symbol-form (parse-template ":foo" '(:foo parse-integer)))))

;;;; concat-template

(addtest (parse-template-test)
  parse-concat-template-1
  (ensure-same '((concat-template (variable-template . :x)
                                   "-"
                                   (variable-template . :y)))
               (symbol-form (parse-template ":(x)-:(y)"))))

(addtest (parse-template-test)
  parse-concat-template-2
  (ensure-same '("foo"
                 (concat-template (variable-template . :x) "-" (variable-template . :y))
                 "bar")
               (symbol-form (parse-template "foo/:(x)-:(y)/bar"))))

(addtest (parse-template-test)
  parse-concat-template-3
  (ensure-same '("foo"
                 "bar"
                 (concat-template (variable-template . :x) "-" (variable-template . :y) ".html"))
               (symbol-form (parse-template "foo/bar/:(x)-:(y).html"))))

(addtest (parse-template-test)
  parse-concat-template-4
  (ensure-same '("foo"
                 "bar"
                 (concat-template "chapter-" (variable-template . :x))
                 "")
               (symbol-form (parse-template "foo/bar/chapter-:(x)/"))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

