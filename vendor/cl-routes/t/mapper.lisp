;;;; mapper.lisp
;;;;
;;;; This file is part of the cl-routes library, released under Lisp-LGPL.
;;;; See file COPYING for details.
;;;;
;;;; Author: Moskvitin Andrey <archimag@gmail.com>

(in-package :routes.test)

(defvar *test-mapper*)

(defclass test-route (route)
  ((condition :initarg :condition :initform nil)
   (name :initarg :name :reader route-name)))

(defmethod route-check-conditions ((route test-route) bindings)
  (let ((fun (slot-value route 'condition)))
    (if fun
        (funcall fun bindings)
        (call-next-method))))

(defun make-test-route (name tmpl &key condition varspecs)
  (connect *test-mapper*
           (make-instance 'test-route
                          :name name
                          :template (parse-template tmpl varspecs)
                          :condition condition)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; mapper-test-suite
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftestsuite mapper-test-suite (routes-test) ()
  (:run-setup :once-per-test-case)
  (:dynamic-variables *test-mapper*)
  (:setup (setf *test-mapper*
                (make-instance 'mapper))
          (make-test-route "wildcard-1" "*foo")
          (make-test-route "wildcard-1" "*foo/bar")
          (make-test-route "main" "/")
          (make-test-route "articles" "articles/")

          (make-test-route "article-concat-3" "articles/:(author)-:(id)-:(part).html")
          (make-test-route "article-concat-1" "articles/:(article-id).html")
          (make-test-route "article-concat-2" "articles/:(author)-:(id).html")          

          (make-test-route "article" "articles/:article-id")
                    
          (make-test-route "article3"
                           "articles/:category/:id"
                           :varspecs (list :category #'parse-integer
                                           :id #'parse-integer))
          (make-test-route "article4"
                           "articles/:author/:id"
                           :varspecs (list :author #'(lambda (item)
                                                       (if (not (parse-integer item :junk-allowed t))
                                                           item))
                                           :id #'parse-integer))          

          (make-test-route "article-wildcard-1" "articles/*path")
          (make-test-route "article-wildcard-2" "articles/check/*path"
                           :varspecs (list :path #'(lambda (path)
                                                     (iter (for item in path)
                                                           (collect (parse-integer item))))))
          ;;(make-test-route "article-wildcard-3" "articles/check/*path")

          (make-test-route "double-1" ":foo/:bar")
          (make-test-route "triple-1" ":foo/:bar/:baz")

          (make-test-route "person" "person:(id)")
          (make-test-route "impossible" "impossible:(siid)"))
  (:teardown (reset-mapper *test-mapper*)))
                                             

(addtest (mapper-test-suite)
  main
  (ensure-same "main"
               (route-name (match *test-mapper* #U"/"))))

(addtest (mapper-test-suite)
  articles
  (ensure-same "articles"
               (route-name (match *test-mapper* "articles/"))))

(addtest (mapper-test-suite)
  article-1
  (ensure-same '("article" (:article-id . "hello-world"))
               (multiple-value-bind (route bindings) (match *test-mapper* "articles/hello-world")
                 (cons (route-name route)
                       bindings))))

(addtest (mapper-test-suite)
  article-2
  (ensure-same '("article" (:article-id . "123"))
               (multiple-value-bind (route bindings) (match *test-mapper* "articles/123")
                 (cons (route-name route)
                       bindings))))

(addtest (mapper-test-suite)
  article-3
  (ensure-same '("article3" (:category . 45) (:id . 1523))
               (multiple-value-bind (route bindings) (match *test-mapper* "articles/45/1523")
                 (cons (route-name route)
                       bindings))))

(addtest (mapper-test-suite)
  article-4
  (ensure-same '("article4" (:author . "chekhov") (:id . 1523))
               (multiple-value-bind (route bindings) (match *test-mapper* "articles/chekhov/1523")
                 (cons (route-name route)
                       bindings))))

(addtest (mapper-test-suite)
  article-concat-1
  (ensure-same '("article-concat-1" (:article-id . "hello"))
               (multiple-value-bind (route bindings) (match *test-mapper* "articles/hello.html")
                 (cons (route-name route)
                       bindings))))

(addtest (mapper-test-suite)
  article-concat-2
  (ensure-same '("article-concat-2" (:author . "hello") (:id . "world"))
               (multiple-value-bind (route bindings) (match *test-mapper* "articles/hello-world.html")
                 (cons (route-name route)
                       bindings))))

(addtest (mapper-test-suite)
  article-concat-3
  (ensure-same '("article-concat-3" (:author . "hello") (:id . "world") (:part . "2"))
               (multiple-value-bind (route bindings) (match *test-mapper* "articles/hello-world-2.html")
                 (cons (route-name route)
                       bindings))))

(addtest (mapper-test-suite)
  article-wildcard-1
  (ensure-same '("article-wildcard-1" (:path "foo" "bar" "baz"))
               (multiple-value-bind (route bindings) (match *test-mapper* "articles/foo/bar/baz")
                 (cons (route-name route)
                       bindings))))

(addtest (mapper-test-suite)
  article-wildcard-2
  (ensure-same '("article-wildcard-2" (:path 1 2 3))
               (multiple-value-bind (route bindings) (match *test-mapper* "articles/check/1/2/3")
                 (cons (route-name route)
                       bindings))))

(addtest (mapper-test-suite)
  triple-1
  (ensure-same '("triple-1" (:foo . "xxx") (:bar . "chekhov") (:baz . "1523"))
               (multiple-value-bind (route bindings) (match *test-mapper* "xxx/chekhov/1523")
                 (cons (route-name route)
                       bindings))))

(addtest (mapper-test-suite)
  wildcard-1
  (ensure-same '("wildcard-1" (:foo "foo" "bar" "baz" "xxx"))
               (multiple-value-bind (route bindings) (match *test-mapper* "foo/bar/baz/xxx")
                 (cons (route-name route)
                       bindings))))

(addtest (mapper-test-suite)
  wildcard-2
  (ensure-same '("wildcard-1" (:foo "foo" "bar" "baz" "xxx" ""))
               (multiple-value-bind (route bindings) (match *test-mapper* "foo/bar/baz/xxx/")
                 (cons (route-name route)
                       bindings))))

(addtest (mapper-test-suite)
  person-1
  (ensure-same '("person" (:id . "1"))
               (multiple-value-bind (route bindings) (match *test-mapper* "person1")
                 (cons (route-name route)
                       bindings))))

(addtest (mapper-test-suite)
  impossible-1
  (ensure-same '("impossible" (:siid . "1"))
               (multiple-value-bind (route bindings) (match *test-mapper* "impossible1")
                 (cons (route-name route)
                       bindings))))
                 