(in-package :cl-user)

(defpackage :documentation
  (:nicknames :doc :wt.documentation :wt.doc)
  (:use :cl :alexandria)
  (:shadow :documentation)
  (:shadowing-import-from :reactive :variable)
  (:import-from :http
                :define-server
                :router
                :listener
                :header
                :status-code
                :entity-json
                :reply)
  (:import-from :utility
                :map-tree)
  (:import-from :reactive
                :define-variable
                :reactive-object)
  (:import-from :component
                :define-component
                :render)
  (:import-from :dom
                :children)
  (:import-from :live
                :define-page
                :page-title
                :page-content)
  (:import-from :html
                :document :text
                :html :head :body
                :h1 :h2 :h3 :a :p :pre :div
                :span :dl :dt :dd :hr :table :thead :tbody
                :th :tr :td :ol :li :br :img :figure :figcaption :section
                :serialize)
  (:import-from :closer-mop
                :compute-class-precedence-list
                :class-direct-superclasses))
