(in-package :documentation)

(define-component documentation ()
  ((title
    :initform "Lisp Web Toolkit")
   (description
    :initform "Object-Oriented Lisp Systems for Rapid Web Application Development")
   (author
    :initform "Xiangyu He"))
  (:render
   (lambda (doc)
     (with-slots (title description author children) doc
       (let ((chapters (list
                        chapter-get-started
                        chapter-uri
                        chapter-json
                        chapter-html
                        chapter-css
                        chapter-javascript
                        chapter-http
                        chapter-websocket)))
         (assign-numbers chapters)
         (mapcar #'assign-chapter-articles chapters)
         (div
          :class "h-entry toc-sidebar"
          (html:style
           :media "all"
           (concatenate 'string *base-style* *default-style* *style*))
          (div
           :class "head"
           (h1 :id "title" title)
           (h2 :id "subtitle" description)
           (dl
            (dt "GitHub")
            (dd (a :href "https://github.com/xh4/web-toolkit"
                   :target "_blank"
                   "https://github.com/xh4/web-toolkit") " "
                   (span :id "github-button"
                         (a :class "github-button"
                            :href "https://github.com/xh4/web-toolkit"
                            :data-icon "octicon-star"
                            :aria-label "Star xh4/web-toolkit on GitHub"
                            :style "margin: 0 10px;"
                            "Star"))
                (html:script :async t :src "https://buttons.github.io/buttons.js"))
            (dt "Status")
            (dd (a
                 :href "https://travis-ci.org/xh4/web-toolkit"
                 :target "_blank"
                 (img
                  :src "https://travis-ci.org/xh4/web-toolkit.svg?branch=master"
                  :alt "Build Status")))
            (dt "Author")
            (dd (a :href "https://xh.coobii.com"
                   :target "_blank"
                   author)
                (span " <")
                (a :href "mailto:xh@coobii.com"
                   "xh@coobii.com")
                (span "> ")
                (a :href "https://twitter.com/xh004?ref_src=twsrc%5Etfw"
                   :class "twitter-follow-button"
                   :data-show-count "false"
                   "Follow @XH004")
                (html:script
                 :async t
                 :src "https://platform.twitter.com/widgets.js"
                 :charset "utf-8"))
            (dt "Update")
            (dd (local-time:format-timestring
                 nil
                 (local-time:now) 
                 :format '(:long-month " " (:day 2) ", " (:year 4)))))
           (p :class "copyright"
              "Copyright © 2018-2020 "
              (a :href "https://xh.coobii.com"
                 :target "_blank"
                 "Xiangyu He")
              ". "
              "Released under the 3-Clause BSD License.")
           (hr))
          (nav chapters)
          chapters))))))

(defun assign-numbers (children)
  (labels ((walk (node no)
             (typecase node
               (addressable
                (setf (no node) no)
                (loop with i = 1
                   for child in (dom:children node)
                   when (typep child 'addressable)
                   do (walk child (append no `(,i))) (incf i)))
               (t (loop for child in (dom:children node)
                     do (walk child no))))))
    (loop with i = 1
       for child in children
       when (typep child 'addressable)
       do (walk child `(,i)) (incf i))))

(defun assign-chapter-articles (chapter)
  (loop for child in (dom:children chapter)
     when (or (typep child 'article)
              (typep child 'symbol/o))
     do (setf (slot-value child 'chapter) chapter)))

(defun nav (children)
  (labels ((walk (node ol)
             (typecase node
               ((or chapter article)
                (dom:append-child
                 ol
                 (li
                  (a :href (format nil "#~A" (id node))
                     (span :class "secno" (format nil "~{~A~^.~}" (no node)))
                     "&nbsp;"
                     (span :class "content" (title node)))
                  (let ((ol (ol :class "toc")))
                    (loop for child in (dom:children node)
                       do (walk child ol))
                    ol))))
               (class/o (dom:append-child
                         ol
                         (li
                          (a :href (format nil "#~A" (id node))
                             (span :class "secno" (format nil "~{~A~^.~}" (no node)))
                             (span :class "content"
                                   "&nbsp;"
                                   (span :class "symbol-type"
                                         "Class")
                                   " "
                                   (format nil "~(~A~)" (slot-value node 'symbol))))
                          (let ((ol (ol :class "toc")))
                            (loop for child in (dom:children node)
                               do (walk child ol))
                            ol))))
               (function/o (dom:append-child
                            ol
                            (li
                             (a :href (format nil "#~A" (id node))
                                (span :class "secno" (format nil "~{~A~^.~}" (no node)))
                                (span :class "content"
                                      "&nbsp;"
                                      (span :class "symbol-type"
                                            "Function")
                                      " "
                                      (format nil "~(~A~)" (slot-value node 'symbol))))
                             (let ((ol (ol :class "toc")))
                               (loop for child in (dom:children node)
                                  do (walk child ol))
                               ol))))
               (accessor/o (dom:append-child
                            ol
                            (li
                             (a :href (format nil "#~A" (id node))
                                (span :class "secno" (format nil "~{~A~^.~}" (no node)))
                                (span :class "content"
                                      "&nbsp;"
                                      (span :class "symbol-type"
                                            "Accessor")
                                      " "
                                      (format nil "~(~A~)" (slot-value node 'symbol))))
                             (let ((ol (ol :class "toc")))
                               (loop for child in (dom:children node)
                                  do (walk child ol))
                               ol))))
               (constant/o (dom:append-child
                            ol
                            (li
                             (a :href (format nil "#~A" (id node))
                                (span :class "secno" (format nil "~{~A~^.~}" (no node)))
                                (span :class "content"
                                      "&nbsp;"
                                      (span :class "symbol-type"
                                            "Constant")
                                      " "
                                      (format nil "~(~A~)" (slot-value node 'symbol))))
                             (let ((ol (ol :class "toc")))
                               (loop for child in (dom:children node)
                                  do (walk child ol))
                               ol))))
               (dom:parent-node (loop for i from 1
                                   for child in (dom:children node)
                                   do (walk child ol))))))
    (html:nav
     :id "toc"
     (h2
      :id "contents"
      "Table of Contents")
     (let ((ol (ol :class "toc")))
       (loop for child in children
          do (walk child ol))
       ol))))

(defun make-documentation ()
  (let ((doc (documentation)))
    (with-slots (title description) doc
      (let ((html (html:serialize
                   (html:document
                    (html:html
                     (html:head
                      (html:meta :charset "utf-8")
                      (html:title title)
                      (html:meta :name "description"
                                 :content description))
                     (html:body
                      doc))))))
        (let ((pathname (merge-pathnames
                         "documentation/index.html"
                         (asdf:system-source-directory (asdf:find-system :wt)))))
          (with-open-file (stream pathname
                                  :direction :output
                                  :if-exists :supersede)
            (format stream "~A" html))
          pathname)))))
