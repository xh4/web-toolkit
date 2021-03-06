(in-package :html)

;; https://html.spec.whatwg.org/multipage/parsing.html#tree-construction

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *parser-insertion-modes* '()))

(defmacro define-parser-insertion-mode (name &body body)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (if-let ((i (position ',name *parser-insertion-modes* :key 'first)))
         (setf (nth i *parser-insertion-modes*) '(,name ,@body))
       (appendf *parser-insertion-modes* '((,name ,@body))))
     nil))

(defparameter *debug* nil)

(defparameter document (make-instance 'document))
(defparameter foster-parenting nil)
(defparameter insertion-mode 'initial)
(defparameter original-insertion-mode nil)
(defparameter current-token nil)
(defparameter next-token nil)
(defparameter stack-of-open-elements nil)
(defparameter stack-of-template-insertion-modes nil)
(defparameter head-element-pointer nil)
(defparameter form-element-pointer nil)
(defparameter scripting-flag nil)
(defparameter frameset-ok-flag nil)
(defparameter active-formatting-elements nil)
(defparameter pending-table-character-tokens nil)
(defparameter open-texts nil)

(define-symbol-macro current-node (first stack-of-open-elements))

(define-parser-insertion-mode initial
  (cond
   ((or (eq #\tab current-token) (eq #\newline current-token)
        (eq #\page current-token) (eq #\return current-token) (eq #\space current-token))
    (ignore-token))

   ((typep current-token 'comment-token)
    ;; TODO: Check this
    (insert-comment))

   ((typep current-token 'doctype-token)
    ;; TODO
    (switch-insertion-mode 'before-html))

   (t
    ;; TODO
    (switch-insertion-mode 'before-html)
    (reprocess-current-token))))

(define-parser-insertion-mode before-html
  (cond
   ((typep current-token 'doctype-token)
    (parse-error)
    (ignore-token))

   ((typep current-token 'comment-token)
    ;; TODO: Check this
    (insert-comment))

   ((or (eq #\tab current-token) (eq #\newline current-token)
        (eq #\page current-token) (eq #\return current-token) (eq #\space current-token))
    (ignore-token))

   ((a-start-tag-whose-tag-name-is "html")
    (let ((element (create-element-for-token dom:html-namespace document)))
      (append-child document element)
      (push element stack-of-open-elements))
    (switch-insertion-mode 'before-head))

   ((an-end-tag-whose-tag-name-is-one-of '("head" "body" "html" "br"))
    ;; Same as T
    (let ((element (make-instance 'element :local-name "html")))
      (append-child document element)
      (push element stack-of-open-elements))
    (switch-insertion-mode 'before-head)
    (reprocess-current-token))

   ((typep current-token 'end-tag)
    (parse-error)
    (ignore-token))

   (t
    (let ((element (make-instance 'element :local-name "html")))
      (append-child document element)
      (push element stack-of-open-elements))
    (switch-insertion-mode 'before-head)
    (reprocess-current-token))))

(define-parser-insertion-mode before-head
  (cond
   ((or (eq #\tab current-token) (eq #\newline current-token)
        (eq #\page current-token) (eq #\return current-token) (eq #\space current-token))
    (ignore-token))

   ((typep current-token 'comment-token)
    (insert-comment))

   ((typep current-token 'doctype-token)
    (parse-error)
    (ignore-token))

   ((a-start-tag-whose-tag-name-is "html")
    (process-token-using-in-body-insertion-mode))

   ((a-start-tag-whose-tag-name-is "head")
    (let ((head (insert-html-element-for-token)))
      (setf head-element-pointer head)
      (switch-insertion-mode 'in-head)))

   ((an-end-tag-whose-tag-name-is-one-of '("head" "body" "html" "br"))
    ;; Same as T
    (let ((current-token (make-instance 'start-tag :tag-name "head")))
      (let ((head (insert-html-element-for-token)))
        (setf head-element-pointer head)))
    (switch-insertion-mode 'in-head)
    (reprocess-current-token))

   ((typep current-token 'end-tag)
    (parse-error)
    (ignore-token))

   (t
    (let ((current-token (make-instance 'start-tag :tag-name "head")))
      (let ((head (insert-html-element-for-token)))
        (setf head-element-pointer head)))
    (switch-insertion-mode 'in-head)
    (reprocess-current-token))))

(define-parser-insertion-mode in-head
  (cond
   ((or (eq #\tab current-token) (eq #\newline current-token)
        (eq #\page current-token) (eq #\return current-token) (eq #\space current-token))
    (insert-character))

   ((typep current-token 'comment-token)
    (insert-comment))

   ((typep current-token 'doctype-token)
    (parse-error)
    (ignore-token))

   ((a-start-tag-whose-tag-name-is "html")
    (process-token-using-in-body-insertion-mode))

   ((a-start-tag-whose-tag-name-is-one-of '("base" "basefont" "bgsound" "link"))
    (insert-html-element-for-token)
    (pop stack-of-open-elements)
    (acknowledge-token-self-closing-flag))

   ((a-start-tag-whose-tag-name-is "meta")
    (let ((element (insert-html-element-for-token)))
      (declare (ignore element))
      (pop stack-of-open-elements)
      (acknowledge-token-self-closing-flag)
      #|TODO: Handle encoding changing|#))

   ((a-start-tag-whose-tag-name-is "title")
    (parse-generic-rcdata-element))

   ((or (and scripting-flag
             (a-start-tag-whose-tag-name-is "noscript"))
        (a-start-tag-whose-tag-name-is-one-of '("noframes" "style")))
    (parse-generic-rawtext-element))

   ((and (not scripting-flag)
         (a-start-tag-whose-tag-name-is "noscript"))
    (insert-html-element-for-token)
    (switch-insertion-mode 'in-head-noscript))

   ;; TODO: Check this
   ((a-start-tag-whose-tag-name-is "script")
    (let ((adjusted-insertion-location (appropriate-place-for-inserting-node)))
      (let ((element (create-element-for-token dom:html-namespace
                                               adjusted-insertion-location)))
        (push element stack-of-open-elements))
      (switch-state 'script-data-state)
      (setf original-insertion-mode insertion-mode)
      (switch-insertion-mode 'text)))

   ((an-end-tag-whose-tag-name-is "head")
    (assert (string-equal "head" (dom:tag-name current-node)))
    (pop stack-of-open-elements)
    (switch-insertion-mode 'after-head))

   ((an-end-tag-whose-tag-name-is-one-of '("body" "html" "br"))
    ;; Same as T
    (assert (string-equal "head" (dom:tag-name current-node)))
    (pop stack-of-open-elements)
    (switch-insertion-mode 'after-head)
    (reprocess-current-token))

   ((a-start-tag-whose-tag-name-is "template")
    (insert-html-element-for-token)
    (appendf active-formatting-elements (list (make-marker)))
    (setf frameset-ok-flag nil)
    (switch-insertion-mode 'in-template)
    (push 'in-template stack-of-template-insertion-modes))

   ((an-end-tag-whose-tag-name-is "template")
    (if (not (find "template" stack-of-open-elements
                   :test 'string-equal
                   :key 'dom:tag-name))
        (progn
          (parse-error)
          (ignore-token))
      (progn
        (generate-all-implied-end-tags-thoroughly)
        (unless (string-equal "template" (dom:tag-name current-node))
          (parse-error))
        (loop for element = (pop stack-of-open-elements)
              until (string-equal "template" (dom:tag-name element)))
        (clear-active-formatting-elements-upto-last-marker)
        (pop stack-of-template-insertion-modes)
        (reset-insertion-mode-appropriately))))

   ((or (a-start-tag-whose-tag-name-is "head")
        (typep current-token 'end-tag))
    (parse-error)
    (ignore-token))

   (t
    (assert (string-equal "head" (dom:tag-name current-node)))
    (pop stack-of-open-elements)
    (switch-insertion-mode 'after-head)
    (reprocess-current-token))))

(define-parser-insertion-mode in-head-noscript
  (cond
   ((typep current-token 'doctype-token)
    (parse-error)
    (ignore-token))

   ((a-start-tag-whose-tag-name-is "html")
    (process-token-using-in-body-insertion-mode))

   ((an-end-tag-whose-tag-name-is "noscript")
    (assert (string-equal "noscript" (dom:tag-name current-node)))
    (pop stack-of-open-elements)
    (assert (string-equal "head" (dom:tag-name current-node)))
    (switch-insertion-mode 'in-head))

   ((or (or (eq #\tab current-token) (eq #\newline current-token)
            (eq #\page current-token) (eq #\return current-token) (eq #\space current-token))
        (typep current-token 'comment-token)
        (a-start-tag-whose-tag-name-is-one-of '("basefont" "bgsound" "link"
                                                "meta" "noframes" "style")))
    (process-token-using-in-head-insertion-mode))

   ((an-end-tag-whose-tag-name-is "br")
    ;; Same as T
    (parse-error)
    (assert (string-equal "noscript" (dom:tag-name current-node)))
    (pop stack-of-open-elements)
    (assert (string-equal "head" (dom:tag-name current-node)))
    (switch-insertion-mode 'in-head)
    (reprocess-current-token))

   ((or (a-start-tag-whose-tag-name-is-one-of '("head" "noscript"))
        (typep current-token 'end-tag))
    (parse-error)
    (ignore-token))

   (t
    (parse-error)
    (assert (string-equal "noscript" (dom:tag-name current-node)))
    (pop stack-of-open-elements)
    (assert (string-equal "head" (dom:tag-name current-node)))
    (switch-insertion-mode 'in-head)
    (reprocess-current-token))))

(define-parser-insertion-mode after-head
  (cond
   ((or (eq #\tab current-token) (eq #\newline current-token)
        (eq #\page current-token) (eq #\return current-token) (eq #\space current-token))
    (insert-character))

   ((typep current-token 'comment-token)
    (insert-comment))

   ((typep current-token 'doctype-token)
    (parse-error)
    (ignore-token))

   ((a-start-tag-whose-tag-name-is "html")
    (process-token-using-in-body-insertion-mode))

   ((a-start-tag-whose-tag-name-is "body")
    (insert-html-element-for-token)
    (setf frameset-ok-flag nil)
    (switch-insertion-mode 'in-body))

   ((a-start-tag-whose-tag-name-is "frameset")
    (insert-html-element-for-token)
    (switch-insertion-mode 'in-frameset))

   ((a-start-tag-whose-tag-name-is-one-of '("base" "basefont" "bgsound" "link"
                                            "meta" "noframes" "script" "style"
                                            "template" "title"))
    (parse-error)
    (let ((node head-element-pointer))
      (push node stack-of-open-elements)
      (process-token-using-in-head-insertion-mode)
      (removef stack-of-open-elements node)))

   ((an-end-tag-whose-tag-name-is "template")
    (process-token-using-in-head-insertion-mode))

   ((an-end-tag-whose-tag-name-is-one-of '("body" "html" "br"))
    ;; Same as T
    (let ((current-token (make-instance 'start-tag :tag-name "body")))
      (insert-html-element-for-token))
    (switch-insertion-mode 'in-body)
    (reprocess-current-token))

   ((or (a-start-tag-whose-tag-name-is "head")
        (typep current-token 'end-tag))
    (parse-error)
    (ignore-token))

   (t
    (let ((current-token (make-instance 'start-tag :tag-name "body")))
      (insert-html-element-for-token))
    (switch-insertion-mode 'in-body)
    (reprocess-current-token))))

(define-parser-insertion-mode in-body
  (cond
   ((eq #\null current-token)
    (parse-error)
    (ignore-token))

   ((or (eq #\tab current-token) (eq #\newline current-token)
        (eq #\page current-token) (eq #\return current-token) (eq #\space current-token))
    (reconstruct-active-formatting-elements)
    (insert-character))

   ((typep current-token 'cl:character)
    (reconstruct-active-formatting-elements)
    (insert-character)
    (setf frameset-ok-flag nil))

   ((typep current-token 'comment-token)
    (insert-comment))

   ((typep current-token 'doctype-token)
    (parse-error)
    (ignore-token))

   ((a-start-tag-whose-tag-name-is "html")
    (parse-error)
    (if (find "template" stack-of-open-elements
              :test 'string-equal
              :key 'dom:tag-name)
        (ignore-token)
      #|TODO|#))

   ((or (a-start-tag-whose-tag-name-is-one-of '("base" "basefont" "bgsound" "link"
                                                "meta" "noframes" "script" "style"
                                                "template" "title"))
        (an-end-tag-whose-tag-name-is "template"))
    (process-token-using-in-head-insertion-mode))

   ((a-start-tag-whose-tag-name-is "body")
    (parse-error)
    (if (or (= 1 (length stack-of-open-elements))
            (not (string-equal "body" (dom:tag-name (second stack-of-open-elements))))
            (find "template" stack-of-open-elements
                  :test 'string-equal
                  :key 'dom:tag-name))
        (ignore-token)
      (progn
        (setf frameset-ok-flag nil)
        #|TODO|#)))

   ((a-start-tag-whose-tag-name-is "frameset")
    (parse-error)
    #|TODO|#)

   ((typep current-token 'end-of-file)
    (if stack-of-template-insertion-modes
        (process-token-using-in-template-insertion-mode)
      (progn
        (if (not (find-if (lambda (element)
                            (member (dom:tag-name element)
                                    '("dd" "dt" "li" "optgroup" "option"
                                      "p" "rb" "rp" "rt" "rtc"
                                      "tbody" "td" "tfoot" "th" "thead" "tr"
                                      "body" "html")
                                    :test 'string-equal))
                          stack-of-open-elements))
            (parse-error))
        (stop-parsing))))

   ((an-end-tag-whose-tag-name-is "body")
    (if (not (have-element-in-scope-p "body"))
        (progn
          (parse-error)
          (ignore-token))
      (if (not (find-if (lambda (element)
                          (member (dom:tag-name element)
                                  '("dd" "dt" "li" "optgroup" "option"
                                    "p" "rb" "rp" "rt" "rtc"
                                    "tbody" "td" "tfoot" "th" "thead" "tr"
                                    "body" "html")
                                  :test 'string-equal))
                        stack-of-open-elements))
          (parse-error)))
    (switch-insertion-mode 'after-body))

   ((an-end-tag-whose-tag-name-is "html")
    (if (not (have-element-in-scope-p "body"))
        (progn
          (parse-error)
          (ignore-token))
      (if (not (find-if (lambda (element)
                          (member (dom:tag-name element)
                                  '("dd" "dt" "li" "optgroup" "option"
                                    "p" "rb" "rp" "rt" "rtc"
                                    "tbody" "td" "tfoot" "th" "thead" "tr"
                                    "body" "html")
                                  :test 'string-equal))
                        stack-of-open-elements))
          (parse-error)))
    (switch-insertion-mode 'after-body)
    (reprocess-current-token))

   ((a-start-tag-whose-tag-name-is-one-of '("address" "article" "aside" "blockquote"
                                            "center" "details" "dialog" "dir" "div" "dl"
                                            "fieldset" "figcaption" "figure" "footer" "header"
                                            "hgroup" "main" "menu" "nav" "ol" "p"
                                            "section" "summary" "ul"))
    (when (have-element-in-button-scope-p "p")
      (close-p-element))
    (insert-html-element-for-token))

   ((a-start-tag-whose-tag-name-is-one-of '("h1" "h2" "h3" "h4" "h5" "h6"))
    (when (have-element-in-button-scope-p "p")
      (close-p-element))
    (when (and (typep current-node 'element)
               (member (dom:tag-name current-node)
                       '("h1" "h2" "h3" "h4" "h5" "h6")
                       :test 'string-equal))
      (parse-error)
      (pop stack-of-open-elements))
    (insert-html-element-for-token))

   ((a-start-tag-whose-tag-name-is-one-of '("pre" "listing"))
    (when (have-element-in-button-scope-p "p")
      (close-p-element))
    (insert-html-element-for-token)
    (when (eq #\newline next-token)
      (setf next-token nil))
    (setf frameset-ok-flag nil))

   ((a-start-tag-whose-tag-name-is "form")
    (if (and form-element-pointer
             (not (find "template" stack-of-open-elements
                        :test 'string-equal
                        :key 'dom:tag-name)))
        (progn
          (parse-error)
          (ignore-token))
      (progn
        (when (have-element-in-button-scope-p "p")
          (close-p-element))
        (let ((element (insert-html-element-for-token)))
          (unless (find "template" stack-of-open-elements
                        :test 'string-equal
                        :key 'dom:tag-name)
            (setf form-element-pointer element))))))

   ((a-start-tag-whose-tag-name-is "li")
    (setf frameset-ok-flag nil)
    (loop for element in stack-of-open-elements
          if (string-equal "li" (dom:tag-name element))
          do (progn
               (generate-implied-end-tags :except "li")
               (unless (string-equal "li" (dom:tag-name current-node))
                 (parse-error))
               (loop for element = (pop stack-of-open-elements)
                     until (string-equal "li" (dom:tag-name element)))
               (when (have-element-in-button-scope-p "p")
                 (close-p-element))
               (return))
          else if (and (special-element-p element)
                       (not (or (string-equal "address" (dom:tag-name element))
                                (string-equal "div" (dom:tag-name element))
                                (string-equal "p" (dom:tag-name element)))))
          do (progn
               (when (have-element-in-button-scope-p "p")
                 (close-p-element))
               (return)))
    (insert-html-element-for-token))

   ((a-start-tag-whose-tag-name-is-one-of '("dd" "dt"))
    (setf frameset-ok-flag nil)
    (loop for element in stack-of-open-elements
          if (string-equal "dd" (dom:tag-name element))
          do (progn
               (generate-implied-end-tags :except "dd")
               (unless (string-equal "dd" (dom:tag-name current-node))
                 (parse-error))
               (loop for element = (pop stack-of-open-elements)
                     until (string-equal "dd" (dom:tag-name element)))
               (when (have-element-in-button-scope-p "p")
                 (close-p-element))
               (return))
          else if (string-equal "dt" (dom:tag-name element))
          do (progn
               (generate-implied-end-tags :except "dt")
               (unless (string-equal "dt" (dom:tag-name current-node))
                 (parse-error))
               (loop for element = (pop stack-of-open-elements)
                     until (string-equal "dt" (dom:tag-name element)))
               (when (have-element-in-button-scope-p "p")
                 (close-p-element))
               (return))
          else if (and (special-element-p element)
                       (not (or (string-equal "address" (dom:tag-name element))
                                (string-equal "div" (dom:tag-name element))
                                (string-equal "p" (dom:tag-name element)))))
          do (progn
               (when (have-element-in-button-scope-p "p")
                 (close-p-element))
               (return)))
    (insert-html-element-for-token))

   ((a-start-tag-whose-tag-name-is "plaintext")
    (when (have-element-in-button-scope-p "p")
      (close-p-element))
    (insert-html-element-for-token)
    (switch-state 'plaintext-state))

   ((a-start-tag-whose-tag-name-is "button")
    (when (have-element-in-scope-p "button")
      (parse-error)
      (generate-implied-end-tags)
      (loop for element = (pop stack-of-open-elements)

            until (string-equal "button" (dom:tag-name element))))
    (reconstruct-active-formatting-elements)
    (insert-html-element-for-token)
    (setf frameset-ok-flag nil))

   ((an-end-tag-whose-tag-name-is-one-of '("address" "article" "aside"
                                           "blockquote" "button" "center"
                                           "details" "dialog" "dir" "div" "dl"
                                           "fieldset" "figcaption" "figure"
                                           "footer" "header" "hgroup" "listing"
                                           "main" "menu" "nav" "ol" "pre"
                                           "section" "summary" "ul"))
    (if (not (have-element-in-scope-p (slot-value current-token 'tag-name)))
        (progn
          (parse-error)
          (ignore-token))
      (progn
        (generate-implied-end-tags)
        (unless (string-equal (dom:tag-name current-node)
                              (slot-value current-token 'tag-name))
          (parse-error))
        (loop for element = (pop stack-of-open-elements)
              until (string-equal (dom:tag-name element)
                                  (slot-value current-token 'tag-name))))))

   ((an-end-tag-whose-tag-name-is "form")
    (if (not (find "template" stack-of-open-elements
                   :test 'string-equal
                   :key 'dom:tag-name))
        (let ((node form-element-pointer))
          (setf form-element-pointer nil)
          (if (or (null node)
                  (have-element-in-scope-p node))
              (progn
                (parse-error)
                (ignore-token))
            (progn
              (generate-implied-end-tags)
              (unless (eq current-node node)
                (parse-error))
              (removef stack-of-open-elements node))))
      (progn
        (if (not (have-element-in-scope-p "form"))
            (progn
              (parse-error)
              (ignore-token))
          (progn
            (generate-implied-end-tags)
            (unless (string-equal "form" (dom:tag-name current-node))
              (parse-error))
            (loop for element = (pop stack-of-open-elements)
                  until (string-equal "form" (dom:tag-name element))))))))

   ((an-end-tag-whose-tag-name-is "p")
    (unless (have-element-in-button-scope-p "p")
      (parse-error)
      (let ((current-token (make-instance 'start-tag :tag-name "p")))
        (insert-html-element-for-token)))
    (close-p-element))

   ((an-end-tag-whose-tag-name-is "li")
    (if (not (have-element-in-button-scope-p "li"))
        (progn
          (parse-error)
          (ignore-token))
      (progn
        (generate-implied-end-tags :except "li")
        (unless (string-equal "li" (dom:tag-name current-node))
          (parse-error))
        (loop for element = (pop stack-of-open-elements)
              until (string-equal "li" (dom:tag-name element))))))

   ((an-end-tag-whose-tag-name-is-one-of '("dd" "dt"))
    (if (not (have-element-in-scope-p (slot-value current-token 'tag-name)))
        (progn
          (parse-error)
          (ignore-token))
      (progn
        (generate-implied-end-tags :except (slot-value current-token 'tag-name))
        (unless (string-equal (dom:tag-name current-node)
                              (slot-value current-token 'tag-name))
          (parse-error))
        (loop for element = (pop stack-of-open-elements)
              until (member (dom:tag-name element)
                            (slot-value current-token 'tag-name))))))

   ((an-end-tag-whose-tag-name-is-one-of '("h1" "h2" "h3" "h4" "h5" "h6"))
    ;; TODO: Check this
    (if (not (or (have-element-in-scope-p "h1")
                 (have-element-in-scope-p "h2")
                 (have-element-in-scope-p "h3")
                 (have-element-in-scope-p "h4")
                 (have-element-in-scope-p "h5")
                 (have-element-in-scope-p "h6")))
        (progn
          (parse-error)
          (ignore-token))
      (progn
        (generate-implied-end-tags)
        (unless (string-equal (dom:tag-name current-node)
                              (slot-value current-token 'tag-name))
          (parse-error))
        (loop for element = (pop stack-of-open-elements)
              until (member (dom:tag-name element)
                            '("h1" "h2" "h3" "h4" "h5" "h6")
                            :test 'string-equal)))))

   ((an-end-tag-whose-tag-name-is "sarcasm"))

   ((a-start-tag-whose-tag-name-is "a")
    (let ((last-marker-position (position-if 'marker-p active-formatting-elements
                                             :from-end t)))
      (when-let ((a
                  (or (and last-marker-position
                           (find "a" (subseq active-formatting-elements
                                             (1+ last-marker-position)
                                             (length active-formatting-elements))
                                 :test 'string-equal
                                 :key 'dom:tag-name))
                      (and (null last-marker-position)
                           (find "a" active-formatting-elements
                                 :test 'string-equal
                                 :key 'dom:tag-name)))))
        (parse-error)
        (adoption-agency)
        (removef active-formatting-elements a)
        (removef stack-of-open-elements a))
      (reconstruct-active-formatting-elements)
      (let ((element (insert-html-element-for-token)))
        (appendf active-formatting-elements (list element)))))

   ((a-start-tag-whose-tag-name-is-one-of '("b" "big" "code" "em"
                                            "font" "i" "s" "small" "strike"
                                            "strong" "tt" "u"))
    (reconstruct-active-formatting-elements)
    (let ((element (insert-html-element-for-token)))
      (push-onto-active-formatting-elements element)))

   ;; TODO
   ((a-start-tag-whose-tag-name-is "nobr"))

   ((an-end-tag-whose-tag-name-is-one-of '("a" "b" "big" "code" "em"
                                           "font" "i" "s" "small"
                                           "strike" "strong" "tt" "u"))
    (adoption-agency))

   ((a-start-tag-whose-tag-name-is-one-of '("applet" "marquee" "object"))
    (reconstruct-active-formatting-elements)
    (insert-html-element-for-token)
    (appendf active-formatting-elements (list (make-marker)))
    (setf frameset-ok-flag nil))

   ((an-end-tag-whose-tag-name-is-one-of '("applet" "marquee" "object"))
    (if (not (have-element-in-scope-p (slot-value current-token 'tag-name)))
        (progn
          (parse-error)
          (ignore-token))
      (progn
        (generate-implied-end-tags)
        (unless (string-equal (dom:tag-name current-node)
                              (slot-value current-token 'tag-name))
          (parse-error))
        (loop for element = (pop stack-of-open-elements)
              until (string-equal (dom:tag-name element)
                                  (slot-value current-token 'tag-name)))
        (clear-active-formatting-elements-upto-last-marker))))

   ((a-start-tag-whose-tag-name-is "table")
    #|TODO: If the Document is not set to quirks mode, and the stack of open elements has a p element in button scope, then close a p element.|#
    (insert-html-element-for-token)
    (setf frameset-ok-flag nil)
    (switch-insertion-mode 'in-table))

   ((an-end-tag-whose-tag-name-is "br")
    (parse-error)
    #|TODO: Drop the attributes from the current-token, and act as described in the next entry; i.e. act as if this was a "br" start tag token with no attributes, rather than the end tag token that it actually is.|#)

   ((a-start-tag-whose-tag-name-is-one-of '("area" "br" "embed"
                                            "img" "keygen" "wbr"))
    (reconstruct-active-formatting-elements)
    (insert-html-element-for-token)
    (pop stack-of-open-elements)
    (acknowledge-token-self-closing-flag)
    (setf frameset-ok-flag nil))

   ((a-start-tag-whose-tag-name-is "input")
    (reconstruct-active-formatting-elements)
    (insert-html-element-for-token)
    (pop stack-of-open-elements)
    (acknowledge-token-self-closing-flag)
    #|TODO: If the token does not have an attribute with the name "type", or if it does, but that attribute's value is not an ASCII case-insensitive match for the string "hidden", then: set the frameset-ok flag to "not ok".|#)

   ((a-start-tag-whose-tag-name-is-one-of '("param" "source" "track"))
    (insert-html-element-for-token)
    (pop stack-of-open-elements)
    (acknowledge-token-self-closing-flag))

   ((a-start-tag-whose-tag-name-is "hr")
    (when (have-element-in-button-scope-p "p")
      (close-p-element))
    (insert-html-element-for-token)
    (pop stack-of-open-elements)
    (acknowledge-token-self-closing-flag)
    (setf frameset-ok-flag nil))

   ((a-start-tag-whose-tag-name-is "image")
    (parse-error)
    (setf (slot-value current-token 'tag-name) "img")
    (reprocess-current-token))

   ((a-start-tag-whose-tag-name-is "textarea")
    (insert-html-element-for-token)
    ;; Newlines at the start of textarea elements are ignored as an authoring convenience.
    (when (eq #\newline next-token)
      (setf next-token nil))
    (switch-state 'rcdata-state)
    (setf original-insertion-mode insertion-mode)
    (setf frameset-ok-flag nil)
    (switch-insertion-mode 'text))

   ((a-start-tag-whose-tag-name-is "xmp")
    (when (have-element-in-button-scope-p "p")
      (close-p-element))
    (reconstruct-active-formatting-elements)
    (setf frameset-ok-flag nil)
    (parse-generic-rawtext-element))

   ((a-start-tag-whose-tag-name-is "iframe")
    (setf frameset-ok-flag nil)
    (parse-generic-rawtext-element))

   ((or (a-start-tag-whose-tag-name-is "noembed")
        (and scripting-flag
             (a-start-tag-whose-tag-name-is "noscript")))
    (parse-generic-rawtext-element))

   ((a-start-tag-whose-tag-name-is "select")
    (reconstruct-active-formatting-elements)
    (insert-html-element-for-token)
    (setf frameset-ok-flag nil)
    (if (member insertion-mode '(in-table in-caption in-table-body
                                          in-row in-cell))
        (switch-insertion-mode 'in-select-in-table)
      (switch-insertion-mode 'in-select)))

   ((a-start-tag-whose-tag-name-is-one-of '("optgroup" "option"))
    (when (string-equal "option" (dom:tag-name current-node))
      (pop stack-of-open-elements))
    (reconstruct-active-formatting-elements)
    (insert-html-element-for-token))

   ((a-start-tag-whose-tag-name-is-one-of '("rb" "rtc"))
    (when (have-element-in-scope-p "ruby")
      (generate-implied-end-tags)
      (unless (string-equal "ruby" (dom:tag-name current-node))
        (parse-error)))
    (insert-html-element-for-token))

   ((a-start-tag-whose-tag-name-is-one-of '("rp" "rt"))
    (when (have-element-in-scope-p "ruby")
      (generate-implied-end-tags :except "rtc")
      (unless (or (string-equal "rtc" (dom:tag-name current-node))
                  (string-equal "ruby" (dom:tag-name current-node)))
        (parse-error)))
    (insert-html-element-for-token))

   ((a-start-tag-whose-tag-name-is "math")
    (reconstruct-active-formatting-elements)
    (adjust-mathml-attributes)
    (adjust-foreign-attributes)
    (insert-foreign-element-for-token dom:mathml-namespace)
    (when (slot-value current-token 'self-closing-flag)
      (pop stack-of-open-elements)
      (acknowledge-token-self-closing-flag)))

   ((a-start-tag-whose-tag-name-is "svg")
    (reconstruct-active-formatting-elements)
    (adjust-svg-attributes)
    (adjust-foreign-attributes)
    (insert-foreign-element-for-token dom:svg-namespace)
    (when (slot-value current-token 'self-closing-flag)
      (pop stack-of-open-elements)
      (acknowledge-token-self-closing-flag)))

   ((a-start-tag-whose-tag-name-is-one-of '("caption" "col" "colgroup"
                                            "frame" "head" "tbody" "td"
                                            "tfoot" "th" "thead" "tr"))
    (parse-error)
    (ignore-token))

   ((typep current-token 'start-tag)
    (reconstruct-active-formatting-elements)
    (insert-html-element-for-token))

   ;; https://github.com/html5lib/html5lib-python/blob/master/html5lib/html5parser.py#L1635
   ((typep current-token 'end-tag)
    (loop for node in stack-of-open-elements
          do (if (string-equal (dom:tag-name node)
                               (slot-value current-token 'tag-name))
                 (progn
                   (generate-implied-end-tags)
                   (unless (string-equal (dom:tag-name (first stack-of-open-elements))
                                         (slot-value current-token 'tag-name))
                     (parse-error))
                   (loop for n = (pop stack-of-open-elements)
                         until (eq n node))
                   (return))
               (progn
                 (when (special-element-p node)
                   (parse-error)
                   (ignore-token)
                   (return))))))))

(define-parser-insertion-mode text
  (cond
   ((typep current-token 'cl:character)
    (insert-character))

   ((typep current-token 'end-of-file)
    (parse-error)
    (pop stack-of-open-elements)
    (switch-insertion-mode original-insertion-mode)
    (reprocess-current-token))

   ;; Don't need this because we don't have a script executing environment
   ;; ((an-end-tag-whose-tag-name-is "script"))

   ((typep current-token 'end-tag)
    (pop stack-of-open-elements)
    (switch-insertion-mode original-insertion-mode))))

(define-parser-insertion-mode in-table
  (cond
   ((and (member (dom:tag-name current-node)
                 '("table" "tbody" "tfoot" "thead" "tr")
                 :test 'string-equal)
         (typep current-token 'cl:character))
    (setf pending-table-character-tokens nil)
    (setf original-insertion-mode insertion-mode)
    (switch-insertion-mode 'in-table-text)
    (reprocess-current-token))

   ((typep current-token 'comment-token)
    (insert-comment))

   ((typep current-token 'doctype-token)
    (parse-error)
    (ignore-token))

   ((a-start-tag-whose-tag-name-is "caption")
    (clear-stack-back-to-table-context)
    (appendf active-formatting-elements (list (make-marker)))
    (insert-html-element-for-token)
    (switch-insertion-mode 'in-caption))

   ((a-start-tag-whose-tag-name-is "colgroup")
    (clear-stack-back-to-table-context)
    (insert-html-element-for-token)
    (switch-insertion-mode 'in-column-group))

   ((a-start-tag-whose-tag-name-is "col")
    (clear-stack-back-to-table-context)
    (let ((current-token (make-instance 'start-tag :tag-name "colgroup")))
      (insert-html-element-for-token))
    (switch-insertion-mode 'in-column-group)
    (reprocess-current-token))

   ((a-start-tag-whose-tag-name-is-one-of '("tbody" "tfoot" "thead"))
    (clear-stack-back-to-table-context)
    (insert-html-element-for-token)
    (switch-insertion-mode 'in-table-body))

   ((a-start-tag-whose-tag-name-is-one-of '("td" "th" "tr"))
    (clear-stack-back-to-table-context)
    (let ((current-token (make-instance 'start-tag :tag-name "tbody")))
      (insert-html-element-for-token))
    (switch-insertion-mode 'in-table-body)
    (reprocess-current-token))

   ((a-start-tag-whose-tag-name-is "table")
    (parse-error)
    (if (not (have-element-in-table-scope-p "table"))
        (ignore-token)
      (progn
        (loop for element = (pop stack-of-open-elements)
              until (string-equal "table" (dom:tag-name element)))
        (reset-insertion-mode-appropriately)
        (reprocess-current-token))))

   ((an-end-tag-whose-tag-name-is "table")
    (if (not (have-element-in-table-scope-p "table"))
        (progn
          (parse-error)
          (ignore-token))
      (progn
        (loop for element = (pop stack-of-open-elements)
              until (string-equal "table" (dom:tag-name element)))
        (reset-insertion-mode-appropriately))))

   ((an-end-tag-whose-tag-name-is-one-of '("body" "caption" "col"
                                           "colgroup" "html" "tbody"
                                           "td" "tfoot" "th" "thead" "tr"))
    (parse-error)
    (ignore-token))

   ((or (a-start-tag-whose-tag-name-is-one-of '("style" "script" "template"))
        (an-end-tag-whose-tag-name-is "template"))
    (process-token-using-in-head-insertion-mode))

   ((a-start-tag-whose-tag-name-is "input")
    (let ((attribute (find "type" (slot-value current-token 'attributes)
                           :test 'equal
                           :key 'attribute-name)))
      (if (or (null attribute)
              (not (string-equal "hidden" (attribute-value attribute))))
          ;; Same as T
          (progn
            (parse-error)
            (setf foster-parenting t)
            (process-token-using-in-body-insertion-mode)
            (setf foster-parenting nil))
        (progn
          (parse-error)
          (insert-html-element-for-token)
          (pop stack-of-open-elements)
          (acknowledge-token-self-closing-flag)))))

   ((a-start-tag-whose-tag-name-is "form")
    (parse-error)
    (if (or (find "template" stack-of-open-elements
                  :test 'string-equal
                  :key 'dom:tag-name)
            form-element-pointer)
        (ignore-token)
      (progn
        (let ((element (insert-html-element-for-token)))
          (setf form-element-pointer element))
        (pop stack-of-open-elements))))

   ((typep current-token 'end-of-file)
    (process-token-using-in-body-insertion-mode))

   (t
    (parse-error)
    (setf foster-parenting t)
    (process-token-using-in-body-insertion-mode)
    (setf foster-parenting nil))))

(define-parser-insertion-mode in-table-text
  (cond
   ((eq #\null current-token)
    (parse-error)
    (ignore-token))

   ((typep current-token 'cl:character)
    (appendf pending-table-character-tokens (list current-token)))

   (t
    (if (find-if-not 'ascii-whitespace-p pending-table-character-tokens)
        (progn
          (parse-error)
          (loop with previous-token = current-token
                for character-token in pending-table-character-tokens
                do (progn
                     (setf current-token character-token)
                     ;; Same as T in `in-table` insertion mode
                     (parse-error)
                     (setf foster-parenting t)
                     (process-token-using-in-body-insertion-mode)
                     (setf foster-parenting nil))
                finally (setf current-token previous-token)))
      (progn
        (loop for character-token in pending-table-character-tokens
              do (insert-character character-token))
        (switch-insertion-mode original-insertion-mode)
        (reprocess-current-token))))))

(define-parser-insertion-mode in-caption
  (cond
   ((an-end-tag-whose-tag-name-is "caption")
    (if (not (have-element-in-table-scope-p "caption"))
        (progn
          (parse-error)
          (ignore-token))
      (generate-implied-end-tags))
    (unless (string-equal "caption" (dom:tag-name current-node))
      (parse-error))
    (loop for element = (pop stack-of-open-elements)
          until (string-equal "caption" (dom:tag-name element)))
    (clear-active-formatting-elements-upto-last-marker)
    (switch-insertion-mode 'in-table))

   ((or (a-start-tag-whose-tag-name-is-one-of '("caption" "col" "colgroup"
                                                "tbody" "td" "tfoot"
                                                "th" "thead" "tr"))
        (an-end-tag-whose-tag-name-is "table"))
    (if (not (have-element-in-table-scope-p "caption"))
        (progn
          (parse-error)
          (ignore-token))
      (generate-implied-end-tags))
    (unless (equal "caption" (dom:tag-name current-node))
      (parse-error))
    (loop for element = (pop stack-of-open-elements)
          until (string-equal "caption" (dom:tag-name element)))
    (clear-active-formatting-elements-upto-last-marker)
    (switch-insertion-mode 'in-table)
    (reprocess-current-token))

   ((an-end-tag-whose-tag-name-is-one-of '("body" "col" "colgroup" "html"
                                           "tbody" "td" "tfoot" "th" "thead" "tr"))
    (parse-error)
    (ignore-token))

   (t
    (process-token-using-in-body-insertion-mode))))

(define-parser-insertion-mode in-column-group
  (cond
   ((or (eq #\tab current-token) (eq #\newline current-token)
        (eq #\page current-token) (eq #\return current-token) (eq #\space current-token))
    (insert-character))

   ((typep current-token 'comment-token)
    (insert-comment))

   ((typep current-token 'doctype-token)
    (parse-error)
    (ignore-token))

   ((a-start-tag-whose-tag-name-is "html")
    (process-token-using-in-body-insertion-mode))

   ((a-start-tag-whose-tag-name-is "col")
    (insert-html-element-for-token)
    (pop stack-of-open-elements)
    (acknowledge-token-self-closing-flag))

   ((an-end-tag-whose-tag-name-is "colgroup")
    (if (not (string-equal "colgroup" (dom:tag-name current-node)))
        (progn
          (parse-error)
          (ignore-token))
      (progn
        (pop stack-of-open-elements)
        (switch-insertion-mode 'in-table))))

   ((an-end-tag-whose-tag-name-is "col")
    (parse-error)
    (ignore-token))

   ((or (a-start-tag-whose-tag-name-is "template")
        (an-end-tag-whose-tag-name-is "template"))
    (process-token-using-in-head-insertion-mode))

   ((typep current-token 'end-of-file)
    (process-token-using-in-body-insertion-mode))

   (t
    (if (not (string-equal "colgroup" (dom:tag-name current-node)))
        (progn
          (parse-error)
          (ignore-token))
      (pop stack-of-open-elements))
    (switch-insertion-mode 'in-table)
    (reprocess-current-token))))

(define-parser-insertion-mode in-table-body
  (cond
   ((a-start-tag-whose-tag-name-is "tr")
    (clear-stack-back-to-table-body-context)
    (insert-html-element-for-token)
    (switch-insertion-mode 'in-row))

   ((a-start-tag-whose-tag-name-is-one-of '("th" "td"))
    (parse-error)
    (clear-stack-back-to-table-body-context)
    (let ((current-token (make-instance 'start-tag :tag-name "tr")))
      (insert-html-element-for-token))
    (switch-insertion-mode 'in-row)
    (reprocess-current-token))

   ((an-end-tag-whose-tag-name-is-one-of '("tbody" "tfoot" "thead"))
    (if (not (have-element-in-table-scope-p (slot-value current-token 'tag-name)))
        (progn
          (parse-error)
          (ignore-token))
      (progn
        (clear-stack-back-to-table-body-context)
        (pop stack-of-open-elements)
        (switch-insertion-mode 'in-table))))

   ((or (a-start-tag-whose-tag-name-is-one-of '("caption" "col" "colgroup" "tbody"
                                                "tfoot" "thead"))
        (an-end-tag-whose-tag-name-is "table"))
    (if (not (or (have-element-in-table-scope-p "tbody")
                 (have-element-in-table-scope-p "thead")
                 (have-element-in-table-scope-p "tfoot")))
        (progn
          (parse-error)
          (ignore-token))
      (progn
        (clear-stack-back-to-table-body-context)
        (pop stack-of-open-elements)
        (switch-insertion-mode 'in-table)
        (reprocess-current-token))))

   ((an-end-tag-whose-tag-name-is-one-of '("body" "caption" "col" "colgroup"
                                           "html" "td" "th" "tr"))
    (parse-error)
    (ignore-token))

   (t
    (process-token-using-in-table-insertion-mode))))

(define-parser-insertion-mode in-row
  (cond
   ((a-start-tag-whose-tag-name-is-one-of '("th" "td"))
    (clear-stack-back-to-table-row-context)
    (insert-html-element-for-token)
    (switch-insertion-mode 'in-cell)
    (appendf active-formatting-elements (list (make-marker))))

   ((an-end-tag-whose-tag-name-is "tr")
    (if (not (have-element-in-table-scope-p "tr"))
        (progn
          (parse-error)
          (ignore-token))
      (progn
        (clear-stack-back-to-table-row-context)
        (assert (string-equal "tr" (dom:tag-name current-node)))
        (pop stack-of-open-elements)
        (switch-insertion-mode 'in-table-body))))

   ((or (a-start-tag-whose-tag-name-is-one-of '("caption" "col" "colgroup"
                                                "tbody" "tfoot" "thead" "tr"))
        (an-end-tag-whose-tag-name-is "table"))
    (if (not (have-element-in-table-scope-p "tr"))
        (progn
          (parse-error)
          (ignore-token))
      (progn
        (clear-stack-back-to-table-row-context)
        (assert (string-equal "tr" (dom:tag-name current-node)))
        (pop stack-of-open-elements)
        (switch-insertion-mode 'in-table-body)
        (reprocess-current-token))))

   ((an-end-tag-whose-tag-name-is-one-of '("tbody" "tfoot" "thead"))
    (when (not (have-element-in-table-scope-p (slot-value current-token 'tag-name)))
      (parse-error)
      (ignore-token))
    (if (not (have-element-in-table-scope-p "tr"))
        (ignore-token)
      (progn
        (clear-stack-back-to-table-row-context)
        (assert (string-equal "tr" (dom:tag-name current-node)))
        (pop stack-of-open-elements)
        (switch-insertion-mode 'in-table-body)
        (reprocess-current-token))))

   ((an-end-tag-whose-tag-name-is-one-of '("body" "caption" "col" "colgroup"
                                           "html" "td" "th"))
    (parse-error)
    (ignore-token))

   (t
    (process-token-using-in-table-insertion-mode))))

(define-parser-insertion-mode in-cell
  (cond
   ((an-end-tag-whose-tag-name-is-one-of '("td" "th"))
    (if (not (have-element-in-table-scope-p (slot-value current-token 'tag-name)))
        (progn
          (parse-error)
          (ignore-token))
      (generate-implied-end-tags))
    (unless (string-equal (dom:tag-name current-node)
                          (slot-value current-token 'tag-name))
      (parse-error))
    (loop for element = (pop stack-of-open-elements)
          until (string-equal (dom:tag-name element)
                              (slot-value current-token 'tag-name)))
    (clear-active-formatting-elements-upto-last-marker)
    (switch-insertion-mode 'in-row))

   ((a-start-tag-whose-tag-name-is-one-of '("caption" "col" "colgroup"
                                            "tbody" "td" "tfoot"
                                            "th" "thead" "tr"))
    (if (not (or (have-element-in-table-scope-p "td")
                 (have-element-in-table-scope-p "th")))
        (progn
          (parse-error)
          (ignore-token))
      (progn
        (close-cell)
        (reprocess-current-token))))

   ((an-end-tag-whose-tag-name-is-one-of '("body" "caption" "col" "colgroup" "html"))
    (parse-error)
    (ignore-token))

   ((an-end-tag-whose-tag-name-is-one-of '("table" "tbody" "tfoot" "thead" "tr"))
    (if (not (have-element-in-table-scope-p (slot-value current-token 'tag-name)))
        (progn
          (parse-error)
          (ignore-token))
      (progn
        (close-cell)
        (reprocess-current-token))))

   (t
    (process-token-using-in-body-insertion-mode))))

(define-parser-insertion-mode in-select
  (cond
   ((eq #\null current-token)
    (parse-error)
    (ignore-token))

   ((typep current-token 'cl:character)
    (insert-character))

   ((typep current-token 'comment-token)
    (insert-comment))

   ((typep current-token 'doctype-token)
    (parse-error)
    (ignore-token))

   ((a-start-tag-whose-tag-name-is "html")
    (process-token-using-in-body-insertion-mode))

   ((a-start-tag-whose-tag-name-is "option")
    (when (string-equal "option" (dom:tag-name current-node))
      (pop stack-of-open-elements))
    (insert-html-element-for-token))

   ((a-start-tag-whose-tag-name-is "optgroup")
    (when (string-equal "option" (dom:tag-name current-node))
      (pop stack-of-open-elements))
    (when (string-equal "optgroup" (dom:tag-name current-node))
      (pop stack-of-open-elements))
    (insert-html-element-for-token))

   ((an-end-tag-whose-tag-name-is "optgroup")
    (when (and (string-equal "option" (dom:tag-name current-node))
               (string-equal "optgroup" (dom:tag-name (second stack-of-open-elements))))
      (pop stack-of-open-elements))
    (if (string-equal "optgroup" (dom:tag-name current-node))
        (pop stack-of-open-elements)
      (progn
        (parse-error)
        (ignore-token))))

   ((an-end-tag-whose-tag-name-is "option")
    (if (string-equal "option" (dom:tag-name current-node))
        (pop stack-of-open-elements)
      (progn
        (parse-error)
        (ignore-token))))

   ((an-end-tag-whose-tag-name-is "select")
    (if (not (have-element-in-select-scope-p "select"))
        (progn
          (parse-error)
          (ignore-token))
      (progn
        (loop for element = (pop stack-of-open-elements)
              until (string-equal "select" (dom:tag-name element)))
        (reset-insertion-mode-appropriately))))

   ((a-start-tag-whose-tag-name-is "select")
    (parse-error)
    (if (not (have-element-in-select-scope-p "select"))
        (ignore-token)
      (progn
        (loop for element = (pop stack-of-open-elements)
              until (string-equal "select" (dom:tag-name element)))
        (reset-insertion-mode-appropriately))))

   ((a-start-tag-whose-tag-name-is-one-of '("input" "keygen" "textarea"))
    (parse-error)
    (if (not (have-element-in-select-scope-p "select"))
        (ignore-token)
      (progn
        (loop for element = (pop stack-of-open-elements)
              until (string-equal "select" (dom:tag-name element)))
        (reset-insertion-mode-appropriately)
        (reprocess-current-token))))

   ((or (a-start-tag-whose-tag-name-is-one-of '("script" "template"))
        (an-end-tag-whose-tag-name-is "template"))
    (process-token-using-in-head-insertion-mode))

   ((typep current-token 'end-of-file)
    (process-token-using-in-body-insertion-mode))

   (t
    (parse-error)
    (ignore-token))))

(define-parser-insertion-mode in-select-in-table
  (cond
   ((a-start-tag-whose-tag-name-is-one-of '("caption" "table" "tbody"
                                            "tfoot" "thead" "tr" "td" "th"))
    (parse-error)
    (loop for element = (pop stack-of-open-elements)
          until (string-equal "select" (dom:tag-name element)))
    (reset-insertion-mode-appropriately)
    (reprocess-current-token))

   ((an-end-tag-whose-tag-name-is-one-of '("caption" "table" "tbody"
                                           "tfoot" "thead" "tr" "td" "th"))
    (parse-error)
    (if (not (have-element-in-table-scope-p (slot-value current-token 'tag-name)))
        (ignore-token)
      (progn
        (loop for element = (pop stack-of-open-elements)
              until (string-equal "select" (dom:tag-name element)))
        (reset-insertion-mode-appropriately)
        (reprocess-current-token))))

   (t
    (process-token-using-in-select-insertion-mode))))

(define-parser-insertion-mode in-template
  (cond
   ((or (typep current-token 'cl:character)
        (typep current-token 'comment-token)
        (typep current-token 'doctype-token))
    (process-token-using-in-body-insertion-mode))

   ((or (a-start-tag-whose-tag-name-is-one-of '("base" "basefont" "bgsound"
                                                "link" "meta" "noframes"
                                                "script" "style" "template" "title"))
        (an-end-tag-whose-tag-name-is "template"))
    (process-token-using-in-head-insertion-mode))

   ((a-start-tag-whose-tag-name-is-one-of '("caption" "colgroup" "tbody"
                                            "tfoot" "thead"))
    (pop stack-of-template-insertion-modes)
    (push 'in-table stack-of-template-insertion-modes)
    (switch-insertion-mode 'in-table)
    (reprocess-current-token))

   ((a-start-tag-whose-tag-name-is "col")
    (pop stack-of-template-insertion-modes)
    (push 'in-column-group stack-of-template-insertion-modes)
    (switch-insertion-mode 'in-column-group)
    (reprocess-current-token))

   ((a-start-tag-whose-tag-name-is "tr")
    (pop stack-of-template-insertion-modes)
    (push 'in-table-body stack-of-template-insertion-modes)
    (switch-insertion-mode 'in-table-body)
    (reprocess-current-token))

   ((a-start-tag-whose-tag-name-is-one-of '("td" "th"))
    (pop stack-of-template-insertion-modes)
    (push 'in-row stack-of-template-insertion-modes)
    (switch-insertion-mode 'in-row)
    (reprocess-current-token))

   ((typep current-token 'start-tag)
    (pop stack-of-template-insertion-modes)
    (push 'in-body stack-of-template-insertion-modes)
    (switch-insertion-mode 'in-body)
    (reprocess-current-token))

   ((typep current-token 'end-tag)
    (parse-error)
    (ignore-token))

   ((typep current-token 'end-of-file)
    (if (not (find "template" stack-of-open-elements
                   :test 'string-equal
                   :key 'dom:tag-name))
        (stop-parsing)
      (parse-error))
    (loop for element = (pop stack-of-open-elements)
          until (string-equal "template" (dom:tag-name element)))
    (clear-active-formatting-elements-upto-last-marker)
    (pop stack-of-template-insertion-modes)
    (reset-insertion-mode-appropriately)
    (reprocess-current-token))))

(define-parser-insertion-mode after-body
  (cond
   ((or (eq #\tab current-token) (eq #\newline current-token)
        (eq #\page current-token) (eq #\return current-token) (eq #\space current-token))
    (process-token-using-in-body-insertion-mode))

   ;; TODO
   ((typep current-token 'comment-token))

   ((typep current-token 'doctype-token)
    (parse-error)
    (ignore-token))

   ((a-start-tag-whose-tag-name-is "html")
    (process-token-using-in-body-insertion-mode))

   ((an-end-tag-whose-tag-name-is "html")
    ;; TODO
    (switch-insertion-mode 'after-after-body))

   ((typep current-token 'end-of-file)
    (stop-parsing))

   (t
    (parse-error)
    (switch-insertion-mode 'in-body)
    (reprocess-current-token))))

(define-parser-insertion-mode in-frameset
  (cond
   ((or (eq #\tab current-token) (eq #\newline current-token)
        (eq #\page current-token) (eq #\return current-token) (eq #\space current-token))
    (insert-character))

   ((typep current-token 'comment-token)
    (insert-comment))

   ((typep current-token 'doctype-token)
    (parse-error)
    (ignore-token))

   ((a-start-tag-whose-tag-name-is "html")
    (process-token-using-in-body-insertion-mode))

   ((a-start-tag-whose-tag-name-is "frameset")
    (insert-html-element-for-token))

   ;; TODO
   ((an-end-tag-whose-tag-name-is "frameset")
    (pop stack-of-open-elements)
    (switch-insertion-mode 'after-frameset))

   ((a-start-tag-whose-tag-name-is "frame")
    (insert-html-element-for-token)
    (pop stack-of-open-elements)
    (acknowledge-token-self-closing-flag))

   ((a-start-tag-whose-tag-name-is "noframes")
    (process-token-using-in-head-insertion-mode))

   ;; TODO
   ((typep current-token 'end-of-file)
    (when (not (string-equal "html" (dom:tag-name current-node)))
      (parse-error))
    (stop-parsing))

   (t
    (parse-error)
    (ignore-token))))

(define-parser-insertion-mode after-frameset
  (cond
   ((or (eq #\tab current-token) (eq #\newline current-token)
        (eq #\page current-token) (eq #\return current-token) (eq #\space current-token))
    (insert-character))

   ((typep current-token 'comment-token)
    (insert-comment))

   ((typep current-token 'doctype-token)
    (parse-error)
    (ignore-token))

   ((a-start-tag-whose-tag-name-is "html")
    (process-token-using-in-body-insertion-mode))

   ((an-end-tag-whose-tag-name-is "html")
    (switch-insertion-mode 'after-after-frameset))

   ((a-start-tag-whose-tag-name-is "noframes")
    (process-token-using-in-head-insertion-mode))

   ((typep current-token 'end-of-file)
    (stop-parsing))

   (t
    (parse-error)
    (ignore-token))))


(define-parser-insertion-mode after-after-body
  (cond
   ;; TODO
   ((typep current-token 'comment-token))

   ((or (typep current-token 'doctype-token)
        (or (eq #\tab current-token) (eq #\newline current-token)
            (eq #\page current-token) (eq #\return current-token) (eq #\space current-token))
        (a-start-tag-whose-tag-name-is "html"))
    (process-token-using-in-body-insertion-mode))

   ((typep current-token 'end-of-file)
    (stop-parsing))

   (t
    (parse-error)
    (switch-insertion-mode 'in-body)
    (reprocess-current-token))))

(define-parser-insertion-mode after-after-frameset
  (cond
   ;; TODO
   ((typep current-token 'comment-token))

   ((or (typep current-token 'doctype-token)
        (or (eq #\tab current-token) (eq #\newline current-token)
            (eq #\page current-token) (eq #\return current-token) (eq #\space current-token))
        (a-start-tag-whose-tag-name-is "html"))
    (process-token-using-in-body-insertion-mode))

   ((typep current-token 'end-of-file)
    (stop-parsing))

   ((a-start-tag-whose-tag-name-is "noframes")
    (process-token-using-in-head-insertion-mode))

   (t
    (parse-error)
    (ignore-token))))

(defmacro process-token-in-foreign-content ()
  `(cond
    ((eq #\null current-token)
     (parse-error)
     (insert-character +replacement-character+))
    ((or (eq #\tab current-token) (eq #\newline current-token)
         (eq #\page current-token) (eq #\return current-token)
         (eq #\space current-token))
     (insert-character))
    ((typep current-token 'cl:character)
     (insert-character)
     (setf frameset-ok-flag nil))
    ((typep current-token 'doctype-token)
     (parse-error)
     (ignore-token))
    ((or (a-start-tag-whose-tag-name-is-one-of '("b" "big" "blockquote" "body" "br" "center" "code" "dd" "div" "dl" "dt" "em" "embed" "h1" "h2" "h3" "h4" "h5" "h6" "head" "hr" "i" "img" "li" "listing" "menu" "meta" "nobr" "ol" "p" "pre" "ruby" "s" "small" "span" "strong" "strike" "sub" "sup" "table" "tt" "u" "ul" "var"))
         )
     (parse-error)
     (pop stack-of-open-elements)
     (reprocess-current-token))
    ((typep current-token 'start-tag)
     (let ((adjusted-current-node (adjusted-current-node)))
       (insert-foreign-element-for-token (dom:namespace-uri adjusted-current-node))))
    ((typep current-token 'end-tag))))

(defstruct marker)

(defun a-start-tag-whose-tag-name-is (name)
  (and (typep current-token 'start-tag)
       (string-equal name (slot-value current-token 'tag-name))))

(defun a-start-tag-whose-tag-name-is-one-of (names)
  (and (typep current-token 'start-tag)
       (member (slot-value current-token 'tag-name) names
               :test 'string-equal)))

(defun an-end-tag-whose-tag-name-is (name)
  (and (typep current-token 'end-tag)
       (string-equal name (slot-value current-token 'tag-name))))

(defun an-end-tag-whose-tag-name-is-one-of (names)
  (and (typep current-token 'end-tag)
       (member (slot-value current-token 'tag-name) names
               :test 'string-equal)))

(defun parse-error ()
  (cerror "Continue" 'parse-error))

(defun ignore-token ())

(defun adjusted-current-node ()
  ;; TODO: Case for HTML fragment parsing
  current-node)

(defun insert-character (&optional (token current-token))
  (let ((data token))
    (let ((adjusted-insertion-location (appropriate-place-for-inserting-node)))
      (when (typep adjusted-insertion-location 'document)
        (return-from insert-character))
      (let ((text (dom:last-child adjusted-insertion-location)))
        (if (typep text 'text)
            (write-char data (slot-value text 'data-stream))
          (let ((text (make-instance 'text :data-stream (make-string-output-stream))))
            (write-char data (slot-value text 'data-stream))
            (push text open-texts)
            (append-child adjusted-insertion-location text)))))))

(defun insert-comment (&optional position)
  (declare (ignore position)))

(defparameter *html-element-table* nil)

(defun make-html-element-table ()
  (setf *html-element-table* (make-hash-table :test 'equal))
  (loop for symbol in '(a abbr address area article aside audio
                          b base bdi bdo blockquote body br button
                          canvas caption cite code col colgroup
                          data datalist dd del details dfn dialog div dl dt
                          em embed
                          fieldset figcaption figure footer form
                          h1 h2 h3 h4 h5 h6 head header hgroup hr html
                          i iframe img input ins
                          kbd
                          label legend li link
                          main mark menu menuitem meta meter
                          nav noscript
                          object ol optgroup option output
                          p param picture pre progress
                          q
                          rb rp rt rtc ruby
                          s samp script section select slot small source
                          span strong style sub summary sup
                          table tbody td template textarea tfoot th thead title tr track
                          u ul
                          var video
                          wbr)
        do (setf (gethash (string-downcase (symbol-name symbol))
                          *html-element-table*)
                 symbol)
        finally (return *html-element-table*)))

(defparameter *svg-element-table* nil)

(defun make-svg-element-table ()
  (setf *svg-element-table* (make-hash-table :test 'equal))
  (loop for symbol in '(svg:svg svg:g svg:defs svg:desc svg:metadata
                                svg:title svg:symbol svg:use svg:switch
                                svg:path svg:rect svg:circle svg:ellipse
                                svg:line svg:polyline svg:polygon svg:text
                                svg:tspan svg:text-path svg:image
                                svg:foreign-object svg:marker svg:a svg:view)
        do (setf (gethash (string-downcase (symbol-name symbol))
                          *svg-element-table*)
                 symbol)
        finally (return *svg-element-table*)))

;; TODO: Respect parent
(defun create-element-for-token (&optional namespace parent)
  (unless *html-element-table* (make-html-element-table))
  (unless *svg-element-table* (make-svg-element-table))
  (let ((tag-name (slot-value current-token 'tag-name)))
    (let ((element-class (cond
                          ((or (null namespace)
                               (equal dom:html-namespace namespace))
                           (or (gethash tag-name *html-element-table*)
                             'element))
                          ((equal dom:svg-namespace namespace)
                           (or (gethash tag-name *svg-element-table*)
                             'svg:element))
                          (t 'element))))
      (let ((element (make-instance element-class
                                    :local-name tag-name
                                    :namespace namespace)))
        (loop for attribute in (slot-value current-token 'attributes)
           do (dom:set-attribute
               element
               (attribute-name attribute)
               (attribute-value attribute)))
        element))))

(defun insert-html-element-for-token ()
  (insert-foreign-element-for-token dom:html-namespace))

(defun insert-foreign-element-for-token (namespace)
  (let ((adjusted-insertion-location (appropriate-place-for-inserting-node)))
    (let ((element (create-element-for-token namespace)))
      (append-child adjusted-insertion-location element)
      (push element stack-of-open-elements)
      element)))

;; TODO
(defun appropriate-place-for-inserting-node (&optional override-target)
  (let ((target (if override-target
                    override-target
                    current-node)))
    target))

;; TODO: Check exact node condition
(defun have-element-in-specific-scope-p (target scope)
  (check-type target (or element string))
  (loop for open-element in stack-of-open-elements
     if (typecase target
          (element (eq target open-element))
          (string (string-equal target (dom:tag-name open-element))))
     do (return t)
     else if (typecase scope
               (list (typecase target
                       (element (find (dom:tag-name target) scope
                                      :test 'string-equal))
                       (string (find target scope
                                     :test 'string-equal))))
               (function (typecase target
                           (element (funcall scope (dom:tag-name target)))
                           (string (funcall scope target)))))
     do (return)))

(defun have-element-in-scope-p (element)
  (have-element-in-specific-scope-p
   element
   '("applet"
     "caption"
     "html"
     "table"
     "td"
     "th"
     "marquee"
     "object"
     "template"
     ;; MathML
     "mi"
     "mo"
     "mn"
     "ms"
     "mtext"
     "annotation-xml"
     ;; SVG
     "foreignObject"
     "desc"
     "title")))

(defun have-element-in-list-item-scope-p (element)
  (have-element-in-specific-scope-p
   element
   '("applet"
     "caption"
     "html"
     "table"
     "td"
     "th"
     "marquee"
     "object"
     "template"
     ;; MathML
     "mi"
     "mo"
     "mn"
     "ms"
     "mtext"
     "annotation-xml"
     ;; SVG
     "foreignObject"
     "desc"
     "title"
     ;; additional
     "ol"
     "ul")))

(defun have-element-in-button-scope-p (element)
  (have-element-in-specific-scope-p
   element
   '("applet"
     "caption"
     "html"
     "table"
     "td"
     "th"
     "marquee"
     "object"
     "template"
     ;; MathML
     "mi"
     "mo"
     "mn"
     "ms"
     "mtext"
     "annotation-xml"
     ;; SVG
     "foreignObject"
     "desc"
     "title"
     ;; additional
     "button")))

(defun have-element-in-table-scope-p (element)
  (have-element-in-specific-scope-p
   element
   '("html"
     "table"
     "template")))

(defun have-element-in-select-scope-p (element)
  (have-element-in-specific-scope-p
   element
   (lambda (tag-name)
     (not (member tag-name '("optgroup" "option") :test 'string-equal)))))

(defun special-element-p (element)
  (let ((tag-name (typecase element
                    (element (dom:tag-name element))
                    (string element))))
    (member tag-name
            '("address" "applet" "area" "article" "aside"
              "base" "basefont" "bgsound" "blockquote"
              "body" "br" "button" "caption" "center" "col"
              "colgroup" "dd" "details" "dir" "div" "dl"
              "dt" "embed" "fieldset" "figcaption" "figure"
              "footer" "form" "frame" "frameset" "h1" "h2" "h3"
              "h4" "h5" "h6" "head" "header" "hgroup" "hr"
              "html" "iframe" "img" "input" "keygen" "li" "link"
              "listing" "main" "marquee" "menu" "meta" "nav"
              "noembed" "noframes" "noscript" "object" "ol" "p"
              "param" "plaintext" "pre" "script" "section" "select"
              "source" "style" "summary" "table" "tbody" "td"
              "template" "textarea" "tfoot" "th" "thead" "title"
              "tr" "track" "ul" "wbr" "xmp"
              ;; MathML
              "mi" "mo" "mn" "ms" "mtext" "annotation-xml"
              ;; SVG
              "foreignObject" "desc" "title")
            :test 'string-equal)))

(defun formatting-element-p (element)
  (let ((tag-name (typecase element
                    (element (dom:tag-name element))
                    (string element))))
    (member tag-name
            '("a" "b" "big" "code" "em" "font" "i" "nobr" "s"
              "small" "strike" "strong" "tt" "u")
            :test 'string-equal)))

(defun ordinary-element-p (element)
  (not (or (special-element-p element)
           (formatting-element-p element))))

(defun parse-generic-rawtext-element ()
  (insert-html-element-for-token)
  (setf state 'rawtext-state)
  (setf original-insertion-mode insertion-mode)
  (setf insertion-mode 'text))

(defun parse-generic-rcdata-element ()
  (insert-html-element-for-token)
  (setf state 'rcdata-state)
  (setf original-insertion-mode insertion-mode)
  (setf insertion-mode 'text))

(defun reconstruct-active-formatting-elements ()
  (when (null active-formatting-elements)
    (return-from reconstruct-active-formatting-elements))
  (let* ((i (1- (length active-formatting-elements)))
         (entry (nth* i active-formatting-elements)))
    (when (or (typep entry 'marker)
              (find entry stack-of-open-elements))
      (return-from reconstruct-active-formatting-elements))
    (loop while (and (not (typep entry 'marker))
                     (not (find entry stack-of-open-elements)))
          do (when (= i 0)
               (setf i -1)
               (return))
          (decf i)
          (setf entry (nth* i active-formatting-elements)))
    (loop
     (incf i)
     (setf entry (nth* i active-formatting-elements))
     (let ((current-token (make-instance 'start-tag
                                         ;; TODO: attributes?
                                         :tag-name (string-downcase (dom:tag-name entry)))))
       (let ((element (insert-html-element-for-token)))
         (setf (nth* i active-formatting-elements) element)
         (when (eq element (nth* -1 active-formatting-elements))
           (return)))))))

(defun clear-active-formatting-elements-upto-last-marker ()
  (reversef active-formatting-elements)
  (let ((entry (pop active-formatting-elements)))
    (loop while (and active-formatting-elements
                     (not (typep entry 'marker)))
          do (setf entry (pop active-formatting-elements))))
  (reversef active-formatting-elements))

;; TODO
(defun push-onto-active-formatting-elements (element)
  (appendf active-formatting-elements (list element)))

(defun generate-implied-end-tags (&key except)
  (let ((tag-name (dom:tag-name current-node)))
    (when (and (member tag-name '("dd" "dt" "li" "optgroup" "option"
                                  "p" "rb" "rp" "rt" "rtc")
                       :test 'string-equal)
               (not (string-equal tag-name except)))
      (pop stack-of-open-elements)
      (generate-implied-end-tags :except except))))

(defun generate-all-implied-end-tags-thoroughly ()
  (let ((tag-name (dom:tag-name current-node)))
    (when (member tag-name '("caption" "colgroup" "dd" "dt"
                             "li" "optgroup" "option"
                             "p" "rb" "rp" "rt" "rtc" "tbody" "td"
                             "tfoot" "th" "thead" "tr")
                  :test 'string-equal)
      (pop stack-of-open-elements)
      (generate-all-implied-end-tags-thoroughly))))

(defun adoption-agency ()
  (let (subject
        node
        last-node
        outer-loop-counter
        inner-loop-counter
        formatting-element
        furthest-block
        common-ancestor
        bookmark
        element-above-node)
    ;; Step 1
    (setf subject (slot-value current-token 'tag-name))
    ;; Step 2
    (when (and (string-equal subject (dom:tag-name current-node))
               (not (find current-node active-formatting-elements)))
      (pop stack-of-open-elements)
      (return-from adoption-agency))
    ;; Step 3
    (setf outer-loop-counter 0)
    (tagbody
     :outer-loop
     ;; Step 4
     (when (>= outer-loop-counter 8)
       (return-from adoption-agency))
     ;; Step 5
     (incf outer-loop-counter)
     ;; Step 6
     (let ((last-marker-position (position-if 'marker-p active-formatting-elements
                                              :from-end t)))
       (setf formatting-element
             (find subject active-formatting-elements
                   :start (if last-marker-position
                              (1+ last-marker-position)
                            0)
                   :test 'string-equal
                   :key 'dom:tag-name
                   :from-end t))
       (unless formatting-element
         ;; Act as `any other end tag` entry from `in-body` insertion mode
         (loop for node in stack-of-open-elements
               do (if (string-equal (dom:tag-name node)
                                    (slot-value current-token 'tag-name))
                      (progn
                        (generate-implied-end-tags)
                        (unless (string-equal (dom:tag-name (first stack-of-open-elements))
                                              (slot-value current-token 'tag-name))
                          (parse-error))
                        (loop for n = (pop stack-of-open-elements)
                              until (eq n node))
                        (return))
                    (progn
                      (when (special-element-p node)
                        (parse-error)
                        (ignore-token)
                        (return)))))
         (return-from adoption-agency)))
     ;; Step 7
     (unless (find formatting-element stack-of-open-elements)
       (parse-error)
       (removef stack-of-open-elements formatting-element)
       (return-from adoption-agency))
     ;; Step 8
     (when (and (find formatting-element stack-of-open-elements)
                (not (have-element-in-scope-p formatting-element)))
       (parse-error)
       (return-from adoption-agency))
     ;; Step 9
     (unless (eq current-node formatting-element)
       (parse-error))
     ;; Step 10
     (let ((formatting-element-position (position formatting-element
                                                  stack-of-open-elements)))
       (when (and (> formatting-element-position 0)
                  (special-element-p (nth (1- formatting-element-position)
                                          stack-of-open-elements)))
         (setf furthest-block (nth (1- formatting-element-position)
                                   stack-of-open-elements))))
     ;; Step 11
     (unless furthest-block
       (loop for el = (pop stack-of-open-elements)
             until (eq el formatting-element))
       (removef active-formatting-elements formatting-element)
       (return-from adoption-agency))
     ;; Step 12
     (let ((formatting-element-position (position formatting-element
                                                  stack-of-open-elements)))
       (setf common-ancestor (nth (1+ formatting-element-position)
                                  stack-of-open-elements)))
     ;; Step 13
     (setf bookmark (position formatting-element active-formatting-elements))
     ;; Step 14
     (setf node furthest-block
           last-node furthest-block)
     ;; Step 14.1
     (setf inner-loop-counter 0)
     ;; Step 14.2
     :inner-loop
     (incf inner-loop-counter)
     ;; Step 14.3
     (let ((node-position (position node stack-of-open-elements)))
       (if node-position
           (setf node (nth (1+ node-position) stack-of-open-elements))
         (setf node element-above-node)))
     ;; Step 14.4
     (when (eq node formatting-element)
       (go :next-step-in-the-overall-algorithm))
     ;; Step 14.5
     (when (and (> inner-loop-counter 3)
                (find node active-formatting-elements))
       (removef active-formatting-elements node))
     ;; Step 14.6
     ;; This step may remove `node` from `stack-of-open-elements`,
     ;;   we need to record the `the element that was immediately above node
     ;;   in the stack of open elements before node was removed.` because
     ;;   step 14.3 need this.
     (unless (find node active-formatting-elements)
       (let ((node-position (position node stack-of-open-elements)))
         (setf element-above-node (nth (1+ node-position) stack-of-open-elements))
         (removef stack-of-open-elements node))
       (go :inner-loop))
     ;; Step 14.7
     (let ((element (create-element-for-token
                     dom:html-namespace
                     common-ancestor)))
       (let ((node-position (find node active-formatting-elements)))
         (setf (nth node-position active-formatting-elements) element))
       (let ((node-position (find node stack-of-open-elements)))
         (setf (nth node-position stack-of-open-elements) element))
       (setf node element))

     ;; Step 14.8
     (when (eq last-node furthest-block)
       (setf bookmark (1+ (position node active-formatting-elements))))
     ;; Step 14.9
     (when-let ((parent (dom:parent-node last-node)))
       (dom:remove-child parent last-node))
     (append-child last-node node)
     ;; Step 14.10
     (setf last-node node)
     ;; Step 14.11
     (go :inner-loop)
     :next-step-in-the-overall-algorithm
     ;; Step 15
     (let ((place (appropriate-place-for-inserting-node common-ancestor)))
       (append-child place last-node))
     ;; Step 16
     (let ((element (create-element-for-token
                     dom:html-namespace
                     furthest-block)))
       ;; Step 17
       (loop for child in (dom:node-list-nodes (dom:child-nodes furthest-block))
          do (append-child element child))
       (loop for child in (dom:node-list-nodes (dom:child-nodes furthest-block))
          do (dom:remove-child furthest-block child))
       ;; Step 18
       (append-child furthest-block element)
       ;; Step 19
       (removef active-formatting-elements formatting-element)
       (setf active-formatting-elements
             (append (subseq active-formatting-elements
                             0
                             bookmark)
                     (list element)
                     (subseq active-formatting-elements bookmark)))
       ;; Step 20
       (removef stack-of-open-elements formatting-element)
       (let ((furthest-block-position (position furthest-block
                                                stack-of-open-elements)))
         (setf stack-of-open-elements
               (append (subseq stack-of-open-elements 0 furthest-block-position)
                       (list element)
                       (subseq stack-of-open-elements furthest-block-position))))
       ;; Step 21
       (go :outer-loop)))))

(defun close-p-element ()
  (generate-implied-end-tags :except "p")
  (unless (string-equal "p" (dom:tag-name current-node))
    (parse-error))
  (loop for element = (pop stack-of-open-elements)
        until (string-equal "p" (dom:tag-name element))))

(defun close-cell ()
  (generate-implied-end-tags)
  (when (not (or (string-equal "td" (dom:tag-name current-node))
                 (string-equal "th" (dom:tag-name current-node))))
    (parse-error))
  (loop for element = (pop stack-of-open-elements)
        until (or (string-equal "td" (dom:tag-name element))
                  (string-equal "th" (dom:tag-name element))))
  (clear-active-formatting-elements-upto-last-marker)
  (setf insertion-mode 'in-row))

;; TODO
(defun clear-stack-back-to-table-context ())

;; TODO
(defun clear-stack-back-to-table-body-context ())

;; TODO
(defun clear-stack-back-to-table-row-context ())

(defun reset-insertion-mode-appropriately ()
  (let (last
        node
        ancestor)
    (macrolet ((switch-insertion-mode (insertion-mode)
                 `(setf insertion-mode ,insertion-mode)))
      (block nil
        ;; Step 1
        (setf last nil)
        ;; Step 2
        (setf node (first stack-of-open-elements))
        ;; Step 3
        (tagbody
         :loop
         (when (eq node (car (last stack-of-open-elements)))
           (progn
             (setf last t)
             #|TODO|#))
         ;; Step 4
         (when (string-equal "select" (dom:tag-name node))
           (tagbody
            ;; Step 4.1
            (when last (go :done))
            ;; Step 4.2
            (setf ancestor node)
            ;; Step 4.3
            :loop
            (when (eq ancestor (car (last stack-of-open-elements)))
              (go :done))
            ;; Step 4.4
            ;; TODO: Check this
            (let ((ancestor-position (position ancestor stack-of-open-elements)))
              (setf ancestor (nth (1+ ancestor-position) stack-of-open-elements)))
            ;; Step 4.5
            (when (string-equal "template" (dom:tag-name ancestor))
              (go :done))
            ;; Step 4.6
            (when (string-equal "table" (dom:tag-name ancestor))
              (switch-insertion-mode 'in-select-in-table)
              (return))
            ;; Step 4.7
            (go :loop)
            ;; Step 4.8
            :done
            (switch-insertion-mode 'in-select)
            (return)))
         ;; Step 5
         (when (and (or (string-equal "td" (dom:tag-name node))
                        (string-equal "th" (dom:tag-name node)))
                    (not last))
           (switch-insertion-mode 'in-cell)
           (return))
         ;; Step 6
         (when (string-equal "tr" (dom:tag-name node))
           (switch-insertion-mode 'in-row)
           (return))
         ;; Step 7
         (when (or (string-equal "tbody" (dom:tag-name node))
                   (string-equal "thead" (dom:tag-name node))
                   (string-equal "tfoot" (dom:tag-name node)))
           (switch-insertion-mode 'in-table-body)
           (return))
         ;; Step 8
         (when (string-equal "caption" (dom:tag-name node))
           (switch-insertion-mode 'in-caption)
           (return))
         ;; Step 9
         (when (string-equal "colgroup" (dom:tag-name node))
           (switch-insertion-mode 'in-column-group)
           (return))
         ;; Step 10
         (when (string-equal "table" (dom:tag-name node))
           (switch-insertion-mode 'in-table)
           (return))
         ;; Step 11
         (when (string-equal "template" (dom:tag-name node))
           (switch-insertion-mode (first stack-of-template-insertion-modes))
           (return))
         ;; Step 12
         (when (and (string-equal "head" (dom:tag-name node))
                    (not last))
           (switch-insertion-mode 'in-head)
           (return))
         ;; Step 13
         (when (string-equal "body" (dom:tag-name node))
           (switch-insertion-mode 'in-body)
           (return))
         ;; Step 14
         (when (string-equal "frameset" (dom:tag-name node))
           (switch-insertion-mode 'in-frameset)
           (return))
         ;; Step 15
         (when (string-equal "html" (dom:tag-name node))
           (if (null head-element-pointer)
               (progn
                 (switch-insertion-mode 'before-head)
                 (return))
             (progn
               (switch-insertion-mode 'after-head)
               (return))))
         ;; Step 16
         (when last
           (switch-insertion-mode 'in-body)
           (return))
         ;; Step 17
         (let ((node-position (position node stack-of-open-elements)))
           (setf node (nth (1+ node-position) stack-of-open-elements)))
         ;; Step 18
         (go :loop))))))

;; TODO
(defun acknowledge-token-self-closing-flag ())

;; TODO
(defun adjust-mathml-attributes ())

;; TODO
(defun adjust-svg-attributes ())

;; TODO
(defun adjust-foreign-attributes ())

(defun html-integration-point-p (element)
  (or (and (equal dom:svg-namespace (dom:namespace-uri element))
           (member (dom:local-name element)
                   '("foreignObject" "desc" "title")
                   :test 'equal))))

(defmacro define-parse-procedure ()
  `(defun %parse (stream)
     (macrolet
         (;; tokenize
          ,@(loop for error-name in *parse-errors*
              collect `(,error-name () '(cerror "Continue" ',error-name)))
          (switch-state (state)
            `(progn
               (setf state ,state)))
          (reconsume-in (state)
            `(progn
               (reconsume)
               (setf state ,state)
               (go :switch-state)))
          (emit (token)
            `(let ((token ,token))
               (when *debug*
                 (format t "Emit ~S from ~A~%" token state))
               (push token pending-tokens)))
          (appropriate-end-tag-token-p (end-tag-token)
            `(and (typep ,end-tag-token 'end-tag)
                  last-start-tag-token
                  (string-equal (slot-value ,end-tag-token 'tag-name)
                                (slot-value last-start-tag-token 'tag-name))))
          ;; parse
          ,@(loop for (insertion-mode) in *parser-insertion-modes*
              collect `(,(intern (format nil "PROCESS-TOKEN-USING-~A-INSERTION-MODE"
                                         insertion-mode))
                        ()
                        '(go ,(make-keyword insertion-mode))))
          (switch-insertion-mode (insertion-mode)
            `(progn
               (setf insertion-mode ,insertion-mode)))
          (stop-parsing ()
            `(go :end))
          (reprocess-current-token ()
            `(go :switch-insertion-mode)))
       (let (;; tokenizer variables
             (state 'data-state)
             (return-state nil)
             (current-input-character nil)
             (current-tag-token nil)
             (current-doctype-token nil)
             (current-attribute nil)
             (current-comment-token nil)
             (character-reference-code nil)
             (last-start-tag-token nil)
             (temporary-buffer nil)
             ;; parser variables
             (document (make-instance 'document))
             (foster-parenting nil)
             (insertion-mode 'initial)
             (original-insertion-mode nil)
             (current-token nil)
             (next-token nil)
             (stack-of-open-elements nil)
             (stack-of-template-insertion-modes nil)
             (head-element-pointer nil)
             (form-element-pointer nil)
             (scripting-flag nil)
             (frameset-ok-flag nil)
             (active-formatting-elements nil)
             (pending-table-character-tokens nil)
             (open-texts nil))
         (tagbody
          ;; tokenize
          :switch-state
          (case state
            ,@(loop for (state) in *tokenizer-states*
                    collect `(,state (go ,(make-keyword state)))))
          ;; tokenize states
          ,@(loop for (state . body) in *tokenizer-states*
                  append `(,(make-keyword state)
                           (handler-bind ((parse-error
                                           (lambda (e)
                                             (declare (ignore e))
                                             (continue))))
                             ,@body)
                           (nreversef pending-tokens)
                           (if pending-tokens
                               (go :consume-token)
                             (go :switch-state))))
          :consume-token
          (let ((token (pop pending-tokens)))
            (setf current-token token)
            (when (typep token 'start-tag)
              (setf last-start-tag-token token)))
          ;; parse
          :tree-construction-dispatch
          (if (or (null stack-of-open-elements)
                  (let ((adjusted-current-node (adjusted-current-node)))
                    (or (equal dom:html-namespace
                               (dom:namespace-uri adjusted-current-node))
                        (and (html-integration-point-p adjusted-current-node)
                             (or (typep current-token 'start-tag)
                                 (typep current-token 'cl:character)))
                        (typep current-token 'end-of-file))))
              (go :switch-insertion-mode)
            (progn
              (handler-bind ((parse-error
                              (lambda (e)
                                (declare (ignore e))
                                (continue))))
                (process-token-in-foreign-content))
              (if pending-tokens
                  (go :consume-token)
                (go :switch-state))))
          :switch-insertion-mode
          (case insertion-mode
            ,@(loop for (insertion-mode) in *parser-insertion-modes*
                    collect `(,insertion-mode (go ,(make-keyword insertion-mode)))))
          ;; insertion modes
          ,@(loop for (insertion-mode . body) in *parser-insertion-modes*
                  append `(,(make-keyword insertion-mode)
                           (when *debug*
                             (format t "Process ~S using ~A insertion mode~%"
                                     current-token ',insertion-mode))
                           (handler-bind ((parse-error
                                           (lambda (e)
                                             (declare (ignore e))
                                             (continue))))
                             ,@body)
                           (if pending-tokens
                               (go :consume-token)
                             (go :switch-state))))
          :end)
         (loop for text in open-texts
               for stream = (slot-value text 'data-stream)
               for data = (get-output-stream-string stream)
               do (progn
                    (close stream)
                    (setf (slot-value text 'dom:data) data
                          (slot-value text 'data-stream) nil))
               finally (setf open-texts nil))
         document))))

(define-parse-procedure)

(defgeneric parse (source)
  (:method ((string string))
   (with-input-from-string (stream string)
     (%parse stream)))
  (:method ((stream cl:stream))
   (%parse stream)))
