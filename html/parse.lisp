(in-package :html)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *insertion-modes*
    '(initial
      before-html
      before-head
      in-head
      in-head-noscript
      after-head
      in-body
      text
      in-table
      in-table-text
      in-caption
      in-column-group
      in-table-body
      in-row
      in-cell
      in-select
      in-select-in-table
      in-template
      after-body
      in-frameset
      after-frameset
      after-after-body
      after-after-frameset)))

(defmacro define-parser-insertion-mode (name &body body)
  (let ((function-name (intern (format nil "PROCESS-TOKEN-IN-~A-INSERTION-MODE" name))))
    `(defun ,function-name (parser)
       (with-slots
           (tokenizer
            document
            (token current-token)
            foster-parenting
            stack-of-open-elements
            stack-of-template-insertion-modes
            adjusted-current-node
            head-element-pointer
            form-element-pointer
            insertion-mode
            original-insertion-mode
            frameset-ok-flag
            pending-table-character-tokens
            active-formatting-elements) parser
         (symbol-macrolet
             ((next-token nil)
              (current-node (first (slot-value
                                    parser
                                    'stack-of-open-elements))))
           (macrolet
               ((switch-to (insertion-mode)
                  `(progn
                     (format t "~A -> ~A~%"
                             (slot-value parser 'insertion-mode)
                             ,insertion-mode)
                     (setf (slot-value parser 'insertion-mode) ,insertion-mode))))
             (flet
                 (,@(loop for insertion-mode in *insertion-modes*
                      collect `(,(intern (format nil "PROCESS-TOKEN-IN-~A-INSERTION-MODE" insertion-mode))
                                ()
                                (,(intern (format nil "PROCESS-TOKEN-IN-~A-INSERTION-MODE" insertion-mode)) parser)))
                  (parse-error ()
                    (parse-error parser))
                  (ignore-token ()
                    (ignore-token parser))
                  (stop-parsing ())
                  (reprocess-current-token ()
                    (tree-construction-dispatcher parser (slot-value parser 'current-token)))
                  (insert-comment (&optional position)
                    (declare (ignore position)))
                  (insert-character ()
                    (insert-character parser (slot-value parser 'current-token)))
                  (create-element (token)
                    (create-element-for-token token "html"))
                  (a-start-tag-whose-tag-name-is (name)
                    (let ((token (slot-value parser 'current-token)))
                      (and (typep token 'start-tag)
                           (equal name (slot-value token 'tag-name)))))
                  (a-start-tag-whose-tag-name-is-one-of (names)
                    (let ((token (slot-value parser 'current-token)))
                      (and (typep token 'start-tag)
                           (member (slot-value token 'tag-name) names :test 'equal))))
                  (an-end-tag-whose-tag-name-is (name)
                    (let ((token (slot-value parser 'current-token)))
                      (and (typep token 'end-tag)
                           (equal name (slot-value token 'tag-name)))))
                  (an-end-tag-whose-tag-name-is-one-of (names)
                    (let ((token (slot-value parser 'current-token)))
                      (and (typep token 'end-tag)
                           (member (slot-value token 'tag-name) names :test 'equal))))
                  (insert-html-element-for-token (&optional token)
                    (unless token
                      (setf token (slot-value parser 'current-token)))
                    (insert-html-element-for-token parser token))
                  (close-p-element ()
                    (close-p-element parser))
                  (close-cell ()
                    (close-cell parser))
                  (acknowledge-token-self-closing-flag ())
                  (reconstruct-active-formatting-elements ()
                    (reconstruct-active-formatting-elements parser))
                  (have-element-in-scope-p (tag-name)
                    (have-element-in-scope-p parser tag-name))
                  (have-element-in-list-item-scope-p (tag-name)
                    (have-element-in-list-scope-p parser tag-name))
                  (have-element-in-button-scope-p (tag-name)
                    (have-element-in-button-scope-p parser tag-name))
                  (have-element-in-table-scope-p (tag-name)
                    (have-element-in-table-scope-p parser tag-name))
                  (have-element-in-select-scope-p (tag-name)
                    (have-element-in-select-scope-p parser tag-name))
                  (parse-generic-rawtext-element ()
                    (parse-generic-rawtext-element parser))
                  (parse-generic-rcdata-element ()
                    (parse-generic-rcdata-element parser))
                  (generate-implied-end-tags (&key except)
                    (generate-implied-end-tags parser :except except))
                  (clear-stack-back-to-table-context ())
                  (reset-insertion-mode-appropriately ())
                  (clear-active-formatting-elements-upto-last-marker ())
                  (adoption-agency () (adoption-agency parser))
                  (push-onto-active-formatting-elements (element)
                    (push-onto-active-formatting-elements parser element)))
               ,@body)))))))

(define-parser-insertion-mode initial
  (cond
   ((or (eq #\tab token) (eq #\newline token)
        (eq #\page token) (eq #\return token) (eq #\space token))
    (ignore-token))
   
   ((typep token 'comment-token)
    (insert-comment))
   
   ((typep token 'doctype-token)
    ;; ...
    (switch-to 'before-html))

   (t
    ;; ...
    (switch-to 'before-html)
    (reprocess-current-token))))

(define-parser-insertion-mode before-html
  (cond
   ((typep token 'doctype-token)
    (parse-error))

   ((typep token 'comment-token)
    (insert-comment))

   ((or (eq #\tab token) (eq #\newline token)
        (eq #\page token) (eq #\return token) (eq #\space token))
    (ignore-token))
    
   ((a-start-tag-whose-tag-name-is "html")
    (let ((element (create-element token)))
      (dom:append-child document element)
      (push element stack-of-open-elements))
    (switch-to 'before-head))
    
   ((an-end-tag-whose-tag-name-is-one-of '("head" "body" "html" "br"))
    ;; Same as T
    (let ((element (make-instance 'element :tag-name "html")))
      (dom:append-child document element)
      (push element stack-of-open-elements))
    (switch-to 'before-head))
    
   ((typep token 'end-tag)
    (parse-error))
    
   (t
    (let ((element (make-instance 'element :tag-name "html")))
      (dom:append-child document element)
      (push element stack-of-open-elements))
    (switch-to 'before-head)
    (reprocess-current-token))))

(define-parser-insertion-mode before-head
  (cond
   ((or (eq #\tab token) (eq #\newline token)
        (eq #\page token) (eq #\return token) (eq #\space token))
    (ignore-token))
   
   ((typep token 'comment-token)
    (insert-comment))
   
   ((typep token 'doctype-token)
    (parse-error)
    (ignore-token))
   
   ((a-start-tag-whose-tag-name-is "html")
    (process-token-in-in-body-insertion-mode))

   ((a-start-tag-whose-tag-name-is "head")
    (let ((head (insert-html-element-for-token)))
      (setf head-element-pointer head)
      (switch-to 'in-head)))

   ((an-end-tag-whose-tag-name-is-one-of '("head" "body" "html" "br"))
    ;; Same as T
    (let ((head (insert-html-element-for-token (make-instance 'start-tag :tag-name "head"))))
      (setf head-element-pointer head))
    (switch-to 'in-head)
    (reprocess-current-token))
   
   ((typep token 'end-tag)
    (parse-error)
    (ignore-token))
   
   (t
    (let ((head (insert-html-element-for-token (make-instance 'start-tag :tag-name "head"))))
      (setf head-element-pointer head))
    (switch-to 'in-head)
    (reprocess-current-token))))

(define-parser-insertion-mode in-head
  (cond
   ((or (eq #\tab token) (eq #\newline token)
        (eq #\page token) (eq #\return token) (eq #\space token))
    (insert-character))
   
   ((typep token 'comment-token)
    (insert-comment))
   
   ((typep token 'doctype-token)
    (parse-error)
    (ignore-token))
   
   ((a-start-tag-whose-tag-name-is "html")
    (process-token-in-in-body-insertion-mode))
   
   ((a-start-tag-whose-tag-name-is-one-of '("base" "basefont" "bgsound" "link"))
    (let ((element (insert-html-element-for-token)))
      ;; TODO
      (declare (ignore element))
      (pop stack-of-open-elements))
    (acknowledge-token-self-closing-flag))
   
   ((a-start-tag-whose-tag-name-is "meta")
    (let ((element (insert-html-element-for-token)))
      (declare (ignore element))
      (pop stack-of-open-elements)
      (acknowledge-token-self-closing-flag)
      #|Handle encoding changing|#))

   ((a-start-tag-whose-tag-name-is "title")
    (parse-generic-rcdata-element))

   ;; TODO
   ((or (a-start-tag-whose-tag-name-is "noscript")
        (a-start-tag-whose-tag-name-is-one-of '("noframes" "style"))))
   
   ((a-start-tag-whose-tag-name-is "noscript")
    (insert-html-element-for-token)
    (switch-to 'in-head-noscript))

   ;; TODO
   ((a-start-tag-whose-tag-name-is "script"))

   ((an-end-tag-whose-tag-name-is "head")
    (pop stack-of-open-elements)
    (switch-to 'after-head))

   ((an-end-tag-whose-tag-name-is-one-of '("body" "html" "br"))
    ;; Same as T
    (pop stack-of-open-elements)
    (switch-to 'after-head)
    (reprocess-current-token))

   ;; TODO
   ((a-start-tag-whose-tag-name-is "template"))

   ;; TODO
   ((an-end-tag-whose-tag-name-is "template"))

   ((or (a-start-tag-whose-tag-name-is "head")
        (typep token 'end-tag))
    (parse-error)
    (ignore-token))
   
   (t
    (pop stack-of-open-elements)
    (switch-to 'after-head)
    (reprocess-current-token))))

(define-parser-insertion-mode in-head-noscript
  (cond
   ((typep token 'doctype-token))

   ((a-start-tag-whose-tag-name-is "html"))

   ((an-end-tag-whose-tag-name-is "noscript"))

   ((or (or (eq #\tab token) (eq #\newline token)
            (eq #\page token) (eq #\return token) (eq #\space token))
        (typep token 'comment-token)
        (a-start-tag-whose-tag-name-is-one-of '("basefont" "bgsound" "link"
                                                "meta" "noframes" "style"))))

   ((an-end-tag-whose-tag-name-is "br"))

   ((or (a-start-tag-whose-tag-name-is-one-of '("head" "noscript"))
        ;; Any other end tag
        (typep token 'end-tag)))

   ;; Anything else
   (t)))

(define-parser-insertion-mode after-head
  (cond
   ((or (eq #\tab token) (eq #\newline token)
        (eq #\page token) (eq #\return token) (eq #\space token))
    (insert-character))
   
   ((typep token 'comment-token)
    (insert-comment))
   
   ((typep token 'doctype-token)
    (parse-error)
    (ignore-token))
   
   ((a-start-tag-whose-tag-name-is "html")
    (process-token-in-in-body-insertion-mode))
   
   ((a-start-tag-whose-tag-name-is "body")
    (insert-html-element-for-token)
    (setf frameset-ok-flag nil)
    (switch-to 'in-body))
   
   ((a-start-tag-whose-tag-name-is "frameset")
    (insert-html-element-for-token)
    (switch-to 'in-frameset))

   ((a-start-tag-whose-tag-name-is-one-of '("base" "basefont" "bgsound" "link"
                                            "meta" "noframes" "script" "style"
                                            "template" "title"))
    (parse-error)
    (let ((node head-element-pointer))
      (push node stack-of-open-elements)
      (process-token-in-in-head-insertion-mode)
      (setf stack-of-open-elements (remove node stack-of-open-elements))))

   ;; TODO
   ((an-end-tag-whose-tag-name-is "template"))

   ((an-end-tag-whose-tag-name-is-one-of '("body" "html" "br"))
    ;; Same as T
    (insert-html-element-for-token (make-instance 'start-tag :tag-name "body"))
    (switch-to 'in-body)
    (reprocess-current-token))

   ((or (a-start-tag-whose-tag-name-is "head")
        (typep token 'end-tag))
    (parse-error)
    (ignore-token))
      
   (t
    (insert-html-element-for-token (make-instance 'start-tag :tag-name "body"))
    (switch-to 'in-body)
    (reprocess-current-token))))

(define-parser-insertion-mode in-body
  (cond
   ((eq #\null token)
    (parse-error)
    (ignore-token))
   
   ((or (eq #\tab token) (eq #\newline token)
        (eq #\page token) (eq #\return token) (eq #\space token))
    (reconstruct-active-formatting-elements)
    (insert-character))
   
   ((typep token 'cl:character)
    (reconstruct-active-formatting-elements)
    (insert-character)
    (setf frameset-ok-flag nil))
   
   ((typep token 'comment-token)
    (insert-comment))
   
   ((typep token 'doctype-token)
    (parse-error)
    (ignore-token))
   
   ((a-start-tag-whose-tag-name-is "html")
    (parse-error)
    (if (find "template" stack-of-open-elements
              :test 'equal
              :key (lambda (element) (slot-value element 'dom:tag-name)))
        (ignore-token)
      #|TODO|#))
   
   ((or (a-start-tag-whose-tag-name-is-one-of '("base" "basefont" "bgsound" "link"
                                                "meta" "noframes" "script" "style"
                                                "template" "title"))
        (an-end-tag-whose-tag-name-is "template"))
    (process-token-in-in-head-insertion-mode))
   
   ((a-start-tag-whose-tag-name-is "body")
    (parse-error)
    #|TODO|#)

   ((a-start-tag-whose-tag-name-is "frameset")
    (parse-error)
    #|TODO|#)
   
   ;; An end-of-file token
   ((typep token 'end-of-file)
    (if (null stack-of-template-insertion-modes)
        (process-token-in-in-template-insertion-mode)
      (progn
        (if (find-if-not (lambda (node)
                           (typep node '(or dd dt li optgroup option
                                            p rb rp rt rtc
                                            tbody td tfoot th thead tr
                                            body html)))
                         stack-of-open-elements)
            (parse-error))
        (stop-parsing))))
   
   ((an-end-tag-whose-tag-name-is "body")
    (if (not (have-element-in-scope-p "body"))
        (progn
          (parse-error)
          (ignore-token))
      (if (find-if-not (lambda (node)
                         (typep node '(or dd dt li optgroup option
                                          p rb rp rt rtc
                                          tbody td tfoot th thead tr
                                          body html)))
                       stack-of-open-elements)
          (parse-error)))
    (switch-to 'after-body))

   ((an-end-tag-whose-tag-name-is "html")
    (if (not (have-element-in-scope-p "body"))
        (progn
          (parse-error)
          (ignore-token))
      (if (find-if-not (lambda (node)
                         (typep node '(or dd dt li optgroup option
                                          p rb rp rt rtc
                                          tbody td tfoot th thead tr
                                          body html)))
                       stack-of-open-elements)
          (parse-error)))
    (switch-to 'after-body)
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
               (member (slot-value current-node 'dom:tag-name)
                       '("h1" "h2" "h3" "h4" "h5" "h6")
                       :test 'equal))
      (parse-error)
      (pop stack-of-open-elements))
    (insert-html-element-for-token))

   ((a-start-tag-whose-tag-name-is-one-of '("pre" "listing"))
    (when (have-element-in-button-scope-p "p")
      (close-p-element))
    (insert-html-element-for-token)
    (when (eq #\newline next-token)
      (ignore-next-token))
    (setf frameset-ok-flag nil))

   ((a-start-tag-whose-tag-name-is "form"))

   ;; https://github.com/html5lib/html5lib-python/blob/master/html5lib/html5parser.py#L1111
   ((a-start-tag-whose-tag-name-is "li")
    (let ((node current-node))
      (setf frameset-ok-flag nil)
      (tagbody
       :loop
       (loop while (and (typep node 'element)
                        (equal "li" (slot-value node 'dom:tag-name)))
             do
             (generate-implied-end-tags :except "li")
             (unless (equal "li" (slot-value node 'dom:tag-name))
               (parse-error))
             (loop for element = (pop stack-of-open-elements)
                   until (equal "li" (slot-value node 'dom:tag-name)))
             (go :done))
       (if (and (special-element-p node)
                (not (member (slot-value node 'dom:tag-name)
                             '("address" "div" "p")
                             :test 'equal)))
           (go :done)
         (progn
           ))
       :done
       )))

   ;; TODO
   ((a-start-tag-whose-tag-name-is-one-of '("dd" "dt")))
   
   ((a-start-tag-whose-tag-name-is "plaintext")
    (when (have-element-in-button-scope-p "p")
      (close-p-element))
    (insert-html-element-for-token)
    (setf (slot-value tokenizer 'state) 'plaintext-state))

   ((a-start-tag-whose-tag-name-is "button")
    (when (have-element-in-scope-p "button")
      (parse-error)
      (generate-implied-end-tags)
      (loop for element = (pop stack-of-open-elements)
            until (equal "button" (slot-value element 'dom:tag-name)))))
   
   ((an-end-tag-whose-tag-name-is-one-of '("address" "article" "aside"
                                           "blockquote" "center" "details"
                                           "dialog" "dir" "div" "dl"
                                           "fieldset" "figcaption" "figure"
                                           "footer" "header" "hgroup"
                                           "main" "menu" "nav"
                                           "ol" "p" "section" "summary" "ul"))
    (if (find-if-not (lambda (element)
                       (equal (slot-value token 'tag-name)
                              (slot-value element 'dom:tag-name)))
                     stack-of-open-elements)
        (progn
          (parse-error)
          (ignore-token))
      (progn
        (generate-implied-end-tags)
        (when (equal (slot-value current-node 'tag-name)
                     (slot-value element 'dom:tag-name))
          (parse-error))
        (loop for element = (pop stack-of-open-elements)
              until (equal (slot-value token 'tag-name)
                           (slot-value element 'dom:tag-name))))))

   ;; TODO
   ((an-end-tag-whose-tag-name-is "form"))

   ((an-end-tag-whose-tag-name-is "p")
    (when (have-element-in-button-scope-p "p")
      (parse-error)
      (insert-html-element-for-token (make-instance 'start-tag :tag-name "p")))
    (close-p-element))

   ((an-end-tag-whose-tag-name-is "li")
    (if (not (have-element-in-button-scope-p "li"))
        (progn
          (parse-error)
          (ignore-token))
      (progn
        (generate-implied-end-tags :except "li")
        (unless (equal "li" (slot-value current-node 'dom:tag-name))
          (parse-error))
        (loop for element = (pop stack-of-open-elements)
              until (equal li (slot-value element 'dom:tag-name))))))
   
   ((an-end-tag-whose-tag-name-is-one-of '("dd" "dt"))
    (if (not (have-element-in-scope-p (slot-value token 'tag-name)))
        (progn
          (parse-error)
          (ignore-token))
      (progn
        (generate-implied-end-tags :except (slot-value token 'tag-name))
        (unless (equal (slot-value current-node 'dom:tag-name)
                       (slot-value token 'tag-name))
          (parse-error))
        (loop for element = (pop stack-of-open-elements)
              until (member (slot-value element 'dom:tag-name)
                            (slot-value token 'tag-name))))))

   ((an-end-tag-whose-tag-name-is-one-of '("h1" "h2" "h3" "h4" "h5" "h6"))
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
        (unless (equal (slot-value current-node 'dom:tag-name)
                       (slot-value token 'tag-name))
          (parse-error))
        (loop for element = (pop stack-of-open-elements)
              until (member (slot-value element 'dom:tag-name)
                            '("h1" "h2" "h3" "h4" "h5" "h6")
                            :test 'equal)))))
   
   ((an-end-tag-whose-tag-name-is "sarcasm"))

   ;; TODO
   ((a-start-tag-whose-tag-name-is "a"))

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
    (appendf active-formatting-elements (list (make-instance 'marker)))
    (setf frameset-ok-flag nil))
   
   ((an-end-tag-whose-tag-name-is-one-of '("applet" "marquee" "object"))
    (if (not (have-element-in-scope-p (slot-value token 'tag-name)))
        (progn
          (parse-error)
          (ignore-token))
      (progn
        (generate-implied-end-tags)
        (unless (equal (slot-value token 'tag-name)
                       (slot-value current-node 'dom:tag-name))
          (parse-error))
        (loop for element = (pop stack-of-open-elements)
              until (equal (slot-value element 'dom:tag-name)
                           (slot-value token 'tag-name)))
        (clear-active-formatting-elements-upto-last-marker))))

   ((a-start-tag-whose-tag-name-is "table")
    #|TODO: If the Document is not set to quirks mode, and the stack of open elements has a p element in button scope, then close a p element.|#
    (insert-html-element-for-token)
    (setf frameset-ok-flag nil)
    (switch-to 'in-table))

   ((an-end-tag-whose-tag-name-is "br")
    (parse-error)
    #|TODO: Drop the attributes from the token, and act as described in the next entry; i.e. act as if this was a "br" start tag token with no attributes, rather than the end tag token that it actually is.|#)

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
    (setf (slot-value token 'tag-name) "image")
    (reprocess-current-token))

   ;; TODO
   ((a-start-tag-whose-tag-name-is "textarea"))

   ;; TODO
   ((a-start-tag-whose-tag-name-is "xmp"))

   ((a-start-tag-whose-tag-name-is "iframe")
    (setf frameset-ok-flag nil)
    (parse-generic-rawtext-element))

   ;; TODO
   ((or (a-start-tag-whose-tag-name-is "noembed")
        (a-start-tag-whose-tag-name-is "noscript"))
    (parse-generic-rawtext-element))

   ((a-start-tag-whose-tag-name-is "select")
    (reconstruct-active-formatting-elements)
    (insert-html-element-for-token)
    (setf frameset-ok-flag nil)
    (if (member insertion-mode '(in-table in-caption in-table-body
                                          in-row in-cell))
        (switch-to 'in-select-in-table)
      (switch-to 'in-select)))

   ((a-start-tag-whose-tag-name-is-one-of '("optgroup" "option"))
    (when (equal "option" (slot-value current-node 'dom:tag-name))
      (pop stack-of-open-elements))
    (reconstruct-active-formatting-elements)
    (insert-html-element-for-token))

   ((a-start-tag-whose-tag-name-is-one-of '("rb" "rtc"))
    (when (have-element-in-scope-p "ruby")
      (generate-implied-end-tags)
      (unless (equal "ruby" (slot-value current-node 'dom:tag-name))
        (parse-error)))
    (insert-html-element-for-token))

   ;; https://github.com/html5lib/html5lib-python/blob/master/html5lib/html5parser.py#L1313
   ((a-start-tag-whose-tag-name-is-one-of '("rp" "rt"))
    (when (have-element-in-scope-p "ruby")
      (generate-implied-end-tags :except "rtc")
      (unless (equal "ruby" (slot-value current-node 'dom:tag-name))
        (parse-error)))
    (insert-html-element-for-token))

   ;; TODO
   ((a-start-tag-whose-tag-name-is "math"))

   ;; TODO
   ((a-start-tag-whose-tag-name-is "svg"))

   ((a-start-tag-whose-tag-name-is-one-of '("caption" "col" "colgroup"
                                            "frame" "head" "tbody" "td"
                                            "tfoot" "th" "thead" "tr"))
    (parse-error)
    (ignore-token))
   
   ((typep token 'start-tag)
    (reconstruct-active-formatting-elements)
    (insert-html-element-for-token))

   ;; https://github.com/html5lib/html5lib-python/blob/master/html5lib/html5parser.py#L1635
   ((typep token 'end-tag)
    (loop for node in stack-of-open-elements
          do (if (equal (slot-value node 'dom:tag-name)
                        (slot-value token 'tag-name))
                 (progn
                   (generate-implied-end-tags)
                   (unless (equal (slot-value (first stack-of-open-elements) 'dom:tag-name)
                                  (slot-value token 'tag-name))
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
   ((typep token 'cl:character)
    (insert-character))

   ((typep token 'end-of-file)
    (parse-error)
    (pop stack-of-open-elements)
    (setf insertion-mode original-insertion-mode)
    (reprocess-current-token))

   ;; Don't need this because we don't have a script executing environment
   ;; ((an-end-tag-whose-tag-name-is "script"))

   ((typep token 'end-tag)
    (pop stack-of-open-elements)
    (switch-to original-insertion-mode))))

(define-parser-insertion-mode in-table
  (cond
   ((if (member (slot-value current-node 'dom:tag-name)
                '("table" "tbody" "tfoot" "thead" "tr")
                :test 'equal)
        (typep token 'cl:character))
    (setf pending-table-character-tokens nil)
    (setf original-insertion-mode insertion-mode)
    (switch-to 'in-table-text)
    (reprocess-current-token))

   ((typep token 'comment-token)
    (insert-comment))

   ((typep token 'doctype-token)
    (parse-error)
    (ignore-token))

   ((a-start-tag-whose-tag-name-is "caption")
    (clear-stack-back-to-table-context)
    (appendf active-formatting-elements (list (make-instance 'marker)))
    (insert-html-element-for-token)
    (switch-to 'in-caption))

   ((a-start-tag-whose-tag-name-is "colgroup")
    (clear-stack-back-to-table-context)
    (insert-html-element-for-token)
    (switch-to 'in-column-group))

   ((a-start-tag-whose-tag-name-is "col")
    (clear-stack-back-to-table-context)
    (insert-html-element-for-token (make-instance 'start-tag :tag-name "colgroup"))
    (switch-to 'in-column-group)
    (reprocess-current-token))

   ((a-start-tag-whose-tag-name-is-one-of '("tbody" "tfoot" "thead"))
    (clear-stack-back-to-table-context)
    (insert-html-element-for-token)
    (switch-to 'in-table-body))

   ((a-start-tag-whose-tag-name-is-one-of '("td" "th" "tr"))
    (clear-stack-back-to-table-context)
    (insert-html-element-for-token (make-instance 'start-tag :tag-name "tbody"))
    (switch-to 'in-table-body)
    (reprocess-current-token))

   ((a-start-tag-whose-tag-name-is "table")
    (parse-error)
    (if (not (have-element-in-table-scope-p "table"))
        (ignore-token)
      (progn
        (loop for element = (pop stack-of-open-elements)
              until (equal "table" (slot-value element 'dom:tag-name)))
        (reset-insertion-mode-appropriately)
        (reprocess-current-token))))

   ((an-end-tag-whose-tag-name-is "table")
    (if (not (have-element-in-table-scope-p "table"))
        (ignore-token)
      (progn
        (loop for element = (pop stack-of-open-elements)
              until (equal "table" (slot-value element 'dom:tag-name)))
        (reset-insertion-mode-appropriately))))

   ((an-end-tag-whose-tag-name-is-one-of '("body" "caption" "col"
                                           "colgroup" "html" "tbody"
                                           "td" "tfoot" "th" "thead" "tr"))
    (parse-error)
    (ignore-token))

   ((or (a-start-tag-whose-tag-name-is-one-of '("style" "script" "template"))
        (an-end-tag-whose-tag-name-is "template"))
    (process-token-in-in-head-insertion-mode))

   ;; TODO
   ((a-start-tag-whose-tag-name-is "input"))

   ((a-start-tag-whose-tag-name-is "form")
    (parse-error)
    (if (or (find "template" stack-of-open-elements
                  :test 'equal
                  :key (lambda (element) (slot-value element 'dom:tag-name)))
            form-element-pointer)
        (ignore-token)
      (progn
        (let ((element (insert-html-element-for-token)))
          (setf form-element-pointer element))
        (pop stack-of-open-elements))))

   ((typep token 'end-of-file)
    (process-token-in-in-body-insertion-mode))

   (t
    (parse-error)
    (setf foster-parenting t)
    (process-token-in-in-body-insertion-mode)
    (setf foster-parenting nil))))

(define-parser-insertion-mode in-table-text
  (cond
   ((eq #\null token)
    (parse-error)
    (ignore-token))

   ((typep token 'cl:character)
    (appendf pending-table-character-tokens (list token)))

   ;; TODO
   (t)))

(define-parser-insertion-mode in-caption
  (cond
   ((an-end-tag-whose-tag-name-is "caption")
    (if (have-element-in-table-scope-p "caption")
        (progn
          (parse-error)
          (ignore-token))
      (generate-implied-end-tags))
    (unless (equal "caption" (slot-value current-node 'dom:tag-name))
      (parse-error))
    (loop for element = (pop stack-of-open-elements)
          until (equal "caption" (slot-value element 'dom:tag-name)))
    (clear-active-formatting-elements-upto-last-marker)
    (switch-to 'in-table))

   ((or (a-start-tag-whose-tag-name-is-one-of '("caption" "col" "colgroup"
                                                "tbody" "td" "tfoot"
                                                "th" "thead" "tr"))
        (an-end-tag-whose-tag-name-is "table"))
    (if (have-element-in-table-scope-p "caption")
        (progn
          (parse-error)
          (ignore-token))
      (generate-implied-end-tags))
    (unless (equal "caption" (slot-value current-node 'dom:tag-name))
      (parse-error))
    (loop for element = (pop stack-of-open-elements)
          until (equal "caption" (slot-value element 'dom:tag-name)))
    (clear-active-formatting-elements-upto-last-marker)
    (switch-to 'in-table)
    (reprocess-current-token))

   ((an-end-tag-whose-tag-name-is-one-of '("body" "col" "colgroup" "html"
                                           "tbody" "td" "tfoot" "th" "thead" "tr"))
    (parse-error)
    (ignore-token))

   (t
    (process-token-in-in-body-insertion-mode))))

(define-parser-insertion-mode in-column-group
  (cond
   ((or (eq #\tab token) (eq #\newline token)
        (eq #\page token) (eq #\return token) (eq #\space token))
    (insert-character))

   ((typep token 'comment-token)
    (insert-comment))

   ((typep token 'doctype-token)
    (parse-error)
    (ignore-token))

   ((a-start-tag-whose-tag-name-is "html")
    (process-token-in-in-body-insertion-mode))

   ((a-start-tag-whose-tag-name-is "col")
    (insert-html-element-for-token)
    (pop stack-of-open-elements)
    (acknowledge-token-self-closing-flag))

   ((an-end-tag-whose-tag-name-is "colgroup")
    (if (not (equal "colgroup" (slot-value current-node 'dom:tag-name)))
        (progn
          (parse-error)
          (ignore-token))
      (progn
        (pop stack-of-open-elements)
        (switch-to 'in-table))))

   ((an-end-tag-whose-tag-name-is "col")
    (parse-error)
    (ignore-token))

   ((or (a-start-tag-whose-tag-name-is "template")
        (an-end-tag-whose-tag-name-is "template"))
    (process-token-in-in-head-insertion-mode))

   ((typep token 'end-of-file)
    (process-token-in-in-body-insertion-mode))

   (t
    (if (not (equal "colgroup" (slot-value current-node 'dom:tag-name)))
        (progn
          (parse-error)
          (ignore-token))
      (pop stack-of-open-elements))
    (switch-to 'in-table)
    (reprocess-current-token))))

(define-parser-insertion-mode in-table-body
  (cond
   ((a-start-tag-whose-tag-name-is "tr")
    (clear-stack-back-to-table-context)
    (insert-html-element-for-token)
    (switch-to 'in-row))

   ((a-start-tag-whose-tag-name-is-one-of '("th" "td"))
    (parse-error)
    (clear-stack-back-to-table-context)
    (insert-html-element-for-token (make-instance 'start-tag :tag-name "tr"))
    (switch-to 'in-row)
    (reprocess-current-token))

   ((an-end-tag-whose-tag-name-is-one-of '("tbody" "tfoot" "thead"))
    (if (not (have-element-in-table-scope-p (slot-value token 'tag-name)))
        (progn
          (parse-error)
          (ignore-token))
      (progn
        (clear-stack-back-to-table-context)
        (pop stack-of-open-elements)
        (switch-to 'in-table))))

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
        (clear-stack-back-to-table-context)
        (pop stack-of-open-elements)
        (switch-to 'in-table)
        (reprocess-current-token))))

   ((an-end-tag-whose-tag-name-is-one-of '("body" "caption" "col" "colgroup"
                                           "html" "td" "th" "tr"))
    (parse-error)
    (ignore-token))

   (t
    (process-token-in-in-table-insertion-mode))))

(define-parser-insertion-mode in-row
  (cond
   ((a-start-tag-whose-tag-name-is-one-of '("th" "td"))
    (clear-stack-back-to-table-context)
    (insert-html-element-for-token)
    (switch-to 'in-cell)
    (appendf active-formatting-elements (make-instance 'marker)))

   ((an-end-tag-whose-tag-name-is "tr")
    (if (not (have-element-in-table-scope-p "tr"))
        (progn
          (parse-error)
          (ignore-token))
      (progn
        (clear-stack-back-to-table-context)
        (pop stack-of-open-elements)
        (switch-to 'in-table-body))))

   ((or (a-start-tag-whose-tag-name-is-one-of '("caption" "col" "colgroup"
                                                "tbody" "tfoot" "thead" "tr"))
        (an-end-tag-whose-tag-name-is "table"))
    (if (not (have-element-in-table-scope-p "tr"))
        (progn
          (parse-error)
          (ignore-token))
      (progn
        (clear-stack-back-to-table-context)
        (pop stack-of-open-elements)
        (switch-to 'in-table-body)
        (reprocess-current-token))))

   ((an-end-tag-whose-tag-name-is-one-of '("tbody" "tfoot" "thead"))
    (when (not (have-element-in-table-scope-p (slot-value token 'tag-name)))
      (parse-error)
      (ignore-token))
    (if (not (have-element-in-table-scope-p "tr"))
        (ignore-token)
      (progn
        (clear-stack-back-to-table-context)
        (pop stack-of-open-elements)
        (switch-to 'in-table-body)
        (reprocess-current-token))))

   ((an-end-tag-whose-tag-name-is-one-of '("body" "caption" "col" "colgroup"
                                           "html" "td" "th"))
    (parse-error)
    (ignore-token))

   (t
    (process-token-in-in-table-insertion-mode))))

(define-parser-insertion-mode in-cell
  (cond
   ((an-end-tag-whose-tag-name-is-one-of '("td" "th"))
    (if (not (have-element-in-table-scope-p (slot-value token 'tag-name)))
        (progn
          (parse-error)
          (ignore-token))
      (generate-implied-end-tags))
    (unless (equal (slot-value current-node 'dom:tag-name)
                   (slot-value token 'tag-name))
      (parse-error))
    (loop for element = (pop stack-of-open-elements)
          until (equal (slot-value element 'dom:tag-name)
                       (slot-value token 'tag-name)))
    (clear-active-formatting-elements-upto-last-marker)
    (switch-to 'in-row))

   ((a-start-tag-whose-tag-name-is-one-of '("caption" "col" "colgroup"
                                            "tbody" "tfoot" "thead" "tr"))
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
    (if (not (have-element-in-table-scope-p (slot-value token 'tag-name)))
        (progn
          (parse-error)
          (ignore-token))
      (progn
        (close-cell)
        (reprocess-current-token))))

   (t
    (process-token-in-in-body-insertion-mode))))

(define-parser-insertion-mode in-select
  (cond
   ((eq #\null token)
    (parse-error)
    (ignore-token))

   ((typep token 'cl:character)
    (insert-character))

   ((typep token 'comment-token)
    (insert-comment))

   ((typep token 'doctype-token)
    (parse-error)
    (ignore-token))

   ((a-start-tag-whose-tag-name-is "html")
    (process-token-in-in-body-insertion-mode))

   ((a-start-tag-whose-tag-name-is "option")
    (when (equal "option" (slot-value current-node 'dom:tag-name))
      (pop stack-of-open-elements))
    (insert-html-element-for-token))

   ((a-start-tag-whose-tag-name-is "optgroup")
    (when (equal "option" (slot-value current-node 'dom:tag-name))
      (pop stack-of-open-elements))
    (when (equal "optgroup" (slot-value current-node 'dom:tag-name))
      (pop stack-of-open-elements))
    (insert-html-element-for-token))

   ((an-end-tag-whose-tag-name-is "optgroup")
    (when (and (equal "option" (slot-value current-node 'dom:tag-name))
               (equal "optgroup" (slot-value (second stack-of-open-elements)
                                             'dom:tag-name)))
      (pop stack-of-open-elements))
    (if (equal "optgroup" (slot-value current-node 'dom:tag-name))
        (pop stack-of-open-elements)
      (progn
        (parse-error)
        (ignore-token))))

   ((an-end-tag-whose-tag-name-is "option")
    (if (equal "option" (slot-value current-node 'dom:tag-name))
        (pop stack-of-open-elements)
      (progn
        (parse-error)
        (ignore-token))))

   ((an-end-tag-whose-tag-name-is "select")
    ;; TODO: If the stack of open elements does not have a select element in select scope, this is a parse error; ignore the token. (fragment case)
    ;; Otherwise:
    (loop for element = (pop stack-of-open-elements)
          until (equal "select" (slot-value element 'dom:tag-name)))
    (reset-insertion-mode-appropriately))

   ((a-start-tag-whose-tag-name-is "select")
    (parse-error)
    ;; TODO: If the stack of open elements does not have a select element in select scope, ignore the token. (fragment case)
    ;; Otherwise:
    (loop for element = (pop stack-of-open-elements)
          until (equal "select" (slot-value element 'dom:tag-name)))
    (reset-insertion-mode-appropriately))

   ((a-start-tag-whose-tag-name-is-one-of '("input" "keygen" "textarea"))
    (parse-error)
    ;; TODO: If the stack of open elements does not have a select element in select scope, ignore the token. (fragment case)
    ;; Otherwise:
    (loop for element = (pop stack-of-open-elements)
          until (equal "select" (slot-value element 'dom:tag-name)))
    (reset-insertion-mode-appropriately)
    (reprocess-current-token))

   ((or (a-start-tag-whose-tag-name-is-one-of '("script" "template"))
        (an-end-tag-whose-tag-name-is "template"))
    (process-token-in-in-head-insertion-mode))

   ((typep token 'end-of-file)
    (process-token-in-in-body-insertion-mode))

   (t
    (parse-error)
    (ignore-token))))

(define-parser-insertion-mode in-select-in-table
  (cond
   ((a-start-tag-whose-tag-name-is-one-of '("caption" "table" "tbody"
                                            "tfoot" "thead" "tr" "td" "th"))
    (parse-error)
    (loop for element = (pop stack-of-open-elements)
          until (equal "select" (slot-value element 'dom:tag-name)))
    (reset-insertion-mode-appropriately)
    (reprocess-current-token))
   
   ((an-end-tag-whose-tag-name-is-one-of '("caption" "table" "tbody"
                                           "tfoot" "thead" "tr" "td" "th"))
    (parse-error)
    ;; TODO
    ;; If the stack of open elements does not have an element in table scope that is an HTML element with the same tag name as that of the token, then ignore the token.
    (loop for element = (pop stack-of-open-elements)
          until (equal "select" (slot-value element 'dom:tag-name)))
    (reset-insertion-mode-appropriately)
    (reprocess-current-token))

   (t
    (process-token-in-in-select-insertion-mode))))

(define-parser-insertion-mode in-template
  (cond
   ((or (typep token 'cl:character)
        (typep token 'comment-token)
        (typep token 'doctype-token))
    (process-token-in-in-body-insertion-mode))

   ((or (a-start-tag-whose-tag-name-is-one-of '("base" "basefont" "bgsound"
                                                "link" "meta" "noframes"
                                                "script" "style" "template" "title"))
        (an-end-tag-whose-tag-name-is "template"))
    (process-token-in-in-head-insertion-mode))

   ((a-start-tag-whose-tag-name-is-one-of '("caption" "colgroup" "tbody"
                                            "tfoot" "thead"))
    (pop stack-of-open-elements)
    (push 'in-table stack-of-template-insertion-modes)
    (switch-to 'in-table)
    (reprocess-current-token))

   ((a-start-tag-whose-tag-name-is "col")
    (pop stack-of-open-elements)
    (push 'in-column-group stack-of-template-insertion-modes)
    (switch-to 'in-column-group)
    (reprocess-current-token))

   ((a-start-tag-whose-tag-name-is "tr")
    (pop stack-of-open-elements)
    (push 'in-table-body stack-of-template-insertion-modes)
    (switch-to 'in-table-body)
    (reprocess-current-token))

   ((a-start-tag-whose-tag-name-is-one-of '("td" "th"))
    (pop stack-of-open-elements)
    (push 'in-row stack-of-template-insertion-modes)
    (switch-to 'in-row)
    (reprocess-current-token))

   ((typep token 'start-tag)
    (pop stack-of-open-elements)
    (push 'in-body stack-of-template-insertion-modes)
    (switch-to 'in-body)
    (reprocess-current-token))

   ((typep token 'end-tag)
    (parse-error)
    (ignore-token))

   ((typep token 'end-of-file)
    (if (not (find "template" stack-of-open-elements
                   :test 'equal
                   :key (lambda (element) (slot-value element 'dom:tag-name))))
        (stop-parsing)
      (parse-error))
    (loop for element = (pop stack-of-open-elements)
          until (equal "template" (slot-value element 'dom:tag-name)))
    (clear-active-formatting-elements-upto-last-marker)
    (pop stack-of-template-insertion-modes)
    (reset-insertion-mode-appropriately)
    (reprocess-current-token))))

(define-parser-insertion-mode after-body
  (cond
   ((or (eq #\tab token) (eq #\newline token)
        (eq #\page token) (eq #\return token) (eq #\space token))
    (process-token-in-in-body-insertion-mode))

   ;; TODO
   ((typep token 'comment-token))

   ((typep token 'doctype-token)
    (parse-error)
    (ignore-token))

   ((a-start-tag-whose-tag-name-is "html")
    (process-token-in-in-body-insertion-mode))

   ((an-end-tag-whose-tag-name-is "html")
    ;; TODO
    (switch-to 'after-after-body))

   ((typep token 'end-of-file)
    (stop-parsing))

   (t
    (parse-error)
    (switch-to 'in-body)
    (reprocess-current-token))))

(define-parser-insertion-mode in-frameset
  (cond
   ((or (eq #\tab token) (eq #\newline token)
        (eq #\page token) (eq #\return token) (eq #\space token))
    (insert-character))

   ((typep token 'comment-token)
    (insert-comment))

   ((typep token 'doctype-token)
    (parse-error)
    (ignore-token))

   ((a-start-tag-whose-tag-name-is "html")
    (process-token-in-in-body-insertion-mode))

   ((a-start-tag-whose-tag-name-is "frameset")
    (insert-html-element-for-token))

   ((an-end-tag-whose-tag-name-is "frameset")
    ;; TODO
    (if (equal "html" (slot-value current-node 'dom:tag-name))
        (progn
          (parse-error)
          (ignore-token))
      (pop stack-of-open-elements))
    #|TODO|#)

   ((a-start-tag-whose-tag-name-is "frame")
    (insert-html-element-for-token)
    (pop stack-of-open-elements)
    (acknowledge-token-self-closing-flag))

   ((a-start-tag-whose-tag-name-is "noframes")
    (process-token-in-in-head-insertion-mode))

   ((typep token 'end-of-file)
    ;; TODO
    (when (not (equal "html" (slot-value current-node 'dom:tag-name)))
      (parse-error))
    (stop-parsing))

   (t
    (parse-error)
    (ignore-token))))

(define-parser-insertion-mode after-frameset
  (cond
   ((or (eq #\tab token) (eq #\newline token)
        (eq #\page token) (eq #\return token) (eq #\space token))
    (insert-character))

   ((typep token 'comment-token)
    (insert-comment))

   ((typep token 'doctype-token)
    (parse-error)
    (ignore-token))

   ((a-start-tag-whose-tag-name-is "html")
    (process-token-in-in-body-insertion-mode))

   ((an-end-tag-whose-tag-name-is "html")
    (switch-to 'after-after-frameset))

   ((a-start-tag-whose-tag-name-is "noframes")
    (process-token-in-in-head-insertion-mode))

   ((typep token 'end-of-file)
    (stop-parsing))

   (t
    (parse-error)
    (ignore-token))))


(define-parser-insertion-mode after-after-body
  (cond
   ;; TODO
   ((typep token 'comment-token))

   ((or (typep token 'doctype-token)
        (or (eq #\tab token) (eq #\newline token)
            (eq #\page token) (eq #\return token) (eq #\space token))
        (a-start-tag-whose-tag-name-is "html"))
    (process-token-in-in-body-insertion-mode))

   ((typep token 'end-of-file)
    (stop-parsing))

   (t
    (parse-error)
    (switch-to 'in-body)
    (reprocess-current-token))))

(define-parser-insertion-mode after-after-frameset
  (cond
   ;; TODO
   ((typep token 'comment-token))

   ((or (typep token 'doctype-token)
        (or (eq #\tab token) (eq #\newline token)
            (eq #\page token) (eq #\return token) (eq #\space token))
        (a-start-tag-whose-tag-name-is "html"))
    (process-token-in-in-body-insertion-mode))

   ((typep token 'end-of-file)
    (stop-parsing))

   ((a-start-tag-whose-tag-name-is "noframes")
    (process-token-in-in-head-insertion-mode))

   (t
    (parse-error)
    (ignore-token))))

(defclass parser ()
  ((tokenizer
    :initarg :tokenizer
    :initform nil)
   (document
    :initarg :document
    :initform (make-instance 'document))
   (foster-parenting
    :initform nil)
   (insertion-mode
    :initform 'initial)
   (current-token
    :initform nil)
   (stack-of-open-elements
    :initform nil)
   (stack-of-template-insertion-modes
    :initform nil)
   (head-element-pointer
    :initform nil)
   (frameset-ok-flag
    :initform nil)
   (pending-table-character-tokens
    :initform nil)))

(defun parse-error (parser)
  (declare (ignore parser)))

(defun ignore-token (parser)
  (declare (ignore parser)))

(defun current-node (parser)
  (first (slot-value parser 'stack-of-open-elements)))

(defun adjusted-current-node (parser)
  ;; TODO: Case for HTML fragment parsing
  (current-node parser))

(defun insert-character (parser token)
  (let ((data token))
    (let ((adjusted-insertion-location (appropriate-place-for-inserting-node parser)))
      (when (typep adjusted-insertion-location 'document)
        (return-from insert-character))
      (let ((text (car (last (dom:children adjusted-insertion-location)))))
        (if (typep text 'text)
            (append-char (slot-value text 'dom:data) data)
          (let ((text (make-instance 'text :data (string data))))
            (append-child adjusted-insertion-location text)))))))

;; TODO: Respect namespace
(defun create-element-for-token (token namespace)
  (declare (ignore namespace))
  (check-type token start-tag)
  (make-instance 'element :tag-name (slot-value token 'tag-name)))

(defun insert-html-element-for-token (parser token)
  (insert-foreign-element-for-token parser token "html"))

(defun insert-foreign-element-for-token (parser token namespace)
  (let ((adjusted-insertion-location (appropriate-place-for-inserting-node parser)))
    (let ((element (create-element-for-token token namespace)))
      (append-child adjusted-insertion-location element)
      (push element (slot-value parser 'stack-of-open-elements))
      element)))

(defun appropriate-place-for-inserting-node (parser &optional override-target)
  (let ((target (if override-target
                    override-target
                  (first (slot-value parser 'stack-of-open-elements)))))
    target))

(defun have-element-in-specific-scope-p (parser target-node list)
  (with-slots (stack-of-open-elements) parser
    (let* ((stack stack-of-open-elements)
           (node (pop stack)))
      (tagbody
       :loop
       (if (eq node target-node)
           (return-from have-element-in-specific-scope-p t)
         (if (find (slot-value node 'dom:tag-name)
                   list
                   :test 'equal)
             (return-from have-element-in-specific-scope-p nil)
           (progn
             (setf node (pop stack))
             (go :loop))))))))

(defun have-element-in-scope-p (parser element)
  (have-element-in-specific-scope-p
   parser
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
     #|TODO: elements in MathML and SVG namespaces|#)))

(defun have-element-in-list-item-scope-p (parser element)
  (have-element-in-specific-scope-p
   parser
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
     #|TODO: elements in MathML and SVG namespaces|#
     "ol"
     "ul")))

(defun have-element-in-button-scope-p (parser element)
  (have-element-in-specific-scope-p
   parser
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
     #|TODO: elements in MathML and SVG namespaces|#
     "button")))

(defun have-element-in-table-scope-p (parser element)
  (have-element-in-specific-scope-p
   parser
   element
   '("html"
     "table"
     "template")))

(defun special-element-p (element)
  (let ((tag-name (typecase element
                    (element (slot-value element 'dom:tag-name))
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
              #|TODO: MathML and SVG elements|#)
            :test 'equal)))

(defun formatting-element-p (element)
  (let ((tag-name (typecase element
                    (element (slot-value element 'dom:tag-name))
                    (string element))))
    (member tag-name
            '("a" "b" "big" "code" "em" "font" "i" "nobr" "s"
              "small" "strike" "strong" "tt" "u")
            :test 'equal)))

(defun ordinary-element-p (element)
  (not (or (special-element-p element)
           (formatting-element-p element))))

;; TODO
(defun have-element-in-select-scope-p (parser element)
  (have-element-in-specific-scope-p
   parser
   element
   '()))

(defun parse-generic-rawtext-element (parser)
  (with-slots (stack-of-open-elements
               tokenizer
               original-insertion-mode
               insertion-mode) parser
    (let ((token (first stack-of-open-elements)))
      (insert-html-element-for-token parser token)
      (setf (slot-value tokenizer 'state) 'rawtext-state)
      (setf original-insertion-mode insertion-mode)
      (setf insertion-mode 'text))))

(defun parse-generic-rcdata-element (parser)
  (with-slots (stack-of-open-elements
               tokenizer
               original-insertion-mode
               insertion-mode) parser
    (let ((token (first stack-of-open-elements)))
      (insert-html-element-for-token parser token)
      (setf (slot-value tokenizer 'state) 'rcdata-state)
      (setf original-insertion-mode insertion-mode)
      (setf insertion-mode 'text))))

(defun reconstruct-active-formatting-elements (parser)
  (with-slots (active-formatting-elements stack-of-open-elements) parser
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
       (let ((element (insert-html-element-for-token parser entry)))
         (setf (nth* i active-formatting-elements) element)
         (when (eq element (nth* -1 active-formatting-elements))
           (return)))))))

(defun clear-active-formatting-elements-upto-last-marker (parser)
  (with-slots (active-formatting-elements) parser
    (reversef active-formatting-elements)
    (let ((entry (pop active-formatting-elements)))
      (loop while (and active-formatting-elements
                       (not (typep entry 'marker)))
            do (setf entry (pop active-formatting-elements))))
    (reversef active-formatting-elements)))

(defun push-onto-active-formatting-elements (parser element)
  (declare (ignore parser element)))

(defun generate-implied-end-tags (parser &key except)
  (with-slots (stack-of-open-elements) parser
    (let ((tag-name (slot-value (first stack-of-open-elements) 'dom:tag-name)))
      (when (and (member tag-name '("dd" "dt" "li" "optgroup" "option"
                                    "p" "rb" "rp" "rt" "rtc")
                         :test 'equal)
                 (not (equal tag-name except)))
        (pop stack-of-open-elements)
        (generate-implied-end-tags parser except)))))

(defun adoption-agency (parser)
  (declare (ignore parser)))

(defun close-p-element (parser)
  (generate-implied-end-tags parser :except "p")
  (unless (equal "p" (slot-value (current-node parser) 'dom:tag-name))
    (parse-error parser))
  (loop for element = (pop (slot-value parser 'stack-of-open-elements))
        until (equal "p" (slot-value element 'dom:tag-name))))

(defun close-cell (parser)
  (generate-implied-end-tags parser)
  (when (not (or (equal "td" (slot-value (current-node parser) 'dom:tag-name))
                 (equal "th" (slot-value (current-node parser) 'dom:tag-name))))
    (parse-error parser))
  (loop for element = (pop (slot-value parser 'stack-of-open-elements))
        until (or (equal "td" (slot-value element 'dom:tag-name))
                  (equal "th" (slot-value element 'dom:tag-name))))
  (clear-active-formatting-elements-upto-last-marker parser)
  (setf (slot-value parser 'insertion-mode) 'in-row))

(defun tree-construction-dispatcher (parser token)
  (if (or (null (slot-value parser 'stack-of-open-elements))
          t
          #|TODO: Handle other cases|#)
      (let ((function
             (case (slot-value parser 'insertion-mode)
               ('initial 'process-token-in-initial-insertion-mode)
               ('before-html 'process-token-in-before-html-insertion-mode)
               ('before-head 'process-token-in-before-head-insertion-mode)
               ('in-head 'process-token-in-in-head-insertion-mode)
               ('in-head-noscript 'process-token-in-in-head-noscript-insertion-mode)
               ('after-head 'process-token-in-after-head-insertion-mode)
               ('in-body 'process-token-in-in-body-insertion-mode)
               ('text 'process-token-in-text-insertion-mode)
               ('in-table 'process-token-in-in-table-insertion-mode)
               ('in-table-text 'process-token-in-in-table-text-insertion-mode)
               ('in-caption 'process-token-in-in-caption-insertion-mode)
               ('in-column-group 'process-token-in-in-column-group-insertion-mode)
               ('in-table-body 'process-token-in-in-table-body-insertion-mode)
               ('in-row 'process-token-in-in-row-insertion-mode)
               ('in-cell 'process-token-in-in-cell-insertion-mode)
               ('in-select 'process-token-in-in-select-insertion-mode)
               ('in-select-in-table 'process-token-in-in-select-in-table-insertion-mode)
               ('in-template 'process-token-in-in-template-insertion-mode)
               ('after-body 'process-token-in-after-body-insertion-mode)
               ('in-frameset 'process-token-in-in-frameset-insertion-mode)
               ('after-frameset 'process-token-in-after-frameset-insertion-mode)
               ('after-after-body 'process-token-in-after-after-body-insertion-mode)
               ('after-after-frameset 'process-token-in-after-after-frameset-insertion-mode))))
        (format t "~A~%" token)
        (setf (slot-value parser 'current-token) token)
        (funcall function parser))
    (error "TODO: Process the token according to the rules given in the section for parsing tokens in foreign content.")))

(defgeneric pas (source &key)
  (:method ((source string) &key)
   (with-input-from-string (stream source)
     (pas stream)))
  (:method ((stream stream) &key)
   (let ((tokenizer (make-instance 'tokenizer :stream stream)))
     (let ((parser (make-instance 'parser :tokenizer tokenizer)))
       (loop
        (handler-bind
            ((on-token (lambda (c)
                         (let ((token (slot-value c 'token)))
                           (tree-construction-dispatcher parser token)
                           (when (typep token 'end-of-file)
                             (return (slot-value parser 'document)))))))
          (funcall (slot-value tokenizer 'state) tokenizer)))))))

(defgeneric parse (source &key))

(defmethod parse (source &key)
  (let ((document nil))
    (labels ((transform (node)
               (when node
                 (let ((parent (typecase node
                                 (plump-dom:element
                                  (when-let* ((tag-name (plump:tag-name node))
                                              (constructor (constructor tag-name)))
                                    (construct constructor)))
                                 (plump-dom:text-node
                                  (make-instance 'text :data (plump-dom:text node))))))
                   (when (and parent
                              (typep node 'plump-dom:element))
                     (let ((children (mapcar #'transform
                                             (coerce (plump:children node) 'list))))
                       (loop for child in children
                             do (append-child parent child)))
                     (let ((attributes (plump:attributes node)))
                       (loop for name being the hash-keys of attributes
                             using (hash-value value)
                             do (dom:set-attribute parent name value))))
                   parent))))
      (let ((root (plump:parse source)))
        (when (and (typep root 'plump-dom:root)
                   (plusp (length (plump:children root))))
          (loop for child across (plump:children root)
                if (typep child 'plump-dom:doctype)
                do (setf document (make-instance 'document))
                else if (typep child 'plump-dom:element)
                do (when-let ((element (transform child)))
                     (if document
                         (append-child document element)
                       (return element)))
                finally (return document)))))))
