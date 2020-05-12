(in-package :html)

;; https://html.spec.whatwg.org/multipage/parsing.html#tree-construction

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
       #+sbcl
       (declare (sb-ext:muffle-conditions sb-ext:compiler-note))
       (with-slots
           (tokenizer
            document
            (token current-token)
            next-token
            foster-parenting
            stack-of-open-elements
            stack-of-template-insertion-modes
            adjusted-current-node
            head-element-pointer
            form-element-pointer
            insertion-mode
            original-insertion-mode
            scripting-flag
            frameset-ok-flag
            pending-table-character-tokens
            active-formatting-elements) parser
         (symbol-macrolet
             ((current-node (first (slot-value
                                    parser
                                    'stack-of-open-elements))))
           (macrolet
               ((switch-to (insertion-mode)
                  `(setf (slot-value parser 'insertion-mode) ,insertion-mode))
                (stop-parsing ()
                  `(signal 'stop-parsing)))
             (flet
                 (,@(loop for insertion-mode in *insertion-modes*
                      collect `(,(intern (format nil "PROCESS-TOKEN-IN-~A-INSERTION-MODE" insertion-mode))
                                ()
                                (,(intern (format nil "PROCESS-TOKEN-IN-~A-INSERTION-MODE" insertion-mode)) parser)))
                  (parse-error ()
                    (parse-error parser))
                  (ignore-token ()
                    (ignore-token parser))
                  (reprocess-current-token ()
                    (tree-construction-dispatcher parser))
                  (insert-comment (&optional position)
                    (declare (ignore position)))
                  (insert-character (&optional character-token)
                    (insert-character
                     parser
                     (or character-token
                         (slot-value parser 'current-token))))
                  (create-element-for-token (&optional namespace parent)
                    (let ((token (slot-value parser 'current-token)))
                      (create-element-for-token token namespace parent)))
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
                  (acknowledge-token-self-closing-flag ()
                    (acknowledge-token-self-closing-flag parser))
                  (reconstruct-active-formatting-elements ()
                    (reconstruct-active-formatting-elements parser))
                  (have-element-in-scope-p (tag-name)
                    (have-element-in-scope-p parser tag-name))
                  (have-element-in-list-item-scope-p (tag-name)
                    (have-element-in-list-item-scope-p parser tag-name))
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
                  (generate-all-implied-end-tags-thoroughly ()
                    (generate-all-implied-end-tags-thoroughly parser))
                  (clear-stack-back-to-table-context ()
                    (clear-stack-back-to-table-context parser))
                  (clear-stack-back-to-table-body-context ()
                    (clear-stack-back-to-table-body-context parser))
                  (clear-stack-back-to-table-row-context ()
                    (clear-stack-back-to-table-row-context parser))
                  (reset-insertion-mode-appropriately ()
                    (reset-insertion-mode-appropriately parser))
                  (clear-active-formatting-elements-upto-last-marker ()
                    (clear-active-formatting-elements-upto-last-marker parser))
                  (adoption-agency ()
                    (adoption-agency parser))
                  (push-onto-active-formatting-elements (element)
                    (push-onto-active-formatting-elements parser element))
                  (appropriate-place-for-inserting-node (&optional override-target)
                    (appropriate-place-for-inserting-node parser override-target)))
               ,@body)))))))

(define-condition stop-parsing () ())

(define-parser-insertion-mode initial
  (cond
   ((or (eq #\tab token) (eq #\newline token)
        (eq #\page token) (eq #\return token) (eq #\space token))
    (ignore-token))

   ((typep token 'comment-token)
    ;; TODO: Check this
    (insert-comment))

   ((typep token 'doctype-token)
    ;; TODO
    (switch-to 'before-html))

   (t
    ;; TODO
    (switch-to 'before-html)
    (reprocess-current-token))))

(define-parser-insertion-mode before-html
  (cond
   ((typep token 'doctype-token)
    (parse-error)
    (ignore-token))

   ((typep token 'comment-token)
    ;; TODO: Check this
    (insert-comment))

   ((or (eq #\tab token) (eq #\newline token)
        (eq #\page token) (eq #\return token) (eq #\space token))
    (ignore-token))

   ((a-start-tag-whose-tag-name-is "html")
    (let ((element (create-element-for-token "html" document)))
      (append-child document element)
      (push element stack-of-open-elements))
    (switch-to 'before-head))

   ((an-end-tag-whose-tag-name-is-one-of '("head" "body" "html" "br"))
    ;; Same as T
    (let ((element (make-instance 'element :tag-name "html")))
      (append-child document element)
      (push element stack-of-open-elements))
    (switch-to 'before-head)
    (reprocess-current-token))

   ((typep token 'end-tag)
    (parse-error)
    (ignore-token))

   (t
    (let ((element (make-instance 'element :tag-name "html")))
      (append-child document element)
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
    (let ((head (insert-html-element-for-token
                 (make-instance 'start-tag :tag-name "head"))))
      (setf head-element-pointer head))
    (switch-to 'in-head)
    (reprocess-current-token))

   ((typep token 'end-tag)
    (parse-error)
    (ignore-token))

   (t
    (let ((head (insert-html-element-for-token
                 (make-instance 'start-tag :tag-name "head"))))
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
    (switch-to 'in-head-noscript))

   ;; TODO
   ((a-start-tag-whose-tag-name-is "script"))

   ((an-end-tag-whose-tag-name-is "head")
    (assert (equal "head" (tag-name current-node)))
    (pop stack-of-open-elements)
    (switch-to 'after-head))

   ((an-end-tag-whose-tag-name-is-one-of '("body" "html" "br"))
    ;; Same as T
    (assert (equal "head" (tag-name current-node)))
    (pop stack-of-open-elements)
    (switch-to 'after-head)
    (reprocess-current-token))

   ((a-start-tag-whose-tag-name-is "template")
    (insert-html-element-for-token)
    (appendf active-formatting-elements (list (make-marker)))
    (setf frameset-ok-flag nil)
    (switch-to 'in-template)
    (push 'in-template stack-of-template-insertion-modes))

   ((an-end-tag-whose-tag-name-is "template")
    (if (not (find "template" stack-of-open-elements
                   :test 'equal
                   :key 'tag-name))
        (progn
          (parse-error)
          (ignore-token))
      (progn
        (generate-all-implied-end-tags-thoroughly)
        (unless (equal "template" (tag-name current-node))
          (parse-error))
        (loop for element = (pop stack-of-open-elements)
              until (equal "template" (tag-name element)))
        (clear-active-formatting-elements-upto-last-marker)
        (pop stack-of-template-insertion-modes)
        (reset-insertion-mode-appropriately))))

   ((or (a-start-tag-whose-tag-name-is "head")
        (typep token 'end-tag))
    (parse-error)
    (ignore-token))

   (t
    (assert (equal "head" (tag-name current-node)))
    (pop stack-of-open-elements)
    (switch-to 'after-head)
    (reprocess-current-token))))

(define-parser-insertion-mode in-head-noscript
  (cond
   ((typep token 'doctype-token)
    (parse-error)
    (ignore-token))

   ((a-start-tag-whose-tag-name-is "html")
    (process-token-in-in-body-insertion-mode))

   ((an-end-tag-whose-tag-name-is "noscript")
    (assert (equal "noscript" (tag-name current-node)))
    (pop stack-of-open-elements)
    (assert (equal "head" (tag-name current-node)))
    (switch-to 'in-head))

   ((or (or (eq #\tab token) (eq #\newline token)
            (eq #\page token) (eq #\return token) (eq #\space token))
        (typep token 'comment-token)
        (a-start-tag-whose-tag-name-is-one-of '("basefont" "bgsound" "link"
                                                "meta" "noframes" "style")))
    (process-token-in-in-head-insertion-mode))

   ((an-end-tag-whose-tag-name-is "br")
    ;; Same as T
    (parse-error)
    (assert (equal "noscript" (tag-name current-node)))
    (pop stack-of-open-elements)
    (assert (equal "head" (tag-name current-node)))
    (switch-to 'in-head)
    (reprocess-current-token))

   ((or (a-start-tag-whose-tag-name-is-one-of '("head" "noscript"))
        (typep token 'end-tag))
    (parse-error)
    (ignore-token))

   (t
    (parse-error)
    (assert (equal "noscript" (tag-name current-node)))
    (pop stack-of-open-elements)
    (assert (equal "head" (tag-name current-node)))
    (switch-to 'in-head)
    (reprocess-current-token))))

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
      (removef stack-of-open-elements node)))

   ((an-end-tag-whose-tag-name-is "template")
    (process-token-in-in-head-insertion-mode))

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
              :key 'tag-name)
        (ignore-token)
      #|TODO|#))

   ((or (a-start-tag-whose-tag-name-is-one-of '("base" "basefont" "bgsound" "link"
                                                "meta" "noframes" "script" "style"
                                                "template" "title"))
        (an-end-tag-whose-tag-name-is "template"))
    (process-token-in-in-head-insertion-mode))

   ((a-start-tag-whose-tag-name-is "body")
    (parse-error)
    (if (or (= 1 (length stack-of-open-elements))
            (not (equal "body" (tag-name (second stack-of-open-elements))))
            (find "template" stack-of-open-elements
                  :test 'equal
                  :key 'tag-name))
        (ignore-token)
      (progn
        (setf frameset-ok-flag nil)
        #|TODO|#)))

   ((a-start-tag-whose-tag-name-is "frameset")
    (parse-error)
    #|TODO|#)

   ((typep token 'end-of-file)
    (if stack-of-template-insertion-modes
        (process-token-in-in-template-insertion-mode)
      (progn
        (if (not (find-if (lambda (element)
                            (member (tag-name element)
                                    '("dd" "dt" "li" "optgroup" "option"
                                      "p" "rb" "rp" "rt" "rtc"
                                      "tbody" "td" "tfoot" "th" "thead" "tr"
                                      "body" "html")
                                    :test 'equal))
                          stack-of-open-elements))
            (parse-error))
        (stop-parsing))))

   ((an-end-tag-whose-tag-name-is "body")
    (if (not (have-element-in-scope-p "body"))
        (progn
          (parse-error)
          (ignore-token))
      (if (not (find-if (lambda (element)
                          (member (tag-name element)
                                  '("dd" "dt" "li" "optgroup" "option"
                                    "p" "rb" "rp" "rt" "rtc"
                                    "tbody" "td" "tfoot" "th" "thead" "tr"
                                    "body" "html")
                                  :test 'equal))
                        stack-of-open-elements))
          (parse-error)))
    (switch-to 'after-body))

   ((an-end-tag-whose-tag-name-is "html")
    (if (not (have-element-in-scope-p "body"))
        (progn
          (parse-error)
          (ignore-token))
      (if (not (find-if (lambda (element)
                          (member (tag-name element)
                                  '("dd" "dt" "li" "optgroup" "option"
                                    "p" "rb" "rp" "rt" "rtc"
                                    "tbody" "td" "tfoot" "th" "thead" "tr"
                                    "body" "html")
                                  :test 'equal))
                        stack-of-open-elements))
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
               (member (tag-name current-node)
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
      (setf next-token nil))
    (setf frameset-ok-flag nil))

   ((a-start-tag-whose-tag-name-is "form")
    (if (and form-element-pointer
             (not (find "template" stack-of-open-elements
                        :test 'equal
                        :key 'tag-name)))
        (progn
          (parse-error)
          (ignore-token))
      (progn
        (when (have-element-in-button-scope-p "p")
          (close-p-element))
        (let ((element (insert-html-element-for-token)))
          (unless (find "template" stack-of-open-elements
                        :test 'equal
                        :key 'tag-name)
            (setf form-element-pointer element))))))

   ;; https://github.com/html5lib/html5lib-python/blob/master/html5lib/html5parser.py#L1111
   ;; TODO
   ((a-start-tag-whose-tag-name-is "li")
    (let ((node current-node))
      (setf frameset-ok-flag nil)
      (tagbody
       :loop
       (loop while (and (typep node 'element)
                        (equal "li" (tag-name node)))
             do
             (generate-implied-end-tags :except "li")
             (unless (equal "li" (tag-name node))
               (parse-error))
             (loop for element = (pop stack-of-open-elements)
                   until (equal "li" (tag-name node)))
             (go :done))
       (if (and (special-element-p node)
                (not (member (tag-name node)
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

            until (equal "button" (tag-name element))))
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
    (if (not (have-element-in-scope-p (slot-value token 'tag-name)))
        (progn
          (parse-error)
          (ignore-token))
      (progn
        (generate-implied-end-tags)
        (unless (equal (tag-name current-node)
                       (slot-value token 'tag-name))
          (parse-error))
        (loop for element = (pop stack-of-open-elements)
              until (equal (tag-name element)
                           (slot-value token 'tag-name))))))

   ((an-end-tag-whose-tag-name-is "form")
    (if (not (find "template" stack-of-open-elements
                   :test 'equal
                   :key 'tag-name))
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
            (unless (equal "form" (tag-name current-node))
              (parse-error))
            (loop for element = (pop stack-of-open-elements)
                  until (equal "form" (tag-name element))))))))

   ((an-end-tag-whose-tag-name-is "p")
    (unless (have-element-in-button-scope-p "p")
      (parse-error)
      (insert-html-element-for-token
       (make-instance 'start-tag :tag-name "p")))
    (close-p-element))

   ((an-end-tag-whose-tag-name-is "li")
    (if (not (have-element-in-button-scope-p "li"))
        (progn
          (parse-error)
          (ignore-token))
      (progn
        (generate-implied-end-tags :except "li")
        (unless (equal "li" (tag-name current-node))
          (parse-error))
        (loop for element = (pop stack-of-open-elements)
              until (equal "li" (tag-name element))))))

   ((an-end-tag-whose-tag-name-is-one-of '("dd" "dt"))
    (if (not (have-element-in-scope-p (slot-value token 'tag-name)))
        (progn
          (parse-error)
          (ignore-token))
      (progn
        (generate-implied-end-tags :except (slot-value token 'tag-name))
        (unless (equal (tag-name current-node)
                       (slot-value token 'tag-name))
          (parse-error))
        (loop for element = (pop stack-of-open-elements)
              until (member (tag-name element)
                            (slot-value token 'tag-name))))))

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
        (unless (equal (tag-name current-node)
                       (slot-value token 'tag-name))
          (parse-error))
        (loop for element = (pop stack-of-open-elements)
              until (member (tag-name element)
                            '("h1" "h2" "h3" "h4" "h5" "h6")
                            :test 'equal)))))

   ((an-end-tag-whose-tag-name-is "sarcasm"))

   ((a-start-tag-whose-tag-name-is "a")
    (let ((last-marker-position (position-if 'marker-p active-formatting-elements
                                             :from-end t)))
      (when-let ((a
                  (or (and last-marker-position
                           (find "a" (subseq active-formatting-elements
                                             last-marker-position
                                             (length active-formatting-elements))
                                 :test 'equal
                                 :key 'tag-name))
                      (and (null last-marker-position)
                           (find "a" active-formatting-elements
                                 :test 'equal
                                 :key 'tag-name)))))
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
    (if (not (have-element-in-scope-p (slot-value token 'tag-name)))
        (progn
          (parse-error)
          (ignore-token))
      (progn
        (generate-implied-end-tags)
        (unless (equal (tag-name current-node)
                       (slot-value token 'tag-name))
          (parse-error))
        (loop for element = (pop stack-of-open-elements)
              until (equal (tag-name element)
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
    (setf (slot-value token 'tag-name) "img")
    (reprocess-current-token))

   ((a-start-tag-whose-tag-name-is "textarea")
    (insert-html-element-for-token)
    ;; Newlines at the start of textarea elements are ignored as an authoring convenience.
    (when (eq #\newline next-token)
      (setf next-token nil))
    (with-slots (tokenizer) parser
      (setf (slot-value tokenizer 'state) 'rcdata-state))
    (setf original-insertion-mode insertion-mode)
    (setf frameset-ok-flag nil)
    (setf insertion-mode 'text))

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
        (switch-to 'in-select-in-table)
      (switch-to 'in-select)))

   ((a-start-tag-whose-tag-name-is-one-of '("optgroup" "option"))
    (when (equal "option" (tag-name current-node))
      (pop stack-of-open-elements))
    (reconstruct-active-formatting-elements)
    (insert-html-element-for-token))

   ((a-start-tag-whose-tag-name-is-one-of '("rb" "rtc"))
    (when (have-element-in-scope-p "ruby")
      (generate-implied-end-tags)
      (unless (equal "ruby" (tag-name current-node))
        (parse-error)))
    (insert-html-element-for-token))

   ((a-start-tag-whose-tag-name-is-one-of '("rp" "rt"))
    (when (have-element-in-scope-p "ruby")
      (generate-implied-end-tags :except "rtc")
      (unless (or (equal "rtc" (tag-name current-node))
                  (equal "ruby" (tag-name current-node)))
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
          do (if (equal (tag-name node)
                        (slot-value token 'tag-name))
                 (progn
                   (generate-implied-end-tags)
                   (unless (equal (tag-name (first stack-of-open-elements))
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
   ((and (member (tag-name current-node)
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
    (appendf active-formatting-elements (list (make-marker)))
    (insert-html-element-for-token)
    (switch-to 'in-caption))

   ((a-start-tag-whose-tag-name-is "colgroup")
    (clear-stack-back-to-table-context)
    (insert-html-element-for-token)
    (switch-to 'in-column-group))

   ((a-start-tag-whose-tag-name-is "col")
    (clear-stack-back-to-table-context)
    (insert-html-element-for-token
     (make-instance 'start-tag :tag-name "colgroup"))
    (switch-to 'in-column-group)
    (reprocess-current-token))

   ((a-start-tag-whose-tag-name-is-one-of '("tbody" "tfoot" "thead"))
    (clear-stack-back-to-table-context)
    (insert-html-element-for-token)
    (switch-to 'in-table-body))

   ((a-start-tag-whose-tag-name-is-one-of '("td" "th" "tr"))
    (clear-stack-back-to-table-context)
    (insert-html-element-for-token
     (make-instance 'start-tag :tag-name "tbody"))
    (switch-to 'in-table-body)
    (reprocess-current-token))

   ((a-start-tag-whose-tag-name-is "table")
    (parse-error)
    (if (not (have-element-in-table-scope-p "table"))
        (ignore-token)
      (progn
        (loop for element = (pop stack-of-open-elements)
              until (equal "table" (tag-name element)))
        (reset-insertion-mode-appropriately)
        (reprocess-current-token))))

   ((an-end-tag-whose-tag-name-is "table")
    (if (not (have-element-in-table-scope-p "table"))
        (progn
          (parse-error)
          (ignore-token))
      (progn
        (loop for element = (pop stack-of-open-elements)
              until (equal "table" (tag-name element)))
        (reset-insertion-mode-appropriately))))

   ((an-end-tag-whose-tag-name-is-one-of '("body" "caption" "col"
                                           "colgroup" "html" "tbody"
                                           "td" "tfoot" "th" "thead" "tr"))
    (parse-error)
    (ignore-token))

   ((or (a-start-tag-whose-tag-name-is-one-of '("style" "script" "template"))
        (an-end-tag-whose-tag-name-is "template"))
    (process-token-in-in-head-insertion-mode))

   ((a-start-tag-whose-tag-name-is "input")
    (let ((attribute (find "type" (slot-value token 'attributes)
                           :test 'equal
                           :key 'attribute-name)))
      (if (or (null attribute)
              (not (string-equal "hidden" (attribute-value attribute))))
          ;; Same as T
          (progn
            (parse-error)
            (setf foster-parenting t)
            (process-token-in-in-body-insertion-mode)
            (setf foster-parenting nil))
        (progn
          (parse-error)
          (insert-html-element-for-token)
          (pop stack-of-open-elements)
          (acknowledge-token-self-closing-flag)))))

   ((a-start-tag-whose-tag-name-is "form")
    (parse-error)
    (if (or (find "template" stack-of-open-elements
                  :test 'equal
                  :key 'tag-name)
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

   (t
    (if (find-if-not 'ascii-whitespace-p pending-table-character-tokens)
        (progn
          (parse-error)
          (loop with previous-token = token
                for character-token in pending-table-character-tokens
                do (progn
                     (setf token character-token)
                     ;; Same as T in `in-table` insertion mode
                     (parse-error)
                     (setf foster-parenting t)
                     (process-token-in-in-body-insertion-mode)
                     (setf foster-parenting nil))
                finally (setf token previous-token)))
      (progn
        (loop for character-token in pending-table-character-tokens
              do (insert-character character-token))
        (switch-to original-insertion-mode)
        (reprocess-current-token))))))

(define-parser-insertion-mode in-caption
  (cond
   ((an-end-tag-whose-tag-name-is "caption")
    (if (not (have-element-in-table-scope-p "caption"))
        (progn
          (parse-error)
          (ignore-token))
      (generate-implied-end-tags))
    (unless (equal "caption" (tag-name current-node))
      (parse-error))
    (loop for element = (pop stack-of-open-elements)
          until (equal "caption" (tag-name element)))
    (clear-active-formatting-elements-upto-last-marker)
    (switch-to 'in-table))

   ((or (a-start-tag-whose-tag-name-is-one-of '("caption" "col" "colgroup"
                                                "tbody" "td" "tfoot"
                                                "th" "thead" "tr"))
        (an-end-tag-whose-tag-name-is "table"))
    (if (not (have-element-in-table-scope-p "caption"))
        (progn
          (parse-error)
          (ignore-token))
      (generate-implied-end-tags))
    (unless (equal "caption" (tag-name current-node))
      (parse-error))
    (loop for element = (pop stack-of-open-elements)
          until (equal "caption" (tag-name element)))
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
    (if (not (equal "colgroup" (tag-name current-node)))
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
    (if (not (equal "colgroup" (tag-name current-node)))
        (progn
          (parse-error)
          (ignore-token))
      (pop stack-of-open-elements))
    (switch-to 'in-table)
    (reprocess-current-token))))

(define-parser-insertion-mode in-table-body
  (cond
   ((a-start-tag-whose-tag-name-is "tr")
    (clear-stack-back-to-table-body-context)
    (insert-html-element-for-token)
    (switch-to 'in-row))

   ((a-start-tag-whose-tag-name-is-one-of '("th" "td"))
    (parse-error)
    (clear-stack-back-to-table-body-context)
    (insert-html-element-for-token
     (make-instance 'start-tag :tag-name "tr"))
    (switch-to 'in-row)
    (reprocess-current-token))

   ((an-end-tag-whose-tag-name-is-one-of '("tbody" "tfoot" "thead"))
    (if (not (have-element-in-table-scope-p (slot-value token 'tag-name)))
        (progn
          (parse-error)
          (ignore-token))
      (progn
        (clear-stack-back-to-table-body-context)
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
        (clear-stack-back-to-table-body-context)
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
    (clear-stack-back-to-table-row-context)
    (insert-html-element-for-token)
    (switch-to 'in-cell)
    (appendf active-formatting-elements (list (make-marker))))

   ((an-end-tag-whose-tag-name-is "tr")
    (if (not (have-element-in-table-scope-p "tr"))
        (progn
          (parse-error)
          (ignore-token))
      (progn
        (clear-stack-back-to-table-row-context)
        (assert (equal "tr" (tag-name current-node)))
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
        (clear-stack-back-to-table-row-context)
        (assert (equal "tr" (tag-name current-node)))
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
        (clear-stack-back-to-table-row-context)
        (assert (equal "tr" (tag-name current-node)))
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
    (unless (equal (tag-name current-node)
                   (slot-value token 'tag-name))
      (parse-error))
    (loop for element = (pop stack-of-open-elements)
          until (equal (tag-name element)
                       (slot-value token 'tag-name)))
    (clear-active-formatting-elements-upto-last-marker)
    (switch-to 'in-row))

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
    (when (equal "option" (tag-name current-node))
      (pop stack-of-open-elements))
    (insert-html-element-for-token))

   ((a-start-tag-whose-tag-name-is "optgroup")
    (when (equal "option" (tag-name current-node))
      (pop stack-of-open-elements))
    (when (equal "optgroup" (tag-name current-node))
      (pop stack-of-open-elements))
    (insert-html-element-for-token))

   ((an-end-tag-whose-tag-name-is "optgroup")
    (when (and (equal "option" (tag-name current-node))
               (equal "optgroup" (tag-name (second stack-of-open-elements))))
      (pop stack-of-open-elements))
    (if (equal "optgroup" (tag-name current-node))
        (pop stack-of-open-elements)
      (progn
        (parse-error)
        (ignore-token))))

   ((an-end-tag-whose-tag-name-is "option")
    (if (equal "option" (tag-name current-node))
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
              until (equal "select" (tag-name element)))
        (reset-insertion-mode-appropriately))))

   ((a-start-tag-whose-tag-name-is "select")
    (parse-error)
    (if (not (have-element-in-select-scope-p "select"))
        (ignore-token)
      (progn
        (loop for element = (pop stack-of-open-elements)
              until (equal "select" (tag-name element)))
        (reset-insertion-mode-appropriately))))

   ((a-start-tag-whose-tag-name-is-one-of '("input" "keygen" "textarea"))
    (parse-error)
    (if (not (have-element-in-select-scope-p "select"))
        (ignore-token)
      (progn
        (loop for element = (pop stack-of-open-elements)
              until (equal "select" (tag-name element)))
        (reset-insertion-mode-appropriately)
        (reprocess-current-token))))

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
          until (equal "select" (tag-name element)))
    (reset-insertion-mode-appropriately)
    (reprocess-current-token))

   ((an-end-tag-whose-tag-name-is-one-of '("caption" "table" "tbody"
                                           "tfoot" "thead" "tr" "td" "th"))
    (parse-error)
    (if (not (have-element-in-table-scope-p (slot-value token 'tag-name)))
        (ignore-token)
      (progn
        (loop for element = (pop stack-of-open-elements)
              until (equal "select" (tag-name element)))
        (reset-insertion-mode-appropriately)
        (reprocess-current-token))))

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
    (pop stack-of-template-insertion-modes)
    (push 'in-table stack-of-template-insertion-modes)
    (switch-to 'in-table)
    (reprocess-current-token))

   ((a-start-tag-whose-tag-name-is "col")
    (pop stack-of-template-insertion-modes)
    (push 'in-column-group stack-of-template-insertion-modes)
    (switch-to 'in-column-group)
    (reprocess-current-token))

   ((a-start-tag-whose-tag-name-is "tr")
    (pop stack-of-template-insertion-modes)
    (push 'in-table-body stack-of-template-insertion-modes)
    (switch-to 'in-table-body)
    (reprocess-current-token))

   ((a-start-tag-whose-tag-name-is-one-of '("td" "th"))
    (pop stack-of-template-insertion-modes)
    (push 'in-row stack-of-template-insertion-modes)
    (switch-to 'in-row)
    (reprocess-current-token))

   ((typep token 'start-tag)
    (pop stack-of-template-insertion-modes)
    (push 'in-body stack-of-template-insertion-modes)
    (switch-to 'in-body)
    (reprocess-current-token))

   ((typep token 'end-tag)
    (parse-error)
    (ignore-token))

   ((typep token 'end-of-file)
    (if (not (find "template" stack-of-open-elements
                   :test 'equal
                   :key 'tag-name))
        (stop-parsing)
      (parse-error))
    (loop for element = (pop stack-of-open-elements)
          until (equal "template" (tag-name element)))
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

   ;; TODO
   ((an-end-tag-whose-tag-name-is "frameset")
    (pop stack-of-open-elements)
    (switch-to 'after-frameset))

   ((a-start-tag-whose-tag-name-is "frame")
    (insert-html-element-for-token)
    (pop stack-of-open-elements)
    (acknowledge-token-self-closing-flag))

   ((a-start-tag-whose-tag-name-is "noframes")
    (process-token-in-in-head-insertion-mode))

   ;; TODO
   ((typep token 'end-of-file)
    (when (not (equal "html" (tag-name current-node)))
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

(defstruct marker)

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
   (original-insertion-mode
    :initform 'initial)
   (current-token
    :initform nil)
   (next-token
    :initform nil)
   (stack-of-open-elements
    :initform nil)
   (stack-of-template-insertion-modes
    :initform nil)
   (head-element-pointer
    :initform nil)
   (form-element-pointer
    :initform nil)
   (scripting-flag
    :initform nil)
   (frameset-ok-flag
    :initform nil)
   (active-formatting-elements
    :initform nil)
   (pending-table-character-tokens
    :initform nil)
   (open-texts
    :initform nil)))

(defun parse-error (parser)
  (declare (ignore parser))
  (cerror "Continue" 'parse-error))

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
      (let ((text (car (last (children adjusted-insertion-location)))))
        (if (typep text 'text)
            (write-char data (slot-value text 'data-stream))
          (let ((text (make-instance 'text :data-stream (make-string-output-stream))))
            (write-char data (slot-value text 'data-stream))
            (push text (slot-value parser 'open-texts))
            (append-child adjusted-insertion-location text)))))))

;; TODO: Respect namespace & parent
(defun create-element-for-token (token &optional namespace parent)
  (declare (ignore namespace parent))
  (check-type token start-tag)
  (let ((element (make-instance 'element
                                :tag-name (slot-value token 'tag-name))))
    (loop for attribute in (slot-value token 'attributes)
          do (dom:set-attribute
              element
              (attribute-name attribute)
              (attribute-value attribute)))
    element))

(defun insert-html-element-for-token (parser token)
  (insert-foreign-element-for-token parser token "html"))

(defun insert-foreign-element-for-token (parser token namespace)
  (let ((adjusted-insertion-location (appropriate-place-for-inserting-node parser)))
    (let ((element (create-element-for-token token namespace)))
      (append-child adjusted-insertion-location element)
      (push element (slot-value parser 'stack-of-open-elements))
      element)))

;; TODO
(defun appropriate-place-for-inserting-node (parser &optional override-target)
  (let ((target (if override-target
                    override-target
                  (current-node parser))))
    target))

;; TODO: Check exact node condition
(defun have-element-in-specific-scope-p (parser element list)
  (check-type element (or element string))
  (with-slots (stack-of-open-elements) parser
    (let* ((tag-name (typecase element
                       (element (tag-name element))
                       (string element))))
      (loop for open-element in stack-of-open-elements
            if (or (eq element open-element)
                   (equal tag-name (tag-name open-element)))
            do (return t)
            else if (find tag-name list :test 'equal)
            do (return)))))

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
                    (element (tag-name element))
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
                    (element (tag-name element))
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
  (with-slots (tokenizer
               current-token
               original-insertion-mode
               insertion-mode) parser
    (insert-html-element-for-token parser current-token)
    (setf (slot-value tokenizer 'state) 'rawtext-state)
    (setf original-insertion-mode insertion-mode)
    (setf insertion-mode 'text)))

(defun parse-generic-rcdata-element (parser)
  (with-slots (tokenizer
               current-token
               original-insertion-mode
               insertion-mode) parser
    (insert-html-element-for-token parser current-token)
    (setf (slot-value tokenizer 'state) 'rcdata-state)
    (setf original-insertion-mode insertion-mode)
    (setf insertion-mode 'text)))

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

;; TODO
(defun push-onto-active-formatting-elements (parser element)
  (with-slots (active-formatting-elements) parser
    (appendf active-formatting-elements (list element))))

(defun generate-implied-end-tags (parser &key except)
  (with-slots (stack-of-open-elements) parser
    (let ((tag-name (tag-name (current-node parser))))
      (when (and (member tag-name '("dd" "dt" "li" "optgroup" "option"
                                    "p" "rb" "rp" "rt" "rtc")
                         :test 'equal)
                 (not (equal tag-name except)))
        (pop stack-of-open-elements)
        (generate-implied-end-tags parser :except except)))))

(defun generate-all-implied-end-tags-thoroughly (parser)
  (with-slots (stack-of-open-elements) parser
    (let ((tag-name (tag-name (current-node parser))))
      (when (member tag-name '("caption" "colgroup" "dd" "dt"
                               "li" "optgroup" "option"
                               "p" "rb" "rp" "rt" "rtc" "tbody" "td"
                               "tfoot" "th" "thead" "tr")
                    :test 'equal)
        (pop stack-of-open-elements)
        (generate-all-implied-end-tags-thoroughly parser)))))

(defun adoption-agency (parser)
  (with-slots (stack-of-open-elements active-formatting-elements) parser
    (let ((token (slot-value parser 'current-token))
          subject
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
      (setf subject (slot-value token 'tag-name))
      ;; Step 2
      (let ((current-node (current-node parser)))
        (when (and (equal subject (tag-name current-node))
                   (not (find current-node active-formatting-elements)))
          (pop stack-of-open-elements)
          (return-from adoption-agency)))
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
                     :test 'equal
                     :key 'tag-name
                     :from-end t))
         (unless formatting-element
           ;; Act as `any other end tag` entry from `in-body` insertion mode
           (loop for node in stack-of-open-elements
                 do (if (equal (tag-name node)
                               (slot-value token 'tag-name))
                        (progn
                          (generate-implied-end-tags parser)
                          (unless (equal (tag-name (first stack-of-open-elements))
                                         (slot-value token 'tag-name))
                            (parse-error parser))
                          (loop for n = (pop stack-of-open-elements)
                                until (eq n node))
                          (return))
                      (progn
                        (when (special-element-p node)
                          (parse-error parser)
                          (ignore-token parser)
                          (return)))))
           (return-from adoption-agency)))
       ;; Step 7
       (unless (find formatting-element stack-of-open-elements)
         (parse-error parser)
         (removef stack-of-open-elements formatting-element)
         (return-from adoption-agency))
       ;; Step 8
       (when (and (find formatting-element stack-of-open-elements)
                  (not (have-element-in-scope-p parser formatting-element)))
         (parse-error parser)
         (return-from adoption-agency))
       ;; Step 9
       (unless (eq (current-node parser) formatting-element)
         (parse-error parser))
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
                       (slot-value parser 'current-token)
                       "html"
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
       (when-let ((parent (dom:parent last-node)))
         (removef (slot-value parent 'dom:children) last-node))
       (append-child last-node node)
       ;; Step 14.10
       (setf last-node node)
       ;; Step 14.11
       (go :inner-loop)
       :next-step-in-the-overall-algorithm
       ;; Step 15
       (let ((place (appropriate-place-for-inserting-node parser common-ancestor)))
         (append-child place last-node ))
       ;; Step 16
       (let ((element (create-element-for-token
                       (slot-value parser 'current-token)
                       "html"
                       furthest-block)))
         ;; Step 17
         (loop for child in (children furthest-block)
               do (append-child element child))
         (setf (slot-value furthest-block 'dom:children) nil)
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
         (go :outer-loop))))))

(defun close-p-element (parser)
  (generate-implied-end-tags parser :except "p")
  (unless (equal "p" (tag-name (current-node parser)))
    (parse-error parser))
  (loop for element = (pop (slot-value parser 'stack-of-open-elements))
        until (equal "p" (tag-name element))))

(defun close-cell (parser)
  (generate-implied-end-tags parser)
  (when (not (or (equal "td" (tag-name (current-node parser)))
                 (equal "th" (tag-name (current-node parser)))))
    (parse-error parser))
  (loop for element = (pop (slot-value parser 'stack-of-open-elements))
        until (or (equal "td" (tag-name element))
                  (equal "th" (tag-name element))))
  (clear-active-formatting-elements-upto-last-marker parser)
  (setf (slot-value parser 'insertion-mode) 'in-row))

;; TODO
(defun clear-stack-back-to-table-context (parser)
  (declare (ignore parser)))

;; TODO
(defun clear-stack-back-to-table-body-context (parser)
  (declare (ignore parser)))

;; TODO
(defun clear-stack-back-to-table-row-context (parser)
  (declare (ignore parser)))

(defun reset-insertion-mode-appropriately (parser)
  (with-slots (stack-of-open-elements
               stack-of-template-insertion-modes
               head-element-pointer) parser
    (let (last
          node
          ancestor)
      (macrolet ((switch-to (insertion-mode)
                   `(setf (slot-value parser 'insertion-mode) ,insertion-mode)))
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
           (when (equal "select" (tag-name node))
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
              (when (equal "template" (tag-name ancestor))
                (go :done))
              ;; Step 4.6
              (when (equal "table" (tag-name ancestor))
                (switch-to 'in-select-in-table)
                (return))
              ;; Step 4.7
              (go :loop)
              ;; Step 4.8
              :done
              (switch-to 'in-select)
              (return)))
           ;; Step 5
           (when (and (or (equal "td" (tag-name node))
                          (equal "th" (tag-name node)))
                      (not last))
             (switch-to 'in-cell)
             (return))
           ;; Step 6
           (when (equal "tr" (tag-name node))
             (switch-to 'in-row)
             (return))
           ;; Step 7
           (when (or (equal "tbody" (tag-name node))
                     (equal "thead" (tag-name node))
                     (equal "tfoot" (tag-name node)))
             (switch-to 'in-table-body)
             (return))
           ;; Step 8
           (when (equal "caption" (tag-name node))
             (switch-to 'in-caption)
             (return))
           ;; Step 9
           (when (equal "colgroup" (tag-name node))
             (switch-to 'in-column-group)
             (return))
           ;; Step 10
           (when (equal "table" (tag-name node))
             (switch-to 'in-table)
             (return))
           ;; Step 11
           (when (equal "template" (tag-name node))
             (switch-to (first stack-of-template-insertion-modes))
             (return))
           ;; Step 12
           (when (and (equal "head" (tag-name node))
                      (not last))
             (switch-to 'in-head)
             (return))
           ;; Step 13
           (when (equal "body" (tag-name node))
             (switch-to 'in-body)
             (return))
           ;; Step 14
           (when (equal "frameset" (tag-name node))
             (switch-to 'in-frameset)
             (return))
           ;; Step 15
           (when (equal "html" (tag-name node))
             (if (null head-element-pointer)
                 (progn
                   (switch-to 'before-head)
                   (return))
               (progn
                 (switch-to 'after-head)
                 (return))))
           ;; Step 16
           (when last
             (switch-to 'in-body)
             (return))
           ;; Step 17
           (let ((node-position (position node stack-of-open-elements)))
             (setf node (nth (1+ node-position) stack-of-open-elements)))
           ;; Step 18
           (go :loop)))))))

;; TODO
(defun acknowledge-token-self-closing-flag (parser))

(defun tree-construction-dispatcher (parser)
  (if (or (null (slot-value parser 'stack-of-open-elements))
          t
          #|TODO: Handle other cases|#)
      (let ((function
             (case (slot-value parser 'insertion-mode)
               (initial 'process-token-in-initial-insertion-mode)
               (before-html 'process-token-in-before-html-insertion-mode)
               (before-head 'process-token-in-before-head-insertion-mode)
               (in-head 'process-token-in-in-head-insertion-mode)
               (in-head-noscript 'process-token-in-in-head-noscript-insertion-mode)
               (after-head 'process-token-in-after-head-insertion-mode)
               (in-body 'process-token-in-in-body-insertion-mode)
               (text 'process-token-in-text-insertion-mode)
               (in-table 'process-token-in-in-table-insertion-mode)
               (in-table-text 'process-token-in-in-table-text-insertion-mode)
               (in-caption 'process-token-in-in-caption-insertion-mode)
               (in-column-group 'process-token-in-in-column-group-insertion-mode)
               (in-table-body 'process-token-in-in-table-body-insertion-mode)
               (in-row 'process-token-in-in-row-insertion-mode)
               (in-cell 'process-token-in-in-cell-insertion-mode)
               (in-select 'process-token-in-in-select-insertion-mode)
               (in-select-in-table 'process-token-in-in-select-in-table-insertion-mode)
               (in-template 'process-token-in-in-template-insertion-mode)
               (after-body 'process-token-in-after-body-insertion-mode)
               (in-frameset 'process-token-in-in-frameset-insertion-mode)
               (after-frameset 'process-token-in-after-frameset-insertion-mode)
               (after-after-body 'process-token-in-after-after-body-insertion-mode)
               (after-after-frameset 'process-token-in-after-after-frameset-insertion-mode))))
        (funcall function parser))
    (error "TODO: Process the token according to the rules given in the section for parsing tokens in foreign content.")))

(defgeneric parse (source)
  (:method ((string string))
   (with-input-from-string (stream string)
     (parse stream)))
  (:method ((stream stream))
   (let ((tokenizer (make-instance 'tokenizer :stream stream)))
     (let ((parser (make-instance 'parser :tokenizer tokenizer)))
       (with-slots (document open-texts) parser
         (block :parsing
           (unwind-protect
               (handler-bind
                   ((stop-parsing
                     (lambda (c)
                       (declare (ignore c))
                       (return-from :parsing)))
                    (parse-error 
                     (lambda (e)
                       (declare (ignore e))
                       (continue))))
                 (loop for token = (tokenize tokenizer)
                       until (typep token 'end-of-file)
                       do (with-slots (current-token next-token) parser
                            (if current-token
                                (if next-token
                                    (setf current-token next-token
                                          next-token token)
                                  (setf next-token token))
                              (setf current-token token))
                            (when next-token
                              (tree-construction-dispatcher parser))
                            (when (typep next-token 'end-of-file)
                              (return-from :parsing)))))
             (loop for text in open-texts
                   for stream = (slot-value text 'data-stream)
                   for data = (get-output-stream-string stream)
                   do (progn
                        (close stream)
                        (setf (slot-value text 'dom:data) data
                              (slot-value text 'data-stream) nil))
                   finally (setf open-texts nil))))
         document)))))

(defun trace-parser ()
  (trace
   process-token-in-initial-insertion-mode
   process-token-in-before-html-insertion-mode
   process-token-in-before-head-insertion-mode
   process-token-in-in-head-insertion-mode
   process-token-in-in-head-noscript-insertion-mode
   process-token-in-after-head-insertion-mode
   process-token-in-in-body-insertion-mode
   process-token-in-text-insertion-mode
   process-token-in-in-table-insertion-mode
   process-token-in-in-table-text-insertion-mode
   process-token-in-in-caption-insertion-mode
   process-token-in-in-column-group-insertion-mode
   process-token-in-in-table-body-insertion-mode
   process-token-in-in-row-insertion-mode
   process-token-in-in-cell-insertion-mode
   process-token-in-in-select-insertion-mode
   process-token-in-in-select-in-table-insertion-mode
   process-token-in-in-template-insertion-mode
   process-token-in-after-body-insertion-mode
   process-token-in-in-frameset-insertion-mode
   process-token-in-after-frameset-insertion-mode
   process-token-in-after-after-body-insertion-mode
   process-token-in-after-after-frameset-insertion-mode))
