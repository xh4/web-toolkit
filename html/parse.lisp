(in-package :html)

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
    after-after-frameset))

(defmacro define-parser-insertion-mode (name &body body)
  (let ((function-name (intern (format nil "PROCESS-TOKEN-IN-~A-INSERTION-MODE" name))))
    `(defun ,function-name (parser)
       ,@(unless body '((declare (ignore parser))))
       (symbol-macrolet ((token (slot-value parser 'current-token))
                         (next-token nil)
                         (document nil)
                         (stack-of-open-elements (slot-value parser 'stack-of-open-elements))
                         (adjusted-current-node (adjusted-current-node parser)))
         (macrolet ((switch-to (insertion-mode)
                      `(setf (slot-value parser 'insertion-mode) ,insertion-mode))
                    (parse-error (message)))
           (flet (,@(loop for insertion-mode in *insertion-modes*
                      collect `(,(intern (format nil "PROCESS-TOKEN-IN-~A-INSERTION-MODE" insertion-mode))
                                ()
                                (,(intern (format nil "PROCESS-TOKEN-IN-~A-INSERTION-MODE" insertion-mode)) parser)))
                  (insert-comment (token &optional position)
                    (declare (ignore token position)))
                  (insert-character (character)
                    (declare (ignore character)))
                  (create-element (token)
                    (declare (ignore token)))
                  (a-start-tag-whose-name-is (name))
                  (a-start-tag-whose-name-is-one-of (names))
                  (an-end-tag-whose-name-is (name))
                  (an-end-tag-whose-name-is-one-of (names)))
             ,@(if body
                   body
                 `((error "Parser ~A not implemented" ',name)))))))))

(define-parser-insertion-mode initial
  (cond
   ((or (eq #\tab token) (eq #\newline token)
        (eq #\page token) (eq #\return token) (eq #\space token))
    #|ignore|#)
   
   ((typep token 'comment-token)
    (insert-comment token))
   
   ((typep token 'doctype-token)
    ;; ...
    (switch-to 'before-html))

   (t
    ;; ...
    (switch-to 'before-html))))

(define-parser-insertion-mode before-html
  (cond
   ((typep token 'doctype-token)
    (parse-error "..."))

   ((typep token 'comment-token)
    (insert-comment token))

   ((or (eq #\tab token) (eq #\newline token)
        (eq #\page token) (eq #\return token) (eq #\space token))
    #|ignore|#)
    
   ((a-start-tag-whose-name-is "html")
    (let ((element (create-element token)))
      (dom:append-child document element)
      (push element stack-of-open-elements))
    (switch-to 'before-head))
    
   ((an-end-tag-whose-name-is-one-of '("head" "body" "html" "br"))
    ;; Same as T
    (let ((element (make-instance 'element :tag-name "html")))
      (dom:append-child document element)
      (push element stack-of-open-elements))
    (switch-to 'before-head))
    
   ((typep token 'end-tag)
    (parse-error "..."))
    
   (t
    (let ((element (make-instance 'element :tag-name "html")))
      (dom:append-child document element)
      (push element stack-of-open-elements))
    (switch-to 'before-head))))

(define-parser-insertion-mode before-head
  (cond
   ((or (eq #\tab token) (eq #\newline token)
        (eq #\page token) (eq #\return token) (eq #\space token))
    #|ignore|#)
   
   ((typep token 'comment-token)
    (insert-comment token))
   
   ((typep token 'doctype-token)
    (parse-error "..."))
   
   ((a-start-tag-whose-name-is "html")
    (process-token-in-in-body-insertion-mode))

   ((a-start-tag-whose-name-is "head")
    (and (typep token 'start-tag)
         (equal "head" (slot-value token 'tag-name))))

   ((an-end-tag-whose-name-is-one-of'("head" "body" "html" "br"))
    (and (typep token 'end-tag)
         (member (slot-value token 'tag-name)
                 :test 'equal)))
   
   ((typep token 'end-tag))
   
   (t)))

(define-parser-insertion-mode in-head
  (cond
   ((or (eq #\tab token) (eq #\newline token)
        (eq #\page token) (eq #\return token) (eq #\space token))
    (insert-character token))
   
   ((typep token 'comment-token)
    (insert-comment token))
   
   ((typep token 'doctype-token)
    (parse-error "..."))
   
   ((a-start-tag-whose-name-is "html")
    (process-token-in-in-body-insertion-mode))
   
   ((a-start-tag-whose-name-is-one-of '("base" "basefont" "bgsound" "link")))
   
   ((a-start-tag-whose-name-is "meta"))
   
   ((a-start-tag-whose-name-is "title"))

   ((or (a-start-tag-whose-name-is "noscript")
        (a-start-tag-whose-name-is-one-of '("noframes" "style"))))
   
   ((a-start-tag-whose-name-is "noscript"))
  
   ((a-start-tag-whose-name-is "script"))

   ((an-end-tag-whose-name-is "head"))

   ((an-end-tag-whose-name-is-one-of '("body" "html" "br")))

   ((a-start-tag-whose-name-is "template"))

   ((an-end-tag-whose-name-is "template"))

   ((or (a-start-tag-whose-name-is "head")
        (typep token 'end-tag)))
   
   (t)))

(define-parser-insertion-mode in-head-noscript
  (cond
   ((typep token 'doctype-token))

   ((a-start-tag-whose-name-is "html"))

   ((an-end-tag-whose-name-is "noscript"))

   ((or (or (eq #\tab token) (eq #\newline token)
            (eq #\page token) (eq #\return token) (eq #\space token))
        (typep token 'comment-token)
        (a-start-tag-whose-name-is-one-of '("basefont" "bgsound" "link"
                                            "meta" "noframes" "style"))))

   ((an-end-tag-whose-name-is "br"))

   ((or (a-start-tag-whose-name-is-one-of '("head" "noscript"))
        ;; Any other end tag
        (typep token 'end-tag)))

   ;; Anything else
   (t)))

(define-parser-insertion-mode after-head
  (cond
   ((or (eq #\tab token) (eq #\newline token)
        (eq #\page token) (eq #\return token) (eq #\space token))
    (insert-character token))
   
   ((typep token 'comment-token)
    (insert-comment token))
   
   ((typep token 'doctype-token)
    (parse-error "..."))
   
   ((a-start-tag-whose-name-is "html")
    (process-token-in-in-body-insertion-mode))
   
   ((a-start-tag-whose-name-is "body"))
   
   ((a-start-tag-whose-name-is "frameset"))

   ((a-start-tag-whose-name-is-one-of '("base" "basefont" "bgsound" "link"
                                        "meta" "noframes" "script" "style"
                                        "template" "title")))

   ((an-end-tag-whose-name-is "template"))

   ((an-end-tag-whose-name-is-one-of '("body" "html" "br")))

   ((or (a-start-tag-whose-name-is "head")
        (typep token 'end-tag)))
      
   (t)))

(define-parser-insertion-mode in-body
  (cond
   ((eq #\null token)
    (parse-error "..."))
   
   ((or (eq #\tab token) (eq #\newline token)
        (eq #\page token) (eq #\return token) (eq #\space token)))
   
   ((typep token 'character))
   
   ((typep token 'comment-token)
    (insert-comment token))
   
   ((typep token 'doctype-token)
    (parse-error "..."))
   
   ((a-start-tag-whose-tag-name-is "html"))
   
   ((or (a-start-tag-whose-name-is-one-of '("base" "basefont" "bgsound" "link"
                                            "meta" "noframes" "script" "style"
                                            "template" "title"))
        (an-end-tag-whose-name-is "template")))
   
   
   ((a-start-tag-whose-name-is "body"))

   ((a-start-tag-whose-name-is "frameset"))
   
   ;; An end-of-file token
   ((typep token 'end-of-file))
   
   ((an-end-tag-whose-name-is "body"))

   ((an-end-tag-whose-name-is "html"))
   
   ((a-start-tag-whose-name-is-one-of '("address" "article" "aside" "blockquote"
                                        "center" "details" "dialog" "dir" "div" "dl"
                                        "fieldset" "figcaption" "figure" "footer" "header"
                                        "hgroup" "main" "menu" "nav" "ol" "p"
                                        "section" "summary" "ul")))

   ((a-start-tag-whose-name-is-one-of '("h1" "h2" "h3" "h4" "h5" "h6")))

   ((a-start-tag-whose-name-is-one-of '("pre" "listing")))

   ((a-start-tag-whose-name-is "form"))

   ((a-start-tag-whose-name-is "li"))
   
   ((a-start-tag-whose-name-is-one-of '("dd" "dt")))
   
   ((a-start-tag-whose-name-is "plaintext"))

   ((a-start-tag-whose-name-is "button"))
   

   ((an-end-tag-whose-name-is-one-of '("address" "article" "aside" "blockquote"
                                       "center" "details" "dialog" "dir" "div" "dl"
                                       "fieldset" "figcaption" "figure" "footer" "header"
                                       "hgroup" "main" "menu" "nav" "ol" "p"
                                       "section" "summary" "ul")))

   ((an-end-tag-whose-name-is "form"))

   ((an-end-tag-whose-name-is "p"))

   ((an-end-tag-whose-name-is "li"))
   
   ((an-end-tag-whose-name-is-one-of '("dd" "dt")))

   ((an-end-tag-whose-name-is-one-of '("h1" "h2" "h3" "h4" "h5" "h6")))
   
   ((an-end-tag-whose-name-is "sarcasm"))

   ((a-start-tag-whose-name-is "a"))

   ((a-start-tag-whose-name-is-one-of '("b" "big" "code" "em" "font"
                                        "i" "s" "small" "strike" "strong" "tt" "u")))

   ((a-start-tag-whose-name-is "nobr"))

   ((an-end-tag-whose-name-is-one-of '("a" "b" "big" "code" "em" "font"
                                       "i" "s" "small" "strike" "strong" "tt" "u")))

   ((a-start-tag-whose-name-is-one-of '("applet" "marquee" "object")))
   
   ((an-end-tag-whose-name-is-one-of '("applet" "marquee" "object")))

   ((a-start-tag-whose-name-is "table"))

   ((an-end-tag-whose-name-is "br"))

   ((a-start-tag-whose-name-is-one-of '("area" "br" "embed" "img" "keygen" "wbr")))
   
   ((a-start-tag-whose-name-is "input"))

   ((a-start-tag-whose-name-is-one-of '("param" "source" "track")))

   ((a-start-tag-whose-name-is "hr"))
  
   ((a-start-tag-whose-tag-name-is "image"))

   ((a-start-tag-whose-tag-name-is "textarea"))

   ((a-start-tag-whose-tag-name-is "xmp"))

   ((a-start-tag-whose-tag-name-is "iframe"))

   ((or (a-start-tag-whose-tag-name-is "noembed")
        (a-start-tag-whose-tag-name-is "noscript")))

   ((a-start-tag-whose-tag-name-is "select"))

   ((a-start-tag-whose-tag-name-is-one-of '("optgroup" "option")))

   ((a-start-tag-whose-tag-name-is-one-of '("rb" "rtc")))

   ((a-start-tag-whose-tag-name-is-one-of '("rp" "rt")))

   ((a-start-tag-whose-tag-name-is "math"))

   ((a-start-tag-whose-tag-name-is "svg"))

   ((a-start-tag-whose-tag-name-is-one-of '("caption" "col" "colgroup"
                                            "frame" "head" "tbody" "td" "tfoot"
                                            "th" "thead" "tr")))
   
   ((typep token 'start-tag))

   ((typep token 'end-tag))))

(define-parser-insertion-mode text
  (cond
   ((typep token 'character))

   ((typep token 'end-of-file))

   ((an-end-tag-whose-name-is "script"))

   ((typep token 'end-tag))))

(define-parser-insertion-mode in-table
  (cond
   ((if (typep current-node '(or table tbody tfoot thead tr))
        (typep token 'character)))

   ((typep token 'comment-token))

   ((typep token 'doctype-token))

   ((a-start-tag-whose-name-is "caption"))

   ((a-start-tag-whose-name-is "colgroup"))

   ((a-start-tag-whose-name-is "col"))

   ((a-start-tag-whose-name-is-one-of '("tbody" "tfoot" "thead")))

   ((a-start-tag-whose-name-is-one-of '("td" "th" "tr")))

   ((a-start-tag-whose-name-is "table"))

   ((an-end-tag-whose-name-is "table"))

   ((an-end-tag-whose-name-is-one-of '("body" "caption" "col" "colgroup"
                                       "html" "tbody" "td" "tfoot" "th" "thead" "tr")))

   ((or (a-start-tag-whose-name-is-one-of '("style" "script" "template"))
        (an-end-tag-whose-name-is "template")))

   ((a-start-tag-whose-name-is "input"))

   ((a-start-tag-whose-name-is "form"))

   ((typep token 'end-of-file))

   (t)))

(define-parser-insertion-mode in-table-text
  (cond
   ((eq #\null token))

   ((typep token 'character))

   (t)))

(define-parser-insertion-mode in-caption
  (cond
   ((an-end-tag-whose-name-is "caption"))

   ((or (a-start-tag-whose-name-is-one-of '("caption" "col" "colgroup"
                                            "tbody" "td" "tfoot" "th" "thead" "tr"))
        (an-end-tag-whose-name-is "table")))

   ((an-end-tag-whose-name-is-one-of '("body" "col" "colgroup" "html"
                                       "tbody" "td" "tfoot" "th" "thead" "tr")))

   (t)))

(define-parser-insertion-mode in-column-group
  (cond
   ((or (eq #\tab token) (eq #\newline token)
        (eq #\page token) (eq #\return token) (eq #\space token)))

   ((typep token 'comment-token))

   ((typep token 'doctype-token))

   ((a-start-tag-whose-name-is "html"))

   ((a-start-tag-whose-name-is "col"))

   ((an-end-tag-whose-name-is "colgroup"))

   ((an-end-tag-whose-name-is "col"))

   (or (a-start-tag-whose-name-is "template")
       (an-end-tag-whose-name-is "template"))

   (typep token 'end-of-file)

   (t)))

(define-parser-insertion-mode in-table-body
  (cond
   ((a-start-tag-whose-name-is "tr"))

   ((a-start-tag-whose-name-is-one-of '("th" "td")))

   ((an-end-tag-whose-name-is-one-of '("tbody" "tfoot" "thead")))

   (or (a-start-tag-whose-name-is-one-of '("caption" "col" "colgroup" "tbody"
                                           "tfoot" "thead"))
       (an-end-tag-whose-name-is "table"))

   ((an-end-tag-whose-name-is-one-of '("body" "caption" "col" "colgroup"
                                       "html" "td" "th" "tr")))

   (t)))

(define-parser-insertion-mode in-row
  (cond
   ((a-start-tag-whose-name-is-one-of '("th" "td")))

   ((an-end-tag-whose-name-is "tr"))

   (or (a-start-tag-whose-name-is-one-of '("caption" "col" "colgroup"
                                           "tbody" "tfoot" "thead" "tr"))
       (an-end-tag-whose-name-is "table"))

   ((an-end-tag-whose-name-is-one-of '("tbody" "tfoot" "thead")))

   ((an-end-tag-whose-name-is-one-of '("body" "caption" "col" "colgroup"
                                       "html" "td" "th")))

   (t)))

(define-parser-insertion-mode in-cell
  (cond
   ((an-end-tag-whose-name-is-one-of '("td" "th")))

   ((a-start-tag-whose-name-is-one-of '("caption" "col" "colgroup"
                                        "tbody" "tfoot" "thead" "tr")))

   ((an-end-tag-whose-name-is-one-of '("body" "caption" "col" "colgroup" "html")))

   ((an-end-tag-whose-name-is-one-of '("table" "tbody" "tfoot" "thead" "tr")))

   (t)))

(define-parser-insertion-mode in-select
  (cond
   ((eq #\null token))

   ((typep token 'character))

   ((typep token 'comment-token))

   ((typep token 'doctype-token))

   ((a-start-tag-whose-name-is "html"))

   ((a-start-tag-whose-name-is "option"))

   ((a-start-tag-whose-name-is "optgroup"))

   ((an-end-tag-whose-name-is "optgroup"))

   ((an-end-tag-whose-name-is "option"))

   ((an-end-tag-whose-name-is "select"))

   ((a-start-tag-whose-name-is "select"))

   ((a-start-tag-whose-name-is-one-of '("input" "keygen" "textarea")))

   (or (a-start-tag-whose-name-is-one-of '("script" "template"))
       (an-end-tag-whose-name-is "template"))

   ((typep token end-of-file))

   (t)))

(define-parser-insertion-mode in-select-in-table
  (cond
   ((a-start-tag-whose-name-is-one-of '("caption" "table" "tbody" "tfoot" "thead"
                                        "tr" "td" "th")))
   
   ((an-end-tag-whose-name-is-one-of '("caption" "table" "tbody" "tfoot" "thead"
                                       "tr" "td" "th")))

   (t)))

(define-parser-insertion-mode in-template
  (cond
   (or (typep token 'character)
       (typep token 'comment-token)
       (typep token 'doctype-token))

   (or (a-start-tag-whose-name-is-one-of '("base" "basefont" "bgsound" "link" "meta"
                                           "noframes" "script" "style" "template" "title"))
       (an-end-tag-whose-name-is "template"))

   ((a-start-tag-whose-name-is-one-of '("caption" "colgroup" "tbody" "tfoot" "thead")))

   ((a-start-tag-whose-name-is "col"))

   ((a-start-tag-whose-name-is "tr"))

   ((a-start-tag-whose-name-is-one-of '("td" "th")))

   ((typep token 'start-tag))

   ((typep token 'end-tag))

   ((typep token 'end-of-file))))

(define-parser-insertion-mode after-body
  (cond
   ((or (eq #\tab token) (eq #\newline token)
        (eq #\page token) (eq #\return token) (eq #\space token)))

   ((typep token 'comment-token))

   ((typep token 'doctype-token))

   ((a-start-tag-whose-name-is "html"))

   ((an-end-tag-whose-name-is "html"))

   ((typep token 'end-of-file))

   (t)))

(define-parser-insertion-mode in-frameset
  (cond
   ((or (eq #\tab token) (eq #\newline token)
        (eq #\page token) (eq #\return token) (eq #\space token)))

   ((typep token 'comment-token))

   ((typep token 'doctype-token))

   ((a-start-tag-whose-name-is "html"))

   ((a-start-tag-whose-name-is "frameset"))

   ((an-end-tag-whose-name-is "frameset"))

   ((a-start-tag-whose-name-is "frame"))

   ((a-start-tag-whose-name-is "noframes"))

   ((typep token 'end-of-file))

   (t)))

(define-parser-insertion-mode after-frameset
  (cond
   ((or (eq #\tab token) (eq #\newline token)
        (eq #\page token) (eq #\return token) (eq #\space token)))

   ((typep token 'comment-token))

   ((typep token 'doctype-token))

   ((a-start-tag-whose-name-is "html"))

   ((an-end-tag-whose-name-is "html"))

   ((a-start-tag-whose-name-is "noframes"))

   ((typep token 'end-of-file))

   (t)))


(define-parser-insertion-mode after-after-body
  (cond
   ((typep token 'comment-token))

   (or ((typep token 'doctype-token))
       (or (eq #\tab token) (eq #\newline token)
           (eq #\page token) (eq #\return token) (eq #\space token))
       (a-start-tag-whose-name-is "html"))

   ((typep token 'end-of-file))

   (t)))

(define-parser-insertion-mode after-after-frameset
  (cond
   ((typep token 'comment-token))

   (or ((typep token 'doctype-token))
       (or (eq #\tab token) (eq #\newline token)
           (eq #\page token) (eq #\return token) (eq #\space token))
       (a-start-tag-whose-name-is "html"))

   ((typep token 'end-of-file))

   ((a-start-tag-whose-name-is "noframes"))

   (t)))

(defclass parser ()
  ((tokenizer
    :initarg :tokenizer
    :initform nil)
   (insertion-mode
    :initform nil)
   (current-token
    :initform nil)
   (stack-of-open-elements
    :initform nil)))

(defun adjusted-current-node (parser))

(defun tree-construction-dispatcher (parser token)
  (if (or (null (slot-value parser 'stack-of-open-elements)))
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
        (funcall function parser token))
    (error "TODO: Process the token according to the rules given in the section for parsing tokens in foreign content.")))

(defgeneric parse (source &key)
  (:method ((source string) &key)
   (with-input-from-string (stream source)
     (parse stream)))
  (:method ((stream stream) &key)
   (let ((tokenizer (make-instance 'tokenizer :stream stream)))
     (let ((parser (make-instance 'parser :tokenizer tokenizer)))
       (loop
        (handler-bind
            ((on-token (lambda (c)
                         (let ((token (slot-value c 'token)))
                           (if (typep token 'end-of-file)
                               (return)
                             (tree-construction-dispatcher parser token))))))
          (funcall (slot-value tokenizer 'state) tokenizer)))))))
