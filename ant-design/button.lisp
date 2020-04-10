(in-package :ant-design)

(define-antd-component button ()
  ((disabled :initarg :disabled :initform nil)
   (ghost :initarg :ghost :initform nil)
   (href :initarg :href :initform nil)
   (html-type :initarg :html-type :initform :button)
   (icon :initarg :icon :initform nil)
   (loading :initarg :loading :initform nil)
   (shape :initarg :shape :initform nil)
   (size :initarg :size :initform nil)
   (target :initarg :target :initform nil)
   (type :initarg :type :initform nil)
   (block :initarg :block :initform nil)
   (danger :initarg :danget :initform nil))
  (:render
   (lambda (button)
     (with-slots (disabled ghost href html-type icon
                           loading shape size target type block danger)
         button
       (let ((root (html:button :class "ant-btn")))
         (map-classes root
           (ghost "ant-btn-background-ghost")
           (loading "ant-btn-loading")
           ((eq type :primary) "ant-btn-primary")
           ((eq type :dashed) "ant-btn-dashed")
           ((eq type :link) "ant-btn-link")
           (danger "ant-btn-dangerous")
           ((eq size :large) "ant-btn-lg")
           ((eq size :middle) "ant-btn-md")
           ((eq size :small) "ant-btn-sm")
           (block "ant-btn-block"))
         (map-attributes root
           (disabled "disabled")
           (href "href")
           (html-type "type"))
         root))))
  (:style
   (lambda ()
     (rule ".ant-btn"
       (css:line-height "3143/2000")
       (css:position "relative")
       (css:display "inline-block")
       (css:font-weight "400")
       (css:white-space "nowrap")
       (css:text-align "center")
       (css:background-image "none")
       (css:border "1px solid transparent")
       (property "-webkit-box-shadow" "0 2px 0 rgba0, 0, 0, 3/200")
       (css:box-shadow "0 2px 0 rgba0, 0, 0, 3/200")
       (css:cursor "pointer")
       (property "-webkit-transition" "all 3/10s cubic-bezier129/200, 9/200, 71/200, 1")
       (css:transition "all 3/10s cubic-bezier129/200, 9/200, 71/200, 1")
       (property "-webkit-user-select" "none")
       (property "-moz-user-select" "none")
       (property "-ms-user-select" "none")
       (css:user-select "none")
       (property "-ms-touch-action" "manipulation")
       (css:touch-action "manipulation")
       (css:height "32px")
       (css:padding "4px 15px")
       (css:font-size "14px")
       (css:border-radius "2px")
       (css:color "rgba0, 0, 0, 13/20")
       (css:background-color "#fff")
       (css:border-color "#d9d9d9"))
     (rule ".ant-btn > .anticon"
       (css:line-height "1"))
     (rule '(".ant-btn" ".ant-btn:active" ".ant-btn:focus")
       (css:outline "0"))
     (rule ".ant-btn:not[disabled]:hover"
       (css:text-decoration "none"))
     (rule ".ant-btn:not[disabled]:active"
       (css:outline "0")
       (property "-webkit-box-shadow" "none")
       (css:box-shadow "none"))
     (rule '(".ant-btn.disabled" ".ant-btn[disabled]")
       (css:cursor "not-allowed"))
     (rule '(".ant-btn.disabled > *" ".ant-btn[disabled] > *")
       (css:pointer-events "none"))
     (rule ".ant-btn-lg"
       (css:height "40px")
       (css:padding "32/5px 15px")
       (css:font-size "16px")
       (css:border-radius "2px"))
     (rule ".ant-btn-sm"
       (css:height "24px")
       (css:padding "0px 7px")
       (css:font-size "14px")
       (css:border-radius "2px")
       (css:line-height "22px"))
     (rule ".ant-btn > a:only-child"
       (css:color "currentColor"))
     (rule ".ant-btn > a:only-child::after"
       (css:position "absolute")
       (css:top "0")
       (css:right "0")
       (css:bottom "0")
       (css:left "0")
       (css:background "transparent")
       (css:content "\"\""))
     (rule '(".ant-btn:hover" ".ant-btn:focus")
       (css:color "#40a9ff")
       (css:background-color "#fff")
       (css:border-color "#40a9ff"))
     (rule '(".ant-btn:hover > a:only-child" ".ant-btn:focus > a:only-child")
       (css:color "currentColor"))
     (rule '(".ant-btn:hover > a:only-child::after" ".ant-btn:focus > a:only-child::after")
       (css:position "absolute")
       (css:top "0")
       (css:right "0")
       (css:bottom "0")
       (css:left "0")
       (css:background "transparent")
       (css:content "\"\""))
     (rule '(".ant-btn:active" ".ant-btn.active")
       (css:color "#096dd9")
       (css:background-color "#fff")
       (css:border-color "#096dd9"))
     (rule '(".ant-btn:active > a:only-child" ".ant-btn.active > a:only-child")
       (css:color "currentColor"))
     (rule '(".ant-btn:active > a:only-child::after" ".ant-btn.active > a:only-child::after")
       (css:position "absolute")
       (css:top "0")
       (css:right "0")
       (css:bottom "0")
       (css:left "0")
       (css:background "transparent")
       (css:content "\"\""))
     (rule '(".ant-btn-disabled"
             ".ant-btn.disabled"
             ".ant-btn[disabled]"
             ".ant-btn-disabled:hover"
             ".ant-btn.disabled:hover"
             ".ant-btn[disabled]:hover"
             ".ant-btn-disabled:focus"
             ".ant-btn.disabled:focus"
             ".ant-btn[disabled]:focus"
             ".ant-btn-disabled:active"
             ".ant-btn.disabled:active"
             ".ant-btn[disabled]:active"
             ".ant-btn-disabled.active"
             ".ant-btn.disabled.active"
             ".ant-btn[disabled].active")
       (css:color "rgba0, 0, 0, 1/4")
       (css:background-color "#f5f5f5")
       (css:border-color "#d9d9d9")
       (css:text-shadow "none")
       (property "-webkit-box-shadow" "none")
       (css:box-shadow "none"))
     (rule '(".ant-btn-disabled > a:only-child"
             ".ant-btn.disabled > a:only-child"
             ".ant-btn[disabled] > a:only-child"
             ".ant-btn-disabled:hover > a:only-child"
             ".ant-btn.disabled:hover > a:only-child"
             ".ant-btn[disabled]:hover > a:only-child"
             ".ant-btn-disabled:focus > a:only-child"
             ".ant-btn.disabled:focus > a:only-child"
             ".ant-btn[disabled]:focus > a:only-child"
             ".ant-btn-disabled:active > a:only-child"
             ".ant-btn.disabled:active > a:only-child"
             ".ant-btn[disabled]:active > a:only-child"
             ".ant-btn-disabled.active > a:only-child"
             ".ant-btn.disabled.active > a:only-child"
             ".ant-btn[disabled].active > a:only-child")
       (css:color "currentColor"))
     (rule '(".ant-btn-disabled > a:only-child::after"
             ".ant-btn.disabled > a:only-child::after"
             ".ant-btn[disabled] > a:only-child::after"
             ".ant-btn-disabled:hover > a:only-child::after"
             ".ant-btn.disabled:hover > a:only-child::after"
             ".ant-btn[disabled]:hover > a:only-child::after"
             ".ant-btn-disabled:focus > a:only-child::after"
             ".ant-btn.disabled:focus > a:only-child::after"
             ".ant-btn[disabled]:focus > a:only-child::after"
             ".ant-btn-disabled:active > a:only-child::after"
             ".ant-btn.disabled:active > a:only-child::after"
             ".ant-btn[disabled]:active > a:only-child::after"
             ".ant-btn-disabled.active > a:only-child::after"
             ".ant-btn.disabled.active > a:only-child::after"
             ".ant-btn[disabled].active > a:only-child::after")
       (css:position "absolute")
       (css:top "0")
       (css:right "0")
       (css:bottom "0")
       (css:left "0")
       (css:background "transparent")
       (css:content "\"\""))
     (rule '(".ant-btn:hover" ".ant-btn:focus" ".ant-btn:active" ".ant-btn.active")
       (css:text-decoration "none")
       (css:background "#fff"))
     (rule '(".ant-btn > i" ".ant-btn > span")
       (css:display "inline-block")
       (property "-webkit-transition" "margin-left 3/10s cubic-bezier129/200, 9/200, 71/200, 1")
       (css:transition "margin-left 3/10s cubic-bezier129/200, 9/200, 71/200, 1")
       (css:pointer-events "none"))
     (rule ".ant-btn-primary"
       (css:color "#fff")
       (css:background-color "#1890ff")
       (css:border-color "#1890ff")
       (css:text-shadow "0 -1px 0 rgba0, 0, 0, 3/25")
       (property "-webkit-box-shadow" "0 2px 0 rgba0, 0, 0, 9/200")
       (css:box-shadow "0 2px 0 rgba0, 0, 0, 9/200"))
     (rule ".ant-btn-primary > a:only-child"
       (css:color "currentColor"))
     (rule ".ant-btn-primary > a:only-child::after"
       (css:position "absolute")
       (css:top "0")
       (css:right "0")
       (css:bottom "0")
       (css:left "0")
       (css:background "transparent")
       (css:content "\"\""))
     (rule '(".ant-btn-primary:hover" ".ant-btn-primary:focus")
       (css:color "#fff")
       (css:background-color "#40a9ff")
       (css:border-color "#40a9ff"))
     (rule '(".ant-btn-primary:hover > a:only-child" ".ant-btn-primary:focus > a:only-child")
       (css:color "currentColor"))
     (rule '(".ant-btn-primary:hover > a:only-child::after"
             ".ant-btn-primary:focus > a:only-child::after")
       (css:position "absolute")
       (css:top "0")
       (css:right "0")
       (css:bottom "0")
       (css:left "0")
       (css:background "transparent")
       (css:content "\"\""))
     (rule '(".ant-btn-primary:active" ".ant-btn-primary.active")
       (css:color "#fff")
       (css:background-color "#096dd9")
       (css:border-color "#096dd9"))
     (rule '(".ant-btn-primary:active > a:only-child" ".ant-btn-primary.active > a:only-child")
       (css:color "currentColor"))
     (rule '(".ant-btn-primary:active > a:only-child::after"
             ".ant-btn-primary.active > a:only-child::after")
       (css:position "absolute")
       (css:top "0")
       (css:right "0")
       (css:bottom "0")
       (css:left "0")
       (css:background "transparent")
       (css:content "\"\""))
     (rule '(".ant-btn-primary-disabled"
             ".ant-btn-primary.disabled"
             ".ant-btn-primary[disabled]"
             ".ant-btn-primary-disabled:hover"
             ".ant-btn-primary.disabled:hover"
             ".ant-btn-primary[disabled]:hover"
             ".ant-btn-primary-disabled:focus"
             ".ant-btn-primary.disabled:focus"
             ".ant-btn-primary[disabled]:focus"
             ".ant-btn-primary-disabled:active"
             ".ant-btn-primary.disabled:active"
             ".ant-btn-primary[disabled]:active"
             ".ant-btn-primary-disabled.active"
             ".ant-btn-primary.disabled.active"
             ".ant-btn-primary[disabled].active")
       (css:color "rgba0, 0, 0, 1/4")
       (css:background-color "#f5f5f5")
       (css:border-color "#d9d9d9")
       (css:text-shadow "none")
       (property "-webkit-box-shadow" "none")
       (css:box-shadow "none"))
     (rule '(".ant-btn-primary-disabled > a:only-child"
             ".ant-btn-primary.disabled > a:only-child"
             ".ant-btn-primary[disabled] > a:only-child"
             ".ant-btn-primary-disabled:hover > a:only-child"
             ".ant-btn-primary.disabled:hover > a:only-child"
             ".ant-btn-primary[disabled]:hover > a:only-child"
             ".ant-btn-primary-disabled:focus > a:only-child"
             ".ant-btn-primary.disabled:focus > a:only-child"
             ".ant-btn-primary[disabled]:focus > a:only-child"
             ".ant-btn-primary-disabled:active > a:only-child"
             ".ant-btn-primary.disabled:active > a:only-child"
             ".ant-btn-primary[disabled]:active > a:only-child"
             ".ant-btn-primary-disabled.active > a:only-child"
             ".ant-btn-primary.disabled.active > a:only-child"
             ".ant-btn-primary[disabled].active > a:only-child")
       (css:color "currentColor"))
     (rule '(".ant-btn-primary-disabled > a:only-child::after"
             ".ant-btn-primary.disabled > a:only-child::after"
             ".ant-btn-primary[disabled] > a:only-child::after"
             ".ant-btn-primary-disabled:hover > a:only-child::after"
             ".ant-btn-primary.disabled:hover > a:only-child::after"
             ".ant-btn-primary[disabled]:hover > a:only-child::after"
             ".ant-btn-primary-disabled:focus > a:only-child::after"
             ".ant-btn-primary.disabled:focus > a:only-child::after"
             ".ant-btn-primary[disabled]:focus > a:only-child::after"
             ".ant-btn-primary-disabled:active > a:only-child::after"
             ".ant-btn-primary.disabled:active > a:only-child::after"
             ".ant-btn-primary[disabled]:active > a:only-child::after"
             ".ant-btn-primary-disabled.active > a:only-child::after"
             ".ant-btn-primary.disabled.active > a:only-child::after"
             ".ant-btn-primary[disabled].active > a:only-child::after")
       (css:position "absolute")
       (css:top "0")
       (css:right "0")
       (css:bottom "0")
       (css:left "0")
       (css:background "transparent")
       (css:content "\"\""))
     (rule ".ant-btn-group .ant-btn-primary:not:first-child:not:last-child"
       (css:border-right-color "#40a9ff")
       (css:border-left-color "#40a9ff"))
     (rule ".ant-btn-group .ant-btn-primary:not:first-child:not:last-child:disabled"
       (css:border-color "#d9d9d9"))
     (rule ".ant-btn-group .ant-btn-primary:first-child:not:last-child"
       (css:border-right-color "#40a9ff"))
     (rule ".ant-btn-group .ant-btn-primary:first-child:not:last-child[disabled]"
       (css:border-right-color "#d9d9d9"))
     (rule '(".ant-btn-group .ant-btn-primary:last-child:not:first-child"
             ".ant-btn-group .ant-btn-primary + .ant-btn-primary")
       (css:border-left-color "#40a9ff"))
     (rule '(".ant-btn-group .ant-btn-primary:last-child:not:first-child[disabled]"
             ".ant-btn-group .ant-btn-primary + .ant-btn-primary[disabled]")
       (css:border-left-color "#d9d9d9"))
     (rule ".ant-btn-ghost"
       (css:color "rgba0, 0, 0, 13/20")
       (css:background-color "transparent")
       (css:border-color "#d9d9d9"))
     (rule ".ant-btn-ghost > a:only-child"
       (css:color "currentColor"))
     (rule ".ant-btn-ghost > a:only-child::after"
       (css:position "absolute")
       (css:top "0")
       (css:right "0")
       (css:bottom "0")
       (css:left "0")
       (css:background "transparent")
       (css:content "\"\""))
     (rule '(".ant-btn-ghost:hover" ".ant-btn-ghost:focus")
       (css:color "#40a9ff")
       (css:background-color "transparent")
       (css:border-color "#40a9ff"))
     (rule '(".ant-btn-ghost:hover > a:only-child" ".ant-btn-ghost:focus > a:only-child")
       (css:color "currentColor"))
     (rule '(".ant-btn-ghost:hover > a:only-child::after"
             ".ant-btn-ghost:focus > a:only-child::after")
       (css:position "absolute")
       (css:top "0")
       (css:right "0")
       (css:bottom "0")
       (css:left "0")
       (css:background "transparent")
       (css:content "\"\""))
     (rule '(".ant-btn-ghost:active" ".ant-btn-ghost.active")
       (css:color "#096dd9")
       (css:background-color "transparent")
       (css:border-color "#096dd9"))
     (rule '(".ant-btn-ghost:active > a:only-child" ".ant-btn-ghost.active > a:only-child")
       (css:color "currentColor"))
     (rule '(".ant-btn-ghost:active > a:only-child::after"
             ".ant-btn-ghost.active > a:only-child::after")
       (css:position "absolute")
       (css:top "0")
       (css:right "0")
       (css:bottom "0")
       (css:left "0")
       (css:background "transparent")
       (css:content "\"\""))
     (rule '(".ant-btn-ghost-disabled"
             ".ant-btn-ghost.disabled"
             ".ant-btn-ghost[disabled]"
             ".ant-btn-ghost-disabled:hover"
             ".ant-btn-ghost.disabled:hover"
             ".ant-btn-ghost[disabled]:hover"
             ".ant-btn-ghost-disabled:focus"
             ".ant-btn-ghost.disabled:focus"
             ".ant-btn-ghost[disabled]:focus"
             ".ant-btn-ghost-disabled:active"
             ".ant-btn-ghost.disabled:active"
             ".ant-btn-ghost[disabled]:active"
             ".ant-btn-ghost-disabled.active"
             ".ant-btn-ghost.disabled.active"
             ".ant-btn-ghost[disabled].active")
       (css:color "rgba0, 0, 0, 1/4")
       (css:background-color "#f5f5f5")
       (css:border-color "#d9d9d9")
       (css:text-shadow "none")
       (property "-webkit-box-shadow" "none")
       (css:box-shadow "none"))
     (rule '(".ant-btn-ghost-disabled > a:only-child"
             ".ant-btn-ghost.disabled > a:only-child"
             ".ant-btn-ghost[disabled] > a:only-child"
             ".ant-btn-ghost-disabled:hover > a:only-child"
             ".ant-btn-ghost.disabled:hover > a:only-child"
             ".ant-btn-ghost[disabled]:hover > a:only-child"
             ".ant-btn-ghost-disabled:focus > a:only-child"
             ".ant-btn-ghost.disabled:focus > a:only-child"
             ".ant-btn-ghost[disabled]:focus > a:only-child"
             ".ant-btn-ghost-disabled:active > a:only-child"
             ".ant-btn-ghost.disabled:active > a:only-child"
             ".ant-btn-ghost[disabled]:active > a:only-child"
             ".ant-btn-ghost-disabled.active > a:only-child"
             ".ant-btn-ghost.disabled.active > a:only-child"
             ".ant-btn-ghost[disabled].active > a:only-child")
       (css:color "currentColor"))
     (rule '(".ant-btn-ghost-disabled > a:only-child::after"
             ".ant-btn-ghost.disabled > a:only-child::after"
             ".ant-btn-ghost[disabled] > a:only-child::after"
             ".ant-btn-ghost-disabled:hover > a:only-child::after"
             ".ant-btn-ghost.disabled:hover > a:only-child::after"
             ".ant-btn-ghost[disabled]:hover > a:only-child::after"
             ".ant-btn-ghost-disabled:focus > a:only-child::after"
             ".ant-btn-ghost.disabled:focus > a:only-child::after"
             ".ant-btn-ghost[disabled]:focus > a:only-child::after"
             ".ant-btn-ghost-disabled:active > a:only-child::after"
             ".ant-btn-ghost.disabled:active > a:only-child::after"
             ".ant-btn-ghost[disabled]:active > a:only-child::after"
             ".ant-btn-ghost-disabled.active > a:only-child::after"
             ".ant-btn-ghost.disabled.active > a:only-child::after"
             ".ant-btn-ghost[disabled].active > a:only-child::after")
       (css:position "absolute")
       (css:top "0")
       (css:right "0")
       (css:bottom "0")
       (css:left "0")
       (css:background "transparent")
       (css:content "\"\""))
     (rule ".ant-btn-dashed"
       (css:color "rgba0, 0, 0, 13/20")
       (css:background-color "#fff")
       (css:border-color "#d9d9d9")
       (css:border-style "dashed"))
     (rule ".ant-btn-dashed > a:only-child"
       (css:color "currentColor"))
     (rule ".ant-btn-dashed > a:only-child::after"
       (css:position "absolute")
       (css:top "0")
       (css:right "0")
       (css:bottom "0")
       (css:left "0")
       (css:background "transparent")
       (css:content "\"\""))
     (rule '(".ant-btn-dashed:hover" ".ant-btn-dashed:focus")
       (css:color "#40a9ff")
       (css:background-color "#fff")
       (css:border-color "#40a9ff"))
     (rule '(".ant-btn-dashed:hover > a:only-child" ".ant-btn-dashed:focus > a:only-child")
       (css:color "currentColor"))
     (rule '(".ant-btn-dashed:hover > a:only-child::after"
             ".ant-btn-dashed:focus > a:only-child::after")
       (css:position "absolute")
       (css:top "0")
       (css:right "0")
       (css:bottom "0")
       (css:left "0")
       (css:background "transparent")
       (css:content "\"\""))
     (rule '(".ant-btn-dashed:active" ".ant-btn-dashed.active")
       (css:color "#096dd9")
       (css:background-color "#fff")
       (css:border-color "#096dd9"))
     (rule '(".ant-btn-dashed:active > a:only-child" ".ant-btn-dashed.active > a:only-child")
       (css:color "currentColor"))
     (rule '(".ant-btn-dashed:active > a:only-child::after"
             ".ant-btn-dashed.active > a:only-child::after")
       (css:position "absolute")
       (css:top "0")
       (css:right "0")
       (css:bottom "0")
       (css:left "0")
       (css:background "transparent")
       (css:content "\"\""))
     (rule '(".ant-btn-dashed-disabled"
             ".ant-btn-dashed.disabled"
             ".ant-btn-dashed[disabled]"
             ".ant-btn-dashed-disabled:hover"
             ".ant-btn-dashed.disabled:hover"
             ".ant-btn-dashed[disabled]:hover"
             ".ant-btn-dashed-disabled:focus"
             ".ant-btn-dashed.disabled:focus"
             ".ant-btn-dashed[disabled]:focus"
             ".ant-btn-dashed-disabled:active"
             ".ant-btn-dashed.disabled:active"
             ".ant-btn-dashed[disabled]:active"
             ".ant-btn-dashed-disabled.active"
             ".ant-btn-dashed.disabled.active"
             ".ant-btn-dashed[disabled].active")
       (css:color "rgba0, 0, 0, 1/4")
       (css:background-color "#f5f5f5")
       (css:border-color "#d9d9d9")
       (css:text-shadow "none")
       (property "-webkit-box-shadow" "none")
       (css:box-shadow "none"))
     (rule '(".ant-btn-dashed-disabled > a:only-child"
             ".ant-btn-dashed.disabled > a:only-child"
             ".ant-btn-dashed[disabled] > a:only-child"
             ".ant-btn-dashed-disabled:hover > a:only-child"
             ".ant-btn-dashed.disabled:hover > a:only-child"
             ".ant-btn-dashed[disabled]:hover > a:only-child"
             ".ant-btn-dashed-disabled:focus > a:only-child"
             ".ant-btn-dashed.disabled:focus > a:only-child"
             ".ant-btn-dashed[disabled]:focus > a:only-child"
             ".ant-btn-dashed-disabled:active > a:only-child"
             ".ant-btn-dashed.disabled:active > a:only-child"
             ".ant-btn-dashed[disabled]:active > a:only-child"
             ".ant-btn-dashed-disabled.active > a:only-child"
             ".ant-btn-dashed.disabled.active > a:only-child"
             ".ant-btn-dashed[disabled].active > a:only-child")
       (css:color "currentColor"))
     (rule '(".ant-btn-dashed-disabled > a:only-child::after"
             ".ant-btn-dashed.disabled > a:only-child::after"
             ".ant-btn-dashed[disabled] > a:only-child::after"
             ".ant-btn-dashed-disabled:hover > a:only-child::after"
             ".ant-btn-dashed.disabled:hover > a:only-child::after"
             ".ant-btn-dashed[disabled]:hover > a:only-child::after"
             ".ant-btn-dashed-disabled:focus > a:only-child::after"
             ".ant-btn-dashed.disabled:focus > a:only-child::after"
             ".ant-btn-dashed[disabled]:focus > a:only-child::after"
             ".ant-btn-dashed-disabled:active > a:only-child::after"
             ".ant-btn-dashed.disabled:active > a:only-child::after"
             ".ant-btn-dashed[disabled]:active > a:only-child::after"
             ".ant-btn-dashed-disabled.active > a:only-child::after"
             ".ant-btn-dashed.disabled.active > a:only-child::after"
             ".ant-btn-dashed[disabled].active > a:only-child::after")
       (css:position "absolute")
       (css:top "0")
       (css:right "0")
       (css:bottom "0")
       (css:left "0")
       (css:background "transparent")
       (css:content "\"\""))
     (rule ".ant-btn-danger"
       (css:color "#fff")
       (css:background-color "#ff4d4f")
       (css:border-color "#ff4d4f")
       (css:text-shadow "0 -1px 0 rgba0, 0, 0, 3/25")
       (property "-webkit-box-shadow" "0 2px 0 rgba0, 0, 0, 9/200")
       (css:box-shadow "0 2px 0 rgba0, 0, 0, 9/200"))
     (rule ".ant-btn-danger > a:only-child"
       (css:color "currentColor"))
     (rule ".ant-btn-danger > a:only-child::after"
       (css:position "absolute")
       (css:top "0")
       (css:right "0")
       (css:bottom "0")
       (css:left "0")
       (css:background "transparent")
       (css:content "\"\""))
     (rule '(".ant-btn-danger:hover" ".ant-btn-danger:focus")
       (css:color "#fff")
       (css:background-color "#ff7875")
       (css:border-color "#ff7875"))
     (rule '(".ant-btn-danger:hover > a:only-child" ".ant-btn-danger:focus > a:only-child")
       (css:color "currentColor"))
     (rule '(".ant-btn-danger:hover > a:only-child::after"
             ".ant-btn-danger:focus > a:only-child::after")
       (css:position "absolute")
       (css:top "0")
       (css:right "0")
       (css:bottom "0")
       (css:left "0")
       (css:background "transparent")
       (css:content "\"\""))
     (rule '(".ant-btn-danger:active" ".ant-btn-danger.active")
       (css:color "#fff")
       (css:background-color "#d9363e")
       (css:border-color "#d9363e"))
     (rule '(".ant-btn-danger:active > a:only-child" ".ant-btn-danger.active > a:only-child")
       (css:color "currentColor"))
     (rule '(".ant-btn-danger:active > a:only-child::after"
             ".ant-btn-danger.active > a:only-child::after")
       (css:position "absolute")
       (css:top "0")
       (css:right "0")
       (css:bottom "0")
       (css:left "0")
       (css:background "transparent")
       (css:content "\"\""))
     (rule '(".ant-btn-danger-disabled"
             ".ant-btn-danger.disabled"
             ".ant-btn-danger[disabled]"
             ".ant-btn-danger-disabled:hover"
             ".ant-btn-danger.disabled:hover"
             ".ant-btn-danger[disabled]:hover"
             ".ant-btn-danger-disabled:focus"
             ".ant-btn-danger.disabled:focus"
             ".ant-btn-danger[disabled]:focus"
             ".ant-btn-danger-disabled:active"
             ".ant-btn-danger.disabled:active"
             ".ant-btn-danger[disabled]:active"
             ".ant-btn-danger-disabled.active"
             ".ant-btn-danger.disabled.active"
             ".ant-btn-danger[disabled].active")
       (css:color "rgba0, 0, 0, 1/4")
       (css:background-color "#f5f5f5")
       (css:border-color "#d9d9d9")
       (css:text-shadow "none")
       (property "-webkit-box-shadow" "none")
       (css:box-shadow "none"))
     (rule '(".ant-btn-danger-disabled > a:only-child"
             ".ant-btn-danger.disabled > a:only-child"
             ".ant-btn-danger[disabled] > a:only-child"
             ".ant-btn-danger-disabled:hover > a:only-child"
             ".ant-btn-danger.disabled:hover > a:only-child"
             ".ant-btn-danger[disabled]:hover > a:only-child"
             ".ant-btn-danger-disabled:focus > a:only-child"
             ".ant-btn-danger.disabled:focus > a:only-child"
             ".ant-btn-danger[disabled]:focus > a:only-child"
             ".ant-btn-danger-disabled:active > a:only-child"
             ".ant-btn-danger.disabled:active > a:only-child"
             ".ant-btn-danger[disabled]:active > a:only-child"
             ".ant-btn-danger-disabled.active > a:only-child"
             ".ant-btn-danger.disabled.active > a:only-child"
             ".ant-btn-danger[disabled].active > a:only-child")
       (css:color "currentColor"))
     (rule '(".ant-btn-danger-disabled > a:only-child::after"
             ".ant-btn-danger.disabled > a:only-child::after"
             ".ant-btn-danger[disabled] > a:only-child::after"
             ".ant-btn-danger-disabled:hover > a:only-child::after"
             ".ant-btn-danger.disabled:hover > a:only-child::after"
             ".ant-btn-danger[disabled]:hover > a:only-child::after"
             ".ant-btn-danger-disabled:focus > a:only-child::after"
             ".ant-btn-danger.disabled:focus > a:only-child::after"
             ".ant-btn-danger[disabled]:focus > a:only-child::after"
             ".ant-btn-danger-disabled:active > a:only-child::after"
             ".ant-btn-danger.disabled:active > a:only-child::after"
             ".ant-btn-danger[disabled]:active > a:only-child::after"
             ".ant-btn-danger-disabled.active > a:only-child::after"
             ".ant-btn-danger.disabled.active > a:only-child::after"
             ".ant-btn-danger[disabled].active > a:only-child::after")
       (css:position "absolute")
       (css:top "0")
       (css:right "0")
       (css:bottom "0")
       (css:left "0")
       (css:background "transparent")
       (css:content "\"\""))
     (rule ".ant-btn-link"
       (css:color "#1890ff")
       (css:background-color "transparent")
       (css:border-color "transparent")
       (property "-webkit-box-shadow" "none")
       (css:box-shadow "none"))
     (rule ".ant-btn-link > a:only-child"
       (css:color "currentColor"))
     (rule ".ant-btn-link > a:only-child::after"
       (css:position "absolute")
       (css:top "0")
       (css:right "0")
       (css:bottom "0")
       (css:left "0")
       (css:background "transparent")
       (css:content "\"\""))
     (rule '(".ant-btn-link:hover" ".ant-btn-link:focus")
       (css:color "#40a9ff")
       (css:background-color "transparent")
       (css:border-color "#40a9ff"))
     (rule '(".ant-btn-link:hover > a:only-child" ".ant-btn-link:focus > a:only-child")
       (css:color "currentColor"))
     (rule '(".ant-btn-link:hover > a:only-child::after"
             ".ant-btn-link:focus > a:only-child::after")
       (css:position "absolute")
       (css:top "0")
       (css:right "0")
       (css:bottom "0")
       (css:left "0")
       (css:background "transparent")
       (css:content "\"\""))
     (rule '(".ant-btn-link:active" ".ant-btn-link.active")
       (css:color "#096dd9")
       (css:background-color "transparent")
       (css:border-color "#096dd9"))
     (rule '(".ant-btn-link:active > a:only-child" ".ant-btn-link.active > a:only-child")
       (css:color "currentColor"))
     (rule '(".ant-btn-link:active > a:only-child::after"
             ".ant-btn-link.active > a:only-child::after")
       (css:position "absolute")
       (css:top "0")
       (css:right "0")
       (css:bottom "0")
       (css:left "0")
       (css:background "transparent")
       (css:content "\"\""))
     (rule '(".ant-btn-link-disabled"
             ".ant-btn-link.disabled"
             ".ant-btn-link[disabled]"
             ".ant-btn-link-disabled:hover"
             ".ant-btn-link.disabled:hover"
             ".ant-btn-link[disabled]:hover"
             ".ant-btn-link-disabled:focus"
             ".ant-btn-link.disabled:focus"
             ".ant-btn-link[disabled]:focus"
             ".ant-btn-link-disabled:active"
             ".ant-btn-link.disabled:active"
             ".ant-btn-link[disabled]:active"
             ".ant-btn-link-disabled.active"
             ".ant-btn-link.disabled.active"
             ".ant-btn-link[disabled].active")
       (css:color "rgba0, 0, 0, 1/4")
       (css:background-color "#f5f5f5")
       (css:border-color "#d9d9d9")
       (css:text-shadow "none")
       (property "-webkit-box-shadow" "none")
       (css:box-shadow "none"))
     (rule '(".ant-btn-link-disabled > a:only-child"
             ".ant-btn-link.disabled > a:only-child"
             ".ant-btn-link[disabled] > a:only-child"
             ".ant-btn-link-disabled:hover > a:only-child"
             ".ant-btn-link.disabled:hover > a:only-child"
             ".ant-btn-link[disabled]:hover > a:only-child"
             ".ant-btn-link-disabled:focus > a:only-child"
             ".ant-btn-link.disabled:focus > a:only-child"
             ".ant-btn-link[disabled]:focus > a:only-child"
             ".ant-btn-link-disabled:active > a:only-child"
             ".ant-btn-link.disabled:active > a:only-child"
             ".ant-btn-link[disabled]:active > a:only-child"
             ".ant-btn-link-disabled.active > a:only-child"
             ".ant-btn-link.disabled.active > a:only-child"
             ".ant-btn-link[disabled].active > a:only-child")
       (css:color "currentColor"))
     (rule '(".ant-btn-link-disabled > a:only-child::after"
             ".ant-btn-link.disabled > a:only-child::after"
             ".ant-btn-link[disabled] > a:only-child::after"
             ".ant-btn-link-disabled:hover > a:only-child::after"
             ".ant-btn-link.disabled:hover > a:only-child::after"
             ".ant-btn-link[disabled]:hover > a:only-child::after"
             ".ant-btn-link-disabled:focus > a:only-child::after"
             ".ant-btn-link.disabled:focus > a:only-child::after"
             ".ant-btn-link[disabled]:focus > a:only-child::after"
             ".ant-btn-link-disabled:active > a:only-child::after"
             ".ant-btn-link.disabled:active > a:only-child::after"
             ".ant-btn-link[disabled]:active > a:only-child::after"
             ".ant-btn-link-disabled.active > a:only-child::after"
             ".ant-btn-link.disabled.active > a:only-child::after"
             ".ant-btn-link[disabled].active > a:only-child::after")
       (css:position "absolute")
       (css:top "0")
       (css:right "0")
       (css:bottom "0")
       (css:left "0")
       (css:background "transparent")
       (css:content "\"\""))
     (rule '(".ant-btn-link:hover" ".ant-btn-link:focus" ".ant-btn-link:active")
       (css:border-color "transparent"))
     (rule '(".ant-btn-link-disabled"
             ".ant-btn-link.disabled"
             ".ant-btn-link[disabled]"
             ".ant-btn-link-disabled:hover"
             ".ant-btn-link.disabled:hover"
             ".ant-btn-link[disabled]:hover"
             ".ant-btn-link-disabled:focus"
             ".ant-btn-link.disabled:focus"
             ".ant-btn-link[disabled]:focus"
             ".ant-btn-link-disabled:active"
             ".ant-btn-link.disabled:active"
             ".ant-btn-link[disabled]:active"
             ".ant-btn-link-disabled.active"
             ".ant-btn-link.disabled.active"
             ".ant-btn-link[disabled].active")
       (css:color "rgba0, 0, 0, 1/4")
       (css:background-color "transparent")
       (css:border-color "transparent")
       (css:text-shadow "none")
       (property "-webkit-box-shadow" "none")
       (css:box-shadow "none"))
     (rule '(".ant-btn-link-disabled > a:only-child"
             ".ant-btn-link.disabled > a:only-child"
             ".ant-btn-link[disabled] > a:only-child"
             ".ant-btn-link-disabled:hover > a:only-child"
             ".ant-btn-link.disabled:hover > a:only-child"
             ".ant-btn-link[disabled]:hover > a:only-child"
             ".ant-btn-link-disabled:focus > a:only-child"
             ".ant-btn-link.disabled:focus > a:only-child"
             ".ant-btn-link[disabled]:focus > a:only-child"
             ".ant-btn-link-disabled:active > a:only-child"
             ".ant-btn-link.disabled:active > a:only-child"
             ".ant-btn-link[disabled]:active > a:only-child"
             ".ant-btn-link-disabled.active > a:only-child"
             ".ant-btn-link.disabled.active > a:only-child"
             ".ant-btn-link[disabled].active > a:only-child")
       (css:color "currentColor"))
     (rule '(".ant-btn-link-disabled > a:only-child::after"
             ".ant-btn-link.disabled > a:only-child::after"
             ".ant-btn-link[disabled] > a:only-child::after"
             ".ant-btn-link-disabled:hover > a:only-child::after"
             ".ant-btn-link.disabled:hover > a:only-child::after"
             ".ant-btn-link[disabled]:hover > a:only-child::after"
             ".ant-btn-link-disabled:focus > a:only-child::after"
             ".ant-btn-link.disabled:focus > a:only-child::after"
             ".ant-btn-link[disabled]:focus > a:only-child::after"
             ".ant-btn-link-disabled:active > a:only-child::after"
             ".ant-btn-link.disabled:active > a:only-child::after"
             ".ant-btn-link[disabled]:active > a:only-child::after"
             ".ant-btn-link-disabled.active > a:only-child::after"
             ".ant-btn-link.disabled.active > a:only-child::after"
             ".ant-btn-link[disabled].active > a:only-child::after")
       (css:position "absolute")
       (css:top "0")
       (css:right "0")
       (css:bottom "0")
       (css:left "0")
       (css:background "transparent")
       (css:content "\"\""))
     (rule ".ant-btn-dangerous"
       (css:color "#ff4d4f")
       (css:background-color "#fff")
       (css:border-color "#ff4d4f"))
     (rule ".ant-btn-dangerous > a:only-child"
       (css:color "currentColor"))
     (rule ".ant-btn-dangerous > a:only-child::after"
       (css:position "absolute")
       (css:top "0")
       (css:right "0")
       (css:bottom "0")
       (css:left "0")
       (css:background "transparent")
       (css:content "\"\""))
     (rule '(".ant-btn-dangerous:hover" ".ant-btn-dangerous:focus")
       (css:color "#ff7875")
       (css:background-color "#fff")
       (css:border-color "#ff7875"))
     (rule '(".ant-btn-dangerous:hover > a:only-child" ".ant-btn-dangerous:focus > a:only-child")
       (css:color "currentColor"))
     (rule '(".ant-btn-dangerous:hover > a:only-child::after"
             ".ant-btn-dangerous:focus > a:only-child::after")
       (css:position "absolute")
       (css:top "0")
       (css:right "0")
       (css:bottom "0")
       (css:left "0")
       (css:background "transparent")
       (css:content "\"\""))
     (rule '(".ant-btn-dangerous:active" ".ant-btn-dangerous.active")
       (css:color "#d9363e")
       (css:background-color "#fff")
       (css:border-color "#d9363e"))
     (rule '(".ant-btn-dangerous:active > a:only-child"
             ".ant-btn-dangerous.active > a:only-child")
       (css:color "currentColor"))
     (rule '(".ant-btn-dangerous:active > a:only-child::after"
             ".ant-btn-dangerous.active > a:only-child::after")
       (css:position "absolute")
       (css:top "0")
       (css:right "0")
       (css:bottom "0")
       (css:left "0")
       (css:background "transparent")
       (css:content "\"\""))
     (rule '(".ant-btn-dangerous-disabled"
             ".ant-btn-dangerous.disabled"
             ".ant-btn-dangerous[disabled]"
             ".ant-btn-dangerous-disabled:hover"
             ".ant-btn-dangerous.disabled:hover"
             ".ant-btn-dangerous[disabled]:hover"
             ".ant-btn-dangerous-disabled:focus"
             ".ant-btn-dangerous.disabled:focus"
             ".ant-btn-dangerous[disabled]:focus"
             ".ant-btn-dangerous-disabled:active"
             ".ant-btn-dangerous.disabled:active"
             ".ant-btn-dangerous[disabled]:active"
             ".ant-btn-dangerous-disabled.active"
             ".ant-btn-dangerous.disabled.active"
             ".ant-btn-dangerous[disabled].active")
       (css:color "rgba0, 0, 0, 1/4")
       (css:background-color "#f5f5f5")
       (css:border-color "#d9d9d9")
       (css:text-shadow "none")
       (property "-webkit-box-shadow" "none")
       (css:box-shadow "none"))
     (rule '(".ant-btn-dangerous-disabled > a:only-child"
             ".ant-btn-dangerous.disabled > a:only-child"
             ".ant-btn-dangerous[disabled] > a:only-child"
             ".ant-btn-dangerous-disabled:hover > a:only-child"
             ".ant-btn-dangerous.disabled:hover > a:only-child"
             ".ant-btn-dangerous[disabled]:hover > a:only-child"
             ".ant-btn-dangerous-disabled:focus > a:only-child"
             ".ant-btn-dangerous.disabled:focus > a:only-child"
             ".ant-btn-dangerous[disabled]:focus > a:only-child"
             ".ant-btn-dangerous-disabled:active > a:only-child"
             ".ant-btn-dangerous.disabled:active > a:only-child"
             ".ant-btn-dangerous[disabled]:active > a:only-child"
             ".ant-btn-dangerous-disabled.active > a:only-child"
             ".ant-btn-dangerous.disabled.active > a:only-child"
             ".ant-btn-dangerous[disabled].active > a:only-child")
       (css:color "currentColor"))
     (rule '(".ant-btn-dangerous-disabled > a:only-child::after"
             ".ant-btn-dangerous.disabled > a:only-child::after"
             ".ant-btn-dangerous[disabled] > a:only-child::after"
             ".ant-btn-dangerous-disabled:hover > a:only-child::after"
             ".ant-btn-dangerous.disabled:hover > a:only-child::after"
             ".ant-btn-dangerous[disabled]:hover > a:only-child::after"
             ".ant-btn-dangerous-disabled:focus > a:only-child::after"
             ".ant-btn-dangerous.disabled:focus > a:only-child::after"
             ".ant-btn-dangerous[disabled]:focus > a:only-child::after"
             ".ant-btn-dangerous-disabled:active > a:only-child::after"
             ".ant-btn-dangerous.disabled:active > a:only-child::after"
             ".ant-btn-dangerous[disabled]:active > a:only-child::after"
             ".ant-btn-dangerous-disabled.active > a:only-child::after"
             ".ant-btn-dangerous.disabled.active > a:only-child::after"
             ".ant-btn-dangerous[disabled].active > a:only-child::after")
       (css:position "absolute")
       (css:top "0")
       (css:right "0")
       (css:bottom "0")
       (css:left "0")
       (css:background "transparent")
       (css:content "\"\""))
     (rule ".ant-btn-dangerous.ant-btn-primary"
       (css:color "#fff")
       (css:background-color "#ff4d4f")
       (css:border-color "#ff4d4f")
       (css:text-shadow "0 -1px 0 rgba0, 0, 0, 3/25")
       (property "-webkit-box-shadow" "0 2px 0 rgba0, 0, 0, 9/200")
       (css:box-shadow "0 2px 0 rgba0, 0, 0, 9/200"))
     (rule ".ant-btn-dangerous.ant-btn-primary > a:only-child"
       (css:color "currentColor"))
     (rule ".ant-btn-dangerous.ant-btn-primary > a:only-child::after"
       (css:position "absolute")
       (css:top "0")
       (css:right "0")
       (css:bottom "0")
       (css:left "0")
       (css:background "transparent")
       (css:content "\"\""))
     (rule '(".ant-btn-dangerous.ant-btn-primary:hover"
             ".ant-btn-dangerous.ant-btn-primary:focus")
       (css:color "#fff")
       (css:background-color "#ff7875")
       (css:border-color "#ff7875"))
     (rule '(".ant-btn-dangerous.ant-btn-primary:hover > a:only-child"
             ".ant-btn-dangerous.ant-btn-primary:focus > a:only-child")
       (css:color "currentColor"))
     (rule '(".ant-btn-dangerous.ant-btn-primary:hover > a:only-child::after"
             ".ant-btn-dangerous.ant-btn-primary:focus > a:only-child::after")
       (css:position "absolute")
       (css:top "0")
       (css:right "0")
       (css:bottom "0")
       (css:left "0")
       (css:background "transparent")
       (css:content "\"\""))
     (rule '(".ant-btn-dangerous.ant-btn-primary:active"
             ".ant-btn-dangerous.ant-btn-primary.active")
       (css:color "#fff")
       (css:background-color "#d9363e")
       (css:border-color "#d9363e"))
     (rule '(".ant-btn-dangerous.ant-btn-primary:active > a:only-child"
             ".ant-btn-dangerous.ant-btn-primary.active > a:only-child")
       (css:color "currentColor"))
     (rule '(".ant-btn-dangerous.ant-btn-primary:active > a:only-child::after"
             ".ant-btn-dangerous.ant-btn-primary.active > a:only-child::after")
       (css:position "absolute")
       (css:top "0")
       (css:right "0")
       (css:bottom "0")
       (css:left "0")
       (css:background "transparent")
       (css:content "\"\""))
     (rule '(".ant-btn-dangerous.ant-btn-primary-disabled"
             ".ant-btn-dangerous.ant-btn-primary.disabled"
             ".ant-btn-dangerous.ant-btn-primary[disabled]"
             ".ant-btn-dangerous.ant-btn-primary-disabled:hover"
             ".ant-btn-dangerous.ant-btn-primary.disabled:hover"
             ".ant-btn-dangerous.ant-btn-primary[disabled]:hover"
             ".ant-btn-dangerous.ant-btn-primary-disabled:focus"
             ".ant-btn-dangerous.ant-btn-primary.disabled:focus"
             ".ant-btn-dangerous.ant-btn-primary[disabled]:focus"
             ".ant-btn-dangerous.ant-btn-primary-disabled:active"
             ".ant-btn-dangerous.ant-btn-primary.disabled:active"
             ".ant-btn-dangerous.ant-btn-primary[disabled]:active"
             ".ant-btn-dangerous.ant-btn-primary-disabled.active"
             ".ant-btn-dangerous.ant-btn-primary.disabled.active"
             ".ant-btn-dangerous.ant-btn-primary[disabled].active")
       (css:color "rgba0, 0, 0, 1/4")
       (css:background-color "#f5f5f5")
       (css:border-color "#d9d9d9")
       (css:text-shadow "none")
       (property "-webkit-box-shadow" "none")
       (css:box-shadow "none"))
     (rule '(".ant-btn-dangerous.ant-btn-primary-disabled > a:only-child"
             ".ant-btn-dangerous.ant-btn-primary.disabled > a:only-child"
             ".ant-btn-dangerous.ant-btn-primary[disabled] > a:only-child"
             ".ant-btn-dangerous.ant-btn-primary-disabled:hover > a:only-child"
             ".ant-btn-dangerous.ant-btn-primary.disabled:hover > a:only-child"
             ".ant-btn-dangerous.ant-btn-primary[disabled]:hover > a:only-child"
             ".ant-btn-dangerous.ant-btn-primary-disabled:focus > a:only-child"
             ".ant-btn-dangerous.ant-btn-primary.disabled:focus > a:only-child"
             ".ant-btn-dangerous.ant-btn-primary[disabled]:focus > a:only-child"
             ".ant-btn-dangerous.ant-btn-primary-disabled:active > a:only-child"
             ".ant-btn-dangerous.ant-btn-primary.disabled:active > a:only-child"
             ".ant-btn-dangerous.ant-btn-primary[disabled]:active > a:only-child"
             ".ant-btn-dangerous.ant-btn-primary-disabled.active > a:only-child"
             ".ant-btn-dangerous.ant-btn-primary.disabled.active > a:only-child"
             ".ant-btn-dangerous.ant-btn-primary[disabled].active > a:only-child")
       (css:color "currentColor"))
     (rule '(".ant-btn-dangerous.ant-btn-primary-disabled > a:only-child::after"
             ".ant-btn-dangerous.ant-btn-primary.disabled > a:only-child::after"
             ".ant-btn-dangerous.ant-btn-primary[disabled] > a:only-child::after"
             ".ant-btn-dangerous.ant-btn-primary-disabled:hover > a:only-child::after"
             ".ant-btn-dangerous.ant-btn-primary.disabled:hover > a:only-child::after"
             ".ant-btn-dangerous.ant-btn-primary[disabled]:hover > a:only-child::after"
             ".ant-btn-dangerous.ant-btn-primary-disabled:focus > a:only-child::after"
             ".ant-btn-dangerous.ant-btn-primary.disabled:focus > a:only-child::after"
             ".ant-btn-dangerous.ant-btn-primary[disabled]:focus > a:only-child::after"
             ".ant-btn-dangerous.ant-btn-primary-disabled:active > a:only-child::after"
             ".ant-btn-dangerous.ant-btn-primary.disabled:active > a:only-child::after"
             ".ant-btn-dangerous.ant-btn-primary[disabled]:active > a:only-child::after"
             ".ant-btn-dangerous.ant-btn-primary-disabled.active > a:only-child::after"
             ".ant-btn-dangerous.ant-btn-primary.disabled.active > a:only-child::after"
             ".ant-btn-dangerous.ant-btn-primary[disabled].active > a:only-child::after")
       (css:position "absolute")
       (css:top "0")
       (css:right "0")
       (css:bottom "0")
       (css:left "0")
       (css:background "transparent")
       (css:content "\"\""))
     (rule ".ant-btn-dangerous.ant-btn-link"
       (css:color "#ff4d4f")
       (css:background-color "transparent")
       (css:border-color "transparent")
       (property "-webkit-box-shadow" "none")
       (css:box-shadow "none"))
     (rule ".ant-btn-dangerous.ant-btn-link > a:only-child"
       (css:color "currentColor"))
     (rule ".ant-btn-dangerous.ant-btn-link > a:only-child::after"
       (css:position "absolute")
       (css:top "0")
       (css:right "0")
       (css:bottom "0")
       (css:left "0")
       (css:background "transparent")
       (css:content "\"\""))
     (rule '(".ant-btn-dangerous.ant-btn-link:hover" ".ant-btn-dangerous.ant-btn-link:focus")
       (css:color "#40a9ff")
       (css:background-color "transparent")
       (css:border-color "#40a9ff"))
     (rule '(".ant-btn-dangerous.ant-btn-link:hover > a:only-child"
             ".ant-btn-dangerous.ant-btn-link:focus > a:only-child")
       (css:color "currentColor"))
     (rule '(".ant-btn-dangerous.ant-btn-link:hover > a:only-child::after"
             ".ant-btn-dangerous.ant-btn-link:focus > a:only-child::after")
       (css:position "absolute")
       (css:top "0")
       (css:right "0")
       (css:bottom "0")
       (css:left "0")
       (css:background "transparent")
       (css:content "\"\""))
     (rule '(".ant-btn-dangerous.ant-btn-link:active" ".ant-btn-dangerous.ant-btn-link.active")
       (css:color "#096dd9")
       (css:background-color "transparent")
       (css:border-color "#096dd9"))
     (rule '(".ant-btn-dangerous.ant-btn-link:active > a:only-child"
             ".ant-btn-dangerous.ant-btn-link.active > a:only-child")
       (css:color "currentColor"))
     (rule '(".ant-btn-dangerous.ant-btn-link:active > a:only-child::after"
             ".ant-btn-dangerous.ant-btn-link.active > a:only-child::after")
       (css:position "absolute")
       (css:top "0")
       (css:right "0")
       (css:bottom "0")
       (css:left "0")
       (css:background "transparent")
       (css:content "\"\""))
     (rule '(".ant-btn-dangerous.ant-btn-link-disabled"
             ".ant-btn-dangerous.ant-btn-link.disabled"
             ".ant-btn-dangerous.ant-btn-link[disabled]"
             ".ant-btn-dangerous.ant-btn-link-disabled:hover"
             ".ant-btn-dangerous.ant-btn-link.disabled:hover"
             ".ant-btn-dangerous.ant-btn-link[disabled]:hover"
             ".ant-btn-dangerous.ant-btn-link-disabled:focus"
             ".ant-btn-dangerous.ant-btn-link.disabled:focus"
             ".ant-btn-dangerous.ant-btn-link[disabled]:focus"
             ".ant-btn-dangerous.ant-btn-link-disabled:active"
             ".ant-btn-dangerous.ant-btn-link.disabled:active"
             ".ant-btn-dangerous.ant-btn-link[disabled]:active"
             ".ant-btn-dangerous.ant-btn-link-disabled.active"
             ".ant-btn-dangerous.ant-btn-link.disabled.active"
             ".ant-btn-dangerous.ant-btn-link[disabled].active")
       (css:color "rgba0, 0, 0, 1/4")
       (css:background-color "#f5f5f5")
       (css:border-color "#d9d9d9")
       (css:text-shadow "none")
       (property "-webkit-box-shadow" "none")
       (css:box-shadow "none"))
     (rule '(".ant-btn-dangerous.ant-btn-link-disabled > a:only-child"
             ".ant-btn-dangerous.ant-btn-link.disabled > a:only-child"
             ".ant-btn-dangerous.ant-btn-link[disabled] > a:only-child"
             ".ant-btn-dangerous.ant-btn-link-disabled:hover > a:only-child"
             ".ant-btn-dangerous.ant-btn-link.disabled:hover > a:only-child"
             ".ant-btn-dangerous.ant-btn-link[disabled]:hover > a:only-child"
             ".ant-btn-dangerous.ant-btn-link-disabled:focus > a:only-child"
             ".ant-btn-dangerous.ant-btn-link.disabled:focus > a:only-child"
             ".ant-btn-dangerous.ant-btn-link[disabled]:focus > a:only-child"
             ".ant-btn-dangerous.ant-btn-link-disabled:active > a:only-child"
             ".ant-btn-dangerous.ant-btn-link.disabled:active > a:only-child"
             ".ant-btn-dangerous.ant-btn-link[disabled]:active > a:only-child"
             ".ant-btn-dangerous.ant-btn-link-disabled.active > a:only-child"
             ".ant-btn-dangerous.ant-btn-link.disabled.active > a:only-child"
             ".ant-btn-dangerous.ant-btn-link[disabled].active > a:only-child")
       (css:color "currentColor"))
     (rule '(".ant-btn-dangerous.ant-btn-link-disabled > a:only-child::after"
             ".ant-btn-dangerous.ant-btn-link.disabled > a:only-child::after"
             ".ant-btn-dangerous.ant-btn-link[disabled] > a:only-child::after"
             ".ant-btn-dangerous.ant-btn-link-disabled:hover > a:only-child::after"
             ".ant-btn-dangerous.ant-btn-link.disabled:hover > a:only-child::after"
             ".ant-btn-dangerous.ant-btn-link[disabled]:hover > a:only-child::after"
             ".ant-btn-dangerous.ant-btn-link-disabled:focus > a:only-child::after"
             ".ant-btn-dangerous.ant-btn-link.disabled:focus > a:only-child::after"
             ".ant-btn-dangerous.ant-btn-link[disabled]:focus > a:only-child::after"
             ".ant-btn-dangerous.ant-btn-link-disabled:active > a:only-child::after"
             ".ant-btn-dangerous.ant-btn-link.disabled:active > a:only-child::after"
             ".ant-btn-dangerous.ant-btn-link[disabled]:active > a:only-child::after"
             ".ant-btn-dangerous.ant-btn-link-disabled.active > a:only-child::after"
             ".ant-btn-dangerous.ant-btn-link.disabled.active > a:only-child::after"
             ".ant-btn-dangerous.ant-btn-link[disabled].active > a:only-child::after")
       (css:position "absolute")
       (css:top "0")
       (css:right "0")
       (css:bottom "0")
       (css:left "0")
       (css:background "transparent")
       (css:content "\"\""))
     (rule '(".ant-btn-dangerous.ant-btn-link:hover" ".ant-btn-dangerous.ant-btn-link:focus")
       (css:color "#ff7875")
       (css:background-color "transparent")
       (css:border-color "transparent"))
     (rule '(".ant-btn-dangerous.ant-btn-link:hover > a:only-child"
             ".ant-btn-dangerous.ant-btn-link:focus > a:only-child")
       (css:color "currentColor"))
     (rule '(".ant-btn-dangerous.ant-btn-link:hover > a:only-child::after"
             ".ant-btn-dangerous.ant-btn-link:focus > a:only-child::after")
       (css:position "absolute")
       (css:top "0")
       (css:right "0")
       (css:bottom "0")
       (css:left "0")
       (css:background "transparent")
       (css:content "\"\""))
     (rule ".ant-btn-dangerous.ant-btn-link:active"
       (css:color "#d9363e")
       (css:background-color "transparent")
       (css:border-color "transparent"))
     (rule ".ant-btn-dangerous.ant-btn-link:active > a:only-child"
       (css:color "currentColor"))
     (rule ".ant-btn-dangerous.ant-btn-link:active > a:only-child::after"
       (css:position "absolute")
       (css:top "0")
       (css:right "0")
       (css:bottom "0")
       (css:left "0")
       (css:background "transparent")
       (css:content "\"\""))
     (rule '(".ant-btn-dangerous.ant-btn-link-disabled"
             ".ant-btn-dangerous.ant-btn-link.disabled"
             ".ant-btn-dangerous.ant-btn-link[disabled]"
             ".ant-btn-dangerous.ant-btn-link-disabled:hover"
             ".ant-btn-dangerous.ant-btn-link.disabled:hover"
             ".ant-btn-dangerous.ant-btn-link[disabled]:hover"
             ".ant-btn-dangerous.ant-btn-link-disabled:focus"
             ".ant-btn-dangerous.ant-btn-link.disabled:focus"
             ".ant-btn-dangerous.ant-btn-link[disabled]:focus"
             ".ant-btn-dangerous.ant-btn-link-disabled:active"
             ".ant-btn-dangerous.ant-btn-link.disabled:active"
             ".ant-btn-dangerous.ant-btn-link[disabled]:active"
             ".ant-btn-dangerous.ant-btn-link-disabled.active"
             ".ant-btn-dangerous.ant-btn-link.disabled.active"
             ".ant-btn-dangerous.ant-btn-link[disabled].active")
       (css:color "rgba0, 0, 0, 1/4")
       (css:background-color "transparent")
       (css:border-color "transparent")
       (css:text-shadow "none")
       (property "-webkit-box-shadow" "none")
       (css:box-shadow "none"))
     (rule '(".ant-btn-dangerous.ant-btn-link-disabled > a:only-child"
             ".ant-btn-dangerous.ant-btn-link.disabled > a:only-child"
             ".ant-btn-dangerous.ant-btn-link[disabled] > a:only-child"
             ".ant-btn-dangerous.ant-btn-link-disabled:hover > a:only-child"
             ".ant-btn-dangerous.ant-btn-link.disabled:hover > a:only-child"
             ".ant-btn-dangerous.ant-btn-link[disabled]:hover > a:only-child"
             ".ant-btn-dangerous.ant-btn-link-disabled:focus > a:only-child"
             ".ant-btn-dangerous.ant-btn-link.disabled:focus > a:only-child"
             ".ant-btn-dangerous.ant-btn-link[disabled]:focus > a:only-child"
             ".ant-btn-dangerous.ant-btn-link-disabled:active > a:only-child"
             ".ant-btn-dangerous.ant-btn-link.disabled:active > a:only-child"
             ".ant-btn-dangerous.ant-btn-link[disabled]:active > a:only-child"
             ".ant-btn-dangerous.ant-btn-link-disabled.active > a:only-child"
             ".ant-btn-dangerous.ant-btn-link.disabled.active > a:only-child"
             ".ant-btn-dangerous.ant-btn-link[disabled].active > a:only-child")
       (css:color "currentColor"))
     (rule '(".ant-btn-dangerous.ant-btn-link-disabled > a:only-child::after"
             ".ant-btn-dangerous.ant-btn-link.disabled > a:only-child::after"
             ".ant-btn-dangerous.ant-btn-link[disabled] > a:only-child::after"
             ".ant-btn-dangerous.ant-btn-link-disabled:hover > a:only-child::after"
             ".ant-btn-dangerous.ant-btn-link.disabled:hover > a:only-child::after"
             ".ant-btn-dangerous.ant-btn-link[disabled]:hover > a:only-child::after"
             ".ant-btn-dangerous.ant-btn-link-disabled:focus > a:only-child::after"
             ".ant-btn-dangerous.ant-btn-link.disabled:focus > a:only-child::after"
             ".ant-btn-dangerous.ant-btn-link[disabled]:focus > a:only-child::after"
             ".ant-btn-dangerous.ant-btn-link-disabled:active > a:only-child::after"
             ".ant-btn-dangerous.ant-btn-link.disabled:active > a:only-child::after"
             ".ant-btn-dangerous.ant-btn-link[disabled]:active > a:only-child::after"
             ".ant-btn-dangerous.ant-btn-link-disabled.active > a:only-child::after"
             ".ant-btn-dangerous.ant-btn-link.disabled.active > a:only-child::after"
             ".ant-btn-dangerous.ant-btn-link[disabled].active > a:only-child::after")
       (css:position "absolute")
       (css:top "0")
       (css:right "0")
       (css:bottom "0")
       (css:left "0")
       (css:background "transparent")
       (css:content "\"\""))
     (rule ".ant-btn-icon-only"
       (css:width "32px")
       (css:height "32px")
       (css:padding "12/5px 0")
       (css:font-size "16px")
       (css:border-radius "2px"))
     (rule ".ant-btn-icon-only > *"
       (css:font-size "16px"))
     (rule ".ant-btn-icon-only.ant-btn-lg"
       (css:width "40px")
       (css:height "40px")
       (css:padding "49/10px 0")
       (css:font-size "18px")
       (css:border-radius "2px"))
     (rule ".ant-btn-icon-only.ant-btn-lg > *"
       (css:font-size "18px"))
     (rule ".ant-btn-icon-only.ant-btn-sm"
       (css:width "24px")
       (css:height "24px")
       (css:padding "0px 0")
       (css:font-size "14px")
       (css:border-radius "2px"))
     (rule ".ant-btn-icon-only.ant-btn-sm > *"
       (css:font-size "14px"))
     (rule ".ant-btn-icon-only > i"
       (css:vertical-align "middle"))
     (rule ".ant-btn-round"
       (css:height "32px")
       (css:padding "4px 16px")
       (css:font-size "14px")
       (css:border-radius "32px"))
     (rule ".ant-btn-round.ant-btn-lg"
       (css:height "40px")
       (css:padding "32/5px 20px")
       (css:font-size "16px")
       (css:border-radius "40px"))
     (rule ".ant-btn-round.ant-btn-sm"
       (css:height "24px")
       (css:padding "0px 12px")
       (css:font-size "14px")
       (css:border-radius "24px"))
     (rule ".ant-btn-round.ant-btn-icon-only"
       (css:width "auto"))
     (rule '(".ant-btn-circle" ".ant-btn-circle-outline")
       (css:min-width "32px")
       (css:padding-right "0")
       (css:padding-left "0")
       (css:text-align "center")
       (css:border-radius "50%"))
     (rule '(".ant-btn-circle.ant-btn-lg" ".ant-btn-circle-outline.ant-btn-lg")
       (css:min-width "40px")
       (css:border-radius "50%"))
     (rule '(".ant-btn-circle.ant-btn-sm" ".ant-btn-circle-outline.ant-btn-sm")
       (css:min-width "24px")
       (css:border-radius "50%"))
     (rule ".ant-btn::before"
       (css:position "absolute")
       (css:top "-1px")
       (css:right "-1px")
       (css:bottom "-1px")
       (css:left "-1px")
       (css:z-index "1")
       (css:display "none")
       (css:background "#fff")
       (css:border-radius "inherit")
       (css:opacity "7/20")
       (property "-webkit-transition" "opacity 1/5s")
       (css:transition "opacity 1/5s")
       (css:content "\"\"")
       (css:pointer-events "none"))
     (rule ".ant-btn .anticon"
       (property "-webkit-transition" "margin-left 3/10s cubic-bezier129/200, 9/200, 71/200, 1")
       (css:transition "margin-left 3/10s cubic-bezier129/200, 9/200, 71/200, 1"))
     (rule '(".ant-btn .anticon.anticon-plus > svg" ".ant-btn .anticon.anticon-minus > svg")
       (property "shape-rendering" "optimizeSpeed"))
     (rule ".ant-btn.ant-btn-loading"
       (css:position "relative"))
     (rule ".ant-btn.ant-btn-loading:not[disabled]"
       (css:pointer-events "none"))
     (rule ".ant-btn.ant-btn-loading::before"
       (css:display "block"))
     (rule ".ant-btn.ant-btn-loading:not.ant-btn-circle:not.ant-btn-circle-outline:not.ant-btn-icon-only"
       (css:padding-left "29px"))
     (rule ".ant-btn.ant-btn-loading:not.ant-btn-circle:not.ant-btn-circle-outline:not.ant-btn-icon-only .anticon:not:last-child"
       (css:margin-left "-14px"))
     (rule ".ant-btn-sm.ant-btn-loading:not.ant-btn-circle:not.ant-btn-circle-outline:not.ant-btn-icon-only"
       (css:padding-left "24px"))
     (rule ".ant-btn-sm.ant-btn-loading:not.ant-btn-circle:not.ant-btn-circle-outline:not.ant-btn-icon-only .anticon"
       (css:margin-left "-17px"))
     (rule ".ant-btn-group"
       (css:position "relative")
       (css:display "inline-block"))
     (rule '(".ant-btn-group > .ant-btn" ".ant-btn-group > span > .ant-btn")
       (css:position "relative"))
     (rule '(".ant-btn-group > .ant-btn:hover"
             ".ant-btn-group > span > .ant-btn:hover"
             ".ant-btn-group > .ant-btn:focus"
             ".ant-btn-group > span > .ant-btn:focus"
             ".ant-btn-group > .ant-btn:active"
             ".ant-btn-group > span > .ant-btn:active"
             ".ant-btn-group > .ant-btn.active"
             ".ant-btn-group > span > .ant-btn.active")
       (css:z-index "2"))
     (rule '(".ant-btn-group > .ant-btn:disabled" ".ant-btn-group > span > .ant-btn:disabled")
       (css:z-index "0"))
     (rule ".ant-btn-group > .ant-btn-icon-only"
       (css:font-size "14px"))
     (rule '(".ant-btn-group-lg > .ant-btn" ".ant-btn-group-lg > span > .ant-btn")
       (css:height "40px")
       (css:padding "32/5px 15px")
       (css:font-size "16px")
       (css:border-radius "0"))
     (rule ".ant-btn-group-lg > .ant-btn.ant-btn-icon-only"
       (css:width "40px")
       (css:height "40px")
       (css:padding-right "0")
       (css:padding-left "0"))
     (rule '(".ant-btn-group-sm > .ant-btn" ".ant-btn-group-sm > span > .ant-btn")
       (css:height "24px")
       (css:padding "0px 7px")
       (css:font-size "14px")
       (css:border-radius "0"))
     (rule '(".ant-btn-group-sm > .ant-btn > .anticon"
             ".ant-btn-group-sm > span > .ant-btn > .anticon")
       (css:font-size "14px"))
     (rule ".ant-btn-group-sm > .ant-btn.ant-btn-icon-only"
       (css:width "24px")
       (css:height "24px")
       (css:padding-right "0")
       (css:padding-left "0"))
     (rule '(".ant-btn-group .ant-btn + .ant-btn"
             ".ant-btn + .ant-btn-group"
             ".ant-btn-group span + .ant-btn"
             ".ant-btn-group .ant-btn + span"
             ".ant-btn-group > span + span"
             ".ant-btn-group + .ant-btn"
             ".ant-btn-group + .ant-btn-group")
       (css:margin-left "-1px"))
     (rule ".ant-btn-group .ant-btn-primary + .ant-btn:not.ant-btn-primary:not[disabled]"
       (css:border-left-color "transparent"))
     (rule ".ant-btn-group .ant-btn"
       (css:border-radius "0"))
     (rule '(".ant-btn-group > .ant-btn:first-child"
             ".ant-btn-group > span:first-child > .ant-btn")
       (css:margin-left "0"))
     (rule ".ant-btn-group > .ant-btn:only-child"
       (css:border-radius "2px"))
     (rule ".ant-btn-group > span:only-child > .ant-btn"
       (css:border-radius "2px"))
     (rule '(".ant-btn-group > .ant-btn:first-child:not:last-child"
             ".ant-btn-group > span:first-child:not:last-child > .ant-btn")
       (css:border-top-left-radius "2px")
       (css:border-bottom-left-radius "2px"))
     (rule '(".ant-btn-group > .ant-btn:last-child:not:first-child"
             ".ant-btn-group > span:last-child:not:first-child > .ant-btn")
       (css:border-top-right-radius "2px")
       (css:border-bottom-right-radius "2px"))
     (rule ".ant-btn-group-sm > .ant-btn:only-child"
       (css:border-radius "2px"))
     (rule ".ant-btn-group-sm > span:only-child > .ant-btn"
       (css:border-radius "2px"))
     (rule '(".ant-btn-group-sm > .ant-btn:first-child:not:last-child"
             ".ant-btn-group-sm > span:first-child:not:last-child > .ant-btn")
       (css:border-top-left-radius "2px")
       (css:border-bottom-left-radius "2px"))
     (rule '(".ant-btn-group-sm > .ant-btn:last-child:not:first-child"
             ".ant-btn-group-sm > span:last-child:not:first-child > .ant-btn")
       (css:border-top-right-radius "2px")
       (css:border-bottom-right-radius "2px"))
     (rule ".ant-btn-group > .ant-btn-group"
       (css:float "left"))
     (rule ".ant-btn-group > .ant-btn-group:not:first-child:not:last-child > .ant-btn"
       (css:border-radius "0"))
     (rule ".ant-btn-group > .ant-btn-group:first-child:not:last-child > .ant-btn:last-child"
       (css:padding-right "8px")
       (css:border-top-right-radius "0")
       (css:border-bottom-right-radius "0"))
     (rule ".ant-btn-group > .ant-btn-group:last-child:not:first-child > .ant-btn:first-child"
       (css:padding-left "8px")
       (css:border-top-left-radius "0")
       (css:border-bottom-left-radius "0"))
     (rule '(".ant-btn-rtl.ant-btn-group .ant-btn + .ant-btn"
             ".ant-btn-rtl.ant-btn + .ant-btn-group"
             ".ant-btn-rtl.ant-btn-group span + .ant-btn"
             ".ant-btn-rtl.ant-btn-group .ant-btn + span"
             ".ant-btn-rtl.ant-btn-group > span + span"
             ".ant-btn-rtl.ant-btn-group + .ant-btn"
             ".ant-btn-rtl.ant-btn-group + .ant-btn-group"
             ".ant-btn-group-rtl.ant-btn-group .ant-btn + .ant-btn"
             ".ant-btn-group-rtl.ant-btn + .ant-btn-group"
             ".ant-btn-group-rtl.ant-btn-group span + .ant-btn"
             ".ant-btn-group-rtl.ant-btn-group .ant-btn + span"
             ".ant-btn-group-rtl.ant-btn-group > span + span"
             ".ant-btn-group-rtl.ant-btn-group + .ant-btn"
             ".ant-btn-group-rtl.ant-btn-group + .ant-btn-group")
       (css:margin-right "-1px")
       (css:margin-left "auto"))
     (rule ".ant-btn-group.ant-btn-group-rtl"
       (property "direction" "rtl"))
     (rule '(".ant-btn-group-rtl.ant-btn-group > .ant-btn:first-child:not:last-child"
             ".ant-btn-group-rtl.ant-btn-group > span:first-child:not:last-child > .ant-btn")
       (css:border-top-left-radius "0")
       (css:border-top-right-radius "2px")
       (css:border-bottom-right-radius "2px")
       (css:border-bottom-left-radius "0"))
     (rule '(".ant-btn-group-rtl.ant-btn-group > .ant-btn:last-child:not:first-child"
             ".ant-btn-group-rtl.ant-btn-group > span:last-child:not:first-child > .ant-btn")
       (css:border-top-left-radius "2px")
       (css:border-top-right-radius "0")
       (css:border-bottom-right-radius "0")
       (css:border-bottom-left-radius "2px"))
     (rule '(".ant-btn-group-rtl.ant-btn-group-sm > .ant-btn:first-child:not:last-child"
             ".ant-btn-group-rtl.ant-btn-group-sm > span:first-child:not:last-child > .ant-btn")
       (css:border-top-left-radius "0")
       (css:border-top-right-radius "2px")
       (css:border-bottom-right-radius "2px")
       (css:border-bottom-left-radius "0"))
     (rule '(".ant-btn-group-rtl.ant-btn-group-sm > .ant-btn:last-child:not:first-child"
             ".ant-btn-group-rtl.ant-btn-group-sm > span:last-child:not:first-child > .ant-btn")
       (css:border-top-left-radius "2px")
       (css:border-top-right-radius "0")
       (css:border-bottom-right-radius "0")
       (css:border-bottom-left-radius "2px"))
     (rule '(".ant-btn:focus > span" ".ant-btn:active > span")
       (css:position "relative"))
     (rule '(".ant-btn > .anticon + span" ".ant-btn > span + .anticon")
       (css:margin-left "8px"))
     (rule ".ant-btn-background-ghost"
       (css:color "#fff")
       (css:background "transparent")
       (css:border-color "#fff"))
     (rule ".ant-btn-background-ghost.ant-btn-primary"
       (css:color "#1890ff")
       (css:background-color "transparent")
       (css:border-color "#1890ff")
       (css:text-shadow "none"))
     (rule ".ant-btn-background-ghost.ant-btn-primary > a:only-child"
       (css:color "currentColor"))
     (rule ".ant-btn-background-ghost.ant-btn-primary > a:only-child::after"
       (css:position "absolute")
       (css:top "0")
       (css:right "0")
       (css:bottom "0")
       (css:left "0")
       (css:background "transparent")
       (css:content "\"\""))
     (rule '(".ant-btn-background-ghost.ant-btn-primary:hover"
             ".ant-btn-background-ghost.ant-btn-primary:focus")
       (css:color "#40a9ff")
       (css:background-color "transparent")
       (css:border-color "#40a9ff"))
     (rule '(".ant-btn-background-ghost.ant-btn-primary:hover > a:only-child"
             ".ant-btn-background-ghost.ant-btn-primary:focus > a:only-child")
       (css:color "currentColor"))
     (rule '(".ant-btn-background-ghost.ant-btn-primary:hover > a:only-child::after"
             ".ant-btn-background-ghost.ant-btn-primary:focus > a:only-child::after")
       (css:position "absolute")
       (css:top "0")
       (css:right "0")
       (css:bottom "0")
       (css:left "0")
       (css:background "transparent")
       (css:content "\"\""))
     (rule '(".ant-btn-background-ghost.ant-btn-primary:active"
             ".ant-btn-background-ghost.ant-btn-primary.active")
       (css:color "#096dd9")
       (css:background-color "transparent")
       (css:border-color "#096dd9"))
     (rule '(".ant-btn-background-ghost.ant-btn-primary:active > a:only-child"
             ".ant-btn-background-ghost.ant-btn-primary.active > a:only-child")
       (css:color "currentColor"))
     (rule '(".ant-btn-background-ghost.ant-btn-primary:active > a:only-child::after"
             ".ant-btn-background-ghost.ant-btn-primary.active > a:only-child::after")
       (css:position "absolute")
       (css:top "0")
       (css:right "0")
       (css:bottom "0")
       (css:left "0")
       (css:background "transparent")
       (css:content "\"\""))
     (rule '(".ant-btn-background-ghost.ant-btn-primary-disabled"
             ".ant-btn-background-ghost.ant-btn-primary.disabled"
             ".ant-btn-background-ghost.ant-btn-primary[disabled]"
             ".ant-btn-background-ghost.ant-btn-primary-disabled:hover"
             ".ant-btn-background-ghost.ant-btn-primary.disabled:hover"
             ".ant-btn-background-ghost.ant-btn-primary[disabled]:hover"
             ".ant-btn-background-ghost.ant-btn-primary-disabled:focus"
             ".ant-btn-background-ghost.ant-btn-primary.disabled:focus"
             ".ant-btn-background-ghost.ant-btn-primary[disabled]:focus"
             ".ant-btn-background-ghost.ant-btn-primary-disabled:active"
             ".ant-btn-background-ghost.ant-btn-primary.disabled:active"
             ".ant-btn-background-ghost.ant-btn-primary[disabled]:active"
             ".ant-btn-background-ghost.ant-btn-primary-disabled.active"
             ".ant-btn-background-ghost.ant-btn-primary.disabled.active"
             ".ant-btn-background-ghost.ant-btn-primary[disabled].active")
       (css:color "rgba0, 0, 0, 1/4")
       (css:background-color "#f5f5f5")
       (css:border-color "#d9d9d9")
       (css:text-shadow "none")
       (property "-webkit-box-shadow" "none")
       (css:box-shadow "none"))
     (rule '(".ant-btn-background-ghost.ant-btn-primary-disabled > a:only-child"
             ".ant-btn-background-ghost.ant-btn-primary.disabled > a:only-child"
             ".ant-btn-background-ghost.ant-btn-primary[disabled] > a:only-child"
             ".ant-btn-background-ghost.ant-btn-primary-disabled:hover > a:only-child"
             ".ant-btn-background-ghost.ant-btn-primary.disabled:hover > a:only-child"
             ".ant-btn-background-ghost.ant-btn-primary[disabled]:hover > a:only-child"
             ".ant-btn-background-ghost.ant-btn-primary-disabled:focus > a:only-child"
             ".ant-btn-background-ghost.ant-btn-primary.disabled:focus > a:only-child"
             ".ant-btn-background-ghost.ant-btn-primary[disabled]:focus > a:only-child"
             ".ant-btn-background-ghost.ant-btn-primary-disabled:active > a:only-child"
             ".ant-btn-background-ghost.ant-btn-primary.disabled:active > a:only-child"
             ".ant-btn-background-ghost.ant-btn-primary[disabled]:active > a:only-child"
             ".ant-btn-background-ghost.ant-btn-primary-disabled.active > a:only-child"
             ".ant-btn-background-ghost.ant-btn-primary.disabled.active > a:only-child"
             ".ant-btn-background-ghost.ant-btn-primary[disabled].active > a:only-child")
       (css:color "currentColor"))
     (rule '(".ant-btn-background-ghost.ant-btn-primary-disabled > a:only-child::after"
             ".ant-btn-background-ghost.ant-btn-primary.disabled > a:only-child::after"
             ".ant-btn-background-ghost.ant-btn-primary[disabled] > a:only-child::after"
             ".ant-btn-background-ghost.ant-btn-primary-disabled:hover > a:only-child::after"
             ".ant-btn-background-ghost.ant-btn-primary.disabled:hover > a:only-child::after"
             ".ant-btn-background-ghost.ant-btn-primary[disabled]:hover > a:only-child::after"
             ".ant-btn-background-ghost.ant-btn-primary-disabled:focus > a:only-child::after"
             ".ant-btn-background-ghost.ant-btn-primary.disabled:focus > a:only-child::after"
             ".ant-btn-background-ghost.ant-btn-primary[disabled]:focus > a:only-child::after"
             ".ant-btn-background-ghost.ant-btn-primary-disabled:active > a:only-child::after"
             ".ant-btn-background-ghost.ant-btn-primary.disabled:active > a:only-child::after"
             ".ant-btn-background-ghost.ant-btn-primary[disabled]:active > a:only-child::after"
             ".ant-btn-background-ghost.ant-btn-primary-disabled.active > a:only-child::after"
             ".ant-btn-background-ghost.ant-btn-primary.disabled.active > a:only-child::after"
             ".ant-btn-background-ghost.ant-btn-primary[disabled].active > a:only-child::after")
       (css:position "absolute")
       (css:top "0")
       (css:right "0")
       (css:bottom "0")
       (css:left "0")
       (css:background "transparent")
       (css:content "\"\""))
     (rule ".ant-btn-background-ghost.ant-btn-danger"
       (css:color "#ff4d4f")
       (css:background-color "transparent")
       (css:border-color "#ff4d4f")
       (css:text-shadow "none"))
     (rule ".ant-btn-background-ghost.ant-btn-danger > a:only-child"
       (css:color "currentColor"))
     (rule ".ant-btn-background-ghost.ant-btn-danger > a:only-child::after"
       (css:position "absolute")
       (css:top "0")
       (css:right "0")
       (css:bottom "0")
       (css:left "0")
       (css:background "transparent")
       (css:content "\"\""))
     (rule '(".ant-btn-background-ghost.ant-btn-danger:hover"
             ".ant-btn-background-ghost.ant-btn-danger:focus")
       (css:color "#ff7875")
       (css:background-color "transparent")
       (css:border-color "#ff7875"))
     (rule '(".ant-btn-background-ghost.ant-btn-danger:hover > a:only-child"
             ".ant-btn-background-ghost.ant-btn-danger:focus > a:only-child")
       (css:color "currentColor"))
     (rule '(".ant-btn-background-ghost.ant-btn-danger:hover > a:only-child::after"
             ".ant-btn-background-ghost.ant-btn-danger:focus > a:only-child::after")
       (css:position "absolute")
       (css:top "0")
       (css:right "0")
       (css:bottom "0")
       (css:left "0")
       (css:background "transparent")
       (css:content "\"\""))
     (rule '(".ant-btn-background-ghost.ant-btn-danger:active"
             ".ant-btn-background-ghost.ant-btn-danger.active")
       (css:color "#d9363e")
       (css:background-color "transparent")
       (css:border-color "#d9363e"))
     (rule '(".ant-btn-background-ghost.ant-btn-danger:active > a:only-child"
             ".ant-btn-background-ghost.ant-btn-danger.active > a:only-child")
       (css:color "currentColor"))
     (rule '(".ant-btn-background-ghost.ant-btn-danger:active > a:only-child::after"
             ".ant-btn-background-ghost.ant-btn-danger.active > a:only-child::after")
       (css:position "absolute")
       (css:top "0")
       (css:right "0")
       (css:bottom "0")
       (css:left "0")
       (css:background "transparent")
       (css:content "\"\""))
     (rule '(".ant-btn-background-ghost.ant-btn-danger-disabled"
             ".ant-btn-background-ghost.ant-btn-danger.disabled"
             ".ant-btn-background-ghost.ant-btn-danger[disabled]"
             ".ant-btn-background-ghost.ant-btn-danger-disabled:hover"
             ".ant-btn-background-ghost.ant-btn-danger.disabled:hover"
             ".ant-btn-background-ghost.ant-btn-danger[disabled]:hover"
             ".ant-btn-background-ghost.ant-btn-danger-disabled:focus"
             ".ant-btn-background-ghost.ant-btn-danger.disabled:focus"
             ".ant-btn-background-ghost.ant-btn-danger[disabled]:focus"
             ".ant-btn-background-ghost.ant-btn-danger-disabled:active"
             ".ant-btn-background-ghost.ant-btn-danger.disabled:active"
             ".ant-btn-background-ghost.ant-btn-danger[disabled]:active"
             ".ant-btn-background-ghost.ant-btn-danger-disabled.active"
             ".ant-btn-background-ghost.ant-btn-danger.disabled.active"
             ".ant-btn-background-ghost.ant-btn-danger[disabled].active")
       (css:color "rgba0, 0, 0, 1/4")
       (css:background-color "#f5f5f5")
       (css:border-color "#d9d9d9")
       (css:text-shadow "none")
       (property "-webkit-box-shadow" "none")
       (css:box-shadow "none"))
     (rule '(".ant-btn-background-ghost.ant-btn-danger-disabled > a:only-child"
             ".ant-btn-background-ghost.ant-btn-danger.disabled > a:only-child"
             ".ant-btn-background-ghost.ant-btn-danger[disabled] > a:only-child"
             ".ant-btn-background-ghost.ant-btn-danger-disabled:hover > a:only-child"
             ".ant-btn-background-ghost.ant-btn-danger.disabled:hover > a:only-child"
             ".ant-btn-background-ghost.ant-btn-danger[disabled]:hover > a:only-child"
             ".ant-btn-background-ghost.ant-btn-danger-disabled:focus > a:only-child"
             ".ant-btn-background-ghost.ant-btn-danger.disabled:focus > a:only-child"
             ".ant-btn-background-ghost.ant-btn-danger[disabled]:focus > a:only-child"
             ".ant-btn-background-ghost.ant-btn-danger-disabled:active > a:only-child"
             ".ant-btn-background-ghost.ant-btn-danger.disabled:active > a:only-child"
             ".ant-btn-background-ghost.ant-btn-danger[disabled]:active > a:only-child"
             ".ant-btn-background-ghost.ant-btn-danger-disabled.active > a:only-child"
             ".ant-btn-background-ghost.ant-btn-danger.disabled.active > a:only-child"
             ".ant-btn-background-ghost.ant-btn-danger[disabled].active > a:only-child")
       (css:color "currentColor"))
     (rule '(".ant-btn-background-ghost.ant-btn-danger-disabled > a:only-child::after"
             ".ant-btn-background-ghost.ant-btn-danger.disabled > a:only-child::after"
             ".ant-btn-background-ghost.ant-btn-danger[disabled] > a:only-child::after"
             ".ant-btn-background-ghost.ant-btn-danger-disabled:hover > a:only-child::after"
             ".ant-btn-background-ghost.ant-btn-danger.disabled:hover > a:only-child::after"
             ".ant-btn-background-ghost.ant-btn-danger[disabled]:hover > a:only-child::after"
             ".ant-btn-background-ghost.ant-btn-danger-disabled:focus > a:only-child::after"
             ".ant-btn-background-ghost.ant-btn-danger.disabled:focus > a:only-child::after"
             ".ant-btn-background-ghost.ant-btn-danger[disabled]:focus > a:only-child::after"
             ".ant-btn-background-ghost.ant-btn-danger-disabled:active > a:only-child::after"
             ".ant-btn-background-ghost.ant-btn-danger.disabled:active > a:only-child::after"
             ".ant-btn-background-ghost.ant-btn-danger[disabled]:active > a:only-child::after"
             ".ant-btn-background-ghost.ant-btn-danger-disabled.active > a:only-child::after"
             ".ant-btn-background-ghost.ant-btn-danger.disabled.active > a:only-child::after"
             ".ant-btn-background-ghost.ant-btn-danger[disabled].active > a:only-child::after")
       (css:position "absolute")
       (css:top "0")
       (css:right "0")
       (css:bottom "0")
       (css:left "0")
       (css:background "transparent")
       (css:content "\"\""))
     (rule ".ant-btn-background-ghost.ant-btn-dangerous"
       (css:color "#ff4d4f")
       (css:background-color "transparent")
       (css:border-color "#ff4d4f")
       (css:text-shadow "none"))
     (rule ".ant-btn-background-ghost.ant-btn-dangerous > a:only-child"
       (css:color "currentColor"))
     (rule ".ant-btn-background-ghost.ant-btn-dangerous > a:only-child::after"
       (css:position "absolute")
       (css:top "0")
       (css:right "0")
       (css:bottom "0")
       (css:left "0")
       (css:background "transparent")
       (css:content "\"\""))
     (rule '(".ant-btn-background-ghost.ant-btn-dangerous:hover"
             ".ant-btn-background-ghost.ant-btn-dangerous:focus")
       (css:color "#ff7875")
       (css:background-color "transparent")
       (css:border-color "#ff7875"))
     (rule '(".ant-btn-background-ghost.ant-btn-dangerous:hover > a:only-child"
             ".ant-btn-background-ghost.ant-btn-dangerous:focus > a:only-child")
       (css:color "currentColor"))
     (rule '(".ant-btn-background-ghost.ant-btn-dangerous:hover > a:only-child::after"
             ".ant-btn-background-ghost.ant-btn-dangerous:focus > a:only-child::after")
       (css:position "absolute")
       (css:top "0")
       (css:right "0")
       (css:bottom "0")
       (css:left "0")
       (css:background "transparent")
       (css:content "\"\""))
     (rule '(".ant-btn-background-ghost.ant-btn-dangerous:active"
             ".ant-btn-background-ghost.ant-btn-dangerous.active")
       (css:color "#d9363e")
       (css:background-color "transparent")
       (css:border-color "#d9363e"))
     (rule '(".ant-btn-background-ghost.ant-btn-dangerous:active > a:only-child"
             ".ant-btn-background-ghost.ant-btn-dangerous.active > a:only-child")
       (css:color "currentColor"))
     (rule '(".ant-btn-background-ghost.ant-btn-dangerous:active > a:only-child::after"
             ".ant-btn-background-ghost.ant-btn-dangerous.active > a:only-child::after")
       (css:position "absolute")
       (css:top "0")
       (css:right "0")
       (css:bottom "0")
       (css:left "0")
       (css:background "transparent")
       (css:content "\"\""))
     (rule '(".ant-btn-background-ghost.ant-btn-dangerous-disabled"
             ".ant-btn-background-ghost.ant-btn-dangerous.disabled"
             ".ant-btn-background-ghost.ant-btn-dangerous[disabled]"
             ".ant-btn-background-ghost.ant-btn-dangerous-disabled:hover"
             ".ant-btn-background-ghost.ant-btn-dangerous.disabled:hover"
             ".ant-btn-background-ghost.ant-btn-dangerous[disabled]:hover"
             ".ant-btn-background-ghost.ant-btn-dangerous-disabled:focus"
             ".ant-btn-background-ghost.ant-btn-dangerous.disabled:focus"
             ".ant-btn-background-ghost.ant-btn-dangerous[disabled]:focus"
             ".ant-btn-background-ghost.ant-btn-dangerous-disabled:active"
             ".ant-btn-background-ghost.ant-btn-dangerous.disabled:active"
             ".ant-btn-background-ghost.ant-btn-dangerous[disabled]:active"
             ".ant-btn-background-ghost.ant-btn-dangerous-disabled.active"
             ".ant-btn-background-ghost.ant-btn-dangerous.disabled.active"
             ".ant-btn-background-ghost.ant-btn-dangerous[disabled].active")
       (css:color "rgba0, 0, 0, 1/4")
       (css:background-color "#f5f5f5")
       (css:border-color "#d9d9d9")
       (css:text-shadow "none")
       (property "-webkit-box-shadow" "none")
       (css:box-shadow "none"))
     (rule '(".ant-btn-background-ghost.ant-btn-dangerous-disabled > a:only-child"
             ".ant-btn-background-ghost.ant-btn-dangerous.disabled > a:only-child"
             ".ant-btn-background-ghost.ant-btn-dangerous[disabled] > a:only-child"
             ".ant-btn-background-ghost.ant-btn-dangerous-disabled:hover > a:only-child"
             ".ant-btn-background-ghost.ant-btn-dangerous.disabled:hover > a:only-child"
             ".ant-btn-background-ghost.ant-btn-dangerous[disabled]:hover > a:only-child"
             ".ant-btn-background-ghost.ant-btn-dangerous-disabled:focus > a:only-child"
             ".ant-btn-background-ghost.ant-btn-dangerous.disabled:focus > a:only-child"
             ".ant-btn-background-ghost.ant-btn-dangerous[disabled]:focus > a:only-child"
             ".ant-btn-background-ghost.ant-btn-dangerous-disabled:active > a:only-child"
             ".ant-btn-background-ghost.ant-btn-dangerous.disabled:active > a:only-child"
             ".ant-btn-background-ghost.ant-btn-dangerous[disabled]:active > a:only-child"
             ".ant-btn-background-ghost.ant-btn-dangerous-disabled.active > a:only-child"
             ".ant-btn-background-ghost.ant-btn-dangerous.disabled.active > a:only-child"
             ".ant-btn-background-ghost.ant-btn-dangerous[disabled].active > a:only-child")
       (css:color "currentColor"))
     (rule '(".ant-btn-background-ghost.ant-btn-dangerous-disabled > a:only-child::after"
             ".ant-btn-background-ghost.ant-btn-dangerous.disabled > a:only-child::after"
             ".ant-btn-background-ghost.ant-btn-dangerous[disabled] > a:only-child::after"
             ".ant-btn-background-ghost.ant-btn-dangerous-disabled:hover > a:only-child::after"
             ".ant-btn-background-ghost.ant-btn-dangerous.disabled:hover > a:only-child::after"
             ".ant-btn-background-ghost.ant-btn-dangerous[disabled]:hover > a:only-child::after"
             ".ant-btn-background-ghost.ant-btn-dangerous-disabled:focus > a:only-child::after"
             ".ant-btn-background-ghost.ant-btn-dangerous.disabled:focus > a:only-child::after"
             ".ant-btn-background-ghost.ant-btn-dangerous[disabled]:focus > a:only-child::after"
             ".ant-btn-background-ghost.ant-btn-dangerous-disabled:active > a:only-child::after"
             ".ant-btn-background-ghost.ant-btn-dangerous.disabled:active > a:only-child::after"
             ".ant-btn-background-ghost.ant-btn-dangerous[disabled]:active > a:only-child::after"
             ".ant-btn-background-ghost.ant-btn-dangerous-disabled.active > a:only-child::after"
             ".ant-btn-background-ghost.ant-btn-dangerous.disabled.active > a:only-child::after"
             ".ant-btn-background-ghost.ant-btn-dangerous[disabled].active > a:only-child::after")
       (css:position "absolute")
       (css:top "0")
       (css:right "0")
       (css:bottom "0")
       (css:left "0")
       (css:background "transparent")
       (css:content "\"\""))
     (rule ".ant-btn-background-ghost.ant-btn-dangerous.ant-btn-link"
       (css:color "#ff4d4f")
       (css:background-color "transparent")
       (css:border-color "transparent")
       (css:text-shadow "none"))
     (rule ".ant-btn-background-ghost.ant-btn-dangerous.ant-btn-link > a:only-child"
       (css:color "currentColor"))
     (rule ".ant-btn-background-ghost.ant-btn-dangerous.ant-btn-link > a:only-child::after"
       (css:position "absolute")
       (css:top "0")
       (css:right "0")
       (css:bottom "0")
       (css:left "0")
       (css:background "transparent")
       (css:content "\"\""))
     (rule '(".ant-btn-background-ghost.ant-btn-dangerous.ant-btn-link:hover"
             ".ant-btn-background-ghost.ant-btn-dangerous.ant-btn-link:focus")
       (css:color "#ff7875")
       (css:background-color "transparent")
       (css:border-color "transparent"))
     (rule '(".ant-btn-background-ghost.ant-btn-dangerous.ant-btn-link:hover > a:only-child"
             ".ant-btn-background-ghost.ant-btn-dangerous.ant-btn-link:focus > a:only-child")
       (css:color "currentColor"))
     (rule '(".ant-btn-background-ghost.ant-btn-dangerous.ant-btn-link:hover > a:only-child::after"
             ".ant-btn-background-ghost.ant-btn-dangerous.ant-btn-link:focus > a:only-child::after")
       (css:position "absolute")
       (css:top "0")
       (css:right "0")
       (css:bottom "0")
       (css:left "0")
       (css:background "transparent")
       (css:content "\"\""))
     (rule '(".ant-btn-background-ghost.ant-btn-dangerous.ant-btn-link:active"
             ".ant-btn-background-ghost.ant-btn-dangerous.ant-btn-link.active")
       (css:color "#d9363e")
       (css:background-color "transparent")
       (css:border-color "transparent"))
     (rule '(".ant-btn-background-ghost.ant-btn-dangerous.ant-btn-link:active > a:only-child"
             ".ant-btn-background-ghost.ant-btn-dangerous.ant-btn-link.active > a:only-child")
       (css:color "currentColor"))
     (rule '(".ant-btn-background-ghost.ant-btn-dangerous.ant-btn-link:active > a:only-child::after"
             ".ant-btn-background-ghost.ant-btn-dangerous.ant-btn-link.active > a:only-child::after")
       (css:position "absolute")
       (css:top "0")
       (css:right "0")
       (css:bottom "0")
       (css:left "0")
       (css:background "transparent")
       (css:content "\"\""))
     (rule '(".ant-btn-background-ghost.ant-btn-dangerous.ant-btn-link-disabled"
             ".ant-btn-background-ghost.ant-btn-dangerous.ant-btn-link.disabled"
             ".ant-btn-background-ghost.ant-btn-dangerous.ant-btn-link[disabled]"
             ".ant-btn-background-ghost.ant-btn-dangerous.ant-btn-link-disabled:hover"
             ".ant-btn-background-ghost.ant-btn-dangerous.ant-btn-link.disabled:hover"
             ".ant-btn-background-ghost.ant-btn-dangerous.ant-btn-link[disabled]:hover"
             ".ant-btn-background-ghost.ant-btn-dangerous.ant-btn-link-disabled:focus"
             ".ant-btn-background-ghost.ant-btn-dangerous.ant-btn-link.disabled:focus"
             ".ant-btn-background-ghost.ant-btn-dangerous.ant-btn-link[disabled]:focus"
             ".ant-btn-background-ghost.ant-btn-dangerous.ant-btn-link-disabled:active"
             ".ant-btn-background-ghost.ant-btn-dangerous.ant-btn-link.disabled:active"
             ".ant-btn-background-ghost.ant-btn-dangerous.ant-btn-link[disabled]:active"
             ".ant-btn-background-ghost.ant-btn-dangerous.ant-btn-link-disabled.active"
             ".ant-btn-background-ghost.ant-btn-dangerous.ant-btn-link.disabled.active"
             ".ant-btn-background-ghost.ant-btn-dangerous.ant-btn-link[disabled].active")
       (css:color "rgba0, 0, 0, 1/4")
       (css:background-color "#f5f5f5")
       (css:border-color "#d9d9d9")
       (css:text-shadow "none")
       (property "-webkit-box-shadow" "none")
       (css:box-shadow "none"))
     (rule '(".ant-btn-background-ghost.ant-btn-dangerous.ant-btn-link-disabled > a:only-child"
             ".ant-btn-background-ghost.ant-btn-dangerous.ant-btn-link.disabled > a:only-child"
             ".ant-btn-background-ghost.ant-btn-dangerous.ant-btn-link[disabled] > a:only-child"
             ".ant-btn-background-ghost.ant-btn-dangerous.ant-btn-link-disabled:hover > a:only-child"
             ".ant-btn-background-ghost.ant-btn-dangerous.ant-btn-link.disabled:hover > a:only-child"
             ".ant-btn-background-ghost.ant-btn-dangerous.ant-btn-link[disabled]:hover > a:only-child"
             ".ant-btn-background-ghost.ant-btn-dangerous.ant-btn-link-disabled:focus > a:only-child"
             ".ant-btn-background-ghost.ant-btn-dangerous.ant-btn-link.disabled:focus > a:only-child"
             ".ant-btn-background-ghost.ant-btn-dangerous.ant-btn-link[disabled]:focus > a:only-child"
             ".ant-btn-background-ghost.ant-btn-dangerous.ant-btn-link-disabled:active > a:only-child"
             ".ant-btn-background-ghost.ant-btn-dangerous.ant-btn-link.disabled:active > a:only-child"
             ".ant-btn-background-ghost.ant-btn-dangerous.ant-btn-link[disabled]:active > a:only-child"
             ".ant-btn-background-ghost.ant-btn-dangerous.ant-btn-link-disabled.active > a:only-child"
             ".ant-btn-background-ghost.ant-btn-dangerous.ant-btn-link.disabled.active > a:only-child"
             ".ant-btn-background-ghost.ant-btn-dangerous.ant-btn-link[disabled].active > a:only-child")
       (css:color "currentColor"))
     (rule '(".ant-btn-background-ghost.ant-btn-dangerous.ant-btn-link-disabled > a:only-child::after"
             ".ant-btn-background-ghost.ant-btn-dangerous.ant-btn-link.disabled > a:only-child::after"
             ".ant-btn-background-ghost.ant-btn-dangerous.ant-btn-link[disabled] > a:only-child::after"
             ".ant-btn-background-ghost.ant-btn-dangerous.ant-btn-link-disabled:hover > a:only-child::after"
             ".ant-btn-background-ghost.ant-btn-dangerous.ant-btn-link.disabled:hover > a:only-child::after"
             ".ant-btn-background-ghost.ant-btn-dangerous.ant-btn-link[disabled]:hover > a:only-child::after"
             ".ant-btn-background-ghost.ant-btn-dangerous.ant-btn-link-disabled:focus > a:only-child::after"
             ".ant-btn-background-ghost.ant-btn-dangerous.ant-btn-link.disabled:focus > a:only-child::after"
             ".ant-btn-background-ghost.ant-btn-dangerous.ant-btn-link[disabled]:focus > a:only-child::after"
             ".ant-btn-background-ghost.ant-btn-dangerous.ant-btn-link-disabled:active > a:only-child::after"
             ".ant-btn-background-ghost.ant-btn-dangerous.ant-btn-link.disabled:active > a:only-child::after"
             ".ant-btn-background-ghost.ant-btn-dangerous.ant-btn-link[disabled]:active > a:only-child::after"
             ".ant-btn-background-ghost.ant-btn-dangerous.ant-btn-link-disabled.active > a:only-child::after"
             ".ant-btn-background-ghost.ant-btn-dangerous.ant-btn-link.disabled.active > a:only-child::after"
             ".ant-btn-background-ghost.ant-btn-dangerous.ant-btn-link[disabled].active > a:only-child::after")
       (css:position "absolute")
       (css:top "0")
       (css:right "0")
       (css:bottom "0")
       (css:left "0")
       (css:background "transparent")
       (css:content "\"\""))
     (rule ".ant-btn-background-ghost.ant-btn-link"
       (css:color "#1890ff")
       (css:background-color "transparent")
       (css:border-color "transparent")
       (css:text-shadow "none")
       (css:color "#fff"))
     (rule ".ant-btn-background-ghost.ant-btn-link > a:only-child"
       (css:color "currentColor"))
     (rule ".ant-btn-background-ghost.ant-btn-link > a:only-child::after"
       (css:position "absolute")
       (css:top "0")
       (css:right "0")
       (css:bottom "0")
       (css:left "0")
       (css:background "transparent")
       (css:content "\"\""))
     (rule '(".ant-btn-background-ghost.ant-btn-link:hover"
             ".ant-btn-background-ghost.ant-btn-link:focus")
       (css:color "#40a9ff")
       (css:background-color "transparent")
       (css:border-color "transparent"))
     (rule '(".ant-btn-background-ghost.ant-btn-link:hover > a:only-child"
             ".ant-btn-background-ghost.ant-btn-link:focus > a:only-child")
       (css:color "currentColor"))
     (rule '(".ant-btn-background-ghost.ant-btn-link:hover > a:only-child::after"
             ".ant-btn-background-ghost.ant-btn-link:focus > a:only-child::after")
       (css:position "absolute")
       (css:top "0")
       (css:right "0")
       (css:bottom "0")
       (css:left "0")
       (css:background "transparent")
       (css:content "\"\""))
     (rule '(".ant-btn-background-ghost.ant-btn-link:active"
             ".ant-btn-background-ghost.ant-btn-link.active")
       (css:color "#096dd9")
       (css:background-color "transparent")
       (css:border-color "transparent"))
     (rule '(".ant-btn-background-ghost.ant-btn-link:active > a:only-child"
             ".ant-btn-background-ghost.ant-btn-link.active > a:only-child")
       (css:color "currentColor"))
     (rule '(".ant-btn-background-ghost.ant-btn-link:active > a:only-child::after"
             ".ant-btn-background-ghost.ant-btn-link.active > a:only-child::after")
       (css:position "absolute")
       (css:top "0")
       (css:right "0")
       (css:bottom "0")
       (css:left "0")
       (css:background "transparent")
       (css:content "\"\""))
     (rule '(".ant-btn-background-ghost.ant-btn-link-disabled"
             ".ant-btn-background-ghost.ant-btn-link.disabled"
             ".ant-btn-background-ghost.ant-btn-link[disabled]"
             ".ant-btn-background-ghost.ant-btn-link-disabled:hover"
             ".ant-btn-background-ghost.ant-btn-link.disabled:hover"
             ".ant-btn-background-ghost.ant-btn-link[disabled]:hover"
             ".ant-btn-background-ghost.ant-btn-link-disabled:focus"
             ".ant-btn-background-ghost.ant-btn-link.disabled:focus"
             ".ant-btn-background-ghost.ant-btn-link[disabled]:focus"
             ".ant-btn-background-ghost.ant-btn-link-disabled:active"
             ".ant-btn-background-ghost.ant-btn-link.disabled:active"
             ".ant-btn-background-ghost.ant-btn-link[disabled]:active"
             ".ant-btn-background-ghost.ant-btn-link-disabled.active"
             ".ant-btn-background-ghost.ant-btn-link.disabled.active"
             ".ant-btn-background-ghost.ant-btn-link[disabled].active")
       (css:color "rgba0, 0, 0, 1/4")
       (css:background-color "#f5f5f5")
       (css:border-color "#d9d9d9")
       (css:text-shadow "none")
       (property "-webkit-box-shadow" "none")
       (css:box-shadow "none"))
     (rule '(".ant-btn-background-ghost.ant-btn-link-disabled > a:only-child"
             ".ant-btn-background-ghost.ant-btn-link.disabled > a:only-child"
             ".ant-btn-background-ghost.ant-btn-link[disabled] > a:only-child"
             ".ant-btn-background-ghost.ant-btn-link-disabled:hover > a:only-child"
             ".ant-btn-background-ghost.ant-btn-link.disabled:hover > a:only-child"
             ".ant-btn-background-ghost.ant-btn-link[disabled]:hover > a:only-child"
             ".ant-btn-background-ghost.ant-btn-link-disabled:focus > a:only-child"
             ".ant-btn-background-ghost.ant-btn-link.disabled:focus > a:only-child"
             ".ant-btn-background-ghost.ant-btn-link[disabled]:focus > a:only-child"
             ".ant-btn-background-ghost.ant-btn-link-disabled:active > a:only-child"
             ".ant-btn-background-ghost.ant-btn-link.disabled:active > a:only-child"
             ".ant-btn-background-ghost.ant-btn-link[disabled]:active > a:only-child"
             ".ant-btn-background-ghost.ant-btn-link-disabled.active > a:only-child"
             ".ant-btn-background-ghost.ant-btn-link.disabled.active > a:only-child"
             ".ant-btn-background-ghost.ant-btn-link[disabled].active > a:only-child")
       (css:color "currentColor"))
     (rule '(".ant-btn-background-ghost.ant-btn-link-disabled > a:only-child::after"
             ".ant-btn-background-ghost.ant-btn-link.disabled > a:only-child::after"
             ".ant-btn-background-ghost.ant-btn-link[disabled] > a:only-child::after"
             ".ant-btn-background-ghost.ant-btn-link-disabled:hover > a:only-child::after"
             ".ant-btn-background-ghost.ant-btn-link.disabled:hover > a:only-child::after"
             ".ant-btn-background-ghost.ant-btn-link[disabled]:hover > a:only-child::after"
             ".ant-btn-background-ghost.ant-btn-link-disabled:focus > a:only-child::after"
             ".ant-btn-background-ghost.ant-btn-link.disabled:focus > a:only-child::after"
             ".ant-btn-background-ghost.ant-btn-link[disabled]:focus > a:only-child::after"
             ".ant-btn-background-ghost.ant-btn-link-disabled:active > a:only-child::after"
             ".ant-btn-background-ghost.ant-btn-link.disabled:active > a:only-child::after"
             ".ant-btn-background-ghost.ant-btn-link[disabled]:active > a:only-child::after"
             ".ant-btn-background-ghost.ant-btn-link-disabled.active > a:only-child::after"
             ".ant-btn-background-ghost.ant-btn-link.disabled.active > a:only-child::after"
             ".ant-btn-background-ghost.ant-btn-link[disabled].active > a:only-child::after")
       (css:position "absolute")
       (css:top "0")
       (css:right "0")
       (css:bottom "0")
       (css:left "0")
       (css:background "transparent")
       (css:content "\"\""))
     (rule ".ant-btn-two-chinese-chars::first-letter"
       (css:letter-spacing "17/50em"))
     (rule ".ant-btn-two-chinese-chars > *:not.anticon"
       (css:margin-right "-17/50em")
       (css:letter-spacing "17/50em"))
     (rule ".ant-btn-block"
       (css:width "100%"))
     (rule ".ant-btn:empty"
       (css:display "inline-block")
       (css:width "0")
       (css:visibility "hidden")
       (css:content "\"\\\\a0\""))
     (rule "a.ant-btn"
       (css:padding-top "1/10px")
       (css:line-height "30px"))
     (rule "a.ant-btn-lg"
       (css:line-height "38px"))
     (rule "a.ant-btn-sm"
       (css:line-height "22px"))
     (rule ".ant-btn-rtl"
       (property "direction" "rtl"))
     (rule '(".ant-btn-group-rtl.ant-btn-group .ant-btn-primary:last-child:not:first-child"
             ".ant-btn-group-rtl.ant-btn-group .ant-btn-primary + .ant-btn-primary")
       (css:border-right-color "#40a9ff")
       (css:border-left-color "#d9d9d9"))
     (rule '(".ant-btn-group-rtl.ant-btn-group .ant-btn-primary:last-child:not:first-child[disabled]"
             ".ant-btn-group-rtl.ant-btn-group .ant-btn-primary + .ant-btn-primary[disabled]")
       (css:border-right-color "#d9d9d9")
       (css:border-left-color "#40a9ff"))
     (rule ".ant-btn-rtl.ant-btn.ant-btn-loading:not.ant-btn-circle:not.ant-btn-circle-outline:not.ant-btn-icon-only"
       (css:padding-right "29px")
       (css:padding-left "15px"))
     (rule ".ant-btn-rtl.ant-btn.ant-btn-loading:not.ant-btn-circle:not.ant-btn-circle-outline:not.ant-btn-icon-only .anticon:not:last-child"
       (css:margin-right "-14px")
       (css:margin-left "0"))
     (rule ".ant-btn-rtl.ant-btn-sm.ant-btn-loading:not.ant-btn-circle:not.ant-btn-circle-outline:not.ant-btn-icon-only"
       (css:padding-right "24px")
       (css:padding-left "7px"))
     (rule ".ant-btn-rtl.ant-btn-sm.ant-btn-loading:not.ant-btn-circle:not.ant-btn-circle-outline:not.ant-btn-icon-only .anticon"
       (css:margin-right "-17px")
       (css:margin-left "0"))
     (rule '(".ant-btn-rtl.ant-btn > .anticon + span" ".ant-btn-rtl.ant-btn > span + .anticon")
       (css:margin-right "8px")
       (css:margin-left "0")))))
