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
     (with-slots (children disabled ghost href html-type icon
                           loading shape size target type block danger)
         button
       (let ((root (html:button :class "ant-btn" children)))
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
       (property "line-height" "1.5715")
       (property "position" "relative")
       (property "display" "inline-block")
       (property "font-weight" "400")
       (property "white-space" "nowrap")
       (property "text-align" "center")
       (property "background-image" "none")
       (property "border" "1px solid transparent")
       (property "-webkit-box-shadow" "0 2px 0 rgba(0, 0, 0, 0.015)")
       (property "box-shadow" "0 2px 0 rgba(0, 0, 0, 0.015)")
       (property "cursor" "pointer")
       (property "-webkit-transition" "all 0.3s cubic-bezier(0.645, 0.045, 0.355, 1)")
       (property "transition" "all 0.3s cubic-bezier(0.645, 0.045, 0.355, 1)")
       (property "-webkit-user-select" "none")
       (property "-moz-user-select" "none")
       (property "-ms-user-select" "none")
       (property "user-select" "none")
       (property "-ms-touch-action" "manipulation")
       (property "touch-action" "manipulation")
       (property "height" "32px")
       (property "padding" "4px 15px")
       (property "font-size" "14px")
       (property "border-radius" "2px")
       (property "color" "rgba(0, 0, 0, 0.65)")
       (property "background-color" "#fff")
       (property "border-color" "#d9d9d9"))
     (rule ".ant-btn > .anticon"
       (property "line-height" "1"))
     (rule '(".ant-btn" ".ant-btn:active" ".ant-btn:focus")
       (property "outline" "0"))
     (rule ".ant-btn:not([disabled]):hover"
       (property "text-decoration" "none"))
     (rule ".ant-btn:not([disabled]):active"
       (property "outline" "0")
       (property "-webkit-box-shadow" "none")
       (property "box-shadow" "none"))
     (rule '(".ant-btn.disabled" ".ant-btn[disabled]")
       (property "cursor" "not-allowed"))
     (rule '(".ant-btn.disabled > *" ".ant-btn[disabled] > *")
       (property "pointer-events" "none"))
     (rule ".ant-btn-lg"
       (property "height" "40px")
       (property "padding" "6.4px 15px")
       (property "font-size" "16px")
       (property "border-radius" "2px"))
     (rule ".ant-btn-sm"
       (property "height" "24px")
       (property "padding" "0px 7px")
       (property "font-size" "14px")
       (property "border-radius" "2px")
       (property "line-height" "22px"))
     (rule ".ant-btn > a:only-child"
       (property "color" "currentColor"))
     (rule ".ant-btn > a:only-child::after"
       (property "position" "absolute")
       (property "top" "0")
       (property "right" "0")
       (property "bottom" "0")
       (property "left" "0")
       (property "background" "transparent")
       (property "content" "\"\""))
     (rule '(".ant-btn:hover" ".ant-btn:focus")
       (property "color" "#40a9ff")
       (property "background-color" "#fff")
       (property "border-color" "#40a9ff"))
     (rule '(".ant-btn:hover > a:only-child" ".ant-btn:focus > a:only-child")
       (property "color" "currentColor"))
     (rule '(".ant-btn:hover > a:only-child::after" ".ant-btn:focus > a:only-child::after")
       (property "position" "absolute")
       (property "top" "0")
       (property "right" "0")
       (property "bottom" "0")
       (property "left" "0")
       (property "background" "transparent")
       (property "content" "\"\""))
     (rule '(".ant-btn:active" ".ant-btn.active")
       (property "color" "#096dd9")
       (property "background-color" "#fff")
       (property "border-color" "#096dd9"))
     (rule '(".ant-btn:active > a:only-child" ".ant-btn.active > a:only-child")
       (property "color" "currentColor"))
     (rule '(".ant-btn:active > a:only-child::after" ".ant-btn.active > a:only-child::after")
       (property "position" "absolute")
       (property "top" "0")
       (property "right" "0")
       (property "bottom" "0")
       (property "left" "0")
       (property "background" "transparent")
       (property "content" "\"\""))
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
       (property "color" "rgba(0, 0, 0, 0.25)")
       (property "background-color" "#f5f5f5")
       (property "border-color" "#d9d9d9")
       (property "text-shadow" "none")
       (property "-webkit-box-shadow" "none")
       (property "box-shadow" "none"))
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
       (property "color" "currentColor"))
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
       (property "position" "absolute")
       (property "top" "0")
       (property "right" "0")
       (property "bottom" "0")
       (property "left" "0")
       (property "background" "transparent")
       (property "content" "\"\""))
     (rule '(".ant-btn:hover" ".ant-btn:focus" ".ant-btn:active" ".ant-btn.active")
       (property "text-decoration" "none")
       (property "background" "#fff"))
     (rule '(".ant-btn > i" ".ant-btn > span")
       (property "display" "inline-block")
       (property "-webkit-transition" "margin-left 0.3s cubic-bezier(0.645, 0.045, 0.355, 1)")
       (property "transition" "margin-left 0.3s cubic-bezier(0.645, 0.045, 0.355, 1)")
       (property "pointer-events" "none"))
     (rule ".ant-btn-primary"
       (property "color" "#fff")
       (property "background-color" "#1890ff")
       (property "border-color" "#1890ff")
       (property "text-shadow" "0 -1px 0 rgba(0, 0, 0, 0.12)")
       (property "-webkit-box-shadow" "0 2px 0 rgba(0, 0, 0, 0.045)")
       (property "box-shadow" "0 2px 0 rgba(0, 0, 0, 0.045)"))
     (rule ".ant-btn-primary > a:only-child"
       (property "color" "currentColor"))
     (rule ".ant-btn-primary > a:only-child::after"
       (property "position" "absolute")
       (property "top" "0")
       (property "right" "0")
       (property "bottom" "0")
       (property "left" "0")
       (property "background" "transparent")
       (property "content" "\"\""))
     (rule '(".ant-btn-primary:hover" ".ant-btn-primary:focus")
       (property "color" "#fff")
       (property "background-color" "#40a9ff")
       (property "border-color" "#40a9ff"))
     (rule '(".ant-btn-primary:hover > a:only-child" ".ant-btn-primary:focus > a:only-child")
       (property "color" "currentColor"))
     (rule '(".ant-btn-primary:hover > a:only-child::after"
             ".ant-btn-primary:focus > a:only-child::after")
       (property "position" "absolute")
       (property "top" "0")
       (property "right" "0")
       (property "bottom" "0")
       (property "left" "0")
       (property "background" "transparent")
       (property "content" "\"\""))
     (rule '(".ant-btn-primary:active" ".ant-btn-primary.active")
       (property "color" "#fff")
       (property "background-color" "#096dd9")
       (property "border-color" "#096dd9"))
     (rule '(".ant-btn-primary:active > a:only-child" ".ant-btn-primary.active > a:only-child")
       (property "color" "currentColor"))
     (rule '(".ant-btn-primary:active > a:only-child::after"
             ".ant-btn-primary.active > a:only-child::after")
       (property "position" "absolute")
       (property "top" "0")
       (property "right" "0")
       (property "bottom" "0")
       (property "left" "0")
       (property "background" "transparent")
       (property "content" "\"\""))
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
       (property "color" "rgba(0, 0, 0, 0.25)")
       (property "background-color" "#f5f5f5")
       (property "border-color" "#d9d9d9")
       (property "text-shadow" "none")
       (property "-webkit-box-shadow" "none")
       (property "box-shadow" "none"))
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
       (property "color" "currentColor"))
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
       (property "position" "absolute")
       (property "top" "0")
       (property "right" "0")
       (property "bottom" "0")
       (property "left" "0")
       (property "background" "transparent")
       (property "content" "\"\""))
     (rule ".ant-btn-group .ant-btn-primary:not(:first-child):not(:last-child)"
       (property "border-right-color" "#40a9ff")
       (property "border-left-color" "#40a9ff"))
     (rule ".ant-btn-group .ant-btn-primary:not(:first-child):not(:last-child):disabled"
       (property "border-color" "#d9d9d9"))
     (rule ".ant-btn-group .ant-btn-primary:first-child:not(:last-child)"
       (property "border-right-color" "#40a9ff"))
     (rule ".ant-btn-group .ant-btn-primary:first-child:not(:last-child)[disabled]"
       (property "border-right-color" "#d9d9d9"))
     (rule '(".ant-btn-group .ant-btn-primary:last-child:not(:first-child)"
             ".ant-btn-group .ant-btn-primary + .ant-btn-primary")
       (property "border-left-color" "#40a9ff"))
     (rule '(".ant-btn-group .ant-btn-primary:last-child:not(:first-child)[disabled]"
             ".ant-btn-group .ant-btn-primary + .ant-btn-primary[disabled]")
       (property "border-left-color" "#d9d9d9"))
     (rule ".ant-btn-ghost"
       (property "color" "rgba(0, 0, 0, 0.65)")
       (property "background-color" "transparent")
       (property "border-color" "#d9d9d9"))
     (rule ".ant-btn-ghost > a:only-child"
       (property "color" "currentColor"))
     (rule ".ant-btn-ghost > a:only-child::after"
       (property "position" "absolute")
       (property "top" "0")
       (property "right" "0")
       (property "bottom" "0")
       (property "left" "0")
       (property "background" "transparent")
       (property "content" "\"\""))
     (rule '(".ant-btn-ghost:hover" ".ant-btn-ghost:focus")
       (property "color" "#40a9ff")
       (property "background-color" "transparent")
       (property "border-color" "#40a9ff"))
     (rule '(".ant-btn-ghost:hover > a:only-child" ".ant-btn-ghost:focus > a:only-child")
       (property "color" "currentColor"))
     (rule '(".ant-btn-ghost:hover > a:only-child::after"
             ".ant-btn-ghost:focus > a:only-child::after")
       (property "position" "absolute")
       (property "top" "0")
       (property "right" "0")
       (property "bottom" "0")
       (property "left" "0")
       (property "background" "transparent")
       (property "content" "\"\""))
     (rule '(".ant-btn-ghost:active" ".ant-btn-ghost.active")
       (property "color" "#096dd9")
       (property "background-color" "transparent")
       (property "border-color" "#096dd9"))
     (rule '(".ant-btn-ghost:active > a:only-child" ".ant-btn-ghost.active > a:only-child")
       (property "color" "currentColor"))
     (rule '(".ant-btn-ghost:active > a:only-child::after"
             ".ant-btn-ghost.active > a:only-child::after")
       (property "position" "absolute")
       (property "top" "0")
       (property "right" "0")
       (property "bottom" "0")
       (property "left" "0")
       (property "background" "transparent")
       (property "content" "\"\""))
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
       (property "color" "rgba(0, 0, 0, 0.25)")
       (property "background-color" "#f5f5f5")
       (property "border-color" "#d9d9d9")
       (property "text-shadow" "none")
       (property "-webkit-box-shadow" "none")
       (property "box-shadow" "none"))
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
       (property "color" "currentColor"))
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
       (property "position" "absolute")
       (property "top" "0")
       (property "right" "0")
       (property "bottom" "0")
       (property "left" "0")
       (property "background" "transparent")
       (property "content" "\"\""))
     (rule ".ant-btn-dashed"
       (property "color" "rgba(0, 0, 0, 0.65)")
       (property "background-color" "#fff")
       (property "border-color" "#d9d9d9")
       (property "border-style" "dashed"))
     (rule ".ant-btn-dashed > a:only-child"
       (property "color" "currentColor"))
     (rule ".ant-btn-dashed > a:only-child::after"
       (property "position" "absolute")
       (property "top" "0")
       (property "right" "0")
       (property "bottom" "0")
       (property "left" "0")
       (property "background" "transparent")
       (property "content" "\"\""))
     (rule '(".ant-btn-dashed:hover" ".ant-btn-dashed:focus")
       (property "color" "#40a9ff")
       (property "background-color" "#fff")
       (property "border-color" "#40a9ff"))
     (rule '(".ant-btn-dashed:hover > a:only-child" ".ant-btn-dashed:focus > a:only-child")
       (property "color" "currentColor"))
     (rule '(".ant-btn-dashed:hover > a:only-child::after"
             ".ant-btn-dashed:focus > a:only-child::after")
       (property "position" "absolute")
       (property "top" "0")
       (property "right" "0")
       (property "bottom" "0")
       (property "left" "0")
       (property "background" "transparent")
       (property "content" "\"\""))
     (rule '(".ant-btn-dashed:active" ".ant-btn-dashed.active")
       (property "color" "#096dd9")
       (property "background-color" "#fff")
       (property "border-color" "#096dd9"))
     (rule '(".ant-btn-dashed:active > a:only-child" ".ant-btn-dashed.active > a:only-child")
       (property "color" "currentColor"))
     (rule '(".ant-btn-dashed:active > a:only-child::after"
             ".ant-btn-dashed.active > a:only-child::after")
       (property "position" "absolute")
       (property "top" "0")
       (property "right" "0")
       (property "bottom" "0")
       (property "left" "0")
       (property "background" "transparent")
       (property "content" "\"\""))
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
       (property "color" "rgba(0, 0, 0, 0.25)")
       (property "background-color" "#f5f5f5")
       (property "border-color" "#d9d9d9")
       (property "text-shadow" "none")
       (property "-webkit-box-shadow" "none")
       (property "box-shadow" "none"))
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
       (property "color" "currentColor"))
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
       (property "position" "absolute")
       (property "top" "0")
       (property "right" "0")
       (property "bottom" "0")
       (property "left" "0")
       (property "background" "transparent")
       (property "content" "\"\""))
     (rule ".ant-btn-danger"
       (property "color" "#fff")
       (property "background-color" "#ff4d4f")
       (property "border-color" "#ff4d4f")
       (property "text-shadow" "0 -1px 0 rgba(0, 0, 0, 0.12)")
       (property "-webkit-box-shadow" "0 2px 0 rgba(0, 0, 0, 0.045)")
       (property "box-shadow" "0 2px 0 rgba(0, 0, 0, 0.045)"))
     (rule ".ant-btn-danger > a:only-child"
       (property "color" "currentColor"))
     (rule ".ant-btn-danger > a:only-child::after"
       (property "position" "absolute")
       (property "top" "0")
       (property "right" "0")
       (property "bottom" "0")
       (property "left" "0")
       (property "background" "transparent")
       (property "content" "\"\""))
     (rule '(".ant-btn-danger:hover" ".ant-btn-danger:focus")
       (property "color" "#fff")
       (property "background-color" "#ff7875")
       (property "border-color" "#ff7875"))
     (rule '(".ant-btn-danger:hover > a:only-child" ".ant-btn-danger:focus > a:only-child")
       (property "color" "currentColor"))
     (rule '(".ant-btn-danger:hover > a:only-child::after"
             ".ant-btn-danger:focus > a:only-child::after")
       (property "position" "absolute")
       (property "top" "0")
       (property "right" "0")
       (property "bottom" "0")
       (property "left" "0")
       (property "background" "transparent")
       (property "content" "\"\""))
     (rule '(".ant-btn-danger:active" ".ant-btn-danger.active")
       (property "color" "#fff")
       (property "background-color" "#d9363e")
       (property "border-color" "#d9363e"))
     (rule '(".ant-btn-danger:active > a:only-child" ".ant-btn-danger.active > a:only-child")
       (property "color" "currentColor"))
     (rule '(".ant-btn-danger:active > a:only-child::after"
             ".ant-btn-danger.active > a:only-child::after")
       (property "position" "absolute")
       (property "top" "0")
       (property "right" "0")
       (property "bottom" "0")
       (property "left" "0")
       (property "background" "transparent")
       (property "content" "\"\""))
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
       (property "color" "rgba(0, 0, 0, 0.25)")
       (property "background-color" "#f5f5f5")
       (property "border-color" "#d9d9d9")
       (property "text-shadow" "none")
       (property "-webkit-box-shadow" "none")
       (property "box-shadow" "none"))
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
       (property "color" "currentColor"))
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
       (property "position" "absolute")
       (property "top" "0")
       (property "right" "0")
       (property "bottom" "0")
       (property "left" "0")
       (property "background" "transparent")
       (property "content" "\"\""))
     (rule ".ant-btn-link"
       (property "color" "#1890ff")
       (property "background-color" "transparent")
       (property "border-color" "transparent")
       (property "-webkit-box-shadow" "none")
       (property "box-shadow" "none"))
     (rule ".ant-btn-link > a:only-child"
       (property "color" "currentColor"))
     (rule ".ant-btn-link > a:only-child::after"
       (property "position" "absolute")
       (property "top" "0")
       (property "right" "0")
       (property "bottom" "0")
       (property "left" "0")
       (property "background" "transparent")
       (property "content" "\"\""))
     (rule '(".ant-btn-link:hover" ".ant-btn-link:focus")
       (property "color" "#40a9ff")
       (property "background-color" "transparent")
       (property "border-color" "#40a9ff"))
     (rule '(".ant-btn-link:hover > a:only-child" ".ant-btn-link:focus > a:only-child")
       (property "color" "currentColor"))
     (rule '(".ant-btn-link:hover > a:only-child::after"
             ".ant-btn-link:focus > a:only-child::after")
       (property "position" "absolute")
       (property "top" "0")
       (property "right" "0")
       (property "bottom" "0")
       (property "left" "0")
       (property "background" "transparent")
       (property "content" "\"\""))
     (rule '(".ant-btn-link:active" ".ant-btn-link.active")
       (property "color" "#096dd9")
       (property "background-color" "transparent")
       (property "border-color" "#096dd9"))
     (rule '(".ant-btn-link:active > a:only-child" ".ant-btn-link.active > a:only-child")
       (property "color" "currentColor"))
     (rule '(".ant-btn-link:active > a:only-child::after"
             ".ant-btn-link.active > a:only-child::after")
       (property "position" "absolute")
       (property "top" "0")
       (property "right" "0")
       (property "bottom" "0")
       (property "left" "0")
       (property "background" "transparent")
       (property "content" "\"\""))
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
       (property "color" "rgba(0, 0, 0, 0.25)")
       (property "background-color" "#f5f5f5")
       (property "border-color" "#d9d9d9")
       (property "text-shadow" "none")
       (property "-webkit-box-shadow" "none")
       (property "box-shadow" "none"))
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
       (property "color" "currentColor"))
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
       (property "position" "absolute")
       (property "top" "0")
       (property "right" "0")
       (property "bottom" "0")
       (property "left" "0")
       (property "background" "transparent")
       (property "content" "\"\""))
     (rule '(".ant-btn-link:hover" ".ant-btn-link:focus" ".ant-btn-link:active")
       (property "border-color" "transparent"))
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
       (property "color" "rgba(0, 0, 0, 0.25)")
       (property "background-color" "transparent")
       (property "border-color" "transparent")
       (property "text-shadow" "none")
       (property "-webkit-box-shadow" "none")
       (property "box-shadow" "none"))
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
       (property "color" "currentColor"))
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
       (property "position" "absolute")
       (property "top" "0")
       (property "right" "0")
       (property "bottom" "0")
       (property "left" "0")
       (property "background" "transparent")
       (property "content" "\"\""))
     (rule ".ant-btn-dangerous"
       (property "color" "#ff4d4f")
       (property "background-color" "#fff")
       (property "border-color" "#ff4d4f"))
     (rule ".ant-btn-dangerous > a:only-child"
       (property "color" "currentColor"))
     (rule ".ant-btn-dangerous > a:only-child::after"
       (property "position" "absolute")
       (property "top" "0")
       (property "right" "0")
       (property "bottom" "0")
       (property "left" "0")
       (property "background" "transparent")
       (property "content" "\"\""))
     (rule '(".ant-btn-dangerous:hover" ".ant-btn-dangerous:focus")
       (property "color" "#ff7875")
       (property "background-color" "#fff")
       (property "border-color" "#ff7875"))
     (rule '(".ant-btn-dangerous:hover > a:only-child" ".ant-btn-dangerous:focus > a:only-child")
       (property "color" "currentColor"))
     (rule '(".ant-btn-dangerous:hover > a:only-child::after"
             ".ant-btn-dangerous:focus > a:only-child::after")
       (property "position" "absolute")
       (property "top" "0")
       (property "right" "0")
       (property "bottom" "0")
       (property "left" "0")
       (property "background" "transparent")
       (property "content" "\"\""))
     (rule '(".ant-btn-dangerous:active" ".ant-btn-dangerous.active")
       (property "color" "#d9363e")
       (property "background-color" "#fff")
       (property "border-color" "#d9363e"))
     (rule '(".ant-btn-dangerous:active > a:only-child"
             ".ant-btn-dangerous.active > a:only-child")
       (property "color" "currentColor"))
     (rule '(".ant-btn-dangerous:active > a:only-child::after"
             ".ant-btn-dangerous.active > a:only-child::after")
       (property "position" "absolute")
       (property "top" "0")
       (property "right" "0")
       (property "bottom" "0")
       (property "left" "0")
       (property "background" "transparent")
       (property "content" "\"\""))
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
       (property "color" "rgba(0, 0, 0, 0.25)")
       (property "background-color" "#f5f5f5")
       (property "border-color" "#d9d9d9")
       (property "text-shadow" "none")
       (property "-webkit-box-shadow" "none")
       (property "box-shadow" "none"))
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
       (property "color" "currentColor"))
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
       (property "position" "absolute")
       (property "top" "0")
       (property "right" "0")
       (property "bottom" "0")
       (property "left" "0")
       (property "background" "transparent")
       (property "content" "\"\""))
     (rule ".ant-btn-dangerous.ant-btn-primary"
       (property "color" "#fff")
       (property "background-color" "#ff4d4f")
       (property "border-color" "#ff4d4f")
       (property "text-shadow" "0 -1px 0 rgba(0, 0, 0, 0.12)")
       (property "-webkit-box-shadow" "0 2px 0 rgba(0, 0, 0, 0.045)")
       (property "box-shadow" "0 2px 0 rgba(0, 0, 0, 0.045)"))
     (rule ".ant-btn-dangerous.ant-btn-primary > a:only-child"
       (property "color" "currentColor"))
     (rule ".ant-btn-dangerous.ant-btn-primary > a:only-child::after"
       (property "position" "absolute")
       (property "top" "0")
       (property "right" "0")
       (property "bottom" "0")
       (property "left" "0")
       (property "background" "transparent")
       (property "content" "\"\""))
     (rule '(".ant-btn-dangerous.ant-btn-primary:hover"
             ".ant-btn-dangerous.ant-btn-primary:focus")
       (property "color" "#fff")
       (property "background-color" "#ff7875")
       (property "border-color" "#ff7875"))
     (rule '(".ant-btn-dangerous.ant-btn-primary:hover > a:only-child"
             ".ant-btn-dangerous.ant-btn-primary:focus > a:only-child")
       (property "color" "currentColor"))
     (rule '(".ant-btn-dangerous.ant-btn-primary:hover > a:only-child::after"
             ".ant-btn-dangerous.ant-btn-primary:focus > a:only-child::after")
       (property "position" "absolute")
       (property "top" "0")
       (property "right" "0")
       (property "bottom" "0")
       (property "left" "0")
       (property "background" "transparent")
       (property "content" "\"\""))
     (rule '(".ant-btn-dangerous.ant-btn-primary:active"
             ".ant-btn-dangerous.ant-btn-primary.active")
       (property "color" "#fff")
       (property "background-color" "#d9363e")
       (property "border-color" "#d9363e"))
     (rule '(".ant-btn-dangerous.ant-btn-primary:active > a:only-child"
             ".ant-btn-dangerous.ant-btn-primary.active > a:only-child")
       (property "color" "currentColor"))
     (rule '(".ant-btn-dangerous.ant-btn-primary:active > a:only-child::after"
             ".ant-btn-dangerous.ant-btn-primary.active > a:only-child::after")
       (property "position" "absolute")
       (property "top" "0")
       (property "right" "0")
       (property "bottom" "0")
       (property "left" "0")
       (property "background" "transparent")
       (property "content" "\"\""))
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
       (property "color" "rgba(0, 0, 0, 0.25)")
       (property "background-color" "#f5f5f5")
       (property "border-color" "#d9d9d9")
       (property "text-shadow" "none")
       (property "-webkit-box-shadow" "none")
       (property "box-shadow" "none"))
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
       (property "color" "currentColor"))
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
       (property "position" "absolute")
       (property "top" "0")
       (property "right" "0")
       (property "bottom" "0")
       (property "left" "0")
       (property "background" "transparent")
       (property "content" "\"\""))
     (rule ".ant-btn-dangerous.ant-btn-link"
       (property "color" "#ff4d4f")
       (property "background-color" "transparent")
       (property "border-color" "transparent")
       (property "-webkit-box-shadow" "none")
       (property "box-shadow" "none"))
     (rule ".ant-btn-dangerous.ant-btn-link > a:only-child"
       (property "color" "currentColor"))
     (rule ".ant-btn-dangerous.ant-btn-link > a:only-child::after"
       (property "position" "absolute")
       (property "top" "0")
       (property "right" "0")
       (property "bottom" "0")
       (property "left" "0")
       (property "background" "transparent")
       (property "content" "\"\""))
     (rule '(".ant-btn-dangerous.ant-btn-link:hover" ".ant-btn-dangerous.ant-btn-link:focus")
       (property "color" "#40a9ff")
       (property "background-color" "transparent")
       (property "border-color" "#40a9ff"))
     (rule '(".ant-btn-dangerous.ant-btn-link:hover > a:only-child"
             ".ant-btn-dangerous.ant-btn-link:focus > a:only-child")
       (property "color" "currentColor"))
     (rule '(".ant-btn-dangerous.ant-btn-link:hover > a:only-child::after"
             ".ant-btn-dangerous.ant-btn-link:focus > a:only-child::after")
       (property "position" "absolute")
       (property "top" "0")
       (property "right" "0")
       (property "bottom" "0")
       (property "left" "0")
       (property "background" "transparent")
       (property "content" "\"\""))
     (rule '(".ant-btn-dangerous.ant-btn-link:active" ".ant-btn-dangerous.ant-btn-link.active")
       (property "color" "#096dd9")
       (property "background-color" "transparent")
       (property "border-color" "#096dd9"))
     (rule '(".ant-btn-dangerous.ant-btn-link:active > a:only-child"
             ".ant-btn-dangerous.ant-btn-link.active > a:only-child")
       (property "color" "currentColor"))
     (rule '(".ant-btn-dangerous.ant-btn-link:active > a:only-child::after"
             ".ant-btn-dangerous.ant-btn-link.active > a:only-child::after")
       (property "position" "absolute")
       (property "top" "0")
       (property "right" "0")
       (property "bottom" "0")
       (property "left" "0")
       (property "background" "transparent")
       (property "content" "\"\""))
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
       (property "color" "rgba(0, 0, 0, 0.25)")
       (property "background-color" "#f5f5f5")
       (property "border-color" "#d9d9d9")
       (property "text-shadow" "none")
       (property "-webkit-box-shadow" "none")
       (property "box-shadow" "none"))
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
       (property "color" "currentColor"))
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
       (property "position" "absolute")
       (property "top" "0")
       (property "right" "0")
       (property "bottom" "0")
       (property "left" "0")
       (property "background" "transparent")
       (property "content" "\"\""))
     (rule '(".ant-btn-dangerous.ant-btn-link:hover" ".ant-btn-dangerous.ant-btn-link:focus")
       (property "color" "#ff7875")
       (property "background-color" "transparent")
       (property "border-color" "transparent"))
     (rule '(".ant-btn-dangerous.ant-btn-link:hover > a:only-child"
             ".ant-btn-dangerous.ant-btn-link:focus > a:only-child")
       (property "color" "currentColor"))
     (rule '(".ant-btn-dangerous.ant-btn-link:hover > a:only-child::after"
             ".ant-btn-dangerous.ant-btn-link:focus > a:only-child::after")
       (property "position" "absolute")
       (property "top" "0")
       (property "right" "0")
       (property "bottom" "0")
       (property "left" "0")
       (property "background" "transparent")
       (property "content" "\"\""))
     (rule ".ant-btn-dangerous.ant-btn-link:active"
       (property "color" "#d9363e")
       (property "background-color" "transparent")
       (property "border-color" "transparent"))
     (rule ".ant-btn-dangerous.ant-btn-link:active > a:only-child"
       (property "color" "currentColor"))
     (rule ".ant-btn-dangerous.ant-btn-link:active > a:only-child::after"
       (property "position" "absolute")
       (property "top" "0")
       (property "right" "0")
       (property "bottom" "0")
       (property "left" "0")
       (property "background" "transparent")
       (property "content" "\"\""))
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
       (property "color" "rgba(0, 0, 0, 0.25)")
       (property "background-color" "transparent")
       (property "border-color" "transparent")
       (property "text-shadow" "none")
       (property "-webkit-box-shadow" "none")
       (property "box-shadow" "none"))
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
       (property "color" "currentColor"))
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
       (property "position" "absolute")
       (property "top" "0")
       (property "right" "0")
       (property "bottom" "0")
       (property "left" "0")
       (property "background" "transparent")
       (property "content" "\"\""))
     (rule ".ant-btn-icon-only"
       (property "width" "32px")
       (property "height" "32px")
       (property "padding" "2.4px 0")
       (property "font-size" "16px")
       (property "border-radius" "2px"))
     (rule ".ant-btn-icon-only > *"
       (property "font-size" "16px"))
     (rule ".ant-btn-icon-only.ant-btn-lg"
       (property "width" "40px")
       (property "height" "40px")
       (property "padding" "4.9px 0")
       (property "font-size" "18px")
       (property "border-radius" "2px"))
     (rule ".ant-btn-icon-only.ant-btn-lg > *"
       (property "font-size" "18px"))
     (rule ".ant-btn-icon-only.ant-btn-sm"
       (property "width" "24px")
       (property "height" "24px")
       (property "padding" "0px 0")
       (property "font-size" "14px")
       (property "border-radius" "2px"))
     (rule ".ant-btn-icon-only.ant-btn-sm > *"
       (property "font-size" "14px"))
     (rule ".ant-btn-icon-only > i"
       (property "vertical-align" "middle"))
     (rule ".ant-btn-round"
       (property "height" "32px")
       (property "padding" "4px 16px")
       (property "font-size" "14px")
       (property "border-radius" "32px"))
     (rule ".ant-btn-round.ant-btn-lg"
       (property "height" "40px")
       (property "padding" "6.4px 20px")
       (property "font-size" "16px")
       (property "border-radius" "40px"))
     (rule ".ant-btn-round.ant-btn-sm"
       (property "height" "24px")
       (property "padding" "0px 12px")
       (property "font-size" "14px")
       (property "border-radius" "24px"))
     (rule ".ant-btn-round.ant-btn-icon-only"
       (property "width" "auto"))
     (rule '(".ant-btn-circle" ".ant-btn-circle-outline")
       (property "min-width" "32px")
       (property "padding-right" "0")
       (property "padding-left" "0")
       (property "text-align" "center")
       (property "border-radius" "50%"))
     (rule '(".ant-btn-circle.ant-btn-lg" ".ant-btn-circle-outline.ant-btn-lg")
       (property "min-width" "40px")
       (property "border-radius" "50%"))
     (rule '(".ant-btn-circle.ant-btn-sm" ".ant-btn-circle-outline.ant-btn-sm")
       (property "min-width" "24px")
       (property "border-radius" "50%"))
     (rule ".ant-btn::before"
       (property "position" "absolute")
       (property "top" "-1px")
       (property "right" "-1px")
       (property "bottom" "-1px")
       (property "left" "-1px")
       (property "z-index" "1")
       (property "display" "none")
       (property "background" "#fff")
       (property "border-radius" "inherit")
       (property "opacity" "0.35")
       (property "-webkit-transition" "opacity 0.2s")
       (property "transition" "opacity 0.2s")
       (property "content" "\"\"")
       (property "pointer-events" "none"))
     (rule ".ant-btn .anticon"
       (property "-webkit-transition" "margin-left 0.3s cubic-bezier(0.645, 0.045, 0.355, 1)")
       (property "transition" "margin-left 0.3s cubic-bezier(0.645, 0.045, 0.355, 1)"))
     (rule '(".ant-btn .anticon.anticon-plus > svg" ".ant-btn .anticon.anticon-minus > svg")
       (property "shape-rendering" "optimizeSpeed"))
     (rule ".ant-btn.ant-btn-loading"
       (property "position" "relative"))
     (rule ".ant-btn.ant-btn-loading:not([disabled])"
       (property "pointer-events" "none"))
     (rule ".ant-btn.ant-btn-loading::before"
       (property "display" "block"))
     (rule ".ant-btn.ant-btn-loading:not(.ant-btn-circle):not(.ant-btn-circle-outline):not(.ant-btn-icon-only)"
       (property "padding-left" "29px"))
     (rule ".ant-btn.ant-btn-loading:not(.ant-btn-circle):not(.ant-btn-circle-outline):not(.ant-btn-icon-only) .anticon:not(:last-child)"
       (property "margin-left" "-14px"))
     (rule ".ant-btn-sm.ant-btn-loading:not(.ant-btn-circle):not(.ant-btn-circle-outline):not(.ant-btn-icon-only)"
       (property "padding-left" "24px"))
     (rule ".ant-btn-sm.ant-btn-loading:not(.ant-btn-circle):not(.ant-btn-circle-outline):not(.ant-btn-icon-only) .anticon"
       (property "margin-left" "-17px"))
     (rule ".ant-btn-group"
       (property "position" "relative")
       (property "display" "inline-block"))
     (rule '(".ant-btn-group > .ant-btn" ".ant-btn-group > span > .ant-btn")
       (property "position" "relative"))
     (rule '(".ant-btn-group > .ant-btn:hover"
             ".ant-btn-group > span > .ant-btn:hover"
             ".ant-btn-group > .ant-btn:focus"
             ".ant-btn-group > span > .ant-btn:focus"
             ".ant-btn-group > .ant-btn:active"
             ".ant-btn-group > span > .ant-btn:active"
             ".ant-btn-group > .ant-btn.active"
             ".ant-btn-group > span > .ant-btn.active")
       (property "z-index" "2"))
     (rule '(".ant-btn-group > .ant-btn:disabled" ".ant-btn-group > span > .ant-btn:disabled")
       (property "z-index" "0"))
     (rule ".ant-btn-group > .ant-btn-icon-only"
       (property "font-size" "14px"))
     (rule '(".ant-btn-group-lg > .ant-btn" ".ant-btn-group-lg > span > .ant-btn")
       (property "height" "40px")
       (property "padding" "6.4px 15px")
       (property "font-size" "16px")
       (property "border-radius" "0"))
     (rule ".ant-btn-group-lg > .ant-btn.ant-btn-icon-only"
       (property "width" "40px")
       (property "height" "40px")
       (property "padding-right" "0")
       (property "padding-left" "0"))
     (rule '(".ant-btn-group-sm > .ant-btn" ".ant-btn-group-sm > span > .ant-btn")
       (property "height" "24px")
       (property "padding" "0px 7px")
       (property "font-size" "14px")
       (property "border-radius" "0"))
     (rule '(".ant-btn-group-sm > .ant-btn > .anticon"
             ".ant-btn-group-sm > span > .ant-btn > .anticon")
       (property "font-size" "14px"))
     (rule ".ant-btn-group-sm > .ant-btn.ant-btn-icon-only"
       (property "width" "24px")
       (property "height" "24px")
       (property "padding-right" "0")
       (property "padding-left" "0"))
     (rule '(".ant-btn-group .ant-btn + .ant-btn"
             ".ant-btn + .ant-btn-group"
             ".ant-btn-group span + .ant-btn"
             ".ant-btn-group .ant-btn + span"
             ".ant-btn-group > span + span"
             ".ant-btn-group + .ant-btn"
             ".ant-btn-group + .ant-btn-group")
       (property "margin-left" "-1px"))
     (rule ".ant-btn-group .ant-btn-primary + .ant-btn:not(.ant-btn-primary):not([disabled])"
       (property "border-left-color" "transparent"))
     (rule ".ant-btn-group .ant-btn"
       (property "border-radius" "0"))
     (rule '(".ant-btn-group > .ant-btn:first-child"
             ".ant-btn-group > span:first-child > .ant-btn")
       (property "margin-left" "0"))
     (rule ".ant-btn-group > .ant-btn:only-child"
       (property "border-radius" "2px"))
     (rule ".ant-btn-group > span:only-child > .ant-btn"
       (property "border-radius" "2px"))
     (rule '(".ant-btn-group > .ant-btn:first-child:not(:last-child)"
             ".ant-btn-group > span:first-child:not(:last-child) > .ant-btn")
       (property "border-top-left-radius" "2px")
       (property "border-bottom-left-radius" "2px"))
     (rule '(".ant-btn-group > .ant-btn:last-child:not(:first-child)"
             ".ant-btn-group > span:last-child:not(:first-child) > .ant-btn")
       (property "border-top-right-radius" "2px")
       (property "border-bottom-right-radius" "2px"))
     (rule ".ant-btn-group-sm > .ant-btn:only-child"
       (property "border-radius" "2px"))
     (rule ".ant-btn-group-sm > span:only-child > .ant-btn"
       (property "border-radius" "2px"))
     (rule '(".ant-btn-group-sm > .ant-btn:first-child:not(:last-child)"
             ".ant-btn-group-sm > span:first-child:not(:last-child) > .ant-btn")
       (property "border-top-left-radius" "2px")
       (property "border-bottom-left-radius" "2px"))
     (rule '(".ant-btn-group-sm > .ant-btn:last-child:not(:first-child)"
             ".ant-btn-group-sm > span:last-child:not(:first-child) > .ant-btn")
       (property "border-top-right-radius" "2px")
       (property "border-bottom-right-radius" "2px"))
     (rule ".ant-btn-group > .ant-btn-group"
       (property "float" "left"))
     (rule ".ant-btn-group > .ant-btn-group:not(:first-child):not(:last-child) > .ant-btn"
       (property "border-radius" "0"))
     (rule ".ant-btn-group > .ant-btn-group:first-child:not(:last-child) > .ant-btn:last-child"
       (property "padding-right" "8px")
       (property "border-top-right-radius" "0")
       (property "border-bottom-right-radius" "0"))
     (rule ".ant-btn-group > .ant-btn-group:last-child:not(:first-child) > .ant-btn:first-child"
       (property "padding-left" "8px")
       (property "border-top-left-radius" "0")
       (property "border-bottom-left-radius" "0"))
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
       (property "margin-right" "-1px")
       (property "margin-left" "auto"))
     (rule ".ant-btn-group.ant-btn-group-rtl"
       (property "direction" "rtl"))
     (rule '(".ant-btn-group-rtl.ant-btn-group > .ant-btn:first-child:not(:last-child)"
             ".ant-btn-group-rtl.ant-btn-group > span:first-child:not(:last-child) > .ant-btn")
       (property "border-top-left-radius" "0")
       (property "border-top-right-radius" "2px")
       (property "border-bottom-right-radius" "2px")
       (property "border-bottom-left-radius" "0"))
     (rule '(".ant-btn-group-rtl.ant-btn-group > .ant-btn:last-child:not(:first-child)"
             ".ant-btn-group-rtl.ant-btn-group > span:last-child:not(:first-child) > .ant-btn")
       (property "border-top-left-radius" "2px")
       (property "border-top-right-radius" "0")
       (property "border-bottom-right-radius" "0")
       (property "border-bottom-left-radius" "2px"))
     (rule '(".ant-btn-group-rtl.ant-btn-group-sm > .ant-btn:first-child:not(:last-child)"
             ".ant-btn-group-rtl.ant-btn-group-sm > span:first-child:not(:last-child) > .ant-btn")
       (property "border-top-left-radius" "0")
       (property "border-top-right-radius" "2px")
       (property "border-bottom-right-radius" "2px")
       (property "border-bottom-left-radius" "0"))
     (rule '(".ant-btn-group-rtl.ant-btn-group-sm > .ant-btn:last-child:not(:first-child)"
             ".ant-btn-group-rtl.ant-btn-group-sm > span:last-child:not(:first-child) > .ant-btn")
       (property "border-top-left-radius" "2px")
       (property "border-top-right-radius" "0")
       (property "border-bottom-right-radius" "0")
       (property "border-bottom-left-radius" "2px"))
     (rule '(".ant-btn:focus > span" ".ant-btn:active > span")
       (property "position" "relative"))
     (rule '(".ant-btn > .anticon + span" ".ant-btn > span + .anticon")
       (property "margin-left" "8px"))
     (rule ".ant-btn-background-ghost"
       (property "color" "#fff")
       (property "background" "transparent")
       (property "border-color" "#fff"))
     (rule ".ant-btn-background-ghost.ant-btn-primary"
       (property "color" "#1890ff")
       (property "background-color" "transparent")
       (property "border-color" "#1890ff")
       (property "text-shadow" "none"))
     (rule ".ant-btn-background-ghost.ant-btn-primary > a:only-child"
       (property "color" "currentColor"))
     (rule ".ant-btn-background-ghost.ant-btn-primary > a:only-child::after"
       (property "position" "absolute")
       (property "top" "0")
       (property "right" "0")
       (property "bottom" "0")
       (property "left" "0")
       (property "background" "transparent")
       (property "content" "\"\""))
     (rule '(".ant-btn-background-ghost.ant-btn-primary:hover"
             ".ant-btn-background-ghost.ant-btn-primary:focus")
       (property "color" "#40a9ff")
       (property "background-color" "transparent")
       (property "border-color" "#40a9ff"))
     (rule '(".ant-btn-background-ghost.ant-btn-primary:hover > a:only-child"
             ".ant-btn-background-ghost.ant-btn-primary:focus > a:only-child")
       (property "color" "currentColor"))
     (rule '(".ant-btn-background-ghost.ant-btn-primary:hover > a:only-child::after"
             ".ant-btn-background-ghost.ant-btn-primary:focus > a:only-child::after")
       (property "position" "absolute")
       (property "top" "0")
       (property "right" "0")
       (property "bottom" "0")
       (property "left" "0")
       (property "background" "transparent")
       (property "content" "\"\""))
     (rule '(".ant-btn-background-ghost.ant-btn-primary:active"
             ".ant-btn-background-ghost.ant-btn-primary.active")
       (property "color" "#096dd9")
       (property "background-color" "transparent")
       (property "border-color" "#096dd9"))
     (rule '(".ant-btn-background-ghost.ant-btn-primary:active > a:only-child"
             ".ant-btn-background-ghost.ant-btn-primary.active > a:only-child")
       (property "color" "currentColor"))
     (rule '(".ant-btn-background-ghost.ant-btn-primary:active > a:only-child::after"
             ".ant-btn-background-ghost.ant-btn-primary.active > a:only-child::after")
       (property "position" "absolute")
       (property "top" "0")
       (property "right" "0")
       (property "bottom" "0")
       (property "left" "0")
       (property "background" "transparent")
       (property "content" "\"\""))
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
       (property "color" "rgba(0, 0, 0, 0.25)")
       (property "background-color" "#f5f5f5")
       (property "border-color" "#d9d9d9")
       (property "text-shadow" "none")
       (property "-webkit-box-shadow" "none")
       (property "box-shadow" "none"))
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
       (property "color" "currentColor"))
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
       (property "position" "absolute")
       (property "top" "0")
       (property "right" "0")
       (property "bottom" "0")
       (property "left" "0")
       (property "background" "transparent")
       (property "content" "\"\""))
     (rule ".ant-btn-background-ghost.ant-btn-danger"
       (property "color" "#ff4d4f")
       (property "background-color" "transparent")
       (property "border-color" "#ff4d4f")
       (property "text-shadow" "none"))
     (rule ".ant-btn-background-ghost.ant-btn-danger > a:only-child"
       (property "color" "currentColor"))
     (rule ".ant-btn-background-ghost.ant-btn-danger > a:only-child::after"
       (property "position" "absolute")
       (property "top" "0")
       (property "right" "0")
       (property "bottom" "0")
       (property "left" "0")
       (property "background" "transparent")
       (property "content" "\"\""))
     (rule '(".ant-btn-background-ghost.ant-btn-danger:hover"
             ".ant-btn-background-ghost.ant-btn-danger:focus")
       (property "color" "#ff7875")
       (property "background-color" "transparent")
       (property "border-color" "#ff7875"))
     (rule '(".ant-btn-background-ghost.ant-btn-danger:hover > a:only-child"
             ".ant-btn-background-ghost.ant-btn-danger:focus > a:only-child")
       (property "color" "currentColor"))
     (rule '(".ant-btn-background-ghost.ant-btn-danger:hover > a:only-child::after"
             ".ant-btn-background-ghost.ant-btn-danger:focus > a:only-child::after")
       (property "position" "absolute")
       (property "top" "0")
       (property "right" "0")
       (property "bottom" "0")
       (property "left" "0")
       (property "background" "transparent")
       (property "content" "\"\""))
     (rule '(".ant-btn-background-ghost.ant-btn-danger:active"
             ".ant-btn-background-ghost.ant-btn-danger.active")
       (property "color" "#d9363e")
       (property "background-color" "transparent")
       (property "border-color" "#d9363e"))
     (rule '(".ant-btn-background-ghost.ant-btn-danger:active > a:only-child"
             ".ant-btn-background-ghost.ant-btn-danger.active > a:only-child")
       (property "color" "currentColor"))
     (rule '(".ant-btn-background-ghost.ant-btn-danger:active > a:only-child::after"
             ".ant-btn-background-ghost.ant-btn-danger.active > a:only-child::after")
       (property "position" "absolute")
       (property "top" "0")
       (property "right" "0")
       (property "bottom" "0")
       (property "left" "0")
       (property "background" "transparent")
       (property "content" "\"\""))
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
       (property "color" "rgba(0, 0, 0, 0.25)")
       (property "background-color" "#f5f5f5")
       (property "border-color" "#d9d9d9")
       (property "text-shadow" "none")
       (property "-webkit-box-shadow" "none")
       (property "box-shadow" "none"))
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
       (property "color" "currentColor"))
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
       (property "position" "absolute")
       (property "top" "0")
       (property "right" "0")
       (property "bottom" "0")
       (property "left" "0")
       (property "background" "transparent")
       (property "content" "\"\""))
     (rule ".ant-btn-background-ghost.ant-btn-dangerous"
       (property "color" "#ff4d4f")
       (property "background-color" "transparent")
       (property "border-color" "#ff4d4f")
       (property "text-shadow" "none"))
     (rule ".ant-btn-background-ghost.ant-btn-dangerous > a:only-child"
       (property "color" "currentColor"))
     (rule ".ant-btn-background-ghost.ant-btn-dangerous > a:only-child::after"
       (property "position" "absolute")
       (property "top" "0")
       (property "right" "0")
       (property "bottom" "0")
       (property "left" "0")
       (property "background" "transparent")
       (property "content" "\"\""))
     (rule '(".ant-btn-background-ghost.ant-btn-dangerous:hover"
             ".ant-btn-background-ghost.ant-btn-dangerous:focus")
       (property "color" "#ff7875")
       (property "background-color" "transparent")
       (property "border-color" "#ff7875"))
     (rule '(".ant-btn-background-ghost.ant-btn-dangerous:hover > a:only-child"
             ".ant-btn-background-ghost.ant-btn-dangerous:focus > a:only-child")
       (property "color" "currentColor"))
     (rule '(".ant-btn-background-ghost.ant-btn-dangerous:hover > a:only-child::after"
             ".ant-btn-background-ghost.ant-btn-dangerous:focus > a:only-child::after")
       (property "position" "absolute")
       (property "top" "0")
       (property "right" "0")
       (property "bottom" "0")
       (property "left" "0")
       (property "background" "transparent")
       (property "content" "\"\""))
     (rule '(".ant-btn-background-ghost.ant-btn-dangerous:active"
             ".ant-btn-background-ghost.ant-btn-dangerous.active")
       (property "color" "#d9363e")
       (property "background-color" "transparent")
       (property "border-color" "#d9363e"))
     (rule '(".ant-btn-background-ghost.ant-btn-dangerous:active > a:only-child"
             ".ant-btn-background-ghost.ant-btn-dangerous.active > a:only-child")
       (property "color" "currentColor"))
     (rule '(".ant-btn-background-ghost.ant-btn-dangerous:active > a:only-child::after"
             ".ant-btn-background-ghost.ant-btn-dangerous.active > a:only-child::after")
       (property "position" "absolute")
       (property "top" "0")
       (property "right" "0")
       (property "bottom" "0")
       (property "left" "0")
       (property "background" "transparent")
       (property "content" "\"\""))
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
       (property "color" "rgba(0, 0, 0, 0.25)")
       (property "background-color" "#f5f5f5")
       (property "border-color" "#d9d9d9")
       (property "text-shadow" "none")
       (property "-webkit-box-shadow" "none")
       (property "box-shadow" "none"))
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
       (property "color" "currentColor"))
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
       (property "position" "absolute")
       (property "top" "0")
       (property "right" "0")
       (property "bottom" "0")
       (property "left" "0")
       (property "background" "transparent")
       (property "content" "\"\""))
     (rule ".ant-btn-background-ghost.ant-btn-dangerous.ant-btn-link"
       (property "color" "#ff4d4f")
       (property "background-color" "transparent")
       (property "border-color" "transparent")
       (property "text-shadow" "none"))
     (rule ".ant-btn-background-ghost.ant-btn-dangerous.ant-btn-link > a:only-child"
       (property "color" "currentColor"))
     (rule ".ant-btn-background-ghost.ant-btn-dangerous.ant-btn-link > a:only-child::after"
       (property "position" "absolute")
       (property "top" "0")
       (property "right" "0")
       (property "bottom" "0")
       (property "left" "0")
       (property "background" "transparent")
       (property "content" "\"\""))
     (rule '(".ant-btn-background-ghost.ant-btn-dangerous.ant-btn-link:hover"
             ".ant-btn-background-ghost.ant-btn-dangerous.ant-btn-link:focus")
       (property "color" "#ff7875")
       (property "background-color" "transparent")
       (property "border-color" "transparent"))
     (rule '(".ant-btn-background-ghost.ant-btn-dangerous.ant-btn-link:hover > a:only-child"
             ".ant-btn-background-ghost.ant-btn-dangerous.ant-btn-link:focus > a:only-child")
       (property "color" "currentColor"))
     (rule '(".ant-btn-background-ghost.ant-btn-dangerous.ant-btn-link:hover > a:only-child::after"
             ".ant-btn-background-ghost.ant-btn-dangerous.ant-btn-link:focus > a:only-child::after")
       (property "position" "absolute")
       (property "top" "0")
       (property "right" "0")
       (property "bottom" "0")
       (property "left" "0")
       (property "background" "transparent")
       (property "content" "\"\""))
     (rule '(".ant-btn-background-ghost.ant-btn-dangerous.ant-btn-link:active"
             ".ant-btn-background-ghost.ant-btn-dangerous.ant-btn-link.active")
       (property "color" "#d9363e")
       (property "background-color" "transparent")
       (property "border-color" "transparent"))
     (rule '(".ant-btn-background-ghost.ant-btn-dangerous.ant-btn-link:active > a:only-child"
             ".ant-btn-background-ghost.ant-btn-dangerous.ant-btn-link.active > a:only-child")
       (property "color" "currentColor"))
     (rule '(".ant-btn-background-ghost.ant-btn-dangerous.ant-btn-link:active > a:only-child::after"
             ".ant-btn-background-ghost.ant-btn-dangerous.ant-btn-link.active > a:only-child::after")
       (property "position" "absolute")
       (property "top" "0")
       (property "right" "0")
       (property "bottom" "0")
       (property "left" "0")
       (property "background" "transparent")
       (property "content" "\"\""))
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
       (property "color" "rgba(0, 0, 0, 0.25)")
       (property "background-color" "#f5f5f5")
       (property "border-color" "#d9d9d9")
       (property "text-shadow" "none")
       (property "-webkit-box-shadow" "none")
       (property "box-shadow" "none"))
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
       (property "color" "currentColor"))
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
       (property "position" "absolute")
       (property "top" "0")
       (property "right" "0")
       (property "bottom" "0")
       (property "left" "0")
       (property "background" "transparent")
       (property "content" "\"\""))
     (rule ".ant-btn-background-ghost.ant-btn-link"
       (property "color" "#1890ff")
       (property "background-color" "transparent")
       (property "border-color" "transparent")
       (property "text-shadow" "none")
       (property "color" "#fff"))
     (rule ".ant-btn-background-ghost.ant-btn-link > a:only-child"
       (property "color" "currentColor"))
     (rule ".ant-btn-background-ghost.ant-btn-link > a:only-child::after"
       (property "position" "absolute")
       (property "top" "0")
       (property "right" "0")
       (property "bottom" "0")
       (property "left" "0")
       (property "background" "transparent")
       (property "content" "\"\""))
     (rule '(".ant-btn-background-ghost.ant-btn-link:hover"
             ".ant-btn-background-ghost.ant-btn-link:focus")
       (property "color" "#40a9ff")
       (property "background-color" "transparent")
       (property "border-color" "transparent"))
     (rule '(".ant-btn-background-ghost.ant-btn-link:hover > a:only-child"
             ".ant-btn-background-ghost.ant-btn-link:focus > a:only-child")
       (property "color" "currentColor"))
     (rule '(".ant-btn-background-ghost.ant-btn-link:hover > a:only-child::after"
             ".ant-btn-background-ghost.ant-btn-link:focus > a:only-child::after")
       (property "position" "absolute")
       (property "top" "0")
       (property "right" "0")
       (property "bottom" "0")
       (property "left" "0")
       (property "background" "transparent")
       (property "content" "\"\""))
     (rule '(".ant-btn-background-ghost.ant-btn-link:active"
             ".ant-btn-background-ghost.ant-btn-link.active")
       (property "color" "#096dd9")
       (property "background-color" "transparent")
       (property "border-color" "transparent"))
     (rule '(".ant-btn-background-ghost.ant-btn-link:active > a:only-child"
             ".ant-btn-background-ghost.ant-btn-link.active > a:only-child")
       (property "color" "currentColor"))
     (rule '(".ant-btn-background-ghost.ant-btn-link:active > a:only-child::after"
             ".ant-btn-background-ghost.ant-btn-link.active > a:only-child::after")
       (property "position" "absolute")
       (property "top" "0")
       (property "right" "0")
       (property "bottom" "0")
       (property "left" "0")
       (property "background" "transparent")
       (property "content" "\"\""))
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
       (property "color" "rgba(0, 0, 0, 0.25)")
       (property "background-color" "#f5f5f5")
       (property "border-color" "#d9d9d9")
       (property "text-shadow" "none")
       (property "-webkit-box-shadow" "none")
       (property "box-shadow" "none"))
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
       (property "color" "currentColor"))
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
       (property "position" "absolute")
       (property "top" "0")
       (property "right" "0")
       (property "bottom" "0")
       (property "left" "0")
       (property "background" "transparent")
       (property "content" "\"\""))
     (rule ".ant-btn-two-chinese-chars::first-letter"
       (property "letter-spacing" "0.34em"))
     (rule ".ant-btn-two-chinese-chars > *:not(.anticon)"
       (property "margin-right" "-0.34em")
       (property "letter-spacing" "0.34em"))
     (rule ".ant-btn-block"
       (property "width" "100%"))
     (rule ".ant-btn:empty"
       (property "display" "inline-block")
       (property "width" "0")
       (property "visibility" "hidden")
       (property "content" "\"\\\\a0\""))
     (rule "a.ant-btn"
       (property "padding-top" "0.1px")
       (property "line-height" "30px"))
     (rule "a.ant-btn-lg"
       (property "line-height" "38px"))
     (rule "a.ant-btn-sm"
       (property "line-height" "22px"))
     (rule ".ant-btn-rtl"
       (property "direction" "rtl"))
     (rule '(".ant-btn-group-rtl.ant-btn-group .ant-btn-primary:last-child:not(:first-child)"
             ".ant-btn-group-rtl.ant-btn-group .ant-btn-primary + .ant-btn-primary")
       (property "border-right-color" "#40a9ff")
       (property "border-left-color" "#d9d9d9"))
     (rule '(".ant-btn-group-rtl.ant-btn-group .ant-btn-primary:last-child:not(:first-child)[disabled]"
             ".ant-btn-group-rtl.ant-btn-group .ant-btn-primary + .ant-btn-primary[disabled]")
       (property "border-right-color" "#d9d9d9")
       (property "border-left-color" "#40a9ff"))
     (rule ".ant-btn-rtl.ant-btn.ant-btn-loading:not(.ant-btn-circle):not(.ant-btn-circle-outline):not(.ant-btn-icon-only)"
       (property "padding-right" "29px")
       (property "padding-left" "15px"))
     (rule ".ant-btn-rtl.ant-btn.ant-btn-loading:not(.ant-btn-circle):not(.ant-btn-circle-outline):not(.ant-btn-icon-only) .anticon:not(:last-child)"
       (property "margin-right" "-14px")
       (property "margin-left" "0"))
     (rule ".ant-btn-rtl.ant-btn-sm.ant-btn-loading:not(.ant-btn-circle):not(.ant-btn-circle-outline):not(.ant-btn-icon-only)"
       (property "padding-right" "24px")
       (property "padding-left" "7px"))
     (rule ".ant-btn-rtl.ant-btn-sm.ant-btn-loading:not(.ant-btn-circle):not(.ant-btn-circle-outline):not(.ant-btn-icon-only) .anticon"
       (property "margin-right" "-17px")
       (property "margin-left" "0"))
     (rule '(".ant-btn-rtl.ant-btn > .anticon + span" ".ant-btn-rtl.ant-btn > span + .anticon")
       (property "margin-right" "8px")
       (property "margin-left" "0")))))
