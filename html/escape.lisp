(in-package :wt.html)

;; 参考 https://www.owasp.org/index.php/XSS_%28Cross_Site_Scripting%29_Prevention_Cheat_Sheet#XSS_Prevention_Rules

(defun escape-element-content (value &optional stream)
  (escape value (lambda (c)
                  (case c
                    (#\& "&amp;")
                    (#\< "&lt;")
                    (#\> "&gt;")
                    (#\" "&quot;")
                    (#\/ "&#x2F;")))
          :stream stream))

(defun escape-raw-text (value &optional stream)
  (escape value (lambda (c)
                  (case c
                    ;; (#\< "&lt;") ;; 这个会导致 JavaScript 中的 < 出问题
                    ;; (#\> "&gt;");; 这个会导致 JavaScript 中的 > 出问题
                    ;; (#\/ "&#x2F;") ;; 这个会导致 JavaScript 中的 Comment 出问题
                    ))
          :stream stream))

(defun escape-escapable-raw-text (value &optional stream)
  (escape value (lambda (c)
                  (case c
                    (#\& "&amp;")
                    (#\< "&lt;")
                    (#\> "&gt;")
                    (#\/ "&#x2F;")))
          :stream stream))

(defun escape-common-double-quoted-attribute-value (value &optional stream)
  (escape value (lambda (c)
                  (case c
                    (#\& "&amp;")
                    (#\" "&quot;")))
          :stream stream))
