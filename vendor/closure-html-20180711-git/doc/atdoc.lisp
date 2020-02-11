(asdf:operate 'asdf:load-op :atdoc)
(asdf:operate 'asdf:load-op :closure-html)

(atdoc:generate-html-documentation
 '(:chtml :hax)
 (merge-pathnames
  "doc/atdoc/"
  (asdf:component-relative-pathname (asdf:find-system :closure-html)))
 :heading "Closure HTML")
