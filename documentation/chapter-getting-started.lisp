(in-package :documentation)

(define-variable article-get-lisp-web-toolkit
    (article
     :title "Get Lisp Web Toolkit"
     (p
      "To get the source code of " (a :href "https://github.com/xh4/web-toolkit" "Lisp Web Toolkit") ", use the following Git command:")
     (pre
      "git clone https://github.com/xh4/web-toolkit.git")
     (p
      "To make the systems discoverable by " (a :href "https://common-lisp.net/project/asdf/" "ASDF") ", put the source code into directories described in " (a :href "https://common-lisp.net/project/asdf/asdf.html#Configuring-ASDF-to-find-your-systems" "Configuring ASDF to find your systems") ".")
     (p (a :href "https://github.com/xh4/web-toolkit" "Lisp Web Toolkit") " currently runs on " (a :href "http://sbcl.org/" "SBCL") ", " (a :href "https://ccl.clozure.com/" "Clozure CL") " and " (a :href "http://www.lispworks.com/" "LispWorks") " on Linux and macOS, " (a :href "http://www.lispworks.com/" "LispWorks") " on Windows.")))

(define-variable article-load-a-system
    (article
     :title "Load a System"
     (p
      "Evaluate the following code to load a system:")
     (pre
      "(asdf:load-system :wt.uri)
(asdf:load-system :wt.html)
(asdf:load-system :wt.json)
(asdf:load-system :wt.css)
(asdf:load-system :wt.http)
(asdf:load-system :wt.websocket)")
     (p
      "Or load all systems by evaluating:")
     (pre
      "(asdf:load-system :wt)")))

(define-variable chapter-getting-started
    (chapter
     :title "Getting Started"
     article-get-lisp-web-toolkit
     article-load-a-system))
