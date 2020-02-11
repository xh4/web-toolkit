(defsystem find-port
  :author "Fernando Borretti <eudoxiahp@gmail.com>"
  :maintainer "Fernando Borretti <eudoxiahp@gmail.com>"
  :license "MIT"
  :version "0.1"
  :homepage "https://github.com/eudoxia0/find-port"
  :bug-tracker "https://github.com/eudoxia0/find-port/issues"
  :source-control (:git "git@github.com:eudoxia0/find-port.git")
  :depends-on (:usocket)
  :components ((:module "src"
                :serial t
                :components
                ((:file "find-port"))))
  :description "Find open ports programmatically."
  :long-description
  #.(uiop:read-file-string
     (uiop:subpathname *load-pathname* "README.md"))
  :in-order-to ((test-op (test-op find-port-test))))
