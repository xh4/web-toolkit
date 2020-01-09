(in-package :cl-user)

(defpackage :json-test
  (:nicknames :wt.json-test)
  (:use :cl :json :fiveam)
  (:shadow :get)
  (:export :run!)
  #+cl-json-clos
  (:import-from #+(or mcl openmcl) #:ccl
                #+cmu #:clos-mop
                #+sbcl #:sb-mop
                #+(or clisp ecl scl lispworks) #:clos
                #+(or allegro abcl) #:mop
    #:finalize-inheritance))

(in-package :json-test)
(def-suite :json-test)
