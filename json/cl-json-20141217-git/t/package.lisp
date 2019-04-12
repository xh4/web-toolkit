(defpackage :json-test
  (:use :cl-json :cl-json-rpc :common-lisp :5am )
  #+cl-json-clos
  (:import-from #+(or mcl openmcl) #:ccl
                #+cmu #:clos-mop
                #+sbcl #:sb-mop
                #+(or clisp ecl scl lispworks) #:clos
                #+(or allegro abcl) #:mop
    #:finalize-inheritance))

(in-package :cl-json-test)
(def-suite cl-json)
