(in-package :http)

(define-router main-router)

(define-server test-server
    :handler main-router
    :listeners (list
                (listener :port 8001)))
