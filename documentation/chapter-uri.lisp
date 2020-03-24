(in-package :documentation)

(define-variable chapter-uri
    (chapter
     :title "URI"
     (p "The WT.URI system implements URI parser and constructor, with support for UTF-8 characters, IPv6 addresses and query parameters handling. It utilizes recursive descent " (a :href "https://www.cs.nott.ac.uk/~pszgmh/monparsing.pdf" "parser combinators") " to provide a concise implementation that is close to the definition in " (a :href "https://tools.ietf.org/html/rfc3986" "RFC 3986 Uniform Resource Identifier (URI): Generic Syntax") ".")
     (class/o :symbol 'uri
              :summary
              (list
               (p "A class which represents a URI.")
               (p "A URI has the following syntax and components:")
               (figure
                (img :src "https://upload.wikimedia.org/wikipedia/commons/d/d6/URI_syntax_diagram.svg"
                     :alt "URI syntax diagram")
                (figcaption
                 (a :href "https://en.wikipedia.org/wiki/Uniform_Resource_Identifier#/media/File:URI_syntax_diagram.svg"
                    :target "blank"
                    "URI syntax diagram")))))
     (function/o :symbol 'uri
                 :summary (list
                           (p "The \"All-in-One\" function to parse, merge, update and construct a URI.")
                           (p "Parse a URI:")
                           (evil (uri "https://xh.coobii.com"))
                           (p "Construct a URI:")
                           (evil (uri :scheme "https" :host "xh.coobii.com"))
                           (p "Merge two URIs:")
                           (evil (uri "https://xh.coobii.com/foo" "/bar"))
                           (p "Update a URI:")
                           (evil (uri "https://xh.coobii.com" :query '("foo" "bar")))))
     (function/o :symbol 'uri-string
                 :summary (list
                           (p "Do the same thing as the function URI, render the final URI to a string.")
                           (evil (uri-string :scheme "https" :host "xh.coobii.com"))
                           (evil (uri-string "https://xh.coobii.com/foo" "/bar"))
                           (evil (uri-string "https://xh.coobii.com" :port 443))
                           (evil (uri-string :query '("foo" "bar" "goo" "gle")))
                           (evil (uri-string :query '(("foo" . "bar") ("goo" . "gle"))))))
     (accessor/o :symbol 'uri-scheme
                 :summary (list
                           (evil (uri-scheme "https://xh.coobii.com"))
                           (evil (prog1
                                     (setq uri (uri "https://coobii.com"))
                                   (setf (uri-scheme uri) "wss")))))
     (accessor/o :symbol 'uri-userinfo
                 :summary (list
                           (evil (uri-userinfo "https://xh@coobii.com"))
                           (evil (prog1
                                     (setq uri (uri "https://xh@coobii.com"))
                                   (setf (uri-userinfo uri) "someone")))))
     (accessor/o :symbol 'uri-host
                 :summary (list
                           (evil (uri-host "https://coobii.com"))
                           (evil (uri-host "http://127.0.0.1"))
                           (evil (uri-host "http://[3ffe:2a00:100:7031::1]"))
                           (evil (prog1
                                     (setq uri (uri "https://coobii.com"))
                                   (setf (uri-host uri) "xh.coobii.com")))))
     (accessor/o :symbol 'uri-port
                 :summary (list
                           (evil (uri-port "https://coobii.com:443"))
                           (evil (uri-port "https://coobii.com"))
                           (evil (prog1
                                     (setq uri (uri "https://coobii.com"))
                                   (setf (uri-port uri) 443)))))
     (accessor/o :symbol 'uri-path
                 :summary (list
                           (evil (uri-path "https://coobii.com"))
                           (evil (uri-path "https://coobii.com/foo/bar"))
                           (evil (prog1
                                     (setq uri (uri "https://coobii.com/coca"))
                                   (setf (uri-path uri) "/cola")))))
     (accessor/o :symbol 'uri-query
                 :summary (list
                           (evil (uri-query "https://coobii.com?foo=bar"))
                           (evil (uri-query "https://coobii.com?foo=bar" :type :hash-table))
                           (evil (prog1
                                     (setq uri (uri "https://coobii.com"))
                                   (setf (uri-query uri) '(("foo" . "bar")))))))
     (accessor/o :symbol 'uri-fragment
                 :summary (list
                           (evil (uri-fragment "https://coobii.com/#header"))
                           (evil (prog1
                                     (setq uri (uri "https://coobii.com/#header"))
                                   (setf (uri-fragment uri) "footer")))))))
