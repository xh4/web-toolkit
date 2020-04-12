(in-package :documentation)

(define-variable chapter-uri
    (chapter
     :title "URI"
     (p "The WT.URI system implements URI parser and constructor, with support for UTF-8 characters, IPv6 addresses and query parameters handling. It utilizes recursive descent " (a :href "https://www.cs.nott.ac.uk/~pszgmh/monparsing.pdf" "parser combinators") " to provide a concise implementation that is close to the definition in " (a :href "https://tools.ietf.org/html/rfc3986" "RFC 3986 Uniform Resource Identifier (URI): Generic Syntax") ".")
     (class/o :symbol 'uri:uri
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
     (function/o :symbol 'uri:uri
                 :syntax `((uri uri-designator)
                           (uri &rest uri-components)
                           (uri base-uri relative-uri)
                           (uri uri-designator &rest uri-components))
                 :arguments `((uri-designator "A URI instance or a string which represents a URI.")
                              (uri-components "A property list whose key can be " ,(html:code ":scheme") ", " ,(html:code ":userinfo") ", " ,(html:code ":host") ", " ,(html:code ":port") ", " ,(html:code ":path") ", " ,(html:code ":query") " or " ,(html:code ":fragment") ".")
                              (base-uri "A URI designator.")
                              (relative-uri "A URI designator."))
                 :summary (list
                           (p "The \"All-in-One\" function to parse, merge, update and construct a URI.")
                           (p "Parse a URI:")
                           (evil (uri:uri "https://xh.coobii.com") :uri)
                           (p "Construct a URI:")
                           (evil (uri:uri :scheme "https" :host "xh.coobii.com") :uri)
                           (p "Merge two URIs:")
                           (evil (uri:uri "https://xh.coobii.com/foo" "/bar") :uri)
                           (p "Update a URI:")
                           (evil (uri:uri "https://xh.coobii.com" :query '("foo" "bar")) :uri)))
     (function/o :symbol 'uri:uri-string
                 :syntax `((uri-string uri-designator)
                           (uri-string &rest uri-components)
                           (uri-string base-uri relative-uri)
                           (uri-string uri-designator &rest uri-components))
                 :arguments `((uri-designator "A URI instance or a string which represents a URI.")
                              (uri-components "A property list whose key can be " ,(html:code ":scheme") ", " ,(html:code ":userinfo") ", " ,(html:code ":host") ", " ,(html:code ":port") ", " ,(html:code ":path") ", " ,(html:code ":query") " or " ,(html:code ":fragment") ".")
                              (base-uri "A URI designator.")
                              (relative-uri "A URI designator."))
                 :summary (list
                           (p "Do the same thing as the function " (function-ref 'uri:uri) ", render the final URI to a string.")
                           (evil (uri:uri-string :scheme "https" :host "xh.coobii.com"))
                           (evil (uri:uri-string "https://xh.coobii.com/foo" "/bar"))
                           (evil (uri:uri-string "https://xh.coobii.com" :port 443))
                           (evil (uri:uri-string :query '("foo" "bar" "goo" "gle")))
                           (evil (uri:uri-string :query '(("foo" . "bar") ("goo" . "gle"))))))
     (accessor/o :symbol 'uri-scheme
                 :summary (list
                           (evil (uri:uri-scheme "https://xh.coobii.com"))
                           (evil (prog1
                                     (setq uri (uri:uri "https://coobii.com"))
                                   (setf (uri:uri-scheme uri) "wss"))
                                 :uri)))
     (accessor/o :symbol 'uri-userinfo
                 :summary (list
                           (evil (uri:uri-userinfo "https://xh@coobii.com"))
                           (evil (prog1
                                     (setq uri (uri:uri "https://xh@coobii.com"))
                                   (setf (uri:uri-userinfo uri) "someone"))
                                 :uri)))
     (accessor/o :symbol 'uri-host
                 :summary (list
                           (evil (uri:uri-host "https://coobii.com"))
                           (evil (uri:uri-host "http://127.0.0.1"))
                           (evil (uri:uri-host "http://[3ffe:2a00:100:7031::1]"))
                           (evil (prog1
                                     (setq uri (uri:uri "https://coobii.com"))
                                   (setf (uri:uri-host uri) "xh.coobii.com"))
                                 :uri)))
     (accessor/o :symbol 'uri-port
                 :summary (list
                           (evil (uri:uri-port "https://coobii.com:443"))
                           (evil (uri:uri-port "https://coobii.com"))
                           (evil (prog1
                                     (setq uri (uri:uri "https://coobii.com"))
                                   (setf (uri:uri-port uri) 443))
                                 :uri)))
     (accessor/o :symbol 'uri-path
                 :summary (list
                           (evil (uri:uri-path "https://coobii.com"))
                           (evil (uri:uri-path "https://coobii.com/foo/bar"))
                           (evil (prog1
                                     (setq uri (uri:uri "https://coobii.com/coca"))
                                   (setf (uri:uri-path uri) "/cola"))
                                 :uri)))
     (accessor/o :symbol 'uri-query
                 :summary (list
                           (evil (uri:uri-query "https://coobii.com?foo=bar"))
                           (evil (uri:uri-query "https://coobii.com?foo=bar" :type :hash-table))
                           (evil (prog1
                                     (setq uri (uri:uri "https://coobii.com"))
                                   (setf (uri:uri-query uri) '(("foo" . "bar"))))
                                 :uri)))
     (accessor/o :symbol 'uri-fragment
                 :summary (list
                           (evil (uri:uri-fragment "https://coobii.com/#header"))
                           (evil (prog1
                                     (setq uri (uri:uri "https://coobii.com/#header"))
                                   (setf (uri:uri-fragment uri) "footer"))
                                 :uri)))))
