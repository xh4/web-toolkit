(in-package :documentation)

(define-variable chapter-json
    (chapter
     :title "JSON"
     (p "The WT.JSON system implements JSON encoder and decoder based on " (a :href "https://www.json.org/json-en.html" :target "blank" "ECMA-404 The JSON Data Interchange Standard") ". It distinguishes " (html:code "null") ", " (html:code "false") " and " (html:code "[]") " from Lisp's " (html:code "NIL") " thus supports identical transformation between JSON values. It provides object constructor and accessor to build and access nesting JSON objects.")
     (class/o :symbol 'json:true
              :summary (p "A class which represents JSON's true."))
     (constant/o :symbol 'json:true
                 :summary (list
                           (p "The constant value represents JSON's true.")))
     (function/o :symbol 'json:true
                 :summary (list
                           (p "The function turn it's argument to the constant true or NIL.")
                           (evil (json:true t) :json)
                           (evil (json:true 42) :json)
                           (evil (json:true nil) :json)))
     (class/o :symbol 'json:false
              :summary (p "A class which represents JSON's false."))
     (constant/o :symbol 'json:false
                 :summary (list
                           (p "The constant value represents JSON's false.")))
     (function/o :symbol 'json:false
                 :summary (list
                           (p "The function turn it's argument to the constant false or the argument itself.")
                           (evil (json:false nil) :json)
                           (evil (json:false '(1 2 3)))
                           (evil (json:false 42))
                           (evil (json:false (json:object "foo" "bar")) :json)))
     (class/o :symbol 'json:null
              :summary (p "A class which represents JSON's null."))
     (constant/o :symbol 'json:null
                 :summary (list
                           (p "The constant value represents JSON's null.")))
     (function/o :symbol 'json:null
                 :summary (list
                           (p "The function turn it's argument to the constant null or the argument itself.")
                           (evil (json:null nil) :json)
                           (evil (json:null '(1 2 3)))
                           (evil (json:null 42))
                           (evil (json:null (json:object "foo" "bar")) :json)))
     (class/o :symbol 'json:array
              :summary (list
                        (p "A class which represents a JSON array.")
                        (p "Array is printed in it's constructing form.")))
     (function/o :symbol 'json:array
                 :summary (list
                           (p "The function to construct a JSON array from a sequence.")
                           (evil (json:array) :json)
                           (evil (json:array 1 2 3) :json)))
     (class/o :symbol 'json:object
              :summary (list
                        (p "A class which represents a JSON object.")
                        (p "Object is printed in it's constructing form.")))
     (function/o :symbol 'json:object
                 :summary (list
                           (p "The function to construct a JSON object.")
                           (evil (json:object "name" "XH" "email" "xh@coobii.com") :json)
                           (evil (json:object "answers" '(1 2 3)) :json)))
     (function/o :symbol 'json:value
                 :summary (list
                           (p "Get the inner value of certain JSON types.")
                           (evil (json:value json:true) :json)
                           (evil (json:value json:false) :json)
                           (evil (json:value json:null) :json)
                           (evil (json:value (json:array 1 2 3)))
                           (evil (json:value (json:array)))
                           (evil (json:value 42))
                           (evil (json:value "string"))))
     (accessor/o :symbol 'json:get
                 :summary (list
                           (evil (json:get (json:object "foo" "bar") "foo"))
                           (evil
                            (json:get (json:object "name"
                                                   (json:object "first-name" "Xiangyu"
                                                                "last-name" "He"))
                                      "name" "first-name")
                            :json)
                           (evil
                            (prog1
                                (setq object (json:object "name"
                                                          (json:object "first-name" "Xiangyu"
                                                                       "last-name" "He")))
                              (setf (json:get object "name" "first-name") "X"
                                    (json:get object "name" "last-name") "H"))
                            :json)
                           (evil
                            (prog1
                                (setq object (json:object "answers"
                                                          (json:array 0 0 0)))
                              (setf (json:get object "answers" 1) 42))
                            :json)))
     (function/o :symbol 'json:encode
                 :summary (list
                           (evil (json:encode t))
                           (evil (json:encode json:true))
                           (evil (json:encode nil))
                           (evil (json:encode json:false))
                           (evil (json:encode json:null))
                           (evil (json:encode (json:array)))
                           (evil (json:encode '(1 2 3)))
                           (evil (json:encode #(1 2 3)))
                           (evil (json:encode (json:object)))
                           (evil (json:encode
                                  (json:object
                                   "name" "XH"
                                   "email" "xh@coobii.com")))
                           (evil (json:encode
                                  (json:object
                                   "answers" '(1 2 3))))))
     (function/o :symbol 'json:decode
                 :summary (list
                           (evil (json:decode "true") :json)
                           (evil (json:decode "false") :json)
                           (evil (json:decode "null") :json)
                           (evil (json:decode "[]") :json)
                           (evil (json:decode "[1,2,3]") :json)
                           (evil (json:decode "{}") :json)
                           (evil (json:decode
                                  "{\"name\":\"XH\",\"email\":\"xh@coobii.com\"}")
                                 :json)
                           (evil (json:decode
                                  "{\"answers\":[1,2,3]}")
                                 :json)))))
