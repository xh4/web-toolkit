(in-package :uri)

(defun alpha-p (char)
  (or
   (char<= #\a char #\z)
   (char<= #\A char #\Z)))

(define-parser .alpha ()
  (.satisfies 'alpha-p))

(defun digit-p (char)
  (char<= #\0 char #\9))

(define-parser .digit ()
  (.satisfies 'digit-p))

(defun hexdig-p (char)
  (or (digit-p char)
      (char<= #\a char #\f)
      (char<= #\A char #\F)))

(define-parser .hexdig ()
  (.satisfies 'hexdig-p))

(define-parser .pct-encoded ()
  (.seq/s (.eq #\%) (.hexdig) (.hexdig)))

(defun gen-delim-p (char)
  (member char '(#\: #\/ #\? #\# #\[ #\] #\@)))

(define-parser .gen-delims ()
  (.satisfies 'gen-delim-p))

(defun sub-delim-p (char)
  (member char '(#\! #\$ #\& #\' #\( #\)
              #\* #\+ #\, #\; #\=)))

(define-parser .sub-delims ()
  (.satisfies 'sub-delim-p))

(defun reserved-p (char)
  (or (gen-delim-p char)
      (sub-delim-p char)))

(define-parser .reserved ()
  (.satisfies 'reserved-p))

(defun unreserved-p (char)
  (or (alpha-p char)
      (digit-p char)
      (eq char #\=)
      (eq char #\.)
      (eq char #\_)
      (eq char #\~)))

(define-parser .unreserved ()
  (.satisfies 'unreserved-p))

(define-parser .scheme ()
  (.seq/s (.alpha) (.maybe (.any/s (.or (.alpha) (.digit) (.eq #\+) (.eq #\-) (.eq #\.))))))

(define-parser .userinfo ()
  (.maybe (.any/s (.or (.unreserved) (.pct-encoded) (.sub-delims) (.eq #\:)))))

(define-parser .10-99-digit ()
  (.seq (.satisfies (lambda (c)
                      (char<= #\1 c #\9)))
        (.satisfies (lambda (c)
                      (char<= #\0 c #\9)))))

(define-parser .00-49-digit ()
  (.seq (.satisfies (lambda (c)
                      (char<= #\0 c #\4)
                      ))
        (.satisfies (lambda (c)
                      (char<= #\0 c #\9)))))

(define-parser .0-5-digit ()
  (.satisfies (lambda (c)
                (char<= #\0 c #\5))))

(define-parser .dec-octet ()
  (.or (.seq/s (.eq #\2) (.eq #\5) (.0-5-digit))
       (.seq/s (.eq #\2) (.00-49-digit))
       (.seq/s (.eq #\1) (.digit) (.digit))
       (.10-99-digit)
       (.digit)))

(define-parser .ipv4-address ()
  (.seq/s (.dec-octet) (.eq #\.)
          (.dec-octet) (.eq #\.)
          (.dec-octet) (.eq #\.)
          (.dec-octet)))

;; 这部分比我想象的宽松
;; URL 对这部分的定义好像更宽松
(define-parser .reg-name ()
  (.maybe (.any/s (.or (.unreserved) (.pct-encoded) (.sub-delims)))))

(define-parser .host ()
  (.or (.ipv4-address) (.reg-name)))

(define-parser .port ()
  (.maybe (.any/s (.digit))))

(define-parser .authority ()
  (.seq/s (.maybe (.seq/s (.userinfo) (.eq #\@)))
          (.host)
          (.maybe (.seq/s (.eq #\:) (.port)))))

(define-parser .pchar ()
  (.or (.unreserved) (.pct-encoded) (.sub-delims) (.eq #\:) (.eq #\@)))

(define-parser .segment ()
  (.maybe (.any/s (.pchar))))

(define-parser .segment-nz ()
  (.some/s (.pchar)))

(define-parser .segment-nz-nc ()
  ;; non-zero-length segment wihout any colon ":"
  (.some/s (.or (.unreserved) (.pct-encoded) (.sub-delims) (.eq #\@))))

(define-parser .path-abempty ()
  (.maybe (.any/s (.seq/s (.eq #\/) (.segment)))))

(define-parser .path-absolute ()
  (.seq/s (.eq #\/) (.maybe (.seq/s (.segment-nz)
                                    (.maybe (.any/s (.seq/s (.eq #\/) (.segment))))))))

(define-parser .path-noscheme ()
  (.seq/s (.segment-nz-nc) (.maybe (.any/s (.seq/s (.eq #\/) (.segment))))))

(define-parser .path-rootless ()
  (.seq/s (.segment-nz) (.maybe (.any/s (.seq/s (.eq #\/) (.segment))))))

;; path-empty = 0<pchar>
(define-parser .path-empty ()
  (.end))

(define-parser .query ()
  (.maybe (.any/s (.or (.pchar) (.eq #\/) (.eq #\?)))))

(define-parser .fragment ()
  (.maybe (.any/s (.or (.pchar) (.eq #\/) (.eq #\?)))))

(define-parser .relative-part ()
  (.or (.seq (.eq #\/) (.eq #\/) (.authority) (.path-abempty))
       (.path-absolute)
       (.path-noscheme)
       (.path-empty)))

(define-parser .relative-ref ()
  (.seq/s (.relative-part)
          (.maybe (.seq (.eq #\?) (.query)))
          (.maybe (.seq (.eq #\#) (.fragment)))))

(define-parser .hier-part ()
  (.or (.seq (.eq #\/) (.eq #\/) (.authority) (.path-abempty))
       (.path-absolute)
       (.path-rootless)
       (.path-empty)))

(define-parser .absolute-uri ()
  (.seq (.scheme) (.eq #\:) (.hier-part)
        (.maybe (.seq (.eq #\?) (.query)))))

(define-parser .uri ()
  (.seq (.scheme) (.eq #\:) (.hier-part)
        (.maybe (.seq (.eq #\?) (.query)))
        (.maybe (.seq (.eq #\#) (.fragment)))))

(define-parser .uri-reference ()
  (.or (.uri) (.relative-ref)))

(defun parse-uri (uri-string)
  (with-parser-stack (stack :trace '(.scheme
                                     .userinfo .host .port
                                     .path-abempty .path-absolute
                                     .path-noscheme .path-empty
                                     .query .fragment))
    (parse (.uri-reference) (maxpc::make-input uri-string))
    (let ((uri (make-instance 'uri)))
      (loop for parser in (reverse stack)
         for value = (parser-value parser)
         do
           (typecase parser
             (.scheme (setf (uri-scheme uri) value))
             (.userinfo (setf (uri-userinfo uri)
                              (when value
                                (percent-decode-string value))))
             (.host (setf (uri-host uri)
                          (when value
                            (percent-decode-string value))))
             (.port (setf (uri-port uri)
                          (parse-integer value)))
             ((or .path-abempty
                  .path-noscheme
                  .path-absolute)
              (setf (uri-path uri)
                    (when value
                      (percent-decode-string value))))
             (.query (setf (uri-query uri) value))
             (.fragment (setf (uri-fragment uri) value))))
      uri)))

;; (parse-uri "https://xh@coobii.com:80/foo/bar?abc=def#goo")
;; (parse-uri "/foo/bar?abc=def#goo")
;; (parse-uri "///foo/bar?abc=def#goo")
