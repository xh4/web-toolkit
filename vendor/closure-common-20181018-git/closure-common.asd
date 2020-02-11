(defpackage :closure-common-system
  (:use :asdf :cl)
  (:export #:*utf8-runes-readtable*))

(in-package :closure-common-system)

(defvar *utf8-runes-readtable*)

(defclass closure-source-file (cl-source-file) ())

#+sbcl
(defmethod perform :around ((o compile-op) (s closure-source-file))
  ;; shut up already.  Correctness first.
  (handler-bind ((sb-ext:compiler-note #'muffle-warning))
    (let (#+sbcl (*compile-print* nil))
      (call-next-method))))

(progn
  (format t "~&;;; Checking for wide character support...")
  (force-output)
  (flet ((test (code)
	   (and (< code char-code-limit) (code-char code))))
    (cond
      ((not (test 50000))
       (format t " no, reverting to octet strings.~%")
       #+rune-is-character
       (error "conflicting unicode configuration.  Please recompile.")
       (pushnew :rune-is-integer *features*))
      ((test 70000)
       (when (test #xD800)
	 (format t " WARNING: Lisp implementation doesn't use UTF-16, ~
                     but accepts surrogate code points.~%"))
       (format t " yes, using code points.~%")
       #+(or rune-is-integer rune-is-utf-16)
       (error "conflicting unicode configuration.  Please recompile.")
       (pushnew :rune-is-character *features*))
      (t
       (format t " yes, using UTF-16.~%")
       #+(or rune-is-integer (and rune-is-character (not rune-is-utf-16)))
       (error "conflicting unicode configuration.  Please recompile.")
       (pushnew :rune-is-utf-16 *features*)
       (pushnew :rune-is-character *features*)))))

#-rune-is-character
(format t "~&;;; Building Closure with (UNSIGNED-BYTE 16) RUNES~%")

#+rune-is-character
(format t "~&;;; Building Closure with CHARACTER RUNES~%") 

(defsystem :closure-common
    :default-component-class closure-source-file
    :serial t
    :components
    ((:file "package")
     (:file "definline")
     (:file runes
            :pathname
             #-rune-is-character "runes"
             #+rune-is-character "characters")
     #+rune-is-integer (:file "utf8")
     (:file "syntax")
     #-x&y-streams-are-stream (:file "encodings")
     #-x&y-streams-are-stream (:file "encodings-data")
     #-x&y-streams-are-stream (:file "xstream")
     #-x&y-streams-are-stream (:file "ystream")
     #+x&y-streams-are-stream (:file #+scl "stream-scl")
     (:file "hax"))
    :depends-on (#-scl :trivial-gray-streams
		       #+rune-is-character :babel))
