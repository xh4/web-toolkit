;;; net.didierverna.asdf-flv.asd --- ASDF system definition

;; Copyright (C) 2011, 2015 Didier Verna

;; Author: Didier Verna <didier@didierverna.net>

;; This file is part of ASDF-FLV.

;; Copying and distribution of this file, with or without modification,
;; are permitted in any medium without royalty provided the copyright
;; notice and this notice are preserved.  This file is offered as-is,
;; without any warranty.


;;; Commentary:

;; Contents management by FCM version 0.1.


;;; Code:

(asdf:defsystem :net.didierverna.asdf-flv
  :long-name "ASDF File Local Variables"
  :description "ASDF extension to provide support for file-local variables."
  :long-description "\
ASDF-FLV provides support for file-local variables through ASDF. A file-local
variable behaves like *PACKAGE* and *READTABLE* with respect to LOAD and
COMPILE-FILE: a new dynamic binding is created before processing the file, so
that any modification to the variable becomes essentially file-local.

In order to make one or several variables file-local, use the macros
SET-FILE-LOCAL-VARIABLE(S)."
  :author "Didier Verna"
  :mailto "didier@didierverna.net"
  :homepage "http://www.lrde.epita.fr/~didier/software/lisp/misc.php#asdf-flv"
  :source-control "https://github.com/didierverna/asdf-flv"
  :license "GNU All Permissive"
  :version "2.1"
  :serial t
  :components ((:file "package")
	       (:file "asdf-flv")))

;;; net.didierverna.asdf-flv.asd ends here
