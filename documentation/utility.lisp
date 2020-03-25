(in-package :documentation)

(defun remove-identity (string)
  (cl-ppcre:regex-replace "\\s+{?([A-Z0-9]+)?}?\\>$" string ">"))
