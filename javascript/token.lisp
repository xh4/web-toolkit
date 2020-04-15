(in-package :javascript)

(defstruct token
  type
  value
  line-number
  line-start
  start
  end
  octal
  pattern
  flags
  regex)
