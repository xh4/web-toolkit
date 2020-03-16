(in-package :css-test)

(in-suite :css-test)

(test overflow
  (overflow-x :visible)
  (overflow-y :visible :hidden)
  (overflow :visible :hidden)
  (overflow "visible hidden"))
