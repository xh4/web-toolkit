(in-package :css-test)

(in-suite :css-test)

(test box-shadow
  (box-shadow "none")
  (box-shadow "inset rgba(0,0,0,0.4) 64px 64px 12px 40px")
  (box-shadow "inset rgba(0,0,0,0.4) 64px 64px 12px 40px, inset rgba(0,0,0,0.4) 64px 64px 12px 40px"))
