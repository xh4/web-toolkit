(in-package :style)

(define-dimension length ())

(define-dimension relative-length (length))

(define-dimension font-relative-length (relative-length))

(define-dimension/unit em (font-relative-length))

(define-dimension/unit ex (font-relative-length))

(define-dimension/unit ch (font-relative-length))

(define-dimension/unit rem (font-relative-length))

(define-dimension viewport-percentage-length (relative-length))

(define-dimension/unit vw (viewport-percentage-length))

(define-dimension/unit vh (viewport-percentage-length))

(define-dimension/unit vmin (viewport-percentage-length))

(define-dimension/unit vmax (viewport-percentage-length))

(define-dimension absolute-length (length))

(define-dimension/unit cm (absolute-length))

(define-dimension/unit mm (absolute-length))

(define-dimension/unit q (absolute-length))

(define-dimension/unit in (absolute-length))

(define-dimension/unit pt (absolute-length))

(define-dimension/unit pc (absolute-length))

(define-dimension/unit px (absolute-length))
