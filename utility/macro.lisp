(in-package :utility)

(define-constant +merge-tail-calls+
    ;; Cf. https://0branch.com/notes/tco-cl.html#sec-2-5.

    ;; On SBCL debug=0 is sufficient to deactivate insert-debug-catch,
    ;; and to trigger recognize-self-calls (as long as one of speed or
    ;; space is greater than 0).

    ;; CCL does TCO as long as debug<3.

    ;; LispWorks merges tail calls as long as debug<3.

    ;; Allegro will only optimize non-self tail calls if debug<3 and
    ;; speed>2.
    '(declare (optimize (debug 0)
               #+sbcl (space 1)
               #+allegro (speed 3)))
  :test 'equal
  :documentation "Try to ensure that tail calls will be merged.
If you just want portable self-calls, for writing loops using
recursion, use `nlet' or `defloop' instead.
This may not work at all on some Lisps.")
