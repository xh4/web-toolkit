(in-package :reactive-test)

(defmacro variable (symbol)
  `(rx:variable ,symbol))

(defmacro ensure-variable-cleanup (symbols &body body)
  `(unwind-protect
        (progn ,@body)
     (progn
       (loop for symbol in ',symbols
          for variable = (rx:variable symbol)
          do (setf (slot-value variable 'rx::dependency) nil
                   (slot-value variable 'rx::propagation) nil)
            (makunbound symbol)
            (fmakunbound symbol)))))
