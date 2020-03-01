(in-package :component-test)

(defvar *temporary-variable-names* nil)

(defmacro ensure-cleanup (() &body body)
  `(macrolet ((com-test::define-component (name &rest arguments)
                `(progn
                   (push ',name *temporary-variable-names*)
                   (com:define-component ,name ,@arguments)))
              (com-test::define-variable (name form)
                `(progn
                   (push ',name *temporary-variable-names*)
                   (push (intern (format nil "V/~A" ',name)) *temporary-variable-names*)
                   (com:define-variable ,name ,form))))
     (unwind-protect
          (progn ,@body)
       (progn
         (mapcar 'makunbound *temporary-variable-names*)
         (mapcar 'fmakunbound *temporary-variable-names*)
         (setf *temporary-variable-names* nil)))))

(defmacro variable (name)
  `(com:variable ,name))
