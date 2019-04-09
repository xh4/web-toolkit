(in-package :wt)

(defun element-form-p (form)
  (and (listp form) (keywordp (car form))))

(defun segment-element-form (form)
  (let* ((tag (car form))
         (body (loop for rest on (cdr form) by #'cddr
                  unless (and (keywordp (car rest)) (cdr rest))
                  return rest))
         (attributes (ldiff (cdr form) body)))
    (values tag attributes body)))

(defun traverse-html-recursive (form)
  (cond
    ((null form))
    ((atom form)
     ;; (format t "TEXT: ~S~%" form)
     )
    ((element-form-p form)
     (multiple-value-bind (tag attributes body) (segment-element-form form)
       ;; (format t "ELEMENT: ~A~%" tag)
       (traverse-html-recursive body)))
    (t (traverse-html-recursive (car form))
       (traverse-html-recursive (cdr form)))))

(traverse-html-recursive '((:div (:p "foo" (:hr) (:br) "bar"))
                           (:span "haha")))

(defun traverse-html-iterative (form)
  (when (not (null form))
    (let ((stack nil))
      (push form stack)
      (loop for form = (pop stack)
         while form
         do
           (cond
             ((atom form)
              ;; (format t "TEXT: ~S~%" form)
              )
             ((element-form-p form)
              (multiple-value-bind (tag attributes body) (segment-element-form form)
                ;; (format t "ELEMENT: ~A~%" tag)
                (loop for form in (reverse body)
                     do (push form stack))))
             (t (loop for form in (reverse form)
                     do (push form stack))))))))

(traverse-html-iterative '((:div (:p "foo" (:hr) (:br) "bar"))
                           (:span "haha")))

(defun traverse-html (form)
  (when (null form) (return-from traverse-html nil))
  (let ((stack nil))
    (push form stack)
    (loop for form = (pop stack)
       while form
       do
         (cond
           ((atom form)
            (format t "TEXT: ~S~%" form))
           ((element-form-p form)
            (multiple-value-bind (tag attributes body)
                (segment-element-form form)
              (declare (ignore attributes))
              (format t "ELEMENT: ~A~%" tag)
              (loop for form in (reverse body)
                 do (push form stack))))
           (t (loop for form in (reverse form)
                 do (push form stack)))))))

(defun traverse-html (form)
  (when (null form) (return-from traverse-html nil))
  (let ((stack nil))
    (push form stack)
    (loop for form = (pop stack)
       while form
       do
         (cond
           ((atom form)
            ;; (format t "TEXT: ~S~%" form)
            )
           ((and (consp form) (eq (car form) :leave))  ;; Leave a element
            ;; (format t "< ELEMENT: ~A~%" (cdr form))
            )
           ((element-form-p form)
            (multiple-value-bind (tag attributes body)
                (segment-element-form form)
              (declare (ignore attributes))
              ;; (format t "> ELEMENT: ~A~%" tag)
              (push `(:leave . ,tag) stack)            ;; Push a marker on stack
              (loop for form in (reverse body)
                 do (push form stack))))
           (t (loop for form in (reverse form)
                 do (push form stack)))))))

(traverse-html '((:h1 "heading")
                 (:p "foo" (:hr) "bar"
                  (:span "tailing"))))

(defun traverse-html-zipper (form)
  (loop with loc = (zipper form)
     while loc
     do
       (setf loc (go-next loc))))

;; (traverse-html-zipper '((:div (:p "foo" (:hr) (:br) "bar"))
;;                            (:span "haha")))

(defun generate-test-html-form (depth)
  (flet ((wrap-html-form (inner)
           `((:div :class "row" (:div :class "column" ,@inner)
             (:div :class "row" (:div :class "column" ,@inner))))))
    (let ((form nil))
      (loop for i from 1 upto depth
         do
           (setf form (wrap-html-form form)))
      form)))

(defparameter *test-html-form* (generate-test-html-form 8)) ;; Max 13

;; Zipper 方法最大达到 Size 11（SBCL 1.4.0）
;;
;; (time (traverse-html-zipper *test-html-form*))

;; Evaluation took:
;; 6.551 seconds of real time
;; 6.582351 seconds of total run time (5.722262 user, 0.860089 system)
;; [ Run times consist of 3.438 seconds GC time, and 3.145 seconds non-GC time. ]
;; 100.47% CPU
;; 13,742,188,076 processor cycles
;; 4,026,559,504 bytes consed

;; Size 达到 12 时 Zipper 方法堆溢出（SBCL 1.4.0）
;;
;; Heap exhausted during garbage collection: 0 bytes available, 16 requested.
;; Gen  StaPg UbSta LaSta Boxed Unbox    LB   LUB !move    Alloc  Waste     Trig   WP GCs Mem-age
;; 0:     0     0     0     0     0     0     0     0        0      0 21474836    0   0  0.0000
;; 1:     0     0     0     0     0     0     0     0        0      0 21474836    0   0  0.0000
;; 2: 40678     0     0 13143     0     0     0    11 430500128 169696 21474836    0   0  0.9997
;; 3: 61912   814     0 38538   771    87   119    78 1292184272 2643248  2000000 38549   0  0.4528
;; 4:     0     0     0     0     0     0     0     0        0      0  2000000    0   0  0.0000
;; 5:     0     0     0     0     0     0     0     0        0      0  2000000    0   0  0.0000
;; 6:     0     0     0   540   269     0     0     0 25710320 798992  2000000  426   0  0.0000
;; 7: 65535     0     0 12069     0     0     0     0 395476992      0  2000000    0   0  0.0000
;; Total bytes allocated    = 2143871712
;; Dynamic-space-size bytes = 2147483648
;; GC control variables:
;; *GC-INHIBIT* = true
;; *GC-PENDING* = true
;; *STOP-FOR-GC-PENDING* = false
;; fatal error encountered in SBCL pid 9075(tid 0x7fffea81f700):
;; Heap exhausted, game over.

;; (time (traverse-html-recursive *test-html-form*))

;; Form 1
(defun element-form-p (form)
  (and (listp form) (keywordp (car form))))

(defun segment-element-form (form)
  (let* ((tag (car form))
         (body (loop for rest on (cdr form) by #'cddr
                  unless (and (keywordp (car rest)) (cdr rest))
                  return rest))
         (attributes (ldiff (cdr form) body)))
    (values tag attributes body)))

(segment-element-form '(:div))
(segment-element-form '(:div :id "x"))
(segment-element-form '(:div :foo))
(segment-element-form '(:div :id "x" :class "foo"))
(segment-element-form '(:div :id "x" :class "foo" "Goo" "Gle"))

;; Form 2
(defun element-form-p (form)
  (and (listp form) (listp (car form)) (keywordp (caar form))))

(defun segment-element-form (form)
  (let ((tag (caar form))
        (attributes (cdar form))
        (body (cdr form)))
    (values tag attributes body)))

;; Form 3
(defun element-form-p (form)
  (and (listp form) (keywordp (car form))))

(defun segment-element-form (form)
  (let ((tag (car form))
        (attributes (cadr form))
        (body (cddr form)))
    (values tag attributes body)))

;; (with-open-file (stream #p"/home/xh/output.html"
;;                         :direction :output
;;                         :if-exists :supersede
;;                         :if-does-not-exist :create)
;;   (html stream
;;         *test-html-form*))
