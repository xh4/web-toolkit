(in-package :vendor)

(defun system-dependencies (&optional test)
  (let ((all-deps nil))
    (labels ((add-deps (system-name)
               (check-type system-name string)
               (let* ((system (asdf:find-system system-name))
                      (deps (asdf:system-depends-on system)))
                 (loop for dep in deps
                    do
                      (let ((dep-name
                             (cond
                               ;; (:VERSION "fare-utils" "1.0.0")
                               ((consp dep)
                                (case (car dep)
                                  (:version (second dep))
                                  (otherwise (car (last dep)))))
                               ((stringp dep) dep))))
                        (when (stringp dep-name)
                          (unless (search "wt." dep-name)
                            (pushnew dep-name all-deps :test 'string-equal))
                          (add-deps dep-name)))))))
      (add-deps (if test "wt/test" "wt"))
      (sort all-deps 'string<))))

;; (defun install-vendors (&key test)
;;   (loop for sys in (system-dependencies test)
;;      for system = (ql::find-system sys)
;;      for release = (ql::release system)
;;      for archive = (ql::ensure-local-archive-file release)
;;      for tar = (merge-pathnames "release-install.tar" (uiop:temporary-directory))
;;      for output = (ql::relative-to (ql::dist release)
;;                                    (merge-pathnames
;;                                     "vendor/"
;;                                     (asdf:system-source-directory
;;                                      (asdf:find-system "wt"))))
;;      do (ensure-directories-exist tar)
;;        (ensure-directories-exist output)
;;        (ql::gunzip archive tar)
;;        (ql::unpack-tarball tar :directory output)
;;      collect (list sys (slot-value release 'ql-dist::prefix)) into vendors
;;      finally
;;        (setf *vendors* (sort vendors 'string< :key 'first))))

(defparameter *vendors*
  '(("alexandria" "alexandria-20191227-git")
    ("babel" "babel-20191130-git")
    ("babel-streams" "babel-20191130-git")
    ("bordeaux-threads" "bordeaux-threads-v0.8.7")
    ("chunga" "chunga-20180131-git")
    ("cl-base64" "cl-base64-20150923-git")
    ("cl-change-case" "cl-change-case-20191007-git")
    ("cl-cont" "cl-cont-20110219-darcs")
    ("cl-fad" "cl-fad-20190813-git")
    ("cl-ppcre" "cl-ppcre-20190521-git")
    ("cl-ppcre-unicode" "cl-ppcre-20190521-git")
    ("cl-unicode" "cl-unicode-20190521-git")
    ("closer-mop" "closer-mop-20191227-git")
    ("closure-common" "closure-common-20181018-git")
    ("closure-html" "closure-html-20180711-git")
    ("cxml-dom" "cxml-20181018-git")
    ("cxml" "cxml-20181018-git")
    ("find-port" "find-port-20190710-git")
    ("fiveam" "fiveam-v1.4.1")
    ("flexi-streams" "flexi-streams-20190107-git")
    ("ironclad" "ironclad-v0.47")
    ("maxpc" "maxpc-20171130-git")
    ("net.didierverna.asdf-flv" "asdf-flv-version-2.1")
    ("nibbles" "nibbles-20180831-git")
    ("puri" "puri-20180228-git")
    ("split-sequence" "split-sequence-v2.0.0")
    ("trivial-backtrace" "trivial-backtrace-20190710-git")
    ("trivial-features" "trivial-features-20190710-git")
    ("trivial-gray-streams" "trivial-gray-streams-20181018-git")
    ("usocket" "usocket-0.8.3")))

(defun ensure-vendors ()
  (loop for (name directory) in *vendors*
     for pathname = (merge-pathnames
                     (format nil "vendor/~A/" directory)
                     (asdf:system-source-directory
                      (asdf:find-system "wt")))
     for asd-pathname = (merge-pathnames
                         (format nil "~A.asd" name)
                         pathname)
     when (probe-file asd-pathname)
     do
       (setf (gethash name asdf/source-registry::*source-registry*) asd-pathname)
     else do
       (error "ASD file not found for system ~A in ~A" name pathname)))
