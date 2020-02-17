(in-package :vendor)

(defun install ()
  (loop
     with vendor = (merge-pathnames
                       "vendor/"
                       (asdf:system-source-directory
                        (asdf:find-system "wt")))
     with vendor.txt = (merge-pathnames
                        "vendor.txt"
                        (asdf:system-source-directory
                         (asdf:find-system "wt")))
     with processed-system-names = '()
     for dependency in (system-dependencies "wt/test")
     for system-name = (normalize-system-name dependency)
     for system = (ql::find-system system-name)
     for system-file = (system-file system-name)
     for release = (ql::release system)
     for archive = (ql::ensure-local-archive-file release)
     for tar = (merge-pathnames "release-install.tar" (uiop:temporary-directory))
     for system-file-pathname = (merge-pathnames
                                 system-file
                                 (pathname-as-directory
                                  (merge-pathnames (slot-value release 'ql::prefix) vendor)))
     unless (find system-name processed-system-names :test 'equal)
     do (format t "~S~%" (merge-pathnames (slot-value release 'ql::prefix) vendor))
       (push system-name processed-system-names)
       (ensure-directories-exist tar)
       (ensure-directories-exist vendor)
       (ql::gunzip archive tar)
       (ql::unpack-tarball tar :directory vendor)
       (unless (probe-file system-file-pathname)
         (error "ASD file for system ~S not found: ~S" system-name system-file-pathname))
     and
     collect (list system-name (format nil "~A/~A"
                                       (slot-value release 'ql::prefix)
                                       system-file))
     into vendors
     finally
       (setf vendors (sort vendors 'string< :key 'first))
       (setf *vendors* vendors)
       (with-open-file (stream vendor.txt
                               :direction :output
                               :if-does-not-exist :create
                               :if-exists :supersede)
         (loop for (system path) in vendors
            do (format stream "~A ~A~C" system path #\Newline)))))

(defun normalize-system-name (system-name)
  (let ((pos (position #\/ system-name)))
    (if pos
        (subseq system-name 0 pos)
        system-name)))

;; (normalize-system-name "cxml/dom") -> "cxml"
;; (normalize-system-name "cl-unicode/base") -> "cl-unicode"

(defun system-file (system-name)
  (let* ((system (or (ql::find-system system-name)
                     (format t "System ~S not found by quicklisp~%" system-name)))
         (release (ql::release system)))
    (let ((files (slot-value release 'ql::system-files)))
      (if (= (length files) 1)
          (first files)
          (let ((direct-file (concatenate 'string system-name ".asd")))
            (if (find direct-file files :test 'equal)
                direct-file
                (let* ((project-name (slot-value release 'ql::project-name))
                       (project-file (concatenate 'string project-name ".asd")))
                  (if (find project-file files :test 'equal)
                      project-file
                      (error "Unable to decide system file for ~S, choose between ~A" system-name files)))))))))

;; (system-file "cxml-dom") -> "cxml-dom.asd"
;; (system-file "cxml/dom") -> "cxml.asd"
;; (system-file "cl-unicode/base") -> "cl-unicode.asd"

(defun component-present-p (value)
  "Helper function for DIRECTORY-PATHNAME-P which checks whether VALUE
   is neither NIL nor the keyword :UNSPECIFIC."
  (and value (not (eql value :unspecific))))

(defun directory-pathname-p (pathspec)
  "Returns NIL if PATHSPEC \(a pathname designator) does not designate
a directory, PATHSPEC otherwise.  It is irrelevant whether file or
directory designated by PATHSPEC does actually exist."
  (and
   (not (component-present-p (pathname-name pathspec)))
   (not (component-present-p (pathname-type pathspec)))
   pathspec))

(defun pathname-as-directory (pathspec)
  "Converts the non-wild pathname designator PATHSPEC to directory
form."
  (let ((pathname (pathname pathspec)))
    (when (wild-pathname-p pathname)
      (error "Can't reliably convert wild pathnames."))
    (cond ((not (directory-pathname-p pathspec))
           (make-pathname :directory (append (or (pathname-directory pathname)
                                                 (list :relative))
                                             (list (file-namestring pathname)))
                          :name nil
                          :type nil
                          :defaults pathname))
          (t pathname))))
