(in-package :package)

(defclass package ()
  ((name
    :initarg :name
    :initform nil
    :reader package-name)
   (version
    :initarg :version
    :initform nil
    :reader package-version)
   (description
    :initarg :description
    :initform nil
    :reader package-description)
   (homepage
    :initarg :homepage
    :initform nil
    :reader package-homepage)
   (main
    :initarg :main
    :initform nil
    :reader package-main)
   (readme
    :initarg :readme
    :initform nil
    :reader package-readme)
   (readme-filename
    :initarg :readme-filename
    :initform nil
    :reader package-readme-filename)
   (keywords
    :initarg :keywords
    :initform nil
    :reader package-keywords)
   (license
    :initarg :license
    :initform nil
    :reader package-license)
   (scripts
    :initarg :scripts
    :initform nil
    :reader package-scripts)
   (repository
    :initarg :repository
    :initform nil
    :reader package-repository)
   (dependencies
    :initarg :dependencies
    :initform nil
    :reader package-dependencies)
   (development-dependencies
    :initarg :development-dependencies
    :initform nil
    :reader package-development-dependencies)
   (author
    :initarg :author
    :initform nil
    :reader package-author)
   (maintainers
    :initarg :maintainers
    :initform nil
    :reader package-maintainers)))

(defmethod print-object ((package package) stream)
  (print-unreadable-object (package stream :type t)
    (format stream "~A" (package-name package))
    (when-let (version (package-version package))
      (format stream " ~A" (package-version package)))))

(defun make-package-from-metadata (metadata version)
  (let ((package (make-instance 'package)))
    (flet ((get-metadata (&rest accessors)
             (or (apply 'json:get metadata "versions" version accessors)
                 (apply 'json:get metadata accessors))))
      (with-slots (name version description readme readme-filename
                        homepage keywords license repository main scripts
                        dependencies development-dependencies
                        author maintainers) package
        (setf name (get-metadata "name")
              version (get-metadata "version")
              description (get-metadata "description")
              readme (get-metadata "readme")
              readme-filename (get-metadata "readmeFilename")
              homepage (get-metadata "homepage")
              keywords (get-metadata "keywords")
              license (get-metadata "license")
              main (get-metadata "main"))
        (setf repository
              (switch ((get-metadata "repository" "type") :test 'equal)
                ("git" (make-instance 'git-repository
                                      :url (get-metadata "repository" "url")))))
        (when-let (object (get-metadata "author"))
          (setf author (make-instance 'author
                                      :name (json:get object "name")
                                      :email (json:get object "email")
                                      :url (json:get object "url"))))
        (loop for object in (get-metadata "maintainers")
           do (appendf maintainers
                       (list
                        (make-instance 'maintainer
                                       :name (json:get object "name")
                                       :email (json:get object "email")
                                       :url (json:get object "url")))))
        (when-let (object (get-metadata "scripts"))
          (json:do-object (name command object)
            (appendf scripts
                     (list
                      (make-instance 'script
                                     :name name
                                     :command command)))))
        (when-let (object (get-metadata "dependencies"))
          (json:do-object (name version object)
            (appendf dependencies
                     (list (cons name version)))))
        (when-let (object (get-metadata "devDependencies"))
          (json:do-object (name version object)
            (appendf development-dependencies
                     (list (cons name version)))))))
    package))

(defun fetch-package-metadata (name)
  (let ((url (format nil "~A/~A"
                     (registry-endpoint *registry*)
                     name)))
    (multiple-value-bind (stream status)
        (drakma:http-request url
                             :want-stream t
                             :force-binary t
                             :close t)
      (unwind-protect
           (progn
             (when (not (= status 200))
               (error "Registry ~A responsed with status code ~A while fetching metadata for package ~S"
                      *registry* status name))
             (let ((object (json:decode-json stream)))
               (setf (gethash name (registry-metadata *registry*)) object)))
        (close stream)))))

;; NAME is case-sensitive
(defun package (name &optional (version "latest"))
  (check-type name string)
  (check-type version string)
  (multiple-value-bind (metadata present-p)
      (gethash name (registry-metadata *registry*))
    (when (not present-p)
      (setf metadata (fetch-package-metadata name)))
    (let ((version (or (json:get metadata "dist-tags" version)
                       version)))
      (make-package-from-metadata metadata version))))
