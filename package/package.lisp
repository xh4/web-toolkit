(in-package :package)

(defclass maintainer ()
  ((name
    :initarg :name
    :initform nil
    :accessor maintainer-name)
   (email
    :initarg :email
    :initform nil
    :accessor maintainer-email)))

(defmethod print-object ((maintainer maintainer) stream)
  (print-unreadable-object (maintainer stream :type t)
    (format stream "~A <~A>"
            (maintainer-name maintainer)
            (maintainer-email maintainer))))

(defclass script ()
  ((name
    :initarg :name
    :initform nil
    :accessor script-name)
   (command
    :initarg :command
    :initform nil
    :accessor script-command)))

(defmethod print-object ((script script) stream)
  (print-unreadable-object (script stream :type t)
    (format stream "~A" (script-name script))))

(defclass distribution-tag ()
  ((version
    :initarg :version
    :initform nil
    :accessor distribution-tag-version)))

(defclass beta (distribution-tag) ())

(defclass latest (distribution-tag) ())

(defclass repository () ())

(defclass git-repository (repository)
  ((url
    :initarg :url
    :initform nil
    :accessor repository-url)))

(defmethod print-object ((repository git-repository) stream)
  (print-unreadable-object (repository stream :type t)
    (format stream "~A~%" (repository-url repository))))

(defclass distribution ()
  ((name
    :initarg :name
    :initform nil
    :accessor distribution-name)
   (version
    :initarg :version
    :initform nil
    :accessor distribution-version)
   (shasum
    :initarg :version
    :initform nil
    :accessor distribution-shasum)
   (tarball
    :initarg :tarball
    :initform nil
    :accessor distribution-tarball)))

(defclass dependency ()
  ((name
    :initarg :name
    :initform nil
    :accessor dependency-name)
   (version
    :initarg :version
    :initform nil
    :accessor dependency-version)))

(defclass package ()
  ((version
    :initarg :version
    :initform nil
    :accessor package-version)
   (name
    :initarg :name
    :initform nil
    :accessor package-name)
   (title
    :initarg :title
    :initform nil
    :accessor package-title)
   (description
    :initarg :description
    :initform nil
    :accessor package-description)
   (homepage
    :initarg :homepage
    :initform nil
    :accessor package-homepage)
   (main
    :initarg :main
    :initform nil
    :accessor package-main)
   (readme
    :initarg :readme
    :initform nil
    :accessor package-readme)
   (readme-filename
    :initarg :readme-filename
    :initform nil
    :accessor package-readme-filename)
   (keywords
    :initarg :keywords
    :initform nil
    :accessor package-keywords)
   (license
    :initarg :license
    :initform nil
    :accessor package-license)
   (scripts
    :initarg :scripts
    :initform nil
    :accessor package-scripts)
   (repository
    :initarg :repository
    :initform nil
    :accessor package-repository)
   (dependencies
    :initarg :dependencies
    :initform nil
    :accessor package-dependencies)
   (development-dependencies
    :initarg :development-dependencies
    :initform nil
    :accessor package-development-dependencies)
   (maintainers
    :initarg :maintainers
    :initform nil
    :accessor package-maintainers)))

(defmethod print-object ((package package) stream)
  (print-unreadable-object (package stream :type t)
    (format stream "~A" (package-name package))
    (when-let (version (package-version package))
      (format stream " ~A" (package-version package)))))

(defun make-package-from-object (object)
  (let ((package (make-instance 'package)))
    (with-slots (name description title main readme readme-filename
                      homepage scripts keywords license repository
                      maintainers) package
      (setf name (json:get object "name")
            description (json:get object "description")
            title (json:get object "title")
            main (json:get object "main")
            readme (json:get object "readme")
            readme-filename (json:get object "readmeFilename")
            homepage (json:get object "homepage")
            keywords (json:get object "keywords")
            license (json:get object "license"))
      (setf repository
            (switch ((json:get object "repository" "type") :test 'equal)
              ("git" (make-instance 'git-repository
                                    :url (json:get object "repository" "url")))))
      (loop for object in (json:get object "maintainers")
         do (appendf maintainers
                     (list
                      (make-instance 'maintainer
                                     :name (json:get object "name")
                                     :email (json:get object "email")))))
      (when-let (scripts (json:get object "scripts"))
        (json:do-object (name command scripts)
          (appendf scripts
                   (list
                    (make-instance 'script
                                   :name name
                                   :command command))))))
    package))

;; NAME is case-sensitive
(defun package (name &optional version)
  (check-type name string)
  (check-type version (or string null))
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
               (error "Registry responsed with status code ~A" status))
             (let ((object (json:decode-json stream)))
               (make-package-from-object object)))
        (close stream)))))
