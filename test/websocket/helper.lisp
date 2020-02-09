(in-package :websocket-test)

(defun find-executable (name &rest optional-names)
  (when-let ((path (uiop:getenv "PATH")))
    (let ((paths #+win32 (split-sequence #\; path)
                 #+unix (split-sequence #\: path)))
      (loop for path in paths
         for exe-path = (loop for name in (append (list name) optional-names)
                           for exe-path = (merge-pathnames
                                           name
                                           (cl-fad:pathname-as-directory path))
                           when (probe-file exe-path)
                           do (return exe-path))
         when exe-path
         do (return exe-path)))))
