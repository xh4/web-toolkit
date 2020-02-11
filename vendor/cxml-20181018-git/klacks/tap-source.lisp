;;; -*- Mode: Lisp; readtable: runes; -*-
;;;  (c) copyright 2007 David Lichteblau

;;; This library is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU Library General Public
;;; License as published by the Free Software Foundation; either
;;; version 2 of the License, or (at your option) any later version.
;;;
;;; This library is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; Library General Public License for more details.
;;;
;;; You should have received a copy of the GNU Library General Public
;;; License along with this library; if not, write to the
;;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;;; Boston, MA  02111-1307  USA.

(cl:in-package #:cxml)

(defun klacks:make-tapping-source (upstream-source &optional sax-handler)
  (make-instance 'klacks:tapping-source
		 :upstream-source upstream-source
		 :dribble-handler sax-handler))

(defclass klacks:tapping-source (klacks:source)
    ((upstream-source :initarg :upstream-source :accessor upstream-source)
     (dribble-handler :initarg :dribble-handler :accessor dribble-handler)
     (seen-event-p :initform nil :accessor seen-event-p)
     (document-done-p :initform nil :accessor document-done-p)))

(defmethod initialize-instance :after ((instance klacks:tapping-source) &key)
  (let ((s-p (make-instance 'klacksax :source (upstream-source instance))))
    (sax:register-sax-parser (dribble-handler instance) s-p)))


;;; event dribbling

(defun maybe-dribble (source)
  (unless (or (seen-event-p source) (document-done-p source))
    (when (eq (klacks:peek (upstream-source source)) :end-document)
      (setf (document-done-p source) t))
    (klacks:serialize-event (upstream-source source)
			    (dribble-handler source)
			    :consume nil)
    (setf (seen-event-p source) t)))

(defmethod klacks:peek ((source klacks:tapping-source))
  (multiple-value-prog1
      (klacks:peek (upstream-source source))
    (maybe-dribble source)))

(defmethod klacks:peek-value ((source klacks:tapping-source))
  (multiple-value-prog1
      (klacks:peek-value (upstream-source source))
    (maybe-dribble source)))

(defmethod klacks:peek-next ((source klacks:tapping-source))
  (setf (seen-event-p source) nil)
  (multiple-value-prog1
      (klacks:peek-next (upstream-source source))
    (maybe-dribble source)))

(defmethod klacks:consume ((source klacks:tapping-source))
  (maybe-dribble source)
  (multiple-value-prog1
      (klacks:consume (upstream-source source))
    (setf (seen-event-p source) nil)))


;;; loop through

(defmethod klacks:close-source ((source klacks:tapping-source))
  (klacks:close-source (upstream-source source)))

(defmethod klacks:map-attributes (fn (source klacks:tapping-source))
  (klacks:map-attributes fn (upstream-source source)))

(defmethod klacks:map-current-namespace-declarations
    (fn (source klacks:tapping-source))
  (klacks:map-current-namespace-declarations fn (upstream-source source)))

(defmethod klacks:list-attributes ((source klacks:tapping-source))
  (klacks:list-attributes (upstream-source source)))

(defmethod klacks:current-line-number ((source klacks:tapping-source))
  (klacks:current-line-number (upstream-source source)))

(defmethod klacks:current-column-number ((source klacks:tapping-source))
  (klacks:current-column-number (upstream-source source)))

(defmethod klacks:current-system-id ((source klacks:tapping-source))
  (klacks:current-system-id (upstream-source source)))

(defmethod klacks:current-xml-base ((source klacks:tapping-source))
  (klacks:current-xml-base (upstream-source source)))

(defmethod klacks:current-cdata-section-p ((source klacks:tapping-source))
  (klacks:current-cdata-section-p (upstream-source source)))

(defmethod klacks:find-namespace-binding
    (prefix (source klacks:tapping-source))
  (klacks:find-namespace-binding prefix (upstream-source source)))

(defmethod klacks:decode-qname (qname (source klacks:tapping-source))
  (klacks:decode-qname qname (upstream-source source)))
