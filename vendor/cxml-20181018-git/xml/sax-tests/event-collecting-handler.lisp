(cl:in-package #:sax-tests)

(defclass event-collecting-handler ()
  ((event-list :initform '() :accessor event-list)))

(defmethod start-document ((handler event-collecting-handler))
  (push (list :start-document) (event-list handler)))

(defmethod start-element ((handler event-collecting-handler) ns-uri local-name qname attrs)
  (push (list :start-element ns-uri local-name qname attrs)
	(event-list handler)))

(defmethod start-prefix-mapping ((handler event-collecting-handler) prefix uri)
  (push (list :start-prefix-mapping prefix uri)
	(event-list handler)))

(defmethod characters ((handler event-collecting-handler) data)
  (push (list :characters data)
	(event-list handler)))

(defmethod processing-instruction ((handler event-collecting-handler) target data)
  (push (list :processing-instruction target data)
	(event-list handler)))

(defmethod end-prefix-mapping ((handler event-collecting-handler) prefix)
  (push (list :end-prefix-mapping prefix)
	(event-list handler)))

(defmethod end-element ((handler event-collecting-handler) namespace-uri local-name qname)
  (push (list :end-element namespace-uri local-name qname)
	(event-list handler)))

(defmethod end-document ((handler event-collecting-handler))
  (push (list :end-document)
	(event-list handler))

  (nreverse (event-list handler)))
