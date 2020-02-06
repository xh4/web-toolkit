(in-package :http)

(defclass stream (fundamental-binary-input-stream
                  fundamental-binary-output-stream)
  ((upstream
    :initarg :upstream
    :initform nil
    :accessor upstream)
   (length
    :initarg :length
    :initform nil
    :accessor stream-length)
   (readed-length
    :initform 0
    :accessor stream-readed-length)
   (chunking-p
    :initarg chunking-p
    :initform nil
    :accessor stream-chunking-p)))

(defmethod stream-element-type ((stream stream))
  '(unsigned-byte 8))

(defmethod open-stream-p ((stream stream))
  (open-stream-p (upstream stream)))

(defmethod close ((stream stream) &key abort)
  (with-slots (upstream) stream
    (cond ((open-stream-p upstream)
           (close upstream :abort abort))
          (t nil))))

(defmethod trivial-gray-streams:stream-read-byte ((stream stream))
  (with-slots (readed-length) stream
    (prog1
        (read-byte (upstream stream))
      (incf readed-length))))

(defmethod trivial-gray-streams:stream-read-sequence ((stream stream) sequence start end &key)
  (with-slots (readed-length) stream
    (let ((position (read-sequence sequence (upstream stream) :start start :end end)))
      (let ((n (- position start)))
        (prog1
            position
          (incf readed-length n))))))

(defgeneric stream-remaining-length (stream)
  (:method ((stream stream))
    (with-slots (length readed-length chunking-p) stream
      (unless chunking-p
        (- length readed-length)))))

(defgeneric stream-read-remain (stream)
  (:method ((stream stream))
    (when-let ((remaining-length (stream-remaining-length stream)))
      ;; TODO: use smaller buffer
      (alexandria::read-stream-content-into-byte-vector
       stream
       'alexandria::%length remaining-length))))
