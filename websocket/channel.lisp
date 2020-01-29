(in-package :websocket)

(defclass channel-class (standard-class)
  ())

(defclass channel ()
  ()
  (:metaclass channel-class))
