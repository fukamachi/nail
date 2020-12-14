(defpackage #:nail/logger
  (:use #:cl
        #:nail/stack)
  (:import-from #:nail/packet
                #:make-packet
                #:packet-level
                #:packet-created-at
                #:packet-message)
  (:import-from #:nail/stream
                #:nail-stream
                #:nail-stream-output-stream)
  (:export #:logger
           #:make-logger
           #:make-formatter
           #:make-info-stream
           #:make-debug-stream
           #:make-warn-handler
           #:make-error-handler))
(in-package #:nail/logger)

(defun default-formatter (packet package stream)
  (multiple-value-bind (sec min hour)
      (decode-universal-time (packet-created-at packet))
    (format stream "~:[~;~:*~A | ~]<~:@(~A~)> [~2,'0D:~2,'0D:~2,'0D] ~A"
            (and package (string-downcase (package-name package)))
            (packet-level packet)
            hour min sec
            (packet-message packet))))

(defclass logger () ())

(defun make-logger (&rest initargs &key formatter)
  (declare (ignore formatter))
  (apply #'make-instance 'logger initargs))

(defgeneric make-formatter (logger)
  (:method ((logger logger))
    #'default-formatter))

(defgeneric make-info-stream (logger &key output-stream)
  (:method :around ((logger logger) &key output-stream)
    (when (typep output-stream 'nail-stream)
      (setf output-stream (nail-stream-output-stream output-stream)))
    (call-next-method logger :output-stream output-stream))
  (:method ((logger logger) &key output-stream)
    (make-instance 'nail-stream
                   :level :info
                   :output-stream (or output-stream *standard-output*)
                   :formatter (make-formatter logger))))

(defgeneric make-debug-stream (logger &key output-stream)
  (:method :around ((logger logger) &key output-stream)
    (when (typep output-stream 'nail-stream)
      (setf output-stream (nail-stream-output-stream output-stream)))
    (call-next-method logger :output-stream output-stream))
  (:method ((logger logger) &key output-stream)
    (make-instance 'nail-stream
                   :level :debug
                   :output-stream (or output-stream *trace-output*)
                   :formatter (make-formatter logger))))

(defgeneric make-warn-handler (logger &key output-stream)
  (:method ((logger logger) &key output-stream)
    (let ((stream (or output-stream *error-output*)))
      (lambda (e)
        (funcall (make-formatter logger)
                 (make-packet
                   :level :warn
                   :message (princ-to-string e))
                 (called-package)
                 stream)
        (terpri stream)
        (muffle-warning e)))))

(defgeneric make-error-handler (logger &key output-stream)
  (:method ((logger logger) &key output-stream)
    (let ((stream (or output-stream *error-output*)))
      (lambda (e)
        (funcall (make-formatter logger)
                 (make-packet
                   :level :error
                   :message (format nil "~A: ~A" (type-of e) e))
                 (called-package)
                 stream)
        (terpri stream)))))
