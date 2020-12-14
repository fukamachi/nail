(defpackage #:nail/logger
  (:use #:cl
        #:nail/stack)
  (:import-from #:nail/packet
                #:make-packet
                #:packet-level
                #:packet-created-at
                #:packet-message)
  (:import-from #:nail/stream
                #:*enable-logger*
                #:nail-stream
                #:nail-stream-output-stream)
  (:export #:base-logger
           #:logger
           #:logger-level
           #:make-formatter
           #:make-info-stream
           #:make-debug-stream
           #:make-warn-handler
           #:make-error-handler))
(in-package #:nail/logger)

(defun extract-app-name (package-name)
  (when package-name
    (let ((/-pos (position #\/ package-name :test #'char=)))
      (if /-pos
          (subseq package-name 0 /-pos)
          package-name))))

(defun default-formatter (packet package stream)
  (let* ((package-name (and package
                            (string-downcase (package-name package))))
         (app-name (extract-app-name package-name)))
    (multiple-value-bind (sec min hour)
        (decode-universal-time (packet-created-at packet))
      (format stream "~&~:[~;~:*~A | ~]<~:@(~A~)> [~2,'0D:~2,'0D:~2,'0D] ~A~%"
              app-name
              (packet-level packet)
              hour min sec
              (packet-message packet)))))

(defclass base-logger () ())

(defgeneric make-formatter (logger)
  (:method ((logger base-logger))
    #'default-formatter))

(defgeneric make-info-stream (logger &key output-stream)
  (:method :around ((logger base-logger) &key output-stream)
    (when (typep output-stream 'nail-stream)
      (setf output-stream (nail-stream-output-stream output-stream)))
    (call-next-method logger :output-stream output-stream))
  (:method ((logger base-logger) &key output-stream)
    (make-instance 'nail-stream
                   :level :info
                   :output-stream (or output-stream *standard-output*)
                   :formatter (make-formatter logger))))

(defgeneric make-debug-stream (logger &key output-stream)
  (:method :around ((logger base-logger) &key output-stream)
    (when (typep output-stream 'nail-stream)
      (setf output-stream (nail-stream-output-stream output-stream)))
    (call-next-method logger :output-stream output-stream))
  (:method ((logger base-logger) &key output-stream)
    (make-instance 'nail-stream
                   :level :debug
                   :output-stream (or output-stream *trace-output*)
                   :formatter (make-formatter logger))))

(defgeneric make-warn-handler (logger &key output-stream)
  (:method ((logger base-logger) &key output-stream)
    (let ((stream (or output-stream *error-output*)))
      (lambda (e)
        (when *enable-logger*
          (funcall (make-formatter logger)
                   (make-packet
                     :level :warn
                     :message (princ-to-string e))
                   (called-package)
                   stream)
          (muffle-warning e))))))

(defgeneric make-error-handler (logger &key output-stream)
  (:method ((logger base-logger) &key output-stream)
    (let ((stream (or output-stream *error-output*)))
      (lambda (e)
        (when *enable-logger*
          (funcall (make-formatter logger)
                   (make-packet
                     :level :error
                     :message (format nil "~A: ~A" (type-of e) e))
                   (called-package)
                   stream))))))

;;
;; Level logger

(defclass logger (base-logger)
  ((level :initarg :level)))

(declaim (inline level-to-int int-to-level))

(defun level-to-int (level)
  (ecase level
    (:debug 1)
    (:info 2)
    (:warn 3)
    (:error 4)))

(defun int-to-level (int)
  (ecase int
    (1 :debug)
    (2 :info)
    (3 :warn)
    (4 :error)))

(defmethod initialize-instance :around ((logger logger) &key (level :info) &allow-other-keys)
  (call-next-method logger :level (level-to-int level)))

(defmethod make-formatter :around ((logger logger))
  (let ((inner (call-next-method)))
    (lambda (packet package stream)
      (when (<= (slot-value logger 'level)
                (level-to-int (packet-level packet)))
        (funcall inner packet package stream)))))

(defun logger-level (logger)
  (int-to-level (slot-value logger 'level)))

(defun (setf logger-level) (new-level logger)
  (level-to-int new-level))
