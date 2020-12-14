(defpackage #:nail
  (:nicknames #:nail/main)
  (:use #:cl)
  (:import-from #:nail/logger
                #:logger
                #:logger-level
                #:make-formatter
                #:make-info-stream
                #:make-debug-stream
                #:make-warn-handler
                #:make-error-handler)
  (:export #:with-logger
           #:logger
           #:logger-level
           #:make-formatter
           #:make-info-stream
           #:make-debug-stream
           #:make-warn-handler
           #:make-error-handler))
(in-package #:nail)

(defvar *logger* nil)

(defmacro with-logger (logger &body body)
  (let ((warn-handler (gensym "WARN-HANDLER"))
        (error-handler (gensym "ERROR-HANDLER")))
    `(if *logger*
         (progn ,@body)
         (let* ((*logger* ,logger)
                (,warn-handler (make-warn-handler *logger*))
                (,error-handler (make-error-handler *logger*)))
           (handler-bind ((warning ,warn-handler)
                          (error ,error-handler))
             (let ((*standard-output* (make-info-stream *logger*
                                                        :output-stream *standard-output*))
                   (*trace-output* (make-debug-stream *logger*
                                                      :output-stream *trace-output*)))
               ,@body))))))

(defpackage #:nai
  (:use #:cl)
  (:shadow #:debug)
  (:export #:debug
           #:info))
(in-package #:nai)

(defun debug (message)
  (write-line message *trace-output*))

(defun info (message)
  (write-line message *standard-output*))
