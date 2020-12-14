(defpackage #:nail/stream
  (:use #:cl
        #:nail/stack)
  (:import-from #:nail/packet
                #:make-packet)
  (:import-from #:nail/buffer
                #:make-buffer
                #:buffer-package
                #:buffer-created-at
                #:add-chunk
                #:buffer-to-string)
  (:import-from #:trivial-gray-streams
                #:trivial-gray-stream-mixin
                #:fundamental-character-output-stream
                #:stream-write-string
                #:stream-write-char)
  (:export #:nail-stream
           #:nail-stream-output-stream
           #:nail-stream-level
           #:nail-stream-formatter))
(in-package #:nail/stream)

(defclass nail-stream (trivial-gray-stream-mixin fundamental-character-output-stream)
  ((output-stream :initarg :output-stream
                  :accessor nail-stream-output-stream)
   (level :initarg :level
          :accessor nail-stream-level)
   (formatter :initarg :formatter
              :accessor nail-stream-formatter)

   (buffer :initform nil)))

(defun buffer-to-packet (stream)
  (with-slots (level buffer) stream
    (make-packet :level level
                 :message (buffer-to-string buffer)
                 :created-at (buffer-created-at buffer))))

(defun line-end-p (str end)
  (let ((len (length str)))
    (and (/= len 0)
         (char= (aref str (or end (1- len))) #\Linefeed))))

(defun flush-buffer (stream)
  (let ((packet (buffer-to-packet stream)))
    (when packet
      (with-slots (formatter output-stream buffer) stream
        (let ((*standard-output* output-stream))
          (fresh-line output-stream)
          (funcall formatter packet (buffer-package buffer) output-stream)
          (fresh-line output-stream)))))
  (values))

(defmethod stream-write-string ((stream nail-stream) str &optional start end)
  (with-slots (buffer) stream
    (let ((stack (get-prev-stack)))
      (cond
        ((null buffer)
         (setf buffer (make-buffer :package (symbol-package stack))))
        ((eq (symbol-package stack) (buffer-package buffer))
         (flush-buffer stream)
         (setf buffer (make-buffer :package (symbol-package stack)))))

      (add-chunk buffer str start end)
      (when (line-end-p str end)
        (flush-buffer stream)
        (setf buffer nil)))))

(defmethod stream-write-char ((stream nail-stream) char)
  (if (char= char #\Newline)
      (with-slots (buffer) stream
        (when buffer
          (flush-buffer stream)
          (setf buffer nil)))
      (stream-write-string stream (make-string 1 :initial-element char))))
