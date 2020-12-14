(defpackage #:nail/buffer
  (:use #:cl)
  (:import-from #:xsubseq
                #:make-null-concatenated-xsubseqs
                #:coerce-to-string
                #:xsubseq
                #:xnconcf)
  (:export #:buffer
           #:make-buffer
           #:buffer-package
           #:buffer-created-at
           #:add-chunk
           #:buffer-to-string))
(in-package #:nail/buffer)

(defstruct buffer
  package
  (created-at (get-universal-time))
  (xsubseqs (make-null-concatenated-xsubseqs)))

(defun add-chunk (buffer str start end)
  (xnconcf (buffer-xsubseqs buffer)
           (xsubseq str (or start 0) (or end (length str))))
  buffer)

(defun buffer-to-string (buffer)
  (coerce-to-string (buffer-xsubseqs buffer)))
