(defpackage #:nail/packet
  (:use #:cl)
  (:export #:packet
           #:make-packet
           #:packet-level
           #:packet-message))
(in-package #:nail/packet)

(deftype packet-level ()
  '(member :debug :info :warn :error))

(defstruct packet
  (level :info :type packet-level)
  (message nil :type string)
  (created-at (get-universal-time) :type fixnum))
