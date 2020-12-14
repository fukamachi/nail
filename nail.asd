(defsystem "nail"
  :version "0.1.0"
  :author "Eitaro Fukamachi"
  :license "BSD 3-Clause"
  :description "User configurable logger for Common Lisp"
  :depends-on ("trivial-gray-streams"
               "xsubseq"
               "dissect")
  :pathname "src"
  :components
  ((:file "main" :depends-on ("logger"))
   (:file "logger" :depends-on ("packet" "stream" "stack"))
   (:file "packet")
   (:file "stream" :depends-on ("packet" "stack" "buffer"))
   (:file "stack")
   (:file "buffer")))

(register-system-packages "nail" '(#:nai))
