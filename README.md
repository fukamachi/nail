# nail

Experimental project to implement 'logger' just as a 'stream' to keep the application/library independent of any logger libraries.

## Usage

```common-lisp
(ql:quickload :nail)

(defparameter *logger*
  (make-instance 'nail:logger :level :debug))

(defun main ()
  (write-line "Start.")
  (write-line "Just a debug note." *trace-output*)
  (write-line "Bye."))

;; Without logger
(main)
;-> Start.
;   Just a debug note.
;   Bye.

;; With logger
(nail:with-logger *logger*
  (main))

;-> myapp | <INFO> [11:03:14] Start.
;   myapp | <DEBUG> [11:03:15] Just a debug note.
;   myapp | <INFO> [11:12:23] Bye.
```

## Author

* Eitaro Fukamachi (e.arrows@gmail.com)

## Copyright

Copyright (c) 2020 Eitaro Fukamachi (e.arrows@gmail.com)

## License

Licensed under the BSD 3-Clause License.
