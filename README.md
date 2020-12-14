# nail

Experimental project to implement 'logger' just as a 'stream' to keep the application/library independent of any logger libraries.

## Usage

```common-lisp
(ql:quickload :nail)

(defparameter *logger*
  (make-instance 'nail:logger :level :debug)

(nail:with-logger *logger*
  ;; Some application call which outputs logs to *standard-output* and *trace-output*.
  (myapp:main))
;-> myapp | <INFO> [11:03:14] Starting a server.
;-> myapp | <INFO> [11:12:23] Shutting down...
```

## Author

* Eitaro Fukamachi (e.arrows@gmail.com)

## Copyright

Copyright (c) 2020 Eitaro Fukamachi (e.arrows@gmail.com)

## License

Licensed under the BSD 3-Clause License.
