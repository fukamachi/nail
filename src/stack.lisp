(defpackage #:nail/stack
  (:use #:cl)
  (:export #:get-prev-stack
           #:called-package))
(in-package #:nail/stack)

(defun get-prev-stack ()
  (labels ((stack-call (stack)
             (let ((call (dissect:call stack)))
               (typecase call
                 (symbol call)
                 (cons
                   (case (first call)
                     (:method (second call))
                     ((lambda flet labels) nil)
                     (otherwise (second call)))))))
           #+sbcl
           (sbcl-package-p (package)
             (let ((name (package-name package)))
               (eql (mismatch "SB-" name) 3)))
           (system-call-p (call)
             (when call
               (let ((package (symbol-package call)))
                 (or #+sbcl (sbcl-package-p package)
                     (find (package-name package)
                           '(:nail/stack :nail/stream :swank :swank-repl :swank/sbcl :common-lisp)
                           :test #'string=)))))
           (users-stack-p (stack)
             (let ((call (stack-call stack)))
               (and call
                    (or (not (symbolp call))
                        (not (system-call-p call)))))))

    (loop for stack in (dissect:stack)
          when (users-stack-p stack)
            do (return (stack-call stack)))))

(defun called-package ()
  (let ((stack (get-prev-stack)))
    (and stack
         (symbol-package stack))))
