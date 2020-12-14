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
                     ((lambda flet labels)
                      (let ((in (getf (cddr call) :in)))
                        (and (symbolp in) in)))
                     (otherwise (second call)))))))
           #+sbcl
           (sbcl-package-p (name)
             (eql (mismatch "SB-" name) 3))
           (uiop-package-p (name)
             (eql (mismatch "UIOP/" name) 5))
           (nail-package-p (name)
             (or (string= name "NAI")
                 (eql (mismatch "NAIL/" name) 5)))
           (system-call-p (call)
             (when call
               (let ((package-name (package-name (symbol-package call))))
                 (or #+sbcl (sbcl-package-p package-name)
                     (uiop-package-p package-name)
                     (nail-package-p package-name)
                     (find package-name
                           '(:swank :swank-repl :swank/sbcl :common-lisp)
                           :test #'string=)))))
           (users-call-p (call)
             (and call
                  (or (not (symbolp call))
                      (not (system-call-p call))))))
    (declare (inline sbcl-package-p uiop-package-p nail-package-p))

    (loop for stack in (dissect:stack)
          for call = (stack-call stack)
          when (users-call-p call)
          do (return call))))

(defun called-package ()
  (let ((stack (get-prev-stack)))
    (and stack
         (symbol-package stack))))
