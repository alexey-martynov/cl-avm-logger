(in-package :cl-avm-logger)

(defparameter *priority-style* 'plain)
(defparameter *log-stream* *error-output*)

(defun console (priority message)
  (when *log-stream*
    (ccase *priority-style*
      (plain
       (format *log-stream* "[~A] ~A~%" (priority-string priority) message))
      (journal
       (format *log-stream* "<~A> ~A~%" (priority-value priority) message))
      (none
       (format *log-stream* "~A~%" message)))))

(push 'console *available-backends*)
(setf *active-backends* '(console))
