(in-package :cl-avm-logger)

(defparameter *available-backends* '() "Available backends for logging")
(defparameter *active-backends* '() "Currently active backends for logging")

(defparameter *identifier* nil "The program identifier to use in logs.")

(defparameter *properties* nil "AList of additional properties to add to message. The key is string
 with name and value will be formatted via FORMAT. Standard parameters will be added automatically.")

(declaim (ftype (function () (or null string)) identifier))
(defun identifier ()
  *identifier*)

(declaim (ftype (function ((or null string)) (or null string)) (setf identifier)))
(defun (setf identifier) (identifier)
  (setf *identifier* identifier)
  identifier)

(defun log-message (priority message &rest args)
  (when *active-backends*
    (let ((message (apply #'format nil message args)))
      (mapc (lambda (backend) (funcall backend priority message)) *active-backends*)))
  nil)

;; TODO: Add function to set identifier to all backends

(define-condition invalid-priority (error)
  ((priority
    :reader priority
    :initarg :priority))
  (:report (lambda (condition stream)
             (format stream "Invalid priority ~A." (priority condition)))))

(defparameter +priorities+
  '((:emerg . (0 . "EMERG"))
    (:alert . (1 . "ALERT"))
    (:critical . (2 . "CRITICAL"))
    (:error . (3 . "ERROR"))
    (:warning . (4 . "WARN"))
    (:notice . (5 . "NOTICE"))
    (:info . (6 . "INFO"))
    (:debug . (7 . "DEBUG"))))

(defun priority-string (priority)
  (or (cddr (assoc priority +priorities+))
      (error (make-condition 'invalid-priority :priority priority))))

(defun priority-value (priority)
  (or (cadr (assoc priority +priorities+))
      (error (make-condition 'invalid-priority :priority priority))))

(pushnew :cl-avm-logger *features*)
