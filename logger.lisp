(in-package :cl-avm-logger)

(defparameter *available-backends* '() "Available backends for logging")
(defparameter *active-backends* '() "Currently active backends for logging")

(defparameter *identifier* nil "The program identifier to use in logs.")

(defparameter *properties* nil "AList of additional properties to add to message. The key is string
 with name and value will be formatted via FORMAT. Standard parameters will be added automatically.")

(declaim (ftype (function () (or null string)) identifier))
(defun identifier ()
  "Obtain string which is currently used as identifier of program."
  *identifier*)

(declaim (ftype (function ((or null string)) (or null string)) (setf identifier)))
(defun (setf identifier) (identifier)
  "Set new program identifier. Returns IDENTIFIER."
  (setf *identifier* identifier)
  identifier)

(defmacro with-log-properties ((&rest args) &body body)
  (let ((bindings (do* ((a args (cdr a)) (l nil))
                       ((null a) (nreverse l))
                    (when (oddp (length (car a)))
                      (error "Odd number of property assignment to WITH-LOG-PROPERTIES : ~s." a))
                    (push `(cons ,(caar a) ,(cadar a)) l))))
    (if bindings
        `(let ((*properties* (list ,@bindings)))
           ,@body)
        `(progn ,@body))))

(defmacro with-additional-log-properties ((&rest args) &body body)
  (let ((bindings (do* ((a args (cdr a)) (l '*properties*))
                       ((null a) l)
                    (when (oddp (length (car a)))
                      (error "Odd number of property assignment to WITH-ADDITIONAL-LOG-PROPERTIES : ~s." a))
                    (setf l `(cons (cons ,(caar a) ,(cadar a)) ,l)))))
    (if bindings
        `(let ((*properties* ,bindings))
           ,@body)
        `(progn ,@body))))

(defun log-message (priority message &rest args)
  "Create log entry with all *ACTIVE-BACKENDS*. MESSAGE will be formatted with ARGS according
 to FORMAT rules."
  (when *active-backends*
    (let ((message (apply #'format nil message args)))
      (mapc (lambda (backend) (funcall backend priority message)) *active-backends*)))
  nil)

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
