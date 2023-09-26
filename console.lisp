(in-package :cl-avm-logger)

(deftype priority-styles () '(member none plain journal journal-with-timestamp))

(defparameter +iso-8601-format-with-space+ '((:YEAR 4) #\- (:MONTH 2) #\- (:DAY 2) #\Space (:HOUR 2) #\: (:MIN 2) #\: (:SEC 2)
                                             #\. (:USEC 6) :GMT-OFFSET-OR-Z))

(declaim (type (priority-styles) *priority-style*))
(defparameter *priority-style* 'plain)
(defparameter *timestamp-format* +iso-8601-format-with-space+)
(defparameter *timestamp-zone* +utc-zone+)
(defparameter *log-stream* *error-output*)

(defun console (priority message)
  (when *log-stream*
    (let ((timestamp (when *timestamp-format*
                         (format-timestring nil (now) :format *timestamp-format* :timezone *timestamp-zone*)
                         )))
      (ccase *priority-style*
        (plain
         (format *log-stream* "~&~:[~;~0@*~A ~][~A] ~A~%" timestamp (priority-string priority) message))
        (journal
         (format *log-stream* "~&<~A> ~A~%" (priority-value priority) message))
        (journal-with-timestamp
         (format *log-stream* "~&<~A> ~:[~;~1@*~A ~]~A~%" (priority-value priority) timestamp message))
        (none
         (format *log-stream* "~&~&~:[~;~0@*~A ~]~A~%" timestamp message))))))

(push 'console *available-backends*)
(setf *active-backends* '(console))

(pushnew :cl-avm-logger-console *features*)
