(defpackage :cl-avm-logger
  (:use :cl)
  (:export #:log-message
           #:identifier
           #:*properties*
           #:with-log-properties
           #:with-additional-log-properties
           #:*available-backends*
           #:*active-backends*
           #:invalid-priority

           ;; Console backend settings
           #:*priority-style*
           #:*log-stream*
           #:none
           #:console
           #:journal
           ))
