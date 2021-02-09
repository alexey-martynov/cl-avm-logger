(defpackage :cl-avm-logger
  (:use :cl)
  (:export #:log-message
           #:identifier
           #:*properties*
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
