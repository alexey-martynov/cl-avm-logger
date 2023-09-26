(defpackage :cl-avm-logger
  (:use :cl :local-time)
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
           #:*timestamp-format*
           #:*timestamp-timezone*
           #:*log-stream*
           #:none
           #:console
           #:plain
           #:journal
           #:journal-with-timestamp

           #:+iso-8601-format-with-space+
           ))
