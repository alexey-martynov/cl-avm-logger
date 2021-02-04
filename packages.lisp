(defpackage :cl-avm-logger
  (:use :cl)
  (:export #:log-message
          #:*available-backends*
          #:*active-backends*
          #:invalid-priority
          #:*identifier*

          ;; Console backend settings
          #:*priority-style*
          #:*log-stream*
          #:none
          #:console
          #:journal

          ;; Journald backend settings
          #+linux
          #:*properties))
