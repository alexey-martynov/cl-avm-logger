(defpackage :cl-avm-logger
  (:use :cl)
  (:export #:log-message
           #:identifier
           #:*available-backends*
           #:*active-backends*
           #:invalid-priority

           ;; Console backend settings
           #:*priority-style*
           #:*log-stream*
           #:none
           #:console
           #:journal

           ;; Journald backend settings
           ;; This symbols exported on source load
           ;;#+linux
           ;;#:*properties*
           ))
