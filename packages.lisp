(defpackage :cl-avm-logger
  (:use :cl)
  (:nicknames :syslog)
  (:export #:open-log
           #:close-log
           #:message
           #:message*
           #:format-message
           #:format-message*
           #:set-log-mask
           #:get-log-mask
           #:up-to))
