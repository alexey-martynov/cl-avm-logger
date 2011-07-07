(defpackage :cl-avm-logger
  (:use :cl)
  (:nicknames :syslog)
  (:export #:open-log
           #:close-log
           #:format-message
           #:format-message*
           #:set-log-mask
           #:get-log-mask
           #:up-to))
