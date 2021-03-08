(defsystem cl-avm-logger
  :name "cl-avm-logger"
  :description "Simple logging framework with console/file and journald support"
  :author "Alexey Martynov"
  :license "MIT"
  :depends-on (#:cffi)
  :if-feature (:not :cl-avm-logger)
  :components ((:file "packages")
               (:file "logger" :depends-on ("packages"))
               (:file "console" :depends-on ("logger")))
               ;; Temporary disabled due to insufficient testing
               ;;(:file "syslog" :depends-on ("logger") :if-feature (:not :win32))
  )
