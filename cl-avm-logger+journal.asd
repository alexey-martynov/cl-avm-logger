(defsystem cl-avm-logger+journal
  :name "cl-avm-logger"
  :author "Alexey Martynov"
  :depends-on (#:cffi)
  :if-feature (:and :linux (:not :cl-avm-logger))
  :components ((:file "packages")
               (:file "logger" :depends-on ("packages"))
               (:file "console" :depends-on ("logger"))
               ;; Temporary disabled due to insufficient testing
               ;;#-win32
               ;;(:file "syslog" :depends-on ("logger"))
               (:file "journald" :depends-on ("logger"))))
