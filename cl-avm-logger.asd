(defsystem cl-avm-logger
  :name "cl-avm-logger"
  :author "Alexey Martynov"
  :depends-on (#:cffi)
  :components ((:file "packages")
               (:file "logger" :depends-on ("packages"))
               (:file "console" :depends-on ("logger"))
               ;; Temporary disabled due to insufficient testing
               ;;#-win32
               ;;(:file "syslog" :depends-on ("logger"))
               #+linux
               (:file "journald" :depends-on ("logger"))))
