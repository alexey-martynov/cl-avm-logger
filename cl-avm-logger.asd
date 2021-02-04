(defsystem cl-avm-logger
  :name "cl-avm-logger"
  :author "Alexey Martynov"
  :depends-on (#:cffi)
  :if-feature (:not :cl-avm-logger)
  :components ((:file "packages")
               (:file "logger" :depends-on ("packages"))
               (:file "console" :depends-on ("logger"))))
