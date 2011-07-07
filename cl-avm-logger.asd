(defsystem cl-avm-logger
  :name "cl-avm-logger"
  :author "Alexey Martynov"
  :depends-on (#:cffi)
  :components ((:file "packages")
               (:file "logger" :depends-on ("packages"))))
