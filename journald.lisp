(in-package :cl-avm-logger)

(cffi:define-foreign-library systemd-lib
  (:linux (:or "libsystemd.so.0" "libsystemd.so")))

(cffi:use-foreign-library systemd-lib)

(cffi:defcstruct iovec
  "iovec"
  (base :pointer)
  (len :unsigned-long))

(cffi:defcfun sd-journal-sendv :int
  (iov :pointer)
  (n :int))

(defun journald (priority message)
  (let ((args (do* ((item *properties* (cdr item))
                    (processed '("PRIORITY" "MESSAGE"))
                    (result (list (format nil "PRIORITY=~A" (priority-value priority))
                                  (format nil "MESSAGE=~A" message))))
                   ((null item) (nreverse result))
                (unless (member (caar item) processed :test #'string=)
                  (push (format nil "~A=~A" (caar item) (cdar item)) result)
                  (push (caar item) processed)))))
    (when *identifier*
      (setf args (cons (format nil "SYSLOG_IDENTIFIER=~A" *identifier*) args)))
    (let ((items (length args))
          (index 0))
      (cffi:with-foreign-object (data '(:struct iovec) items)
        (mapc (lambda (str)
                (setf (cffi:foreign-slot-value (cffi:mem-aptr data '(:struct iovec) index) '(:struct iovec) 'base)
                      (cffi:foreign-string-alloc str))
                (setf (cffi:foreign-slot-value (cffi:mem-aptr data '(:struct iovec) index) '(:struct iovec) 'len)
                      (length str))
                (incf index))
              args)
        (unwind-protect
             (sd-journal-sendv data items)
          (setf index 0)
          (mapc (lambda (str)
                  (declare (ignore str))
                  (cffi:foreign-string-free (cffi:foreign-slot-value (cffi:mem-aptr data '(:struct iovec) index) '(:struct iovec) 'base))
                  (incf index))
                args)))))
  nil)

(push 'journald *available-backends*)
(setf *active-backends* '(journald))

(pushnew :cl-avm-logger-journald *features*)
