(in-package :cl-avm-logger)

(cffi:defcfun ("openlog" c-openlog) :void
  (ident :string)
  (option :int)
  (facility :int))

(cffi:defcfun ("closelog" c-closelog) :void)

(cffi:defcfun ("syslog" c-syslog) :void
  (priority :int)
  (format :string))

(cffi:defcfun ("setlogmask" c-setlogmask) :int
  (priorities :int))

(defparameter +facilities+ (mapcar #'(lambda (item) (cons (car item) (ash (cdr item) 3)))
                                   '((:kern . 0)
                                     (:user . 1)
                                     (:mail . 2)
                                     (:daemon . 3)
                                     (:auth . 4)
                                     (:syslog . 5)
                                     (:lpr . 6)
                                     (:news . 7)
                                     (:uucp . 8)
                                     (:cron . 9)
                                     (:authpriv . 10)
                                     (:ftp . 11)
                                     (:local0 . 16)
                                     (:local1 . 17)
                                     (:local2 . 18)
                                     (:local3 . 19)
                                     (:local4 . 20)
                                     (:local5 . 21)
                                     (:local6 . 22)
                                     (:local7 . 23))))

(defparameter +flags+
  '((:pid . #x01)
    (:cons . #x02)
    (:odelay . #x04)
    (:ndelay . #x08)
    (:perror . #x20)))

(define-condition invalid-facility (error)
  ((facility
    :reader facility
    :initarg :facility))
  (:report (lambda (condition stream)
             (format stream "Invalid facility ~A." (facility condition)))))

(define-condition invalid-flag (error)
  ((flag
    :reader flag
    :initarg :flag))
  (:report (lambda (condition stream)
             (format stream "Invalid flag ~A." (flag condition)))))

(defun get-facility (facility)
  (or (cdr (assoc facility +facilities+))
      (error (make-condition 'invalid-facility :facility facility))))

(defun get-flag (flag)
  (or (cdr (assoc flag +flags+))
      (error (make-condition 'invalid-flag :flag flag))))

(defvar *c-name* nil)

(defun open-log (name facility &optional options)
  (let ((flags (reduce #'(lambda (result flag)
                            (logior result (get-flag flag)))
                        options
                        :initial-value 0)))
    (when *c-name*
      (cffi:foreign-string-free *c-name*))
    (setf *c-name* (cffi:foreign-string-alloc name))
    (c-openlog *c-name* flags (get-facility facility))))

(defun close-log ()
  (c-closelog)
  (when *c-name*
    (cffi:foreign-string-free *c-name*)))

(defun message* (facility priority text)
  (let ((message (with-output-to-string (result)
                   (do ((idx 0 (1+ idx))
                        (len (length text)))
                       ((= idx len))
                     (if (char= (char text idx) #\%)
                         (write-string "%%" result)
                         (write-char (char text idx) result))))))
    (cffi:with-foreign-string (ctext message)
      (c-syslog (logior (priority-value priority)
                        (if facility (get-facility facility) 0))
                ctext))
    text))

(defun message (priority text)
  (message* '() priority text))

(defmacro format-message* (facility priority text &body body)
  "Format message to syslog."
  ;; FIXME: Order of evaluation for parameters
  (let ((message (gensym))
        (source (gensym)))
    `(let ((prio (priority-value ,priority)))
       (when (/= 0 (logand (c-setlogmask 0) (ash 1 prio)))
         (let ((,source (with-output-to-string (str)
                          (let ((*print-pretty* nil))
                            (format str ,text ,@body)))))
           (let ((,message (with-output-to-string (result)
                             (do ((idx 0 (1+ idx))
                                  (len (length ,source)))
                                 ((= idx len))
                               (if (char= (char ,source idx) #\%)
                                   (write-string "%%" result)
                                   (write-char (char ,source idx) result))))))
             (cffi:with-foreign-string (cmessage ,message)
               (c-syslog (logior prio
                                 (if ,facility (get-facility ,facility) 0))
                         cmessage))
             ,source))))))

(defmacro format-message (priority text &body body)
  "Format message to syslog.
'option' can be any of the +log...+ constants"
  `(format-message* '() ,priority ,text ,@body))

(defun set-log-mask (priorities)
  "Set priorities mask for logging.
'priorities' must be a list of symbols of priorities"
  (let ((mask (reduce #'(lambda (result prio)
                          (logior result (ash 1 prio)))
                      (mapcar #'priority-value priorities)
                      :initial-value 0)))
    (when (/= 0 mask)
      (c-setlogmask mask))))

(defun get-log-mask ()
  (let ((mask (c-setlogmask 0)))
    (reduce #'(lambda (result item)
                (if (/= 0 (logand mask (ash 1 (cdr item))))
                    (cons (car item) result)
                    result))
            +priorities+
            :initial-value '())))

(defun up-to (priority)
  "Make list of priorities that higher or equal to 'priority'"
  (cdr (reduce #'(lambda (result item)
                   (if (car result)
                       result
                       (cons (eq (car item) priority)
                             (cons (car item) (cdr result)))))
               +priorities+
               :initial-value '(nil . ()))))

(pushnew :avm-logger *features*)
(pushnew :syslog *features*)
