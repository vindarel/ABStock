
(in-package :abstock)

(defun save-pid (&optional (filename "PID.txt"))
  "Save the current Lisp PID to this file.
  Necessary to send signals with external processes."
  (let ((pid
         #+sbcl
          (sb-posix:getpid)
          #-sbcl
          (osicat-posix:getpid)))
    (with-open-file (f filename
                       :direction :output
                       :if-does-not-exist :create
                       :if-exists :supersede)
      (princ pid f))))
