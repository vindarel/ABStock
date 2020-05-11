(in-package :abstock/loaders)

(define-condition unknown-key-warning (simple-warning)
  ((reason :initform "This key is not a known field"
           :accessor warn-reason)
   (key :accessor warn-key :initarg :key))
  (:documentation "A warning thrown when reading card data and a key is unknown."))

(defmethod print-condition ((c simple-warning))
  "Print and explain this warning on error output."
  (format t "~a~&" c))

(defmethod print-condition ((c unknown-key-warning))
  (format t "~&warn: ~a: ~a~&" (warn-reason c) (warn-key c)))

(defmethod ignore-condition ((c simple-warning))
  nil)

(defmethod ignore-condition ((c unknown-key-warning))
  nil)

(defmethod raise-condition ((c unknown-key-warning))
  (error (format nil "~a: ~a" (warn-reason c) (warn-key c))))
