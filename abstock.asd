(asdf:defsystem "abstock"
  :version "0.5"
  :author "vindarel"
  :license "GPL3"
  :depends-on (
               :bordeaux-threads
               ;; web client
               :dexador
               :mito
               :str
               :cl-slug
               :local-time
               :cl-cron
               ;; :local-time-duration
               :cl-ppcre
               :parse-float
               :jonathan
               :defclass-std
               :pythonic-string-reader
               :group-by

               ;; web app
               :hunchentoot
               :easy-routes
               :djula

               ;; dev
               :log4cl
               :sentry-client.async           ;; ! not in Quicklisp
               :sentry-client.hunchentoot
               ;; :cl-i18n
               )
  :components ((:module "src"
                :components
                ((:file "abstock")
                 (:file "web")
                 (:file "email")
                 (:file "selection")
                 (:file "utils"))))

  :build-operation "program-op"
  :build-pathname "abstock"
  :entry-point "abstock::main"

  :description "Abelujo's DB as a simple website for clients."
  ;; :long-description
  ;; #.(read-file-string
  ;;    (subpathname *load-pathname* "README.md"))
  :in-order-to ((test-op (test-op "abstock-test"))))

;; smaller binaries.
#+sb-core-compression
(defmethod asdf:perform ((o asdf:image-op) (c asdf:system))
  (uiop:dump-image (asdf:output-file o c) :executable t :compression t))
