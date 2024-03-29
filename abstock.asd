(require "asdf")
(asdf:defsystem "abstock"
  :version "0.11"                        ; duplicated in src/abstock.lisp
  :author "vindarel"
  :license "GPL3"
  :depends-on (
               ;; use CIEL !
               :bordeaux-threads
               :swank
               ;; web client
               :dexador
               :mito
               :str
               :cl-slug
               :local-time
               :cl-cron
               :function-cache
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

               ;; web scraping
               :lquery ;; datasources/dilicom

               ;; scripting
               :unix-opts
               :cl-ansi-text

               ;; utils
               :arrows
               :serapeum

               ;; dev
               :log4cl
               :sentry-client.async ;; ! not in Quicklisp
               :sentry-client.hunchentoot
               ;; :cl-i18n

               ;; others
               :uuid                    ;; already in dependencies of dependencies.
               )
  :components ((:module "src/loaders"
                :components
                ((:file "package")
                 (:file "conditions")
                 (:file "txt-loader")))
               (:module "src/datasources"
                :components
                ((:file "packages")
                 (:file "librairiedeparis")))
               (:module "src"
                :components
                ((:file "abstock")
                 (:file "pagination")
                 (:file "currencies")
                 (:file "utils")
                 (:file "user-content")
                 (:file "parameters")
                 (:file "web")
                 (:file "email")
                 (:file "selection")
                 (:file "api")
                 (:file "system-utils"))))

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
