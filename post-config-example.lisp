(format t "Hello post-config file. Here we can overwrite any functionnality.")

;;; For example, we can define other cron rules.

;; Stop old cron.
(cl-cron:stop-cron)

(defun schedule-db-reload ()
  "Reload the DB 3 times a day:
  - at 13h45
  - at 20h30
  - at 04h30.
  We must have the corresponding cron job to copy the DB."
  (cl-cron:make-cron-job #'get-all-cards :minute 30 :hour 4)
  (cl-cron:make-cron-job #'get-all-shelves :minute 30 :hour 4)
  ;; re-read the selection, either from selection.csv either from the DB
  ;; (books can be selected from Abelujo)
  (cl-cron:make-cron-job #'read-selection :minute 59 :hour 4)

  (cl-cron:make-cron-job #'get-all-cards :minute 45 :hour 13)
  (cl-cron:make-cron-job #'get-all-shelves :minute 45 :hour 13)
  (cl-cron:make-cron-job #'read-selection :minute 59 :hour 13)

  (cl-cron:make-cron-job #'get-all-cards :minute 30 :hour 20)
  (cl-cron:make-cron-job #'get-all-shelves :minute 30 :hour 20)
  (cl-cron:make-cron-job #'read-selection :minute 59 :hour 20)

  (cl-cron:start-cron)
  (log:info "Re-defined the scheduled DB reload for 3 times a day."))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Start new cron.
(schedule-db-reload)
