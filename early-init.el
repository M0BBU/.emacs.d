;; org mode stuff, need to put this early to prevent any errors?
;; TODO: Figure out what using straight with org causes errors
(setq org-log-done t)
(setq org-agenda-files (list "~/org/school/todo.org"))

;; Increase garbage collection threshold for faster start up time.
(setq gc-cons-threshold 100000000)

;; Needed to use the straight package manager.
(setq package-enable-at-startup nil)
