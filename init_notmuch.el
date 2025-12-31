;;; init_notmuch.el --- Notmuch configuration
;;; -*- lexical-binding: t; -*-
                                        ;
(ekr-banner "Notmuch...")

;; dpkg -L elpa-notmuch

;; if the file ~/Daten/Mail/notmuch exists...
(if (file-exists-p "/home/ekr/Daten/Mail/notmuch")
    (progn
      (message "Custom notmuch binary is available.")
      (setopt notmuch-command "/home/ekr/Daten/Mail/notmuch")

      ;; (add-to-list 'load-path "/usr/share/emacs/site-lisp/elpa-src/notmuch-0.35")
      (add-to-list 'load-path "/home/ekr/Daten/Mail/data/elisp/notmuch-0.37")
      (require 'notmuch)

      (message "Notmuch sanity check ... if it hangs, make sure podman runs.")
      (ignore-errors
        (progn
          (notmuch-assert-cli-sane)
          (message "Notmuch sanity check: %s" (notmuch-config-get "user.primary_email"))))

      ;; M-m   show unread mails
      (global-set-key (kbd "M-m")
                      (lambda () (interactive)
                        (progn
                          ; run $HOME/Daten/Mail/poll.sh:
                          (shell-command
                           (concat (getenv "HOME") "/Daten/Mail/poll.sh"))
                          (notmuch-poll)
                          (notmuch-search "tag:inbox and not tag:deleted"))))
      (run-at-time "1 min" nil
                   (lambda ()
                     (ignore-errors
                       (call-interactively (key-binding (kbd "M-m"))))))

      ;; a    "archive" (read) mail, default setting

      ;; d    delete mail
      ;; add the "d" key to notmuch-search-mode-map:
      (define-key notmuch-search-mode-map (kbd "d")
                  (lambda () (interactive)
                    "Delete the current thread and move to the next one."
                    (progn
                      (notmuch-search-tag '("+deleted" "-inbox" "-unread"))
                      (notmuch-search-next-thread))))

      (define-key notmuch-tree-mode-map (kbd "d")
                  (lambda () (interactive)
                    "Delete the current thread and move to the next one."
                    (progn
                      (notmuch-tree-tag '("+deleted" "-inbox" "-unread"))
                      (notmuch-tree-next-thread))))
      )
  (message "Notmuch binary is not available. Notmuch features will be disabled."))
