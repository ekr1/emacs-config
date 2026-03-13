;;; Projectile configuration
;;; -*- lexical-binding: t; -*-

(projectile-mode +1)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
;(define-key projectile-mode-map (kbd "C-c p g") #'projectile-ripgrep)
(define-key projectile-mode-map (kbd "C-c p g") #'deadgrep)

;; deadgrep instead of projectile's search...
;; (define-key projectile-mode-map (kbd "C-c p g") #'projectile-grep t) ; to remove previous def
;; (define-key projectile-mode-map (kbd "C-c p g") #'deadgrep t) ; to remove previous def
;; (let ((args (help-function-arglist #'define-key)))
;;   (cond
;;    ((and args (= (length args) 4))
;;     (define-key projectile-mode-map (kbd "C-c p g") #'deadgrep t))
;;    ((and args (= (length args) 3))
;;     (define-key projectile-mode-map (kbd "C-c p g") #'deadgrep))
;;    (t
;;     (message "Warning: `define-key` does not take 3 or 4 parameters; skipping binding."))))
;; (global-set-key (kbd "C-c p g") #'deadgrep)

(unless (boundp 'projectile-grep-find-ignored-patterns)
  (setopt projectile-grep-find-ignored-patterns '()))
(add-to-list 'projectile-grep-find-ignored-patterns "./.aider.chat.history.md")

;; Requires the `rg` binary in PATH.
(with-eval-after-load 'grep
  ;; Make `M-x grep` default to ripgrep.
  (grep-apply-setting
   'grep-command
   "rg -nH --no-heading --color=never --smart-case ")

  ;; Optional: also make `M-x grep-find` use rg (no find+grep).
  ;; Emacs expects (COMMAND . POINT-OF-DEFAULT-INSERTION).
  (grep-apply-setting
   'grep-find-command
   (cons "rg -nH --no-heading --color=never --smart-case --hidden --glob '!.git/*' -e "
         74)))
