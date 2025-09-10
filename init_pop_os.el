;;; Emacs configuration for Pop!_OS specific settings
;;; -*- lexical-binding: t; -*-

;; map C-v to "paste" (in addition to C-y) for "Smile" on Gnome
(when (and (eq system-type 'gnu/linux)
           (string= (getenv "XDG_CURRENT_DESKTOP") "pop:GNOME"))
  (global-set-key (kbd "C-v") 'clipboard-yank))
