;;; flycheck-indicator-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "flycheck-indicator" "flycheck-indicator.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from flycheck-indicator.el

(defvar flycheck-indicator-mode nil "\
Non-nil if Flycheck-Indicator mode is enabled.
See the `flycheck-indicator-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `flycheck-indicator-mode'.")

(custom-autoload 'flycheck-indicator-mode "flycheck-indicator" nil)

(autoload 'flycheck-indicator-mode "flycheck-indicator" "\
Minor mode to get a fancy mode line indicator for `flycheck-mode'.

When called interactively, toggle
`flycheck-indicator-mode'.  With prefix ARG, enable
`flycheck-indicator-mode' if ARG is positive, otherwise
disable it.

When called from Lisp, enable `flycheck-indicator-mode' if ARG is omitted,
nil or positive.  If ARG is `toggle', toggle `flycheck-indicator-mode'.
Otherwise behave as if called interactively.

\(fn &optional ARG)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "flycheck-indicator" '("flycheck-indicator-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; flycheck-indicator-autoloads.el ends here
