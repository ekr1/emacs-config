(require 'compile)
(require 'uniquify)

;(setq load-path (cons "~/.emacs.d/elisp/icicles" load-path))
;(require 'icicles)
;(icy-mode 0)
; Switching to previous buffer doesn't work anymore?!

(ido-mode t) ; use 'buffer rather than t to use only buffer switching
(ido-everywhere t)

(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(auto-hscroll-mode t)
 '(auto-save-default t)
 '(auto-save-file-name-transforms (quote (("\\`/[^/]*:\\([^/]*/\\)*\\([^/]*\\)\\'" "d:/DOCUME~1/ekraemer/LOCALS~1/Temp/\\2" t) (".*plink:\\(.*\\)" "d:/DOCUME~1/ekraemer/LOCALS~1/Temp/\\1" t))))
 '(c-basic-offset 4)
 '(c-default-style (quote ((awk-mode . "awk") (other . "ellemtel"))))
 '(column-number-mode t)
 '(comment-empty-lines t)
 '(compilation-ask-about-save nil)
 '(compilation-auto-jump-to-first-error t)
 '(compilation-scroll-output (quote first-error))
 '(cperl-autoindent-on-semi t)
 '(cperl-brace-offset -2)
 '(cperl-extra-newline-before-brace t)
 '(cperl-extra-newline-before-brace-multiline t)
 '(cperl-fix-hanging-brace-when-indent t)
 '(cperl-indent-comment-at-column-0 nil)
 '(cperl-indent-level 4)
 '(cperl-indent-parens-as-block nil)
 '(cperl-indent-wrt-brace t)
 '(cperl-invalid-face (quote default))
 '(cperl-merge-trailing-else nil)
 '(cua-mode nil nil (cua-base))
 '(default-frame-alist (quote ((menu-bar-lines . 1) (font . "-unknown-Inconsolata-normal-normal-normal-*-14-*-*-*-m-0-iso10646-1"))))
 '(desktop-files-not-to-save (quote "xyzzy (will crash if this is nil)"))
 '(desktop-globals-to-clear (quote (kill-ring-yank-pointer search-ring search-ring-yank-pointer regexp-search-ring regexp-search-ring-yank-pointer kill-ring)))
 '(desktop-globals-to-save (quote (desktop-missing-file-warning tags-file-name tags-table-list search-ring regexp-search-ring register-alist file-name-history compile-command compilation-directory shell-command-history kill-ring search-ring ...)))
 '(desktop-missing-file-warning nil)
 '(desktop-path (quote ("~/.emacs.d/")))
 '(desktop-restore-eager t)
 '(desktop-save t)
 '(desktop-save-mode t)
 '(flymake-get-project-include-dirs-function (quote ekr-flymake-get-project-include-dirs-imp))
 '(flymake-log-level 1)
 '(grep-find-template "find . <X> -type f <F> | xargs grep <C> -n <R>")
 '(icicle-guess-commands-in-path nil)
 '(icicle-redefine-standard-commands-flag nil)
 '(ido-enable-flex-matching t)
 '(indent-tabs-mode nil)
 '(jit-lock-stealth-verbose t)
 '(max-mini-window-height 1)
 '(max-specpdl-size 10000)
 '(show-paren-mode t)
 '(split-width-threshold 140)
 '(sql-oracle-program "/home/ekr/bin/sqlplus_with_env")
 '(tool-bar-mode nil)
 '(tramp-copy-size-limit 102400)
 '(tramp-default-method "ssh")
 '(tramp-remote-process-environment (quote ("HISTFILE=$HOME/.tramp_history" "HISTSIZE=1" "LC_ALL=C" "TERM=dumb" "EMACS=t" "INSIDE_EMACS=23.1.1,tramp:2.1.15" "CDPATH=" "HISTORY=" "MAIL=" "MAILCHECK=" "MAILPATH=" "autocorrect=" ...)))
 '(tramp-verbose 2)
 '(truncate-lines t)
 '(undo-limit 12000000)
 '(undo-strong-limit 12000000)
 '(uniquify-buffer-name-style (quote post-forward-angle-brackets) nil (uniquify))
 '(use-file-dialog nil)
 '(vc-handled-backends (quote (RCS SVN SCCS Bzr Git Hg Arch)))
 '(vc-svn-diff-switches "-x -b")
 '(x-select-enable-primary nil)
 '(xslt-process-fop-log-level (quote (debug)))
 '(xslt-process-xml-xslt-associations nil))
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(default ((t (:stipple nil :background "white" :foreground "black" :inverse-video nil :box nil :strike-through nil ...)))))

(defun arrange-frame (w h x y)
  "Set the width, height, and x/y position of the current frame"
  (let ((frame (selected-frame)))
    (delete-other-windows)
    (set-frame-position frame x y)
    (set-frame-size frame w h)))
(arrange-frame 160 61 1310 0)
(run-with-idle-timer 1 nil 'arrange-frame 160 61 1310 0)

(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)

(setq default-buffer-file-coding-system 'iso-latin-1-unix)

(setq special-display-buffer-names
           '("*Async Shell Command*" "*grep*" "*compilation*" "*vc-dir*"))

(defun ekr-wiki-update ()
  (interactive)
  "run ./wiki-update.cfg in the correct directory" 
  (if (eq 0 (string-match 
	    "/home/ekr/src/rotlib_ekr/" 
	    (buffer-file-name)))

      ;; create rot-perl-lib docs (ekr)
      (shell-command
       "/home/ekr/src/rotlib_ekr/bin/pdoc-local.sh &")

    ;; ... or wiki docs
    (let ((buffer (find-file-noselect "/opt/rot/app/sdb1/system/quellen/docs/wiki-update.cfg"))
	  (old-window (selected-window))
	  (old-file-name (buffer-file-name)))
      (if buffer
	  (progn
	    (delete-other-windows)
	    (set-buffer buffer)
	    (shell-command 
	     (concat "./wiki-update.cfg -o '/"
		     (file-name-nondirectory old-file-name)
		     "$' &")) 
	    (select-window old-window))
	(message "Open wiki-update.cfg first"))))
  )

(defun ekr-compile-plsql ()
  (interactive)
  "compile the current .sql file in the correct schema"
  (if (eq 0 (string-match 
	     "/opt/rot/app/release/dev/quellen/packages/" 
	     (buffer-file-name)))
      (progn (save-buffer)
             (shell-command
              (concat "date ; echo | node appsys_dev @"
                      (file-name-nondirectory buffer-file-name))))))

(defun ekr-recompile ()
  (interactive)
  "Save all files, wait a little bit, then call (recompile). For compilations that watch file-change-times (RotTestHelper...)"
  (progn (save-some-buffers t)
	 (sleep-for 1.5)
	 (recompile)))

(defun ekr-read-ssh-agent ()
  (interactive)
  "Set the ssh-agent environment"
  (ignore-errors
   (kill-buffer ".ekr-ssh-agent"))
  (with-current-buffer 
      (find-file-noselect "/tmp/.ekr-ssh-agent")
    (buffer-string)
    (string-match "tmp/ssh-[^;]+." (buffer-string))  ; ???!? match-string adds one char before and skips the last one?!
    (setenv "SSH_AUTH_SOCK" (match-string 0))
    (message (getenv "SSH_AUTH_SOCK"))))
	 

(global-set-key (kbd "<f1>") 'ekr-wiki-update)
(global-set-key (kbd "<f2>") 'ekr-recompile)
(global-set-key (kbd "<f3>") 'ekr-compile-plsql)
(global-set-key (kbd "<f4>") 'ekr-read-ssh-agent)
(global-set-key (kbd "M-n") 'next-error)
(global-set-key (kbd "M-p") 'previous-error)
(global-set-key (kbd "C-/") 'comment-or-uncomment-region)

(defun ask-before-closing ()
  "Ask whether or not to close, and then close if y was pressed"
  (interactive)
  (if (y-or-n-p (format "Are you sure you want to exit Emacs? "))
      (if (< emacs-major-version 22)
          (save-buffers-kill-terminal)
        (save-buffers-kill-emacs))
    (message "Canceled exit")))

(global-set-key (kbd "C-x C-c") 'ask-before-closing)

;(setenv "PAGER" "cat")

(run-with-idle-timer 
 15 t 'desktop-save (car desktop-path))

;(find-file-noselect "/ekr@10.226.93.5:/opt")

; remove some weird values (start from scratch)
(setq compilation-error-regexp-alist
      (mapcar 'car compilation-error-regexp-alist-alist))

(add-to-list 'compilation-error-regexp-alist 'ekr-perl-ignore)
(add-to-list 'compilation-error-regexp-alist-alist
 	     '(ekr-perl-ignore " at \\(library \\)?\\(/.*?\\) line \\([0-9]+\\)"
		     2 3 nil 0))

(add-to-list 'compilation-error-regexp-alist 'ekr-imp-log)
(add-to-list 'compilation-error-regexp-alist-alist
 	     '(ekr-imp-log "IMPORT: Log-Datei \\(/opt/rot/dump/.*\.sql\\)<br>"
		     1 nil nil 0))

(add-to-list 'compilation-error-regexp-alist 'ekr-imp-crash)
(add-to-list 'compilation-error-regexp-alist-alist
 	     '(ekr-imp-crash "crash => \\(/tmp/imp-[0-9]+\.err\\) "
		     1 nil nil nil))

(add-to-list 'compilation-error-regexp-alist 'ekr-rot-perl-lib)
(add-to-list 'compilation-error-regexp-alist-alist
 	     '(ekr-rot-perl-lib " at \\(/opt/rot/app/project.*?/vendor/rot-perl-lib/src/Rot/.*?\\) line \\([0-9]+\\)"
		     1 2 nil 0))


(setq compilation-auto-jump-to-next 1)

(setenv "TERM" "dumb")  ; for perldoc etc.

(require 'flymake)
(defun ekr-flymake-get-project-include-dirs-imp (basedir)
  "get basedirs, cdz"
  (if (string-match "^/opt/rot/app/projects?/[^/]+/" basedir)
      (let ((str (substring basedir 0 (nth 1 (match-data 1)))))
        (list (concat str "lib")
              (concat str "vendor/rot-perl-lib/src")))
    '()))

(flymake-get-include-dirs (buffer-file-name));"/opt/rot/app/project/eontdb/lib/Eontdb/Controller/")
;(ekr-flymake-get-project-include-dirs-imp "/opt/rot/app/project/barmer/lib")

(defun flymake-perl-init ()
  (let* ((temp-file   (flymake-init-create-temp-buffer-copy
                       'flymake-create-temp-inplace))
	 (local-file  (file-relative-name
                       temp-file
                       (file-name-directory buffer-file-name))))
    (list "perl" (list "-wc" "-I" "../.." "-I" "../../../vendor/rot-perl-lib/src" local-file)))
)

;(flymake-perl-init)
