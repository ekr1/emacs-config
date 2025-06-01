;;; package --- Summary

;; Initialization 'straight

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; Straight packages

;; M-x straight-pull-recipe-repositories
;; M-x straight-pull-all           # updates packages, not only the lists...

;; write lockfile: straight-freeze-versions

(straight-use-package 'org)
; load the file ./straight/build/org/org-table.el for turn-on-orgtbl, else feature-mode breaks
(load-file (concat (file-name-directory (locate-library "org-table")) "org-table.el"))

(straight-use-package 'use-package)
(straight-use-package 'csv-mode)
(straight-use-package 'with-editor)
(straight-use-package 'php-mode)
(straight-use-package 'org-jira)
(straight-use-package 'json-navigator)
(straight-use-package 'go-eldoc)
(straight-use-package 'typescript-mode)
(straight-use-package 'go-mode)
(straight-use-package 'origami-predef)
(straight-use-package 'dockerfile-mode)
(straight-use-package 'markdown-mode)
(straight-use-package 'kubel)
(straight-use-package 'kubernetes)
(straight-use-package 'projectile)
(straight-use-package 'groovy-mode)
(straight-use-package 'lua-mode)
(straight-use-package 'sqlformat)
(straight-use-package 'magit)
(straight-use-package 'magit-filenotify)
(straight-use-package 'multiple-cursors)
(straight-use-package 'feature-mode)
(straight-use-package 'emmet-mode)
(straight-use-package 'pdf-tools)
(straight-use-package 'powershell)
(straight-use-package 'ahk-mode)
(straight-use-package 'tty-format)
(straight-use-package 'scss-mode)
(straight-use-package 'web-mode)
(straight-use-package 'sql-indent)
(straight-use-package 'marginalia)
(straight-use-package 'forge)
(straight-use-package 'yaml-mode)
(straight-use-package 'recentf)
(straight-use-package 'compile)
(straight-use-package 'highlight-indent-guides)
(straight-use-package 'compile)
(straight-use-package 'plantuml-mode)
(straight-use-package 'deadgrep)
(straight-use-package 'dumb-jump)
(straight-use-package 'load-env-vars)
; (straight-use-package 'show-font)  ; https://protesilaos.com/emacs/show-font - install manually

(add-hook 'xref-backend-functions #'dumb-jump-xref-activate)

; git blame in status line...
(straight-use-package 'emacs-async)
(add-to-list 'load-path "~/.emacs.d/elisp/emsg-blame")
(ignore-errors
  (require 'emsg-blame) ; TODO fails on windows... install occasionally
  (setq emsg-blame-background t)
  (setq emsg-blame-background-color "#444444")
  (global-emsg-blame-mode))

;; (straight-use-package 'llm)
;; (straight-use-package 'ellama)
;; (use-package ellama
;;   :straight t
;;   :init
;;   (require 'llm-ollama)
;;   (setopt ellama-provider
;; 	  (make-llm-ollama
;;            ;; deepseek-coder:6.7b  ->  pretty ok?
;; 	   :chat-model "deepseek-coder-v2"
;; 	   :embedding-model "deepseek-coder-v2")))

;; (use-package transient
;;   :straight (transient
;;              :type git
;;              :host github
;;              :repo "magit/transient"
;;              :version 0.4.1))
;; (use-package magit
;;   :straight (magit
;;              :type git
;;              :host github
;;              :repo "magit/magit"
;;              :version 3.2.1)
;;   :bind ("C-c 0" . magit-status))

(add-to-list 'load-path "~/.emacs.d/elisp")

;; ------------------------ flycheck plus additional modules

(use-package flycheck
  :straight t
  :init (global-flycheck-mode)
  :config
  (use-package flycheck-pos-tip
    :straight t
    :config (flycheck-pos-tip-mode))
  (use-package flycheck-status-emoji
    :straight t
    :config (flycheck-status-emoji-mode))
  (use-package flycheck-checkbashisms
    :straight t
    :hook (flycheck-mode . flycheck-checkbashisms-setup)))

;; (straight-use-package 'flycheck)
;; ; (straight-use-package 'flycheck-color-mode-line) ; https://github.com/flycheck/flycheck-color-mode-line (official) colors the mode line according to the Flycheck status.
;; (straight-use-package 'flycheck-pos-tip) ; shows Flycheck error messages in a graphical popup.
;; (straight-use-package 'flycheck-status-emoji) ; https://github.com/liblit/flycheck-status-emoji adds cute emoji (e.g. 😱 for errors) to Flycheck’s mode line status.
;; (straight-use-package 'flycheck-checkbashisms)
;; ; Possible PHP flycheck extensions: https://github.com/emacs-php/phpstan.el, https://github.com/emacs-php/psalm.el
;; ; Possible Python checkers: https://github.com/msherry/flycheck-pycheckers, https://github.com/chocoelho/flycheck-prospector
;; (global-flycheck-mode)
;; (with-eval-after-load 'flycheck (flycheck-pos-tip-mode))
;; (with-eval-after-load 'flycheck (flycheck-status-emoji-mode))
;; (eval-after-load 'flycheck '(add-hook 'flycheck-mode-hook #'flycheck-checkbashisms-setup))

; required since some compilation vars are used later
(require 'compile)
(require 'feature-mode)

; fix for very slow compiling:
(setq process-adaptive-read-buffering nil)

;(setq load-path (cons "~/.emacs.d/elisp/icicles" load-path))
;(require 'icicles)
;(icy-mode 0)
; Switching to previous buffer doesn't work anymore?!

;(ido-mode t) ; use 'buffer rather than t to use only buffer switching
;(ido-everywhere t)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ahk-indentation 2)
 '(ansi-color-bold-is-bright t)
 '(ansi-color-for-comint-mode t)
 '(ansi-color-names-vector
   ["black" "#d55e00" "#009e73" "#f8ec59" "#0072b2" "#cc79a7" "#56b4e9"
    "white"])
 '(auto-hscroll-mode t)
 '(auto-save-default t)
 '(auto-save-file-name-transforms
   '(("\\`/[^/]*:\\([^/]*/\\)*\\([^/]*\\)\\'"
      "d:/DOCUME~1/ekraemer/LOCALS~1/Temp/\\2" t)
     (".*plink:\\(.*\\)" "d:/DOCUME~1/ekraemer/LOCALS~1/Temp/\\1" t)))
 '(backup-by-copying t)
 '(backup-directory-alist '(("." . "/home/ekr/.emacs.d/backups")))
 '(c-basic-offset 4)
 '(c-default-style '((awk-mode . "awk") (other . "ellemtel")))
 '(column-number-mode t)
 '(comment-empty-lines t)
 '(comment-style 'multi-line)
 '(compilation-ask-about-save nil)
 '(compilation-auto-jump-to-first-error nil)
 '(compilation-context-lines 3)
 '(compilation-mode-hook nil)
 '(compilation-scroll-output t)
 '(copilot-chat-commit-prompt
   "Here is the result of running `git diff --cached`. Please suggest a commit message. Don't add anything else to the response. The following describes conventional commits.\12Do not use any markers around the commit message. Do not add the conventional commit prefix.\12\12Here is the result of `git diff --cached`:\12")
 '(copilot-chat-debug t)
 '(copilot-chat-follow nil)
 '(copilot-chat-frontend 'shell-maker)
 '(copilot-chat-model-ignore-picker t)
 '(copilot-indent-offset-warning-disable t)
 '(cperl-autoindent-on-semi t)
 '(cperl-brace-offset -2)
 '(cperl-extra-newline-before-brace t)
 '(cperl-extra-newline-before-brace-multiline t)
 '(cperl-fix-hanging-brace-when-indent t)
 '(cperl-indent-comment-at-column-0 nil)
 '(cperl-indent-level 4)
 '(cperl-indent-parens-as-block nil)
 '(cperl-indent-wrt-brace t)
 '(cperl-invalid-face 'default)
 '(cperl-merge-trailing-else nil)
 '(custom-enabled-themes '(tango-dark))
 '(deadgrep-extra-arguments '("--no-config" "--sort=path"))
 '(desktop-files-not-to-save '"xyzzy (will crash if this is nil)")
 '(desktop-globals-to-clear
   '(kill-ring-yank-pointer search-ring search-ring-yank-pointer
                            regexp-search-ring
                            regexp-search-ring-yank-pointer kill-ring))
 '(desktop-globals-to-save
   '(desktop-missing-file-warning tags-file-name tags-table-list
                                  search-ring regexp-search-ring
                                  register-alist file-name-history
                                  compile-command
                                  compilation-directory
                                  shell-command-history kill-ring
                                  search-ring \...))
 '(desktop-locals-to-save
   '(desktop-locals-to-save truncate-lines case-fold-search case-replace
                            fill-column overwrite-mode
                            change-log-default-name line-number-mode
                            column-number-mode size-indication-mode
                            buffer-file-coding-system
                            buffer-display-time indent-tabs-mode
                            tab-width indicate-buffer-boundaries
                            indicate-empty-lines
                            show-trailing-whitespace))
 '(desktop-missing-file-warning nil)
 '(desktop-path '("~/.emacs.d/"))
 '(desktop-restore-eager t)
 '(desktop-save t)
 '(desktop-save-mode t)
 '(display-buffer-alist nil)
 '(display-buffer-reuse-frames t)
 '(display-line-numbers t)
 '(dumb-jump-debug t)
 '(dumb-jump-force-searcher 'ag)
 '(emsg-blame-idle-time 5)
 '(feature-cucumber-command "bundle exec cucumber {options} {feature}")
 '(feature-rake-command "bundle exec cucumber {options} {feature}")
 '(file-coding-system-alist
   '(("\\.dz\\'" no-conversion . no-conversion)
     ("\\.txz\\'" no-conversion . no-conversion)
     ("\\.xz\\'" no-conversion . no-conversion)
     ("\\.lzma\\'" no-conversion . no-conversion)
     ("\\.lz\\'" no-conversion . no-conversion)
     ("\\.g?z\\'" no-conversion . no-conversion)
     ("\\.\\(?:tgz\\|svgz\\|sifz\\)\\'" no-conversion . no-conversion)
     ("\\.tbz2?\\'" no-conversion . no-conversion)
     ("\\.bz2\\'" no-conversion . no-conversion)
     ("\\.Z\\'" no-conversion . no-conversion)
     ("\\.elc\\'" . utf-8-emacs) ("\\.el\\'" . prefer-utf-8)
     ("\\.utf\\(-8\\)?\\'" . utf-8)
     ("\\.xml\\'" . xml-find-file-coding-system)
     ("\\(\\`\\|/\\)loaddefs.el\\'" raw-text . raw-text-unix)
     ("\\.tar\\'" no-conversion . no-conversion)
     ("\\.po[tx]?\\'\\|\\.po\\." . po-find-file-coding-system)
     ("\\.\\(tex\\|ltx\\|dtx\\|drv\\)\\'"
      . latexenc-find-file-coding-system)
     ("" undecided) ("\\.rb\\'" . utf-8) ("\\.yml\\'" . utf-8)
     ("\\.erb\\'" . utf-8) ("\\.feature\\'" . utf-8)))
 '(flycheck-highlighting-mode 'lines)
 '(flycheck-ruby-rubocop-executable "bundle exec rubocop")
 '(forge-alist
   '(("github.com" "api.github.com" "github.com" forge-github-repository)
     ("gitlab.com" "gitlab.com/api/v4" "gitlab.com"
      forge-gitlab-repository)
     ("salsa.debian.org" "salsa.debian.org/api/v4" "salsa.debian.org"
      forge-gitlab-repository)
     ("framagit.org" "framagit.org/api/v4" "framagit.org"
      forge-gitlab-repository)
     ("gitlab.gnome.org" "gitlab.gnome.org/api/v4" "gitlab.gnome.org"
      forge-gitlab-repository)
     ("codeberg.org" "codeberg.org/api/v1" "codeberg.org"
      forge-gitea-repository)
     ("code.orgmode.org" "code.orgmode.org/api/v1" "code.orgmode.org"
      forge-gogs-repository)
     ("bitbucket.org" "api.bitbucket.org/2.0" "bitbucket.org"
      forge-bitbucket-repository)
     ("git.savannah.gnu.org" nil "git.savannah.gnu.org"
      forge-cgit**-repository)
     ("git.kernel.org" nil "git.kernel.org" forge-cgit-repository)
     ("repo.or.cz" nil "repo.or.cz" forge-repoorcz-repository)
     ("git.suckless.org" nil "git.suckless.org"
      forge-stagit-repository)
     ("git.sr.ht" nil "git.sr.ht" forge-srht-repository)
     ("atc-github.azure.cloud.bmw" "atc-github.azure.cloud.bmw/api/v3"
      "atc-github.azure.cloud.bmw" forge-github-repository)))
 '(git-commit-summary-max-length 2000)
 '(grep-find-ignored-files
   '(".#*" "*.o" "*~" "*.bin" "*.lbin" "*.so" "*.a" "*.ln" "*.blg"
     "*.bbl" "*.elc" "*.lof" "*.glo" "*.idx" "*.lot" "*.fmt" "*.tfm"
     "*.class" "*.fas" "*.lib" "*.mem" "*.x86f" "*.sparcf" "*.dfsl"
     "*.pfsl" "*.d64fsl" "*.p64fsl" "*.lx64fsl" "*.lx32fsl"
     "*.dx64fsl" "*.dx32fsl" "*.fx64fsl" "*.fx32fsl" "*.sx64fsl"
     "*.sx32fsl" "*.wx64fsl" "*.wx32fsl" "*.fasl" "*.ufsl" "*.fsl"
     "*.dxl" "*.lo" "*.la" "*.gmo" "*.mo" "*.toc" "*.aux" "*.cp"
     "*.fn" "*.ky" "*.pg" "*.tp" "*.vr" "*.cps" "*.fns" "*.kys"
     "*.pgs" "*.tps" "*.vrs" "*.pyc" "*.pyo" "*.gz" "cucumber.json"))
 '(grep-find-template "find . <X> -type f <F> -print0 | xargs -0 grep <C> -n <R>")
 '(highlight-indent-guides-auto-character-face-perc 60)
 '(highlight-indent-guides-method 'character)
 '(highlight-indent-guides-responsive nil)
 '(highlight-indent-guides-suppress-auto-error t)
 '(icicle-guess-commands-in-path nil)
 '(icicle-redefine-standard-commands-flag nil)
 '(ido-confirm-unique-completion t)
 '(ido-enable-flex-matching t)
 '(ido-everywhere t)
 '(indent-tabs-mode nil)
 '(jit-lock-stealth-verbose t)
 '(kubernetes-kubectl-executable "/usr/local/bin/oc")
 '(make-pointer-invisible nil)
 '(max-mini-window-height 1)
 '(max-specpdl-size 10000 t)
 '(mouse-highlight t)
 '(mouse-wheel-down-event 'mouse-4)
 '(mouse-wheel-mode t)
 '(mouse-wheel-progressive-speed nil)
 '(mouse-wheel-scroll-amount '(5 ((shift) . hscroll) ((meta)) ((control) . text-scale)))
 '(mouse-wheel-up-event 'mouse-5)
 '(mpc-browser-tags '(Album|Playlist))
 '(mpc-mpd-music-directory "~/Music/Loop")
 '(notmuch-archive-tags '("-inbox" "-unread"))
 '(notmuch-saved-searches
   '((:name "inbox" :query "tag:inbox and not tag:deleted" :key [105])
     (:name "unread" :query "tag:unread and not tag:deleted" :key
            [117])
     (:name "flagged" :query "tag:flagged" :key [102])
     (:name "sent" :query "tag:sent" :key [116])
     (:name "drafts" :query "tag:draft" :key [100])
     (:name "all mail" :query "*" :key [97])))
 '(notmuch-search-oldest-first nil)
 '(org-export-backends '(ascii html md odt))
 '(org-jira-boards-default-limit 200)
 '(org-jira-custom-jqls
   '((:jql
      " assignee = currentUser() and createdDate < '2022-01-01' order by created DESC "
      :limit 100 :filename "last-years-work")
     (:jql
      " assignee = currentUser() and createdDate >= '2022-01-01' order by created DESC "
      :limit 100 :filename "this-years-work")))
 '(org-startup-with-inline-images t)
 '(plantuml-default-exec-mode 'executable t)
 '(plantuml-jar-path
   "/opt/homebrew/Cellar/plantuml/1.2024.6/libexec/plantuml.jar" t)
 '(projectile-completion-system 'ido)
 '(projectile-globally-ignored-files '("TAGS" "#*#"))
 '(ruby-insert-encoding-magic-comment nil)
 '(safe-local-variable-values
   '((package-lint-main-file . "copilot-chat.el") (ahk-indentation . 2)
     (buffer-file-coding-system . iso-8859-1)
     (buffer-file-coding-system . utf-8)))
 '(save-interprogram-paste-before-kill t)
 '(scroll-error-top-bottom t)
 '(server-mode t)
 '(show-paren-mode t)
 '(smerge-command-prefix "\3d")
 '(special-display-buffer-names nil)
 '(split-width-threshold 140)
 '(sqlformat-command 'pgformatter)
 '(tool-bar-mode nil)
 '(tooltip-mode nil)
 '(tramp-copy-size-limit 102400)
 '(tramp-default-method "ssh")
 '(tramp-remote-process-environment
   '("HISTFILE=$HOME/.tramp_history" "HISTSIZE=1" "LC_ALL=C" "TERM=dumb"
     "EMACS=t" "INSIDE_EMACS=23.1.1,tramp:2.1.15" "CDPATH=" "HISTORY="
     "MAIL=" "MAILCHECK=" "MAILPATH=" "autocorrect=" \...))
 '(tramp-verbose 2)
 '(truncate-lines t)
 '(undo-limit 12000000)
 '(undo-strong-limit 12000000)
 '(uniquify-buffer-name-style 'post-forward-angle-brackets nil (uniquify))
 '(url-proxy-services nil)
 '(use-file-dialog nil)
 '(vc-handled-backends '(RCS SVN SCCS Bzr Git Hg Arch))
 '(vc-svn-diff-switches "-x -b")
 '(whitespace-global-modes '(yaml-mode))
 '(whitespace-line-column 80)
 '(whitespace-style
   '(face trailing tabs empty indentation space-after-tab
          space-before-tab space-mark tab-mark newline-mark))
 '(with-editor-emacsclient-executable nil)
 '(xslt-process-fop-log-level '(debug))
 '(xslt-process-xml-xslt-associations nil)
 '(xterm-mouse-mode t))

 ;; '(feature-cucumber-command
 ;;   "time bin/rake cucumber:rerun_nodb CUCUMBER_OPTS=\"{options}\" FEATURE=\"{feature}\" ")

(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)

; (setq special-display-buffer-names
;           '("*Async Shell Command*" "*grep*" "*compilation*" "*vc-dir*"))

; note: special-display-buffer-names is deprecated, use display-buffer-alist instead
; (setq special-display-buffer-names nil)

;; (defun ekr-wiki-update ()
;;   (interactive)
;;   "run ./wiki-update.cfg in the correct directory"
;;   (if (eq 0 (string-match
;; 	    "/home/ekr/src/rotlib_ekr/"
;; 	    (buffer-file-name)))

;;       ;; create rot-perl-lib docs (ekr)
;;       (shell-command
;;        "/home/ekr/src/rotlib_ekr/bin/pdoc-local.sh &")

;;     ;; ... or wiki docs
;;     (let ((buffer (find-file-noselect "/opt/rot/app/sdb1/system/quellen/docs/wiki-update.cfg"))
;; 	  (old-window (selected-window))
;; 	  (old-file-name (buffer-file-name)))
;;       (if buffer
;; 	  (progn
;; 	    (delete-other-windows)
;; 	    (set-buffer buffer)
;; 	    (shell-command
;; 	     (concat "./wiki-update.cfg -o '/"
;; 		     (file-name-nondirectory old-file-name)
;; 		     "$' &"))
;; 	    (select-window old-window))
;; 	(message "Open wiki-update.cfg first"))))
;;   )

;; (defun ekr-compile-plsql ()
;;   (interactive)
;;   "compile the current .sql file in the correct schema"
;;   (if (eq 0 (string-match
;; 	     "/opt/rot/app/release/dev-ekr/quellen/packages/"
;; 	     (buffer-file-name)))
;;       (progn (save-buffer)
;;              (shell-command
;;               (concat "date ; echo | node appsys_dev @"
;;                       (file-name-nondirectory buffer-file-name))))))

(defun ekr-recompile ()
  "Save all files, wait a little bit, then call (recompile).  For compilations that watch file-change-times (RotTestHelper...)."
  (interactive)
  (progn (save-some-buffers t)
	 ;(sleep-for 1.5)
	 ; (recompile)

         ; use comint-mode (the "t" of compilation-start)
         (let ((default-directory compilation-directory))
           (apply 'compilation-start (list compile-command t)))

         ;; if *compilation* is open in another frame (which is
         ;; visible and raised), then close it in "this" window
         (if (string= "t"
                      (mapconcat (lambda (val)
                                   (if val "t" ""))
                                 (mapcar (lambda (frame)
                                           (unless (eq frame (selected-frame))
                                             (string= "*compilation*"
                                                      (buffer-name
                                                       (window-buffer
                                                        (frame-selected-window frame))))))
                                         (frame-list))
                                 ""))
             ;; we have one other frame which has *compilation* selected, so we
             ;; can close the *compilation* on the current frame
             (walk-windows (lambda (win)
                             (if (string= "*compilation*"
                                          (buffer-name (window-buffer win)))
                                 (delete-window win)))))
         ))

(defun ekr-compilation-finished (buf result)
  "Play a beep when compilation finishes."
  ; if the "afplay" exec exists...
  (if (executable-find "afplay")
      (start-process "*Compilation Finished Beep*" nil "afplay" "/Users/KRAEME/.emacs.d/short_beep.m4a")))
(remove-hook 'compilation-finish-functions 'ekr-compilation-finished)
(add-hook 'compilation-finish-functions 'ekr-compilation-finished)

(defun ekr-git-gui ()
  "Run 'git gui' without a buffer."
  (interactive)
  (start-process "git gui" nil "git" "gui")
  (message "'git gui' started"))

(defun ekr-next-scenario ()
  "Jump to the next cucumber scenario in the compilation buffer."
  (interactive)
  (switch-to-buffer-other-window "*compilation*")
  (search-forward "Szenario:")
  (execute-kbd-macro (kbd "<return>")))

(defun my-previous-scenario ()
  "Jump to the previous cucumber scenario in the compilation buffer."
  (interactive)
  (switch-to-buffer-other-window "*compilation*")
  (search-backward "Szenario:")
  (execute-kbd-macro (kbd "<return>")))

;; (global-set-key (kbd "<f1>") 'ekr-wiki-update)
(global-set-key (kbd "M-a") 'ekr-recompile)
(global-set-key (kbd "M-c") 'ekr-recompile)
;; (global-set-key (kbd "<f3>") 'ekr-compile-plsql)
(global-set-key (kbd "<f5>") 'ekr-git-gui)
(global-set-key (kbd "M-n") 'next-error)
(global-set-key (kbd "M-p") 'previous-error)
(global-set-key (kbd "C-M-n") 'ekr-next-scenario)
(global-set-key (kbd "C-M-p") 'ekr-previous-scenario)
(global-set-key (kbd "C-/") 'comment-or-uncomment-region)
(global-set-key (kbd "C-#") 'comment-or-uncomment-region)

;(global-unset-key (kbd "M-q"))  ; avoid mistaken fill-paragraph when accidentally tying M-q on Mac
(define-key key-translation-map (kbd "M-q") (kbd "@"))
(global-set-key (kbd "M-@") 'fill-paragraph)   ; this is ESC M-q for fill-paragraph

(defun ask-before-closing ()
  "Ask whether or not to close, and then close if y was pressed."
  (interactive)
  (if (y-or-n-p (format "Are you sure you want to exit Emacs? "))
      (if (< emacs-major-version 22)
          (save-buffers-kill-terminal)
        (save-buffers-kill-emacs))
    (message "Canceled exit")))

(global-set-key (kbd "C-x C-c") 'ask-before-closing)

;(find-file-noselect "/ekr@10.226.93.5:/opt")

(defun starts-with-ekr (symb)
      "Returns non-nil if symbol symb starts with 'ekr-'.  Else nil."
      (let ((s (symbol-name symb)))
        (cond ((>= (length s) (length "ekr-"))
               (string-equal (substring s 0 (length "ekr-")) "ekr-"))
              (t nil))))

(setq compilation-error-regexp-alist-alist
      (cl-remove-if (lambda (item) (starts-with-ekr (car item)))
                 compilation-error-regexp-alist-alist))
(setq compilation-error-regexp-alist
      (cl-remove-if 'starts-with-ekr
                 (mapcar 'car compilation-error-regexp-alist-alist)))

; M-x re-builder
; (setq compilation-debug t) ; => then M-x describe-text-properties

; (REGEXP FILE [LINE COLUMN TYPE HYPERLINK HIGHLIGHT...])
; TYPE is 2 or nil for a real error or 1 for warning or 0 for info.
;
; Later entries seem to have higher priority...

; ignore spurious output
;      stmt.c:243:in oci8lib_230.bundle
(add-to-list 'compilation-error-regexp-alist 'ekr-oci8-stmt)
(add-to-list 'compilation-error-regexp-alist-alist
 	     '(ekr-oci8-stmt "^ *\\(stmt.c\\):\\([0-9]+\\):"
		     1 2 nil 0))

; comments on every cucumber line
; Wenn die REST-API für Mobilfunkverträge aufgerufen wird # features/step_definitions/api_mobilfunk_vertraege_steps.rb:17
(add-to-list 'compilation-error-regexp-alist 'ekr-cucumber-comment)
(add-to-list 'compilation-error-regexp-alist-alist
 	     '(ekr-cucumber-comment "# \\(.+rb\\):\\([0-9]+\\)$"
		     1 2 nil 0))

; build in docker... /app/ entfernen
; /app/src/emil/build.xml:226: Javadoc failed: java.io.IOException: Cannot run program "javadoc": error=2, No such file or directory
(add-to-list 'compilation-error-regexp-alist 'ekr-container-remove-app)
(add-to-list 'compilation-error-regexp-alist-alist
 	     '(ekr-container-remove-app "^/app/\\(.+?\\):\\([0-9]+\\):"
                                     1 2 nil 2))

; pytest/stacktrace
; file /app/tests/test_basics.py, line 14
(add-to-list 'compilation-error-regexp-alist 'ekr-pytest-container-trace)
(add-to-list 'compilation-error-regexp-alist-alist
 	     '(ekr-pytest-container-trace "^file /app/\\(.+?\\), line \\([0-9]+\\)$"
                                     1 2 nil 2))

; pytest / yet another format
;  File "/app/application/routes/halerium.py", line 28
(add-to-list 'compilation-error-regexp-alist 'ekr-pytest-app-error)
(add-to-list 'compilation-error-regexp-alist-alist
 	     '(ekr-pytest-app-error "^ *File \"/app/\\(.+?\\)\", line \\([0-9]+\\)$"
                                     1 2 nil 2))

; ignore pytest timing
; #13 3.878 test_main.py:79:4: E0602: Undefined variable '_clear_all' (undefined-variable)
(add-to-list 'compilation-error-regexp-alist 'ekr-pytest-timing)
(add-to-list 'compilation-error-regexp-alist-alist
 	     '(ekr-pytest-timing "^#[0-9]+ [0-9]+.[0-9]+ \\(.+?\\):\\([0-9]+\\):"
                                     1 2 nil 2))

; ignore /usr/local...
; /usr/local/lib/python3.7/site-packages/urllib3/connection.py:170:
(add-to-list 'compilation-error-regexp-alist 'ekr-ignore-usr-local)
(add-to-list 'compilation-error-regexp-alist-alist
 	     '(ekr-ignore-usr-local "^/usr/local/"
                                     nil nil nil 0))

; also jump between Szenarios...
;  Szenario: xxxxx           # /home/xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx.feature:242
(add-to-list 'compilation-error-regexp-alist 'ekr-cucumber-scenario)
(add-to-list 'compilation-error-regexp-alist-alist
 	     '(ekr-cucumber-scenario "Szenario:.*# \\(.+\\.feature\\):\\([0-9]+\\)"
                                     1 2 nil 1))

; stacktrace
;    [  0] "/Users/KRAEME/Documents/src/akp/acnneu/app/controllers/gps_controller.rb:128:in `block (2 levels) in show_analytics'",
(add-to-list 'compilation-error-regexp-alist 'ekr-ruby-stacktrace)
(add-to-list 'compilation-error-regexp-alist-alist
 	     '(ekr-ruby-stacktrace "^ *.[ 0-9]+. \"\\(.+\\.rb\\):\\([0-9]+\\):"
                                     1 2 nil 2))

; ruby stack trace -> rbenv are low prio
;    [  1] "/Users/KRAEME/.rbenv/versions/2.3.3/lib/ruby/gems/2.3.0/gems/actionpack-4.0.6/lib/action_controller/metal/mime_responds.rb:191:in `respond_to'",
(add-to-list 'compilation-error-regexp-alist 'ekr-rbenv)
(add-to-list 'compilation-error-regexp-alist-alist
 	     '(ekr-rbenv "\\([a-zA-Z0-9_./-]*/.rbenv/.*?\\):\\([0-9]+\\):"
		     1 2 nil 0))

; cucumber output => ignore HTML on first line of HTML stacktrace
;            <pre><code>app/models/concerns/gp_asp.rb:60:in `_update_kundenbetreuer_direktvertrieb&#39;
;        <h2>/Users/KRAEME/Documents/src/akp/acnneu/app/views/doculife_api/akte_index.json.jbuilder:14: syntax error, unexpected keyword_ensure, expecting end-of-input</h2>
(add-to-list 'compilation-error-regexp-alist 'ekr-cucumber-html-pre)
(add-to-list 'compilation-error-regexp-alist-alist
 	     '(ekr-cucumber-html-pre ">\\([a-zA-Z0-9_./-]*?\\.[a-z]+\\):\\([0-9]+\\):"
		     1 2 nil 2))

; weird capybara output
;          Showing <i>/Users/KRAEME/Documents/src/akp/acnneu/app/views/doculife_api/akte_index.json.jbuilder</i> where line <b>#5</b> raised:
(add-to-list 'compilation-error-regexp-alist 'ekr-cucumber-showing)
(add-to-list 'compilation-error-regexp-alist-alist
 	     '(ekr-cucumber-showing "Showing <i>\\(.*?\\)</i> where line <b>.\\([0-9]+\\)</b>"
		     1 2 nil 2))

;; ; rspec... # entfernen
;; ;      # ./spec/acceptance/doculife_api_spec.rb:17:in `block (3 levels) in <top (required)>'
;; (add-to-list 'compilation-error-regexp-alist 'ekr-remove-hash)
;; (add-to-list 'compilation-error-regexp-alist-alist
;;  	     '(ekr-remove-hash "# \\(.+?\\):\\([0-9]+\\)$"
;;                                      1 2 nil 2))

; Yet another style of ruby errors...
;	 3: from /Users/ekkehard.kraemer/Documents/src/akp/acn_neu_tools/bin/acn_tools_jira.rb:146:in `add_watcher'
(add-to-list 'compilation-error-regexp-alist 'ekr-ruby-acntools)
(add-to-list 'compilation-error-regexp-alist-alist
 	     '(ekr-ruby-acntools "from \\([^:]+\\):\\([0-9]+\\):" 1 2 nil 2))

; ignore API warnings...
; /app/src/emil/src/de/edag/fps/emil/AnwendungEMIL.java:54: warning: Signal is internal proprietary API and may be removed in a future release
(add-to-list 'compilation-error-regexp-alist 'ekr-ignore-sunapi)
(add-to-list 'compilation-error-regexp-alist-alist
 	     '(ekr-ignore-sunapi ".*is internal proprietary API and may be removed in a future release.*"
		     nil nil nil 0))

; ignore cucumber artifact...
; -e:1:in `<main>'
(add-to-list 'compilation-error-regexp-alist 'ekr-ignore-minuse)
(add-to-list 'compilation-error-regexp-alist-alist
 	     '(ekr-ignore-minuse "^-e:1:in"
		     nil nil nil 0))

; saxon / xslt errors
; Error at xsl:param on line 770 of ipp_measurement.xsl:
(add-to-list 'compilation-error-regexp-alist 'ekr-saxon-error)
(add-to-list 'compilation-error-regexp-alist-alist
 	     '(ekr-saxon-error "^Error at .*? on line \\([0-9]+\\) of \\(.+\\):$"
                                     2 1 nil 2))

; saxon / xslt error with column
; Error at xsl:call-template on line 598 column 78 of ipp_measurement.xsl:
(add-to-list 'compilation-error-regexp-alist 'ekr-saxon-error-col)
(add-to-list 'compilation-error-regexp-alist-alist
 	     '(ekr-saxon-error-col "^Error at .*? on line \\([0-9]+\\) column \\([0-9]+\\) of \\(.+\\):$"
                                     3 1 2 2))

; saxon / backtrace
;  at xsl:call-template name="tolerances" (file:/Users/KRAEME/Documents/src/doorfitting/process-adapter/config/ipp_measurement.xsl#598)
(add-to-list 'compilation-error-regexp-alist 'ekr-saxon-backtrace)
(add-to-list 'compilation-error-regexp-alist-alist
 	     '(ekr-saxon-backtrace " at .*? (file:\\(.+\\)#\\([0-9]+\\))$"
                                     1 2 nil 2))

; PHP with docker prefix
; #1 /opt/app-root/src/webroot/php/application/AjaxConnector.php(2063): Tools::convertTimestampToDate('@1662716581', false)
(add-to-list 'compilation-error-regexp-alist 'ekr-php-approot)
(add-to-list 'compilation-error-regexp-alist-alist
 	     '(ekr-php-approot "#[0-9]+ /opt/app-root/src/\\(.+?.php\\)(\\([0-9]+\\)): "
                                     1 2 nil 2))

; #2  /opt/app-root/src/tests/unit/KafkaTest.php:134
(add-to-list 'compilation-error-regexp-alist 'ekr-php-codeception-backtrace)
(add-to-list 'compilation-error-regexp-alist-alist
 	     '(ekr-php-codeception-backtrace "/opt/app-root/src/\\(.+?.php\\):\\([0-9]+\\)"
                                     1 2 nil 2))

; PHP Codeception snapshots
;  [Snapshot Saved] file:///opt/app-root/src/tests/_output/debug/2023-01-18_14-02-03_63c7fbdb9f1029.50598734.html
(add-to-list 'compilation-error-regexp-alist 'ekr-php-codeception-snapshot)
(add-to-list 'compilation-error-regexp-alist-alist
 	     '(ekr-php-codeception-snapshot ".*\\[Snapshot Saved\\] file:///opt/app-root/src/\\(.+?.html\\)"
                                     1 nil nil 2))

; PHP Codeception HTML
;Html: /opt/app-root/src/tests/_output/ArchivedMeasurementPlanCest.showListOfArchivedPlans.fail.html
(add-to-list 'compilation-error-regexp-alist 'ekr-php-codeception-html)
(add-to-list 'compilation-error-regexp-alist-alist
 	     '(ekr-php-codeception-html "Html: /opt/app-root/src/\\(.+?.html\\)"
                                     1 nil nil 2))

; PHP Codeception Response
;Response: /opt/app-root/src/tests/_output/ArchivedMeasurementPlanCest.showListOfArchivedPlans.fail.html
(add-to-list 'compilation-error-regexp-alist 'ekr-php-codeception-response)
(add-to-list 'compilation-error-regexp-alist-alist
 	     '(ekr-php-codeception-response "Response: /opt/app-root/src/\\(.+?.html\\)"
                                     1 nil nil 2))

; PHP Codeception Png
;Png: /opt/app-root/src/tests/_output/ArchivedMeasurementPlanCest.showListOfArchivedPlans.fail.png
(add-to-list 'compilation-error-regexp-alist 'ekr-php-codeception-png)
(add-to-list 'compilation-error-regexp-alist-alist
 	     '(ekr-php-codeception-png "Png: /opt/app-root/src/\\(.+?.png\\)"
                                     1 nil nil 2))

; rspec error
;     # ./spec/models/dtag_spec.rb:9:in `block (3 levels) in <top (required)>'
(add-to-list 'compilation-error-regexp-alist 'ekr-rspec-error)
(add-to-list 'compilation-error-regexp-alist-alist
 	     '(ekr-rspec-error "# \\(.+?\\):\\([0-9]+\\):"
                                     1 2 nil 2))

; "helm template" error
; Error: template: legacy-webserver-backup/templates/pvc.yaml:10:25:
(add-to-list 'compilation-error-regexp-alist 'ekr-helm-template-error)
(add-to-list 'compilation-error-regexp-alist-alist
 	     '(ekr-helm-template-error "Error: template: .*/\\(templates/.+?\\):\\([0-9]+\\):\\([0-9]+\\):"
                                     1 2 3 2))

; "helm YAML parse" error
; Error: YAML parse error on legacy-webserver-backup/templates/cronjob.yaml: error converting YAML to JSON: yaml: line 28: did not find expected '-' indicator
(add-to-list 'compilation-error-regexp-alist 'ekr-helm-yaml-parse-error)
(add-to-list 'compilation-error-regexp-alist-alist
 	     '(ekr-helm-yaml-parse-error "Error: YAML parse error on .*/\\(templates/.+?\\): error converting YAML to JSON: yaml: line \\([0-9]+\\):"
                                     1 2 nil 2))

; internal helm go errors, ignore
; install.go:222: [debug] Original chart version: ""
(add-to-list 'compilation-error-regexp-alist 'ekr-helm-ignore-go)
(add-to-list 'compilation-error-regexp-alist-alist
 	     '(ekr-helm-ignore-go "[^.:]+\\.go:[0-9]+:"
                                     nil nil nil 0))

; yaml image tag, ignore
;            image: mtr.devops.telekom.de/akp/legacy-webserver-backup:1.0.0
(add-to-list 'compilation-error-regexp-alist 'ekr-yaml-image-tag-ignore)
(add-to-list 'compilation-error-regexp-alist-alist
 	     '(ekr-yaml-image-tag-ignore "[\t ]image:[\t ]"
                                     nil nil nil 0))

; Dockerfiles
; Dockerfile.akp-web:98
(add-to-list 'compilation-error-regexp-alist 'ekr-dockerfile)
(add-to-list 'compilation-error-regexp-alist-alist
 	     '(ekr-dockerfile "^\\(Dockerfile[^:]+\\):\\([0-9]+\\)$"
                                     1 2 nil 2))

; undo the last add-to-list:
; (setq compilation-error-regexp-alist-alist (cdr compilation-error-regexp-alist-alist))

; REGEXP FILE [LINE COLUMN TYPE HYPERLINK HIGHLIGHT...]
; TYPE is 2 or nil for a real error or 1 for warning or 0 for info.

; erstes (also letztes ;) ) entfernen, beim Entwickeln
;(setq compilation-error-regexp-alist-alist (cdr compilation-error-regexp-alist-alist))

; (setenv "TERM" "dumb")  ; for perldoc etc.
; (setenv "PAGER" "cat")
(setenv "EMACS" "1")

;;;; obsolete X mode ;;;;
;;;; obsolete X mode ;;;;;; Font
;;;; obsolete X mode ;;;;;; ====
;;;; obsolete X mode ;;;;
;;;; obsolete X mode ;;;;;; http://emacs-fu.blogspot.de/2009/06/setting-fonts.html
;;;; obsolete X mode ;;;;;; http://damieng.com/blog/2008/05/26/envy-code-r-preview-7-coding-font-released  => Put .ttfs in ~/.fonts
;;;; obsolete X mode ;;;;
;;;; obsolete X mode ;;;;;; .Xresources:
;;;; obsolete X mode ;;;;; -- Usage: "xrdb .Xresources" to activate
;;;; obsolete X mode ;;;;;
;;;; obsolete X mode ;;;;; -- XTerm*faceName: Inconsolata
;;;; obsolete X mode ;;;;; -- XTerm*faceSize: 10
;;;; obsolete X mode ;;;;;
;;;; obsolete X mode ;;;;; XTerm*faceName: Envy Code R
;;;; obsolete X mode ;;;;; XTerm*faceSize: 10
;;;; obsolete X mode ;;;;;
;;;; obsolete X mode ;;;;; Emacs.font: Envy Code R-10
;;;; obsolete X mode ;;;;
;;;; obsolete X mode ;;;;;; Or, older:
;;;; obsolete X mode ;;;;;  '(default-frame-alist (quote ((menu-bar-lines . 1) (font . "-unknown-Inconsolata-normal-normal-normal-*-14-*-*-*-m-0-iso10646-1"))))
;;;; obsolete X mode ;;;;
;;;; obsolete X mode ;;;;;;;;;;;; All of this works well and looks very good, but it slow. Fast mode:
;;;; obsolete X mode ;;;;
;;;; obsolete X mode ;;;;;Emacs*verticalScrollBars: off
;;;; obsolete X mode ;;;;;Emacs.FontBackend: x
;;;; obsolete X mode ;;;;
;;;; obsolete X mode ;;;;
;;;; obsolete X mode ;;;;;; Fontconfig
;;;; obsolete X mode ;;;;;; ==========
;;;; obsolete X mode ;;;;
;;;; obsolete X mode ;;;;;; ~/.fonts/fonts.conf
;;;; obsolete X mode ;;;;; <?xml version="1.0"?>
;;;; obsolete X mode ;;;;; <!DOCTYPE fontconfig SYSTEM "fonts.dtd">
;;;; obsolete X mode ;;;;; <!-- /etc/fonts/fonts.conf file to configure system font access -->
;;;; obsolete X mode ;;;;; <fontconfig>
;;;; obsolete X mode ;;;;;
;;;; obsolete X mode ;;;;; <match target="font">
;;;; obsolete X mode ;;;;;     <edit name="antialias" mode="assign">
;;;; obsolete X mode ;;;;;       <bool>true</bool>
;;;; obsolete X mode ;;;;;     </edit>
;;;; obsolete X mode ;;;;; </match>
;;;; obsolete X mode ;;;;;
;;;; obsolete X mode ;;;;; <match target="font">
;;;; obsolete X mode ;;;;;     <edit name="hinting" mode="assign">
;;;; obsolete X mode ;;;;;       <bool>true</bool>
;;;; obsolete X mode ;;;;;     </edit>
;;;; obsolete X mode ;;;;; </match>
;;;; obsolete X mode ;;;;;
;;;; obsolete X mode ;;;;; <match target="font">
;;;; obsolete X mode ;;;;;     <edit name="autohint" mode="assign">
;;;; obsolete X mode ;;;;;       <bool>false</bool>
;;;; obsolete X mode ;;;;;     </edit>
;;;; obsolete X mode ;;;;; </match>
;;;; obsolete X mode ;;;;;
;;;; obsolete X mode ;;;;; <match target="font">
;;;; obsolete X mode ;;;;;     <edit name="hintstyle" mode="assign">
;;;; obsolete X mode ;;;;;       <const>hintfull</const>
;;;; obsolete X mode ;;;;;     </edit>
;;;; obsolete X mode ;;;;; </match>
;;;; obsolete X mode ;;;;;
;;;; obsolete X mode ;;;;; <match target="font">
;;;; obsolete X mode ;;;;;     <edit name="rgba" mode="assign">
;;;; obsolete X mode ;;;;;       <const>rgb</const>
;;;; obsolete X mode ;;;;;     </edit>
;;;; obsolete X mode ;;;;; </match>
;;;; obsolete X mode ;;;;;
;;;; obsolete X mode ;;;;; <match target="font">
;;;; obsolete X mode ;;;;;     <edit mode="assign" name="lcdfilter">
;;;; obsolete X mode ;;;;;       <const>lcddefault</const>
;;;; obsolete X mode ;;;;;     </edit>
;;;; obsolete X mode ;;;;; </match>
;;;; obsolete X mode ;;;;;
;;;; obsolete X mode ;;;;; </fontconfig>

; cucumber mode
;(add-to-list 'load-path "~/.emacs.d/elisp/feature-mode")
;(setq feature-default-i18n-file "/path/to/gherkin/gem/i18n.yml")
(setq feature-default-language "fi")
(add-to-list 'auto-mode-alist '("\.feature$" . feature-mode))
;; Keybinding	Description
;; C-c ,v	Verify all scenarios in the current buffer file.
;; C-c ,s	Verify the scenario under the point in the current buffer.
;; C-c ,f	Verify all features in project. (Available in feature and ruby files)
;; C-c ,r	Repeat the last verification process.
;; C-c ,g	Go to step-definition under point (requires ruby_parser gem >= 2.0.5)
(define-key feature-mode-map  (kbd "C-c ,v") 'ekr-feature-verify-dev-scenarios-in-buffer)
(define-key feature-mode-map  (kbd "C-c ,V") 'feature-verify-all-scenarios-in-buffer)

(defun ekr-feature-verify-dev-scenarios-in-buffer ()
  "Run all the @dev tagged scenarios defined in current buffer."
  (interactive)
  (let* ((abs-file-name (buffer-file-name))
         ;; Cut off path elements up to and including "acnneu"
         (relative-path (if (and abs-file-name
                                  (string-match "\\(.*\\)/acnneu/\\(.*\\)" abs-file-name))
                            (match-string 2 abs-file-name)
                          abs-file-name))) ; Fallback to the absolute path if "acnneu" not found
    ;; Run Cucumber with the relative path
    (feature-run-cucumber '("--tags @dev") :feature-file relative-path)))

; hiermit erkennt u.a. der Compilation Buffer (=> cucumber) utf8 korrekt
;(prefer-coding-system 'utf-8)
; ... wird aber in .dir-locals.el eingestellt (auf andere Weise - genügt das?)

;; included in 28.2, not required anymore
;; ANSI coloring in compilation buffers
;; (require 'ansi-color)
;; (defun ff/ansi-colorize-buffer ()
;; ;  (setq buffer-read-only nil)
;; ;  (ansi-color-apply-on-region (point-min) (point-max))
;; ;  (setq buffer-read-only t)
;;   )
;; (add-hook 'compilation-filter-hook 'ff/ansi-colorize-buffer)

;; ; ANSI coloring in compilation buffers
(add-hook 'compilation-filter-hook 'ansi-color-compilation-filter)

; ANSI coloring for any buffer
; (require 'tty-format)
;; M-x display-ansi-colors to explicitly decode ANSI color escape sequences

; this accesses variable ansi-color-regexp in tty_format; the variable is undefined
;; (defun display-ansi-colors ()
;;   "Enable ANSI colors in the current buffer."
;;   (interactive)
;;   (format-decode-buffer 'ansi-colors))
;; ;; decode ANSI color escape sequences for *.txt or README files
;; (add-hook 'find-file-hooks 'tty-format-guess)

(require 'ansi-color)
(defun display-ansi-colors ()
  (interactive)
  (ansi-color-apply-on-region (point-min) (point-max)))

;; decode ANSI color escape sequences for .log files
(add-to-list 'auto-mode-alist '("\\.log\\'" . display-ansi-colors))

;;;;;;;;;;;;;;;;;;; PUTTY

;; starten mit "TERM=xterm-256color emacs -nw"
;; Font: Consolas 11 Point, Cleartype
;; SSH cmd:   screen -dR ekremacs bash -c "TERM=xterm-256color emacs -nw"

;; Autohotkey:

;; #IfWinActive, ahk_class PuTTY
;;
;; ; # Win (Windows logo key)
;; ; ! Alt
;; ; ^ Control
;; ; + Shift
;;
;; End:: Send ^e
;; +End:: Send ^{Space}^e
;;
;; Home:: Send ^a
;; +Home:: Send ^{Space}^a
;;
;; ^End:: Send !>
;; +^End:: Send ^{Space}!>
;;
;; ^Home:: Send !<
;; +^Home:: Send ^{Space}!<
;;
;;
;; ^Left:: Send !{Left}
;; ^Right:: Send !{Right}
;;
;; +Left:: Send ^{Space}{Left}
;; +Right:: Send ^{Space}{Right}
;; +Up:: Send ^{Space}{Up}
;; +Down:: Send ^{Space}{Down}
;;
;; #IfWinActive

;;;;;  oben:      '(custom-enabled-themes (quote (deeper-blue)))

; rails pdf prawn
(add-to-list 'auto-mode-alist '("\.pdf\.prawn$" . ruby-mode))

;;;; in PUTTY, not X11:
;; (custom-set-faces
;;  ;; custom-set-faces was added by Custom.
;;  ;; If you edit it by hand, you could mess it up, so be careful.
;;  ;; Your init file should contain only one such instance.
;;  ;; If there is more than one, they won't work right.
;;  '(default ((t (:inherit nil :stipple nil :background "#181818" :foreground "gray80" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 1 :width normal :foundry "default" :family "default"))))
;;  '(font-lock-comment-delimiter-face ((t (:inherit font-lock-comment-face :foreground "gray55"))))
;;  '(font-lock-comment-face ((t (:foreground "gray55")))))

;; (require 'package)
;; (add-to-list 'package-archives
;;              '("melpa-stable" . "http://stable.melpa.org/packages/") t)
;; (when (< emacs-major-version 24)
;;   ;; For important compatibility libraries like cl-lib
;;   (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))
;;
;; ; for package org-plus-contrib -> ox-confluence
;; (add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/") t)
;;
;; (package-initialize) ;; You might already have this line

;;;;;;;;;;;;;;;;;;; mintty xterm-mouse mode (cut, paste, windows clipboard...)
;;; clipboard is via VcXsrv

(xterm-mouse-mode)

; https://hugoheden.wordpress.com/2009/03/08/copypaste-with-emacs-in-terminal/
;; I prefer using the "clipboard" selection (the one the
;; typically is used by c-c/c-v) before the primary selection
;; (that uses mouse-select/middle-button-click)
;(setq x-select-enable-clipboard nil)
;(setq x-select-enable-primary t)

;; If emacs is run in a terminal, the clipboard- functions have no
;; effect. Instead, we use of xsel, see
;; http://www.vergenet.net/~conrad/software/xsel/ -- "a command-line
;; program for getting and setting the contents of the X selection"
;; (unless window-system
;;   (when (getenv "DISPLAY")
;;     ;; Callback for when user cuts
;;     (defun xsel-cut-function (text &optional push)
;;       ;; Insert text to temp-buffer, and "send" content to xsel stdin
;;       (with-temp-buffer
;;         (insert text)
;;         ;; I prefer using the "clipboard" selection (the one the
;;         ;; typically is used by c-c/c-v) before the primary selection
;;         ;; (that uses mouse-select/middle-button-click)
;;         (call-process-region (point-min) (point-max) "xsel-quick" 0 nil 0 "--input")))
;;     ;; Call back for when user pastes
;;     (defun xsel-paste-function()
;;       ;; Find out what is current selection by xsel. If it is different
;;       ;; from the top of the kill-ring (car kill-ring), then return
;;       ;; it. Else, nil is returned, so whatever is in the top of the
;;       ;; kill-ring will be used.
;;       (let ((xsel-output (shell-command-to-string "xsel --output")))
;;         (unless (string= (car kill-ring) xsel-output)
;;           xsel-output )))
;;     ;; Attach callbacks to hooks
;;     (setq interprogram-cut-function 'xsel-cut-function)
;;     (setq interprogram-paste-function 'xsel-paste-function)
;;     ;; Idea from
;;     ;; http://shreevatsa.wordpress.com/2006/10/22/emacs-copypaste-and-x/
;;     ;; http://www.mail-archive.com/help-gnu-emacs@gnu.org/msg03577.html
;;      ))

;; (setq interprogram-cut-function nil)
;; (setq interprogram-paste-function nil)

;; .html.erb etc.

;(add-to-list 'auto-mode-alist '("\.html.erb$" . ruby-mode))
;(add-to-list 'auto-mode-alist '("\.xml.erb$" . xml-mode))
;(add-to-list 'auto-mode-alist '("\.js.erb$" . javascript-mode))

;; (require 'web-mode)
(add-to-list 'auto-mode-alist '("\.erb$" . web-mode))
(add-to-list 'auto-mode-alist '("\.jsp$" . web-mode))

(add-to-list 'auto-mode-alist '("\.builder$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\.axlsx$" . ruby-mode))

(add-to-list 'auto-mode-alist '("\.sql" . sql-mode))

;(load-library "sql-indent/sql-indent")
;(eval-after-load "sql"
;  '(load-library "sql-indent/sql-indent"))

; (require 'git)

(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)

; Ctrl-j etc. für "div" => "<div></div>"
;(add-to-list 'load-path "~/.emacs.d/elisp/emmet-mode")
;; (require 'emmet-mode)
;; (add-hook 'web-mode-hook 'emmet-mode)

;(setq electric-indent-functions-without-reindent (remove 'indent-line-function electric-indent-functions-without-reindent))

(setq-default electric-indent-inhibit t)

(recentf-mode 1)
(setq recentf-max-menu-items 25)
(global-set-key "\C-x\ \C-r" 'recentf-open-files)
;;;
;;; magit
;;;

(setenv "GIT_ASKPASS" "git-gui--askpass")

(global-set-key (kbd "C-x g") 'magit-status)
(global-set-key (kbd "C-x C-g") 'magit-dispatch-popup)

(if (string= default-directory "~/src/akp_test_sf/")
    (progn
      (custom-set-faces
       ;; custom-set-faces was added by Custom.
       ;; If you edit it by hand, you could mess it up, so be careful.
       ;; Your init file should contain only one such instance.
       ;; If there is more than one, they won't work right.
       '(default ((t (:inherit nil :stipple nil :background "DodgerBlue4"
                               :foreground "#eeeeec" :inverse-video nil :box nil :strike-through nil
                               :overline nil :underline nil :slant normal :weight normal :height 113
                               :width normal :foundry "unknown" :family "DejaVu Sans Mono")))))
      (message "Special face for akp_test_sf..."))
  (if (string= default-directory "~/src/svn2_adlerfelsen/")
      (progn
        (custom-set-faces
         '(default ((t (:inherit nil :stipple nil :background "DarkOrange4"
                                 :foreground "#eeeeec" :inverse-video nil :box nil :strike-through nil
                                 :overline nil :underline nil :slant normal :weight normal :height 113
                                 :width normal :foundry "unknown" :family "DejaVu Sans Mono")))))
        (message "Special face for svn2_adlerfelsen..."))
    (progn
      (message "Standard face...")
      (message default-directory))))


; maximise on windows
; (run-at-time "1" nil '(lambda () (toggle-frame-maximized)))

;; (require 'string-inflection)
;; (global-unset-key (kbd "C-q"))
;; (global-set-key (kbd "C-q C-u") 'my-string-inflection-cycle-auto)
;; (defun my-string-inflection-cycle-auto ()
;;   "switching by major-mode"
;;   (interactive)
;;   (cond
;;    ;; for emacs-lisp-mode
;;    ((eq major-mode 'emacs-lisp-mode)
;;     (string-inflection-all-cycle))
;;    ;; for java
;;    ((eq major-mode 'java-mode)
;;     (string-inflection-java-style-cycle))
;;    (t
;;     ;; default
;;     (string-inflection-ruby-style-cycle))))

; avoid extreme pauses on long compilation lines
;; (require 'truncated-compilation-mode)
;; (truncated-compilation-mode)

; frame commands
;(require 'frame-cmds)

;; default smerge bindings
;;
;; C-c ^ RET       smerge-keep-current
;; C-c ^ =         Prefix Command
;; C-c ^ C         smerge-combine-with-next
;; C-c ^ E         smerge-ediff
;; C-c ^ R         smerge-refine
;; C-c ^ a         smerge-keep-all
;; C-c ^ b         smerge-keep-base
;; C-c ^ m         smerge-keep-mine
;; C-c ^ n         smerge-next
;; C-c ^ o         smerge-keep-other
;; C-c ^ p         smerge-prev
;; C-c ^ r         smerge-resolve
;;
;; C-c ^ = <       smerge-diff-base-mine
;; C-c ^ = =       smerge-diff-mine-other
;; C-c ^ = >       smerge-diff-base-other

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(line-number ((t (:inherit (shadow default) :foreground "gray32"))))
 '(mode-line ((t (:background "#2e3436" :foreground "#d3d7cf" :box (:line-width -1 :style released-button))))))

;; (defun notify-compilation-result(buffer msg)
;;   "Notify that the compilation is finished,
;; close the *compilation* buffer if the compilation is successful,
;; and set the focus back to Emacs frame"
;;   (if (string= "*compilation*" (buffer-name buffer))
;;       (if (string-match "^finished" msg)
;;           (progn
;;             (delete-windows-on buffer)
;;             (tooltip-show "\n Compilation Successful :-) \n "))
;;         (tooltip-show "\n Compilation Failed :-( \n "))
;;                                         ; (setq current-frame (car (car (cdr (current-frame-configuration)))))
;;                                         ; (select-frame-set-input-focus current-frame)
;;     ))
;;
;; (add-to-list 'compilation-finish-functions
;; 	     'notify-compilation-result)


;;;;;;; special ruby stuff (packages installed specifically for that)
; https://lorefnon.me/2014/02/02/configuring-emacs-for-rails.html

; ruby shell
(global-set-key (kbd "C-c r r") 'inf-ruby-console-auto)

;;;;;; projectile

(projectile-mode +1)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
(define-key projectile-mode-map (kbd "C-c p g") #'projectile-grep)

; deadgrep instead of projectile's search...
; (define-key projectile-mode-map (kbd "C-c p g") #'projectile-grep t) ; to remove previous def
;(define-key projectile-mode-map (kbd "C-c p g") #'deadgrep t) ; to remove previous def

(let ((args (help-function-arglist #'define-key)))
  (cond
   ((and args (= (length args) 4))
    (define-key projectile-mode-map (kbd "C-c p g") #'deadgrep t))
   ((and args (= (length args) 3))
    (define-key projectile-mode-map (kbd "C-c p g") #'deadgrep))
   (t
    (message "Warning: `define-key` does not take 3 or 4 parameters; skipping binding."))))

(global-set-key (kbd "C-c p g") #'deadgrep) ; doesnt work...

;;;;;;; Mac

; to be able to use Option-7 for | etc.

; originally 'super
(if (boundp 'ns-command-modifier)
    (setq ns-command-modifier 'meta))

; originally 'meta
(if (boundp 'ns-option-modifier)
    (setq ns-option-modifier nil))

; specifically nice for AHK-mode which has indentation bugs when there are trailing spaces
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; naybe start docker-machine first ("docker-machine start default")
(if (file-directory-p "/Users/KRAEME/.docker/machine")
    (progn
      (shell-command "docker-machine start")
      (setenv "DOCKER_HOST" (shell-command-to-string "docker-machine env default | grep DOCKER_HOST | sed -e 's/export DOCKER_HOST=.//' -e 's/.$//' | tr -d '\n'"))
      (setenv "DOCKER_TLS_VERIFY" "1")
      (setenv "DOCKER_CERT_PATH" "/Users/KRAEME/.docker/machine/machines/default")
      (setenv "DOCKER_MACHINE_NAME" "default")))

; DOCKER_BUILDKIT does not look good in *compilation* buffers...
(setenv "BUILDKIT_PROGRESS" "plain")

;;; folding in xml

;; (require 'hideshow)
;; (require 'sgml-mode)
;; (require 'nxml-mode)
;; (add-to-list 'hs-special-modes-alist
;;              '(nxml-mode
;;                "<!--\\|<[^/>]*[^/]>"
;;                "-->\\|</[^/>]*[^/]>"
;;
;;                "<!--"
;;                sgml-skip-tag-forward
;;                nil))
;; (add-hook 'nxml-mode-hook 'hs-minor-mode)
;; (define-key nxml-mode-map (kbd "C-c h") 'hs-toggle-hiding)

; Windows Powershell

;(require 'powershell)
(add-to-list 'auto-mode-alist '("\.ps1$" . powershell-mode))
(add-to-list 'auto-mode-alist '("\.ps2$" . powershell-mode))

; dark mode

(menu-bar-mode -1)
(toggle-scroll-bar -1)

;;;;;;;; embark & marginalia

(use-package embark
  :ensure t

  :bind
  (("C-." . embark-act)         ;; pick some comfortable binding
   ("C-;" . embark-dwim)        ;; good alternative: M-.
   ("C-h B" . embark-bindings)) ;; alternative for `describe-bindings'

  :init

  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)

  :config

  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

; fix Home key on MacOs

(global-set-key (kbd "<home>") 'move-beginning-of-line)
(global-set-key (kbd "<end>") 'move-end-of-line)

(savehist-mode 1)
(add-to-list 'savehist-additional-variables 'compile-command)
(add-to-list 'savehist-additional-variables 'compile-history)
(add-to-list 'savehist-additional-variables 'compilation-directory)
(add-to-list 'savehist-additional-variables 'shell-command-history)

; ox-confluence, export org to confluence/jira markup

; (load-library "ox-confluence")

;(when (version<= "26.0.50" emacs-version )
;  (global-display-line-numbers-mode))

; golang

(defun go-mode-setup ()
  "Also see https://andrewjamesjohnson.com/configuring-emacs-for-go-development/ ."
  (add-hook 'before-save-hook 'gofmt-before-save)
  )
(add-hook 'go-mode-hook 'go-mode-setup)

(if (file-directory-p "~/Documents/src")
  (run-at-time "2 sec" nil (lambda ()
                             (dolist (buffer (buffer-list))
                               (progn
                                 (set-buffer buffer)
                                 (cd "~/Documents/src"))))))

;; org-jira

(setq jiralib-url "https://jira-caps-ext.nttdata-emea.com")

; set up unicode symbols (order matters!)

(set-fontset-font
 t
 'emoji
 (cond
  ((member "Apple Color Emoji" (font-family-list)) "Apple Color Emoji")
  ((member "Noto Color Emoji" (font-family-list)) "Noto Color Emoji")
  ((member "Noto Emoji" (font-family-list)) "Noto Emoji")
  ((member "Segoe UI Emoji" (font-family-list)) "Segoe UI Emoji")  ; 🧗
  ((member "Symbola" (font-family-list)) "Symbola")))

(set-fontset-font
 t
 'symbol
 (cond
  ((member "Segoe UI Symbol" (font-family-list)) "Segoe UI Symbol")
  ((member "Apple Symbols" (font-family-list)) "Apple Symbols")
  ((member "Symbola" (font-family-list)) "Symbola")))

; nice on windows...

(cond
  ((eq system-type 'windows-nt)
   (set-fontset-font t '(#x1F300 . #x1F5FF) "Segoe UI Symbol")))  ; 🔁, Miscellaneous Symbols and Pictographs

; to find fonts for a given character, interactively execute print-all-fonts in *scratch*:

(defun insert-character-with-font (char font)
  "Insert CHAR into the current buffer with FONT."
  (let ((start (point)))
    (insert char)
    (add-text-properties start (point)
                         `(face (:family ,font)))))

(defun print-all-fonts ()
  "Print details of all installed fonts."
  (interactive)
  (font-lock-mode -1)
  (set-fontset-font t 'symbol nil)
  (set-fontset-font t 'emoji nil)
  (let ((fonts (font-family-list)))
    (dolist (font fonts)
      (insert-character-with-font "🔁" font)  ; 01F501 - Segoe UI Symbol is best
      (insert " ")
      (insert-character-with-font "🧗" font)  ; 01F9D7 - only in Segoe UI Emoji
      (insert (prin1-to-string font))
      (insert "\n"))))

; php

(add-to-list 'auto-mode-alist '("\.php$" . php-mode))

; default font

;; Also: C-u C-x = to display the current font
;; Also: (set-face-attribute 'default nil :font "-*-Menlo-regular-normal-normal-*-12-*-*-*-m-0-iso10646-1") ;; default on MacOS 05/2024, very nice
;; Also: M-x show-font-list
;; Also: M-x describe-font - display the concrete name of the current font
;;                         - <Tab> to see all detailled font descriptors
;; ALSO!!! Shift-click the frame to visually choose font!

(defun ekr-set-fonts ()
  "Setup fonts."
  (interactive)
  (message (cond ((eq system-type 'gnu/linux)
                  (set-frame-font "-*-Inconsolata-normal-normal-normal-*-17-*-*-*-m-0-iso10646-1")
                  (set-frame-position (selected-frame) 3630 2)
                  (set-frame-size (selected-frame) 152 78)
                  "Font/windows setup: WSL")
                 ((eq system-type 'windows-nt)
                  (set-frame-font "-outline-Inconsolata SemiExpanded-bold-normal-normal-mono-16-*-*-*-c-*-iso10646-1")
                  "Font/windows setup: Windows native")
                 ((eq system-type 'darwin)
                                        ; "-*-Menlo-regular-normal-normal-*-12-*-*-*-m-0-iso10646-1"
                  "Font/windows setup: MacOS")
                 ("Font/windows setup: Unknown system type"))))

; (set-frame-font "-outline-Inconsolata-regular-normal-normal-mono-16-*-*-*-c-*-iso10646-1")
; (set-frame-font "-outline-Inconsolata SemiExpanded ExtraB-extrabold-normal-normal-mono-16-*-*-*-c-*-iso10646-1")
; (set-frame-font "-outline-Inconsolata SemiExpanded-bold-normal-normal-mono-16-*-*-*-c-*-iso10646-1")

;; (let ((print-length 999)
;;       (print-level 999))
;;   (pp (current-frame-configuration)))

(add-hook 'emacs-startup-hook
          (lambda () (run-at-time "1 sec" nil ekr-set-fonts)))

; plantuml

(setq plantuml-jar-path "/usr/local/Cellar/plantuml/1.2024.3/libexec/plantuml.jar")
(setq plantuml-default-exec-mode 'jar)

; good-auto

;; (defun ekr-open-chat-file ()
;;   "Open or create a chat file with the name format chat-YYYYMMDD.md in ~/Documents/src/ai."
;;   (let* ((current-date (format-time-string "%Y%m%d"))  ; Get current date in YYYYMMDD format
;;          (file-name (concat "~/Documents/ai/chat-" current-date ".md"))  ; Construct file name
;;          (file-path (expand-file-name file-name)))  ; Expand to full path
;;     (find-file-noselect file-path)))  ; Open the file but don't switch to it

;; (defun ekr-get-query ()
;;   "Prompt the user for a query, returning the selected region,
;;   user input, or the output of a shell command if the current buffer
;;   is named COMMIT_EDITMSG."
;;   (cond
;;    ;; Check if the current buffer name begins with "COMMIT_EDITMSG":
;;    ((string-prefix-p "COMMIT_EDITMSG" (buffer-name))
;;
;;     ;; Get the name of the currently active Git branch
;;     (let ((branch-name (string-trim (shell-command-to-string "git rev-parse --abbrev-ref HEAD 2>/dev/null"))))
;;       ;; Create the prompt including the branch name and the git diff
;;       (list (concat ;"Forget all previous context from this chat. "
;;                     "Create a commit message, prefixed with the branch name '" branch-name ":'. "
;;                     "Make sure it does not use more than 80 characters. "
;;                     "Output *only* the message with no additional text. "
;;                     "Do not output any markdown. "
;;                     "Make sure the text is on one line with no newlines. "
;;                     "If you feel the need, you can add a newline separated bullet point list with more details. "
;;                     "Here is the diff:\n"
;;                     (shell-command-to-string "git diff --cached 2>&1")) t)))
;;
;;    ;; ;; Check if there is "TODO" in the current line
;;    ;; ((string-match "TODO" (thing-at-point 'line t))
;;    ;;  (let ((start (max (line-beginning-position -40) (point-min)))
;;    ;;        (end (min (line-end-position 40) (point-max))))
;;    ;;    ;; Fetch the context lines
;;    ;;    (buffer-substring-no-properties start end)))
;;
;;    ;; Check if there is "TODO" in the current line
;;    ((string-match "TODO" (thing-at-point 'line t))
;;     (let ((current-line (line-number-at-pos))                  ; Get the current line number
;;           (full-buffer (buffer-string)))                      ; Get the entire buffer content
;;       ;; Prepare the full prompt with instructions
;;       (concat "The actual prompt is on line " (number-to-string current-line) " and marked by the word 'TODO'. Please find the context below:\n\n"
;;               full-buffer)))                                   ; Return the whole buffer contents
;;
;;    ;; Check if a region is active
;;    ((region-active-p)
;;     (buffer-substring (region-beginning) (region-end)))
;;
;;    ;; Otherwise, prompt the user for input
;;    (t
;;     (read-string "Enter query: "))))

;; (defun ekr-process-answer (query answer)
;;   "Process the provided ANSWER content and insert it into the buffer.
;; QUERY is the original query used to generate the answer."
;;   (let ((answer (with-temp-buffer
;;                   (insert-file-contents "~/bin/good-auto/data/answer.txt")
;;                   (buffer-string)))
;;         (answer-buffer (ekr-open-chat-file)))
;;
;;     (with-current-buffer answer-buffer
;;       (let ((start-pos (point-max)))
;;
;;         ;; Activate the answer buffer for following commands
;;         (pop-to-buffer answer-buffer)
;;
;;         ;; Add prompt as level-1 header, ensuring it is shortened if necessary
;;         (goto-char start-pos)
;;         (insert "\n\n══════════════════════════════════════════════════════════════════════════════════════════════════════════════════\n\n")
;;         (insert "# Prompt: " (replace-regexp-in-string
;;                             "\n" " "
;;                             (if (> (length query) 80)
;;                                 (concat (substring query 0 80) "...")
;;                               query)) "\n\n")
;;
;;         ;; Add new answer, ensuring there is no level-1 header
;;         (insert (replace-regexp-in-string "\\(^\\|\n\\)#" "\\1##" answer))
;;         (insert "\n")
;;
;;         ;; Handle potential ANSI color codes
;;         (display-ansi-colors)
;;
;;         ;; Ensure markdown is active
;;         (markdown-mode)
;;         (markdown-toggle-markup-hiding 1)
;;
;;         ;; Break long lines
;;         (goto-char start-pos)
;;         (while (not (eobp))
;;           (when (> (line-end-position) fill-column)
;;             (fill-paragraph))
;;           (forward-line 1))
;;
;;         ;; Fold away the previous answer
;;         (goto-char start-pos)
;;         (search-backward "# Prompt:" nil t)
;;         (markdown-cycle)
;;
;;         ;; Redisplay in a controlled manner, vertically
;;         (goto-char start-pos)
;;         (search-forward "# Prompt:" nil t)
;;         (beginning-of-line)
;;         (markdown-enter-key)
;;         (recenter 7)
;;
;;         ;; Save to disk
;;         (save-buffer)
;;
;;         ;; Return to previous window
;;         (other-window 1)))))

(defun ekr-try-insert-branch-name (branch-name reps commit-buffer)
  "Wait until copilot has finished (by busy waiting on the *Messages* buffer) and insert the BRANCH-NAME into the commit message.  REPS is the countdown to timeout."

  (if (< reps 0)
      (message "ekr-try-insert-branch-name: did not find completion message, giving up.")
    (progn
      ;; extract the last 3 lines from the buffer *Messages* into a string
      (let ((messages-buffer (get-buffer "*Messages*"))
            (finished nil))
        (progn
          ;; check if copilot is finished
          (when messages-buffer
            (with-current-buffer messages-buffer
              (save-excursion
                (goto-char (point-max))
                (forward-line -3)
                (if (string-match "Commit message generation completed."
                                  (buffer-substring-no-properties (point) (point-max)))
                    (setq finished t)))))
          (if finished
              (with-current-buffer commit-buffer
                ;; insert the branch name at the beginning of the buffer
                (goto-char (point-min))
                (insert (concat branch-name ": ")))
            (progn
              (run-at-time "0.5 sec" nil 'ekr-try-insert-branch-name branch-name (- reps 1) commit-buffer))))))))

(defun ekr-insert-commit-msg ()
  "Run copilot to figure out a commit message.  Make sure the branch name is included."
  (copilot-mode -1)
  (let ((branch-name (string-trim (shell-command-to-string "git rev-parse --abbrev-ref HEAD 2>/dev/null"))))
    (message "flush 1")
    (message "flush 2")
    (message "flush 3")
    (message "flush 4")
    ;; (copilot-chat-insert-commit-message)   ; has a 1 sec timer
    (copilot-chat-insert-commit-message-when-ready)   ; is async with aio
    (run-at-time "0.5 sec" nil 'ekr-try-insert-branch-name branch-name 20 (current-buffer))))

(defun ekr-run-good-auto ()
  "Execute a Good-Auto query."
  (interactive)

  ;; if the current buffer is the git commit message, then run copilot-chat-insert-commit-message
  (if (string-prefix-p "COMMIT_EDITMSG" (buffer-name))
      (ekr-insert-commit-msg)
    (progn

      (error "This function is deprecated and only to be used in COMMIT_EDITMSG.")

      ;; ;; else, do the whole good-auto thing...
      ;;
      ;; ;; Prompt for query and write it to input.txt
      ;;
      ;; (let ((query (ekr-get-query)))
      ;;
      ;;   ;; Delete existing answer file
      ;;   (when (file-exists-p "~/bin/good-auto/data/answer.txt")
      ;;     (delete-file "~/bin/good-auto/data/answer.txt"))
      ;;
      ;;   ;; TODO: query normally is a string. but sometimes it is an list. in this case, insert the first element of it (which will be a string)
      ;;   (with-temp-buffer
      ;;     (insert (if (and (listp query) (not (null query)))
      ;;                 (nth 0 query)  ;; If query is a list, take the first element
      ;;               query))
      ;;     (write-region (point-min) (point-max) "~/bin/good-auto/data/input.txt"))
      ;;
      ;;   ;; Wait until the answer.txt exists, with timeout
      ;;   (let ((file-exists nil)
      ;;         (count 0))
      ;;     (while (and (not file-exists) (< count (* 4 60)))
      ;;       (setq file-exists (file-exists-p "~/bin/good-auto/data/answer.txt"))
      ;;       (sleep-for 0.25)
      ;;       (setq count (1+ count)))
      ;;
      ;;     (if file-exists
      ;;         ;; Read answer content from the file
      ;;         (let ((answer (with-temp-buffer
      ;;                         (insert-file-contents "~/bin/good-auto/data/answer.txt")
      ;;                         (buffer-string))))
      ;;           ;; If query is a list with the t flag, just paste
      ;;           (if (and (listp query)
      ;;                    (eq (nth 1 query) t))
      ;;               (insert answer)
      ;;             ;; Process the answer content if it did not match the list case
      ;;             (ekr-process-answer query answer)))
      ;;
      ;;       (message "No answer file found.")))))))
      )))

(global-set-key (kbd "©") 'ekr-run-good-auto)

; bash-mode

(add-to-list 'auto-mode-alist '("\.sh$" . bash-mode))

; tree-sitter

;; $ tree-sitter init-config
;; -> Library/Application Support/tree-sitter/config.json

;; ------- manual compile ------------------
;; https://www.masteringemacs.org/article/how-to-get-started-tree-sitter
;;

(if (executable-find "tree-sitter")
    (progn
      (message "Tree-sitter binary is available.")

      (setq treesit-language-source-alist
            '(
              (bash "https://github.com/tree-sitter/tree-sitter-bash")
              (cmake "https://github.com/uyha/tree-sitter-cmake")
              (css "https://github.com/tree-sitter/tree-sitter-css")
              (elisp "https://github.com/Wilfred/tree-sitter-elisp")
              (go "https://github.com/tree-sitter/tree-sitter-go")
              (html "https://github.com/tree-sitter/tree-sitter-html")
              (java "https://github.com/tree-sitter/tree-sitter-java")
              (javascript "https://github.com/tree-sitter/tree-sitter-javascript" "master" "src")
              (json "https://github.com/tree-sitter/tree-sitter-json")
              (make "https://github.com/alemuller/tree-sitter-make")
              (markdown "https://github.com/ikatyang/tree-sitter-markdown")
              (python "https://github.com/tree-sitter/tree-sitter-python")
              (ruby "https://github.com/tree-sitter/tree-sitter-ruby")
              (c "https://github.com/tree-sitter/tree-sitter-c")
              (cpp "https://github.com/tree-sitter/tree-sitter-cpp")
              (yaml "https://github.com/ikatyang/tree-sitter-yaml")
              (dockerfile "https://github.com/camdencheek/tree-sitter-dockerfile")
                                        ; (typescript "https://github.com/tree-sitter/tree-sitter-typescript") ; wont build
              ))

      (defvar ekr-treesitter-timestamp-file "~/.emacs.d/tree-sitter/timestamp.txt"
        "The path to the file containing the timestamp.")

      (when (or (not (file-exists-p ekr-treesitter-timestamp-file))
                (time-less-p (time-add (file-attribute-modification-time (file-attributes ekr-treesitter-timestamp-file)) (* 7 86400))
                             (current-time)))
        (dolist (language-source
                 treesit-language-source-alist)
          (let ((language-name (car language-source)))
            (message
             "Installing Treesitter grammar for %s" language-name)
            (treesit-install-language-grammar language-name)))
        (with-temp-file ekr-treesitter-timestamp-file (insert (format-time-string "%Y-%m-%d %H:%M:%S"))))

      (add-to-list 'major-mode-remap-alist '(bash-mode . bash-ts-mode))
      (add-to-list 'major-mode-remap-alist '(cmake-mode . cmake-ts-mode))
      (add-to-list 'major-mode-remap-alist '(css-mode . css-ts-mode))
      (add-to-list 'major-mode-remap-alist '(elisp-mode . elisp-ts-mode))
      (add-to-list 'major-mode-remap-alist '(go-mode . go-ts-mode))
      (add-to-list 'major-mode-remap-alist '(html-mode . html-ts-mode))
      (add-to-list 'major-mode-remap-alist '(java-mode . java-ts-mode))
      (add-to-list 'major-mode-remap-alist '(javascript-mode . javascript-ts-mode))
      (add-to-list 'major-mode-remap-alist '(js-mode . javascript-ts-mode))
      (add-to-list 'major-mode-remap-alist '(js-json-mode . json-ts-mode))
      (add-to-list 'major-mode-remap-alist '(json-mode . json-ts-mode))
      (add-to-list 'major-mode-remap-alist '(make-mode . make-ts-mode))
      (add-to-list 'major-mode-remap-alist '(markdown-mode . markdown-ts-mode))
      (add-to-list 'major-mode-remap-alist '(python-mode . python-ts-mode))
      (add-to-list 'major-mode-remap-alist '(ruby-mode . ruby-ts-mode))
      (add-to-list 'major-mode-remap-alist '(yaml-mode . yaml-ts-mode))
      (add-to-list 'major-mode-remap-alist '(dockerfile-mode . dockerfile-ts-mode))
      (add-to-list 'major-mode-remap-alist '(c-mode . c-ts-mode))
      (add-to-list 'major-mode-remap-alist '(c++-mode . c++-ts-mode))
      (add-to-list 'major-mode-remap-alist '(c-or-c++-mode . c-or-c++-ts-mode))

      ;; (add-to-list 'major-mode-remap-alist '(typescript-mode . typescript-ts-mode))
      )
  (message "Tree-sitter binary is not available. Tree-sitter features will be disabled. Check https://corwin.bru.st/emacs-tree-sitter/ maybe for a windows version."))

; markdown-mode

;; (autoload 'markdown-mode "markdown-mode"
;;    "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist
             '("\\.\\(?:md\\|markdown\\|mkd\\|mdown\\|mkdn\\|mdwn\\)\\'" . markdown-mode))

;; (autoload 'gfm-mode "markdown-mode"
;;    "Major mode for editing GitHub Flavored Markdown files" t)
(add-to-list 'auto-mode-alist '("README\\.md\\'" . gfm-mode))

;;;;;;;; github copilot ;;;;;;;;;;;;;;
;
; -> https://github.com/copilot-emacs/copilot.el
;
; - brew install node / apt install npm nodejs
;
; -> ‘M-x copilot-install-server‘
;
; M-x copilot-install-server
; M-x copilot-login

(if (executable-find "node")
    (progn
      (use-package copilot
        :straight (:host github :repo "copilot-emacs/copilot.el" :files ("*.el"))
        :ensure t)

      ;; first time:
      ;; M-x copilot-install-server
      ;; M-x copilot-login

      ;; suppress ⛔ Warning (copilot): copilot--infer-indentation-offset found no mode-specific indentation offset.
      (add-to-list 'warning-suppress-log-types '(copilot))

      (dolist (mode '(ahk bash-ts c++ c++-ts c c-or-c++ c-or-c++-ts c-ts cmake cmake-ts css css-ts
                          csv dockerfile dockerfile-ts elisp elisp-ts emacs-lisp emmet feature fundamental
                          gfm go go-ts groovy html html-ts java java-ts javascript javascript-ts js-json js
                          json json-ts lua make make-ts markdown markdown-ts nxml perl php plantuml powershell
                          python python-ts ruby ruby-ts scss sgml sh sh-script shell-script sql text typescript
                          typescript-ts web xml yaml yaml-ts))
        (add-hook (intern (concat (symbol-name mode) "-mode-hook")) 'copilot-mode))

                                        ;(add-hook 'after-change-major-mode-hook 'copilot-turn-on-unless-buffer-read-only)

      (when (fboundp 'keymap-set)
        (keymap-set copilot-completion-map "TAB" 'copilot-accept-completion)
        (keymap-set copilot-completion-map "C-<tab>" 'copilot-accept-completion-by-word)
        (keymap-set copilot-completion-map "S-<tab>" 'copilot-accept-completion-by-line)
        (keymap-set copilot-completion-map "C-g" 'copilot-clear-overlay)
        (keymap-set copilot-completion-map "C-<right>" 'copilot-next-completion)
        (keymap-set copilot-completion-map "C-<left>" 'copilot-previous-completion)
        ;; (define-key copilot-completion-map (kbd "") 'copilot-accept-completion-by-paragraph)
        ;; (define-key copilot-mode-map (kbd "TAB") 'copilot-complete)
        (keymap-set copilot-completion-map "C-<return>" 'copilot-panel-complete)))

  ;; else, if `node` not found (Windows...):

  (message "`node` not found, copilot not initialized"))

; copilot chat

(use-package copilot-chat
  :straight (:host github :repo "chep/copilot-chat.el" :files ("*.el"))
  :after (request org markdown-mode shell-maker))

(global-set-key (kbd "Ì") 'copilot-chat-display)

; create a key prefix "C-x c" for copilot-chat commands:
(define-prefix-command 'copilot-chat-prefix)
(global-set-key (kbd "ç") 'copilot-chat-prefix)
(define-key copilot-chat-prefix (kbd "ç") 'copilot-chat-transient)
(define-key copilot-chat-prefix (kbd "d") 'copilot-chat-display)
(define-key copilot-chat-prefix (kbd "y") 'copilot-chat-yank)
(define-key copilot-chat-prefix (kbd "a") 'copilot-chat-add-current-buffer)
(define-key copilot-chat-prefix (kbd "l") 'copilot-chat-list)
(define-key copilot-chat-prefix (kbd "e") 'copilot-chat-explain)
(define-key copilot-chat-prefix (kbd "r") 'copilot-chat-review)
(define-key copilot-chat-prefix (kbd "o") 'copilot-chat-doc)
(define-key copilot-chat-prefix (kbd "f") 'copilot-chat-fix)
(define-key copilot-chat-prefix (kbd "p") 'copilot-chat-optimize)
(define-key copilot-chat-prefix (kbd "t") 'copilot-chat-test)
(define-key copilot-chat-prefix (kbd "u") 'copilot-chat-explain-defun)
(define-key copilot-chat-prefix (kbd "m") 'copilot-chat-insert-commit-message)

(defun ekr-copilot-chat-ask-and-return-string (prompt)
  "Ask copilot chat for a response to PROMPT and return the result."
  (let ((result nil)
        (tmp ""))
    (copilot-chat--ask prompt
                       (lambda (content)
                         (if (string= content copilot-chat--magic)
                             (setq result tmp)
                           (setq tmp (concat tmp content))))
                       t)
    ; wait until result is not nil anymore
    (while (string= result nil)
      (sleep-for 0.5))
    ; return the result
    result))

; ssh-agent on WSL

(unless (file-exists-p "/tmp/ssh-agent.sh")
  (progn
    (message "Starting ssh-agent...")
    (call-process-shell-command "ssh-agent > /tmp/ssh-agent.sh" nil nil)))

(if (file-exists-p "/tmp/ssh-agent.sh")
    (progn
      (message "Loading ssh-agent variables...")

      ; Example /tmp/ssh-agent.sh:
      ; SSH_AUTH_SOCK=/var/folders/06/76_lm2ss1p77xqwf5xzggzf80000gp/T//ssh-aBcA9sNHvSTb/agent.33249; export SSH_AUTH_SOCK;
      ; SSH_AGENT_PID=33250; export SSH_AGENT_PID;
      ; echo Agent pid 33250;

      (with-temp-buffer
        (insert-file-contents "/tmp/ssh-agent.sh")
        (goto-char (point-min))
        (while (re-search-forward "\\([A-Z_]+\\)=\\([^;\n]*\\);?" nil t)
          (progn
            (message "Setting %s to '%s'" (match-string 1) (match-string 2))
            (setenv (match-string 1) (match-string 2)))))))

; yaml-mode

; .tpl = helm charts
(add-to-list 'auto-mode-alist '("\\.tpl$" . yaml-mode))

; provide scrolling for the wheel-up/down events as well (MacOS)

(defun mouse-wheel-text-scale (event)
  "Disable (ignore) ctrl + mouse wheel text scaling by overriding the same def in mwheel.el."
  (interactive (list last-input-event))
  (ignore))

;; The following is not necessary on MacOS Emacs 30.1, scrolling just
;; worked out of the box.
;;
;; (defun my-scroll-up (amount)
;;   "Scroll up by AMOUNT lines."
;;   (interactive "p")
;;   (scroll-up-line amount))
;;
;; (defun my-scroll-down (amount)
;;   "Scroll down by AMOUNT lines."
;;   (interactive "p")
;;   (scroll-down-line amount))
;;
;; (global-set-key (kbd "<wheel-up>") (lambda () (interactive) (my-scroll-down 1)))
;; (global-set-key (kbd "<double-wheel-up>") (lambda () (interactive) (my-scroll-down 2)))
;; (global-set-key (kbd "<triple-wheel-up>") (lambda () (interactive) (my-scroll-down 3)))
;;
;; (global-set-key (kbd "<wheel-down>") (lambda () (interactive) (my-scroll-up 1)))
;; (global-set-key (kbd "<double-wheel-down>") (lambda () (interactive) (my-scroll-up 2)))
;; (global-set-key (kbd "<triple-wheel-down>") (lambda () (interactive) (my-scroll-up 3)))

; tab-bar-mode

(tab-bar-mode 1)

; editorconfig-mode to apply options from .editorconfig files

(editorconfig-mode 1)

; which-key-mode displays a table of key bindings upon entering a
; partial key chord and waiting for a moment

(which-key-mode 1)

; completion-preview-mode automatically shows and updates the
; completion preview according to the text around point

(completion-preview-mode 1)

; ultra-scroll scrolls the display precisely using full trackpad or
; modern mouse capabilities

(if (eq system-type 'darwin)
    (use-package ultra-scroll
      :load-path "~/.emacs.d/elisp/ultra-scroll" ; if you git clone'd instead of using vc
                                        ; :vc (:url "https://github.com/jdtsmith/ultra-scroll") ; For Emacs>=30
      :init
      (setq scroll-conservatively 101 ; important!
            scroll-margin 0)
      :config
      (ultra-scroll-mode 1)))

; fix local working directory for desktop-loaded files

(defun set-default-directory-for-all-buffers ()
  "Set the default directory for all buffers to their respective file directories."
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (progn
        (when (and (buffer-file-name)
                   (file-directory-p (file-name-directory (buffer-file-name))))
          (progn
            (message "Setting default-directory for buffer %s to %s" (buffer-name) (file-name-directory (buffer-file-name)))
            (setq-local default-directory (file-name-directory (buffer-file-name)))))
        (when (and (eq major-mode 'dired-mode)
                   (file-directory-p default-directory))
          (progn
            (message "Setting default-directory for dired buffer %s to %s" (buffer-name) dired-directory)
            (setq-local default-directory dired-directory))))))
  (message "default-directories fixed for desktop-loaded files"))

(defun set-default-directory-for-all-buffers-delayed ()
  "Set the default directory for all buffers to their respective file directories after a delay."
  (run-at-time "4 sec" nil 'set-default-directory-for-all-buffers))

;(set-default-directory-for-all-buffers-delayed)
(add-hook 'desktop-after-read-hook 'set-default-directory-for-all-buffers-delayed)

; aidermacs

(if (executable-find "aider")
    ;; package-install -> aidermacs
    (use-package aidermacs
      :bind (("C-c a" . aidermacs-transient-menu))
      :config
      ;; ; Set API_KEY in .bashrc, that will automatically picked up by aider or in elisp
      ;; (setenv "ANTHROPIC_API_KEY" "sk-...")
      ;; ; defun my-get-openrouter-api-key yourself elsewhere for security reasons
      ;; (setenv "OPENROUTER_API_KEY" (my-get-openrouter-api-key))
      :custom
                                        ; See the Configuration section below
      (aidermacs-use-architect-mode t)
      ;; (aidermacs-default-model "sonnet")
      ))

; notmuch

; dpkg -L elpa-notmuch
(if (executable-find "notmuch")
    (progn
      (message "Notmuch binary is available.")
      (add-to-list 'load-path "/usr/share/emacs/site-lisp/elpa-src/notmuch-0.35")
      (require 'notmuch)

      ;; M-m   show unread mails
      (global-set-key (kbd "M-m")
                      (lambda () (interactive)
                        (progn
                          (notmuch-poll)
                          (notmuch-search "tag:inbox and not tag:deleted"))))

      ;; a    "archive" (read) mail, default setting

      ;; d    delete mail
      ;; add the "d" key to notmuch-search-mode-map:
      (define-key notmuch-search-mode-map (kbd "d")
                  (lambda () (interactive)
                    "Delete the current thread and move to the next one."
                    (progn
                      (notmuch-search-tag '("+deleted" "-inbox" "-unread")))
                      (notmuch-search-next-thread))))

      (run-at-time "1 min" 60
                   (lambda ()
                     (when (and (not (minibufferp))
                                (not (active-minibuffer-window)))
                       (notmuch-poll)))))
  (message "Notmuch binary is not available. Notmuch features will be disabled."))

;; map C-v to "paste" (in addition to C-y) for "Smile" on Gnome
(when (and (eq system-type 'gnu/linux)
           (string= (getenv "XDG_CURRENT_DESKTOP") "pop:GNOME"))
  (global-set-key (kbd "C-v") 'clipboard-yank))

; run server

(server-start)

(provide 'init)
;;; init.el ends here
