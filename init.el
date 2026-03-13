;;; package --- Summary

;; define a "banner" defun that outputs a message with a nice ASCII art border
(defun my-banner (msg)
  "Display a message in a bordered box."
  (let ((border (make-string (length msg) ?-)))
    (message "\n%s\n%s\n%s\n" border msg border)))

(switch-to-buffer "*Messages*")

(my-banner "Start init.el")

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

;; Straight package versions, diff etc. -> see straight_versions.el

(my-banner "Load straight-based packages")

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
;(straight-use-package 'origami-predef)
(straight-use-package 'dockerfile-mode)
(straight-use-package 'markdown-mode)
;(straight-use-package 'kubel)
;(straight-use-package 'kubernetes)
(straight-use-package 'projectile)
(straight-use-package 'groovy-mode)
(straight-use-package 'lua-mode)
(straight-use-package 'sqlformat)
(straight-use-package 'magit)
;(straight-use-package 'magit-filenotify)
(straight-use-package 'multiple-cursors)
(straight-use-package 'feature-mode)
(straight-use-package 'emmet-mode)
(straight-use-package 'pdf-tools)
(straight-use-package 'powershell)
;(straight-use-package 'ahk-mode)
(straight-use-package 'tty-format)
(straight-use-package 'scss-mode)
(straight-use-package 'web-mode)
(straight-use-package 'sql-indent)
(straight-use-package 'marginalia)
;(straight-use-package 'forge)
(straight-use-package 'yaml-mode)
(straight-use-package 'recentf)
(straight-use-package 'compile)
(straight-use-package 'highlight-indent-guides)
(straight-use-package 'plantuml-mode)
(straight-use-package 'deadgrep)
(straight-use-package 'dumb-jump)
(straight-use-package 'load-env-vars)
; (straight-use-package 'show-font)  ; https://protesilaos.com/emacs/show-font - install manually
(straight-use-package 'aidermacs)
(straight-use-package 'sqlite-mode)
(straight-use-package 'jira-markup-mode)
(straight-use-package 'vterm)
(straight-use-package 'lab)
(straight-use-package 'emacs-everywhere)       ; ctrl-cmd-e to open an emacs frame in the current directory, with the current clipboard content as the initial buffer content
;; completion popups, a bit annoying
;; (when (straight-use-package 'corfu)
;;   (global-corfu-mode))
;; (straight-use-package 'kind-icon) ; icons for corfu
(straight-use-package 'shell-maker)
; (straight-use-package 'agent-shell)
(straight-use-package 'ripgrep)
(straight-use-package 'rg)
(use-package expreg
  :straight t
  :bind (("C-+" . expreg-expand)
         ("C--" . expreg-contract)))

;; maybe try:
;;
;; vertico - vertical completion
;; orderless - completion with spaces...
;; diff-hl - git diff in the fringe

(global-set-key [remap dabbrev-expand] 'hippie-expand)

(add-hook 'xref-backend-functions #'dumb-jump-xref-activate)

(my-banner "git blame in status line")

; git blame in status line...
(straight-use-package 'emacs-async)
(add-to-list 'load-path "~/.emacs.d/elisp/emsg-blame")
(ignore-errors
  (require 'emsg-blame) ; TODO fails on windows... install occasionally
  (setopt emsg-blame-background t)
  (setopt emsg-blame-background-color "#444444")
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

(add-to-list 'load-path "~/.emacs.d/elisp")

; MacOS dark mode for frame

(add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
(add-to-list 'default-frame-alist '(ns-appearance . dark))

(menu-bar-mode -1)
(toggle-scroll-bar -1)

;; ------------------------ flycheck plus additional modules

(my-banner "flycheck")

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

(my-banner "compile, feature-mode")

;(setopt load-path (cons "~/.emacs.d/elisp/icicles" load-path))
;(require 'icicles)
;(icy-mode 0)
; Switching to previous buffer doesn't work anymore?!

;(ido-mode t) ; use 'buffer rather than t to use only buffer switching
;(ido-everywhere t)

(my-banner "Custom variables")

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ahk-indentation 2)
 '(aidermacs-backend 'comint)
 '(aidermacs-default-model "see ~/.aider.conf.yml instead!")
 '(aidermacs-extra-args '("--watch-files"))
 '(aidermacs-show-diff-after-change nil)
 '(aidermacs-subtree-only t)
 '(aidermacs-watch-files t)
 '(ansi-color-bold-is-bright t)
 '(ansi-color-for-comint-mode t)
 '(ansi-color-names-vector
   ["black" "#d55e00" "#009e73" "#f8ec59" "#0072b2" "#cc79a7" "#56b4e9"
    "white"])
 '(auto-hscroll-mode t)
 '(auto-save-default t)
 '(backup-by-copying t)
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
 '(copilot-chat-commit-model "gpt-4.1")
 '(copilot-chat-commit-prompt
   "Here is the result of running `git diff --cached`. Please suggest a commit message. Don't add anything else to the response. The following describes conventional commits.\12Do not use any markers around the commit message. Do not add the conventional commit prefix.\12\12Here is the result of `git diff --cached`:\12")
 '(copilot-chat-debug nil)
 '(copilot-chat-default-model "gpt-4.1")
 '(copilot-chat-follow nil)
 '(copilot-chat-frontend 'shell-maker)
 '(copilot-chat-model-ignore-picker t)
 '(copilot-indent-offset-warning-disable t)
 '(copilot-max-char 120000)
 '(copilot-server-log-level 4)
 '(corfu-auto t)
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
 '(deadgrep-display-buffer-function 'switch-to-buffer)
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
 '(desktop-load-locked-desktop 'check-pid)
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
 '(emacs-everywhere-frame-parameters
   '((name . "emacs-everywhere") (fullscreen) (width . 120) (height . 25)))
 '(emigo-python-command "~/.emacs.d/straight/repos/emigo/python3-venv")
 '(emsg-blame-idle-time 5)
 '(feature-cucumber-command "cucumber {options} {feature} | fmt -w 100")
 '(feature-rake-command "cucumber {options} {feature}")
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
      "atc-github.azure.cloud.bmw" forge-github-repository)
     ("gitlab.devops.telekom.de" "gitlab.devops.telekom.de/api/v4/"
      "gitlab.devops.telekom.de" forge-gitlab-repository)
     ("gitlab.caps.nttdata-emea.com"
      "gitlab.caps.nttdata-emea.com/api/v4/"
      "gitlab.caps.nttdata-emea.com" forge-gitlab-repository)))
 '(git-commit-summary-max-length 2000)
 '(global-visual-line-mode t)
 '(global-visual-wrap-prefix-mode t)
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
 '(make-pointer-invisible nil)
 '(markdown-code-lang-modes
   '(("ocaml" . tuareg-mode) ("elisp" . emacs-lisp-mode)
     ("ditaa" . artist-mode) ("asymptote" . asy-mode)
     ("dot" . fundamental-mode) ("sqlite" . sql-mode)
     ("calc" . fundamental-mode) ("C" . c-mode) ("cpp" . c++-mode)
     ("C++" . c++-mode) ("screen" . shell-script-mode)
     ("shell" . sh-mode) ("bash" . sh-mode) ("dataviewjs" . js-mode)))
 '(markdown-fontify-code-blocks-natively t)
 '(max-mini-window-height 1)
 '(max-specpdl-size 10000 t)
 '(message-log-max 10000)
 '(mouse-highlight t)
 '(mouse-wheel-down-event 'mouse-4)
 '(mouse-wheel-mode t)
 '(mouse-wheel-progressive-speed nil)
 '(mouse-wheel-scroll-amount '(5 ((shift) . hscroll)))
 '(mouse-wheel-up-event 'mouse-5)
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
 '(org-startup-with-link-previews t)
 '(plantuml-default-exec-mode 'jar)
 '(plantuml-jar-path
   "/opt/homebrew/Cellar/plantuml/1.2025.4/libexec/plantuml.jar")
 '(projectile-completion-system 'ido)
 '(projectile-global-ignore-file-patterns '(".aider.*"))
 '(projectile-globally-ignored-files '("TAGS" "#*#" ".aider*"))
 '(ruby-insert-encoding-magic-comment nil)
 '(safe-local-variable-values
   '((web-mode-indent-style . 2) (web-mode-block-padding . 2)
     (web-mode-script-padding . 2) (web-mode-style-padding . 2)
     (feature-indent-offset . 2) (feature-indent-level . 2)
     (package-lint-main-file . "copilot-chat.el")
     (ahk-indentation . 2) (buffer-file-coding-system . iso-8859-1)
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
 '(visual-line-fringe-indicators '(left-curly-arrow right-curly-arrow))
 '(vterm-keymap-exceptions
   '("C-c" "C-x" "C-u" "C-g" "C-h" "C-l" "M-x" "M-o" "C-y" "M-y"
     "<prior>"))
 '(whitespace-global-modes '(yaml-mode))
 '(whitespace-line-column 80)
 '(whitespace-style
   '(face trailing tabs empty indentation space-after-tab
          space-before-tab space-mark tab-mark newline-mark))
 '(with-editor-emacsclient-executable nil)
 '(xslt-process-fop-log-level '(debug))
 '(xslt-process-xml-xslt-associations nil)
 '(xterm-mouse-mode t))

(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)

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

;(find-file-noselect "/my@10.226.93.5:/opt")

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

(my-banner "ANSI colors (also see init_compilation.el)...")

(require 'ansi-color)
(defun display-ansi-colors ()
  (interactive)
  (ansi-color-apply-on-region (point-min) (point-max)))

;; decode ANSI color escape sequences for .log files
(add-to-list 'auto-mode-alist '("\\.log\\'" . display-ansi-colors))

;;;;;;;;;;;;;;;;;;; PUTTY

;; starten mit "TERM=xterm-256color emacs -nw"
;; Font: Consolas 11 Point, Cleartype
;; SSH cmd:   screen -dR myemacs bash -c "TERM=xterm-256color emacs -nw"

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
;(setopt x-select-enable-clipboard nil)
;(setopt x-select-enable-primary t)

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
;;     (setopt interprogram-cut-function 'xsel-cut-function)
;;     (setopt interprogram-paste-function 'xsel-paste-function)
;;     ;; Idea from
;;     ;; http://shreevatsa.wordpress.com/2006/10/22/emacs-copypaste-and-x/
;;     ;; http://www.mail-archive.com/help-gnu-emacs@gnu.org/msg03577.html
;;      ))

;; (setopt interprogram-cut-function nil)
;; (setopt interprogram-paste-function nil)

(my-banner "auto-mode-alist, recent, git, etc.")

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

;(setopt electric-indent-functions-without-reindent (remove 'indent-line-function electric-indent-functions-without-reindent))

(setq-default electric-indent-inhibit t)

(recentf-mode 1)
(setopt recentf-max-menu-items 25)
(global-set-key "\C-x\ \C-r" 'recentf-open-files)

;;;
;;; magit & forge
;;;

(setenv "GIT_ASKPASS" "git-gui--askpass")

(global-set-key (kbd "C-x g") 'magit-status)
(global-set-key (kbd "C-x C-g") 'magit-dispatch-popup)

;; forge setup (see https://magit.vc/manual/forge/Setup-for-Another-Gitlab-Instance.html ):
;;
;; 1. customize-variable forge-alist  -> add new github or gitlab host. Info can be found with "glab auth status"
;;
;; 2. configure repo user name (username can be found in the file mentioned by "glab auth status")
;;
;;    git config --global gitlab.APIENDPOINT.user USERNAME
;;
;; 3. token into ~/.authinfo:
;;
;;    machine HOSTNAME login USERNAME^forge password TOKEN
;;
;; 4. M-x auth-source-forget-all-cached
;;
;; 5. M-x forge-pull for initial pull



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
 '(line-number ((t (:foreground "gray38"))))
 '(mode-line (nil)))

(my-banner "Ruby, projectile, ...")

;;;;;;; special ruby stuff (packages installed specifically for that)
; https://lorefnon.me/2014/02/02/configuring-emacs-for-rails.html

;;;;;;; Mac

; to be able to use Option-7 for | etc.

; originally 'super
(if (boundp 'ns-command-modifier)
    (setopt ns-command-modifier 'meta))

; originally 'meta
(if (boundp 'ns-option-modifier)
    (setopt ns-option-modifier nil))

; specifically nice for AHK-mode which has indentation bugs when there are trailing spaces
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; naybe start docker-machine first ("docker-machine start default")
;; (if (file-directory-p "/Users/KRAEME/.docker/machine")
;;     (progn
;;       (shell-command "docker-machine start")
;;       (setenv "DOCKER_HOST" (shell-command-to-string "docker-machine env default | grep DOCKER_HOST | sed -e 's/export DOCKER_HOST=.//' -e 's/.$//' | tr -d '\n'"))
;;       (setenv "DOCKER_TLS_VERIFY" "1")
;;       (setenv "DOCKER_CERT_PATH" "/Users/KRAEME/.docker/machine/machines/default")
;;       (setenv "DOCKER_MACHINE_NAME" "default")))

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

;;;;;;;; embark & marginalia

;; (use-package embark
;;   :ensure t
;;
;;   :bind
;;   (("C-." . embark-act)         ;; pick some comfortable binding
;;    ("C-;" . embark-dwim)        ;; good alternative: M-.
;;    ("C-h B" . embark-bindings)) ;; alternative for `describe-bindings'
;;
;;   :init
;;
;;   ;; Optionally replace the key help with a completing-read interface
;;   (setopt prefix-help-command #'embark-prefix-help-command)
;;
;;   :config
;;
;;   ;; Hide the mode line of the Embark live/completions buffers
;;   (add-to-list 'display-buffer-alist
;;                '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
;;                  nil
;;                  (window-parameters (mode-line-format . none)))))

; fix Home key on MacOs

(global-set-key (kbd "<home>") 'move-beginning-of-line)
(global-set-key (kbd "<end>") 'move-end-of-line)

(savehist-mode 1)
(add-to-list 'savehist-additional-variables 'shell-command-history)

; ox-confluence, export org to confluence/jira markup

; (load-library "ox-confluence")

;(when (version<= "26.0.50" emacs-version )
;  (global-display-line-numbers-mode))

(my-banner "Go, org-jira, unicode, fonts...")

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

(setopt jiralib-url "https://jira-caps-ext.nttdata-emea.com")

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

(defun my-set-fonts ()
  "Setup fonts."
  (interactive)
  (message (cond ((eq system-type 'gnu/linux)
                  (set-frame-font "-*-Inconsolata-normal-normal-normal-*-17-*-*-*-m-0-iso10646-1")
                  ; (set-frame-position (selected-frame) 3630 2)
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
          (lambda () (run-at-time "1 sec" nil 'my-set-fonts)))

(my-banner "plantuml, good-auto, ...")

; plantuml

(defun set-plantuml-preview-background ()
  "Set the background color of the *PLANTUML Preview* buffer to white."
  (when (string-equal (buffer-name) "*PLANTUML Preview*")
    (with-current-buffer "*PLANTUML Preview*"
      (face-remap-add-relative 'default :background "white"))))

(add-hook 'buffer-list-update-hook #'set-plantuml-preview-background)

; bash-mode

(add-to-list 'auto-mode-alist '("\.sh$" . bash-mode))

; tree-sitter

(my-banner "tree-sitter")

;; brew upgrade tree-sitter tree-sitter-cli

;; $ tree-sitter init-config
;; -> Library/Application Support/tree-sitter/config.json

;; ------- manual compile ------------------
;; https://www.masteringemacs.org/article/how-to-get-started-tree-sitter
;;

(if (executable-find "tree-sitter")
    (progn
      (message "Tree-sitter binary is available.")

      ;; For version-mismatch: clone repo, "git blame src/parser.c | grep LANGUAGE_VERSION", find previous commit.
      ;; Then "git describe --tags --abbrev=0 8509e322^". Use that tag in the list below.

      (setopt treesit-language-source-alist
            '(
              (bash "https://github.com/tree-sitter/tree-sitter-bash" "v0.23.3")
              (cmake "https://github.com/uyha/tree-sitter-cmake")
              (css "https://github.com/tree-sitter/tree-sitter-css" "v0.23.2")
              (elisp "https://github.com/Wilfred/tree-sitter-elisp" "1.5.0")
              (go "https://github.com/tree-sitter/tree-sitter-go" "v0.23.4")
              (html "https://github.com/tree-sitter/tree-sitter-html")
              (java "https://github.com/tree-sitter/tree-sitter-java")
              (javascript "https://github.com/tree-sitter/tree-sitter-javascript" "v0.23.1")
              (json "https://github.com/tree-sitter/tree-sitter-json")
              (make "https://github.com/alemuller/tree-sitter-make")
              (markdown "https://github.com/ikatyang/tree-sitter-markdown")
              (python "https://github.com/tree-sitter/tree-sitter-python" "v0.23.6")
              (ruby "https://github.com/tree-sitter/tree-sitter-ruby")
              (c "https://github.com/tree-sitter/tree-sitter-c" "v0.23.6")
              (cpp "https://github.com/tree-sitter/tree-sitter-cpp")
              (yaml "https://github.com/ikatyang/tree-sitter-yaml")
              (dockerfile "https://github.com/camdencheek/tree-sitter-dockerfile")
                                        ; (typescript "https://github.com/tree-sitter/tree-sitter-typescript") ; wont build
              ))

      (defvar my-treesitter-timestamp-file "~/.emacs.d/tree-sitter/timestamp.txt"
        "The path to the file containing the timestamp.")

      (when (or (not (file-exists-p my-treesitter-timestamp-file))
                (time-less-p (time-add (file-attribute-modification-time (file-attributes my-treesitter-timestamp-file)) (* 7 86400))
                             (current-time)))
        (dolist (language-source
                 treesit-language-source-alist)
          (let ((language-name (car language-source)))
            (message
             "Installing Treesitter grammar for %s" language-name)
            (treesit-install-language-grammar language-name)))
        (with-temp-file my-treesitter-timestamp-file (insert (format-time-string "%Y-%m-%d %H:%M:%S"))))

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

(my-banner "ssh-agent, ...")

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

(message "Misc settings...")

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

;; (tab-bar-mode 1)

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
      (setopt scroll-conservatively 101 ; important!
            scroll-margin 0)
      :config
      (ultra-scroll-mode 1)))

; fix local working directory for desktop-loaded files

(my-banner "Fix desktop CWD's...")

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

(load-file (expand-file-name "init_notmuch.el" user-emacs-directory))
(load-file (expand-file-name "init_pop_os.el" user-emacs-directory))
(load-file (expand-file-name "init_projectile.el" user-emacs-directory))
(load-file (expand-file-name "init_copilot_chat.el" user-emacs-directory))
(load-file (expand-file-name "init_compilation.el" user-emacs-directory))

;; Emigo Aider alternative...

;; in ~/.emacs.d/straight/repos/emigo/
;;
;;    python3 -m venv venv
;;    . venv/bin/activate ; pip install -r requirements.txt
;;
;; Customize group and set this python command:
;;
;;     . ~/.emacs.d/straight/repos/emigo/venv/bin/activate && python3
;;
;; 02/2026: unusable. Does not set an Editor-Version; and passes much too many tokens.
;;
;; (use-package emigo
;;   :straight (:host github :repo "MatthewZMD/emigo" :files (:defaults "*.py" "*.el"))
;;   :config
;;   (emigo-enable) ;; Starts the background process automatically
;;   :custom
;;   ;; Encourage using OpenRouter with Deepseek
;;   (emigo-model "github_copilot/gpt-4.1")
;;   (emigo-base-url (getenv "OPENAI_API_BASE"))
;;   (emigo-api-key (getenv "OPENAI_API_KEY")))

; stuff

;; lab / gitlab

(defun my-set-lab-host ()
  "Set the lab-host variable to the host of the git repo in the current working directory.
Also try to discover and set `lab-token` using the `glab` CLI config."
  (interactive)
  (let* ((git-url (string-trim (shell-command-to-string "git config --get remote.origin.url 2>/dev/null")))
         (host (cond
                ;; HTTPS/HTTP style: https://gitlab.example.com/group/project.git
                ((string-match "\\(https?://[^/]+\\)" git-url)
                 (match-string 1 git-url))
                ;; SSH style: git@gitlab.example.com:group/project.git
                ((string-match "\\`[^@]+@\\([^:]+\\):" git-url)
                 (concat "https://" (match-string 1 git-url)))))
         ;; Try to derive the GitLab host without protocol
         (api-host (when host
                     (replace-regexp-in-string "\\`https?://\\([^/]+\\).*" "\\1" host)))
         ;; Ask glab directly for the token of this host; ignore errors (2>/dev/null)
         (token (when api-host
                  (let ((out (string-trim
                              (shell-command-to-string
                               (format "glab config get token --host %s 2>/dev/null" api-host)))))
                    (and (not (string-empty-p out)) out)))))
    (setq lab-host host)
    (when token
      (setq lab-token token))
    (message "lab-host set to %s%s"
             lab-host
             (if token " (lab-token updated from glab config)" ""))))

(bind-key "C-x l" lab-map)

;; Fix sed/GNU sed issue for M-x man

(setq Man-sed-command "gsed")

;; fix a window

(defun my/toggle-fixate-window ()
  "Toggle dedicating the selected window to its buffer, fixing its size,
and preventing it from being removed by `delete-other-windows` (C-x 1)."
  (interactive)
  (let* ((win (selected-window))
         (new (not (window-dedicated-p win))))
    (set-window-dedicated-p win new)
    (set-window-parameter win 'window-size-fixed new)
    (set-window-parameter win 'no-delete-other-windows new)))

; Show possible key bindings after a short delay

(which-key-mode 1)

(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

; run server

(my-banner "Start Server...")

(server-start)

(provide 'init)

(my-banner "init.el loaded successfully!")
