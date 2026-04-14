;;; Emacs configuration for custom variables managed by Emacs
;;; -*- lexical-binding: t; -*-

(my-banner "Custom variables")

(setq custom-file "init_custom_variables.el")

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ahk-indentation 2)
 '(aidermacs-backend 'comint)
 '(aidermacs-default-model "see ~/.aider.conf.yml instead!")
 '(aidermacs-extra-args '(""))
 '(aidermacs-program "set this in init_copilot*el instead!")
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
   "Here is the result of running `git diff --cached`. Please suggest a commit message. Don't add anything else to the response. The following describes conventional commits.\12Do not use any markers around the commit message. Do not add the conventional commit prefix. Stick to one line, shorter is better. No need to mention whitespace changes.\12\12Here is the result of `git diff --cached`:\12")
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

(add-hook 'magit-log-mode-hook (lambda () (visual-line-mode -1)))
(add-hook 'magit-status-mode-hook (lambda () (visual-line-mode -1)))
(add-hook 'compilation-mode-hook (lambda () (visual-line-mode -1)))
