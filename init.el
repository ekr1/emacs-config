; If this does not load at all (no error message), maybe remove ~/.emacs.

; nohup emacs --daemon >/dev/null &
; killall emacsclient ; emacsclient -t /dev/null

; export TERM=xterm-256color
; screen -dR emacs -c .screenrc.emacs

(add-to-list 'load-path "~/.emacs.d/elisp")

(require 'compile)
; fix for very slow compiling:
(setq process-adaptive-read-buffering nil)

(require 'uniquify)

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
 '(ansi-color-names-vector
   ["black" "#d55e00" "#009e73" "#f8ec59" "#0072b2" "#cc79a7" "#56b4e9" "white"])
 '(auto-hscroll-mode t)
 '(auto-save-default t)
 '(auto-save-file-name-transforms
   (quote
    (("\\`/[^/]*:\\([^/]*/\\)*\\([^/]*\\)\\'" "d:/DOCUME~1/ekraemer/LOCALS~1/Temp/\\2" t)
     (".*plink:\\(.*\\)" "d:/DOCUME~1/ekraemer/LOCALS~1/Temp/\\1" t))))
 '(backup-by-copying t)
 '(backup-directory-alist (quote (("." . "/home/ekr/.emacs.d/backups"))))
 '(c-basic-offset 4)
 '(c-default-style (quote ((awk-mode . "awk") (other . "ellemtel"))))
 '(column-number-mode t)
 '(comment-empty-lines t)
 '(compilation-ask-about-save nil)
 '(compilation-auto-jump-to-first-error nil)
 '(compilation-context-lines 3)
 '(compilation-mode-hook nil)
 '(compilation-scroll-output nil)
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
 '(custom-enabled-themes (quote (tango-dark)))
 '(desktop-files-not-to-save (quote "xyzzy (will crash if this is nil)"))
 '(desktop-globals-to-clear
   (quote
    (kill-ring-yank-pointer search-ring search-ring-yank-pointer regexp-search-ring regexp-search-ring-yank-pointer kill-ring)))
 '(desktop-globals-to-save
   (quote
    (desktop-missing-file-warning tags-file-name tags-table-list search-ring regexp-search-ring register-alist file-name-history compile-command compilation-directory shell-command-history kill-ring search-ring \.\.\.)))
 '(desktop-missing-file-warning nil)
 '(desktop-path (quote ("~/.emacs.d/")))
 '(desktop-restore-eager t)
 '(desktop-save nil)
 '(desktop-save-mode nil)
 '(display-buffer-alist nil)
 '(display-buffer-reuse-frames t)
 '(feature-cucumber-command
   "time bin/rake cucumber:rerun_nodb CUCUMBER_OPTS=\"{options}\" FEATURE=\"{feature}\" ")
 '(grep-find-ignored-files
   (quote
    (".#*" "*.o" "*~" "*.bin" "*.lbin" "*.so" "*.a" "*.ln" "*.blg" "*.bbl" "*.elc" "*.lof" "*.glo" "*.idx" "*.lot" "*.fmt" "*.tfm" "*.class" "*.fas" "*.lib" "*.mem" "*.x86f" "*.sparcf" "*.dfsl" "*.pfsl" "*.d64fsl" "*.p64fsl" "*.lx64fsl" "*.lx32fsl" "*.dx64fsl" "*.dx32fsl" "*.fx64fsl" "*.fx32fsl" "*.sx64fsl" "*.sx32fsl" "*.wx64fsl" "*.wx32fsl" "*.fasl" "*.ufsl" "*.fsl" "*.dxl" "*.lo" "*.la" "*.gmo" "*.mo" "*.toc" "*.aux" "*.cp" "*.fn" "*.ky" "*.pg" "*.tp" "*.vr" "*.cps" "*.fns" "*.kys" "*.pgs" "*.tps" "*.vrs" "*.pyc" "*.pyo" "*.gz")))
 '(grep-find-template "find . <X> -type f <F> -print | xargs grep <C> -n <R>")
 '(icicle-guess-commands-in-path nil)
 '(icicle-redefine-standard-commands-flag nil)
 '(ido-confirm-unique-completion t)
 '(ido-enable-flex-matching t)
 '(ido-everywhere t)
 '(indent-tabs-mode nil)
 '(jit-lock-stealth-verbose t)
 '(make-pointer-invisible nil)
 '(max-mini-window-height 1)
 '(max-specpdl-size 10000)
 '(mouse-highlight t)
 '(projectile-globally-ignored-files (quote ("TAGS" "#*#")))
 '(ruby-insert-encoding-magic-comment nil)
 '(safe-local-variable-values
   (quote
    ((buffer-file-coding-system . iso-8859-1)
     (buffer-file-coding-system . utf-8))))
 '(select-enable-clipboard t)
 '(select-enable-primary t)
 '(show-paren-mode t)
 '(split-width-threshold 140)
 '(tool-bar-mode nil)
 '(tooltip-mode nil)
 '(tramp-copy-size-limit 102400)
 '(tramp-default-method "ssh")
 '(tramp-remote-process-environment
   (quote
    ("HISTFILE=$HOME/.tramp_history" "HISTSIZE=1" "LC_ALL=C" "TERM=dumb" "EMACS=t" "INSIDE_EMACS=23.1.1,tramp:2.1.15" "CDPATH=" "HISTORY=" "MAIL=" "MAILCHECK=" "MAILPATH=" "autocorrect=" \.\.\.)))
 '(tramp-verbose 2)
 '(truncate-lines t)
 '(undo-limit 12000000)
 '(undo-strong-limit 12000000)
 '(uniquify-buffer-name-style (quote post-forward-angle-brackets) nil (uniquify))
 '(url-proxy-services
   (quote
    (("https" . "A.B.C.D:8080")
     ("http" . "A.B.C.D:8080"))))
 '(use-file-dialog nil)
 '(vc-handled-backends (quote (RCS SVN SCCS Bzr Git Hg Arch)))
 '(vc-svn-diff-switches "-x -b")
 '(with-editor-emacsclient-executable nil)
 '(xslt-process-fop-log-level (quote (debug)))
 '(xslt-process-xml-xslt-associations nil)
 '(xterm-mouse-mode t))

(defun arrange-frame (w h x y)
  "Set the width, height, and x/y position of the current frame"
  (let ((frame (selected-frame)))
    (delete-other-windows)
    (set-frame-position frame x y)
    (set-frame-size frame w h)))
(defun arrange-my-frame ()
  ""
  (arrange-frame 148 68 494 22))
(add-hook 'emacs-startup-hook 'arrange-my-frame)
(arrange-my-frame)

(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)

(setq default-buffer-file-coding-system 'iso-latin-1-unix)

;(setq special-display-buffer-names
;           '("*Async Shell Command*" "*grep*" "*compilation*" "*vc-dir*"))
(setq special-display-buffer-names nil)

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
  (interactive)
  "Save all files, wait a little bit, then call (recompile). For compilations that watch file-change-times (RotTestHelper...)"
  (progn (save-some-buffers t)
	 (sleep-for 1.5)
	 (recompile)
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

(defun ekr-git-gui ()
  (interactive)
  "Run 'git gui' without a buffer"
  (start-process "git gui" nil "git" "gui")
  (message "'git gui' started"))

(defun ekr-next-scenario ()
  (interactive)
  "Jump to the next cucumber scenario in the compilation buffer"
  (switch-to-buffer-other-window "*compilation*")
  (search-forward "Szenario:")
  (execute-kbd-macro (kbd "<return>"))
  ))

(defun my-previous-scenario ()
  (interactive)
  "Jump to the previous cucumber scenario in the compilation buffer"
  (switch-to-buffer-other-window "*compilation*")
  (search-backward "Szenario:")
  (execute-kbd-macro (kbd "<return>"))
  )

;; (global-set-key (kbd "<f1>") 'ekr-wiki-update)
(global-set-key (kbd "<f2>") 'ekr-recompile)
;; (global-set-key (kbd "<f3>") 'ekr-compile-plsql)
(global-set-key (kbd "<f4>") 'ekr-read-ssh-agent)
(global-set-key (kbd "<f5>") 'ekr-git-gui)
(global-set-key (kbd "M-n") 'next-error)
(global-set-key (kbd "M-p") 'previous-error)
(global-set-key (kbd "C-M-n") 'ekr-next-scenario)
(global-set-key (kbd "C-M-p") 'ekr-previous-scenario)
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

;(find-file-noselect "/ekr@10.226.93.5:/opt")

(defun starts-with-ekr (symb)
      "returns non-nil if symbol symb starts with 'ekr-'.  Else nil."
      (let ((s (symbol-name symb)))
        (cond ((>= (length s) (length "ekr-"))
               (string-equal (substring s 0 (length "ekr-")) "ekr-"))
              (t nil))))

(require 'cl)
(setq compilation-error-regexp-alist-alist
      (remove-if (lambda (item) (starts-with-ekr (car item)))
                 compilation-error-regexp-alist-alist))
(setq compilation-error-regexp-alist
      (remove-if 'starts-with-ekr
                 (mapcar 'car compilation-error-regexp-alist-alist)))

; erstes entfernen, beim Entwickeln
;(setq compilation-error-regexp-alist-alist
;      (cdr compilation-error-regexp-alist-alist))


; (REGEXP FILE [LINE COLUMN TYPE HYPERLINK HIGHLIGHT...])

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

(add-to-list 'compilation-error-regexp-alist 'ekr-rails-test)
(add-to-list 'compilation-error-regexp-alist-alist
 	     '(ekr-rails-test "\\[\\(/[^: ]+\\):\\([0-9]+\\)\\]"
		     1 2 nil 1))

;(add-to-list 'compilation-error-regexp-alist 'ekr-cucumber-error-ansijunk)
;(add-to-list 'compilation-error-regexp-alist-alist
; 	     '(ekr-cucumber-error-ansijunk " \\([^ ]+\\):\\([0-9]+\\):"
;		     1 2 nil 2))

(add-to-list 'compilation-error-regexp-alist 'ekr-cucumber-comment)
(add-to-list 'compilation-error-regexp-alist-alist
 	     '(ekr-cucumber-comment "# \\(.+\\):\\([0-9]+\\)"
		     1 2 nil 0))

; also jump between...
;  Szenario: xxxxx           # /home/xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx.feature:242
(add-to-list 'compilation-error-regexp-alist 'ekr-cucumber-scenario)
(add-to-list 'compilation-error-regexp-alist-alist
 	     '(ekr-cucumber-scenario "Szenario: .+ # \\(.+\\):\\([0-9]+\\)"
                                     1 2 nil 1))

; You can implement step definitions for undefined steps with these snippets:
(add-to-list 'compilation-error-regexp-alist 'ekr-cucumber-pending)
(add-to-list 'compilation-error-regexp-alist-alist
 	     '(ekr-cucumber-pending "You can implement step definitions for undefined steps with these snippets"
                                     1 1 nil 1))

;; Der Rest sollte weit hinten stehen um am Anfang der Liste zu erscheinen...

(add-to-list 'compilation-error-regexp-alist 'ekr-perl-ignore)
(add-to-list 'compilation-error-regexp-alist-alist
 	     '(ekr-perl-ignore " at \\(library \\)?\\(/.*?\\) line \\([0-9]+\\)"
		     2 3 nil 0))

(add-to-list 'compilation-error-regexp-alist 'ekr-perl-ignore-http)
(add-to-list 'compilation-error-regexp-alist-alist
 	     '(ekr-perl-ignore-http "\\(http://\\)"
		     1 nil nil 0))

(add-to-list 'compilation-error-regexp-alist 'ekr-perl-ignore-c-in-so)
(add-to-list 'compilation-error-regexp-alist-alist
 	     '(ekr-perl-ignore-c-in-so "\.c:[0-9]+:in [^ ]+\.so"
		     1 1 nil 0))

(add-to-list 'compilation-error-regexp-alist 'ekr-rbenv)
(add-to-list 'compilation-error-regexp-alist-alist
 	     '(ekr-rbenv "\\(/home/ekr/.rbenv/.*?\\):\\([0-9]+\\):"
		     1 2 nil 0))

(add-to-list 'compilation-error-regexp-alist 'ekr-perl-ignore-timestamp)
(add-to-list 'compilation-error-regexp-alist-alist
 	     '(ekr-perl-ignore-timestamp "[0-9]+/[0-9]+/[0-9]+ [0-9]+:[0-9]+:[0-9]+"
		     1 1 nil 0))

; Compilation started at Mon Feb 17 14:05:36
(add-to-list 'compilation-error-regexp-alist 'ekr-perl-ignore-timestamp2)
(add-to-list 'compilation-error-regexp-alist-alist
 	     '(ekr-perl-ignore-timestamp2 "Com.* started at .* [0-9]+:[0-9]+:[0-9]+"
		     1 1 nil 0))

;            got: "06.01.2014 10:42:14"
(add-to-list 'compilation-error-regexp-alist 'ekr-perl-ignore-timestamp3)
(add-to-list 'compilation-error-regexp-alist-alist
 	     '(ekr-perl-ignore-timestamp3 "[0-9]+\.[0-9]+\.[0-9]+ [0-9]+:[0-9]+:[0-9]+"
		     1 1 nil 0))

;   ..... RSpec::Expectations::ExpectationNotMetError ....
(add-to-list 'compilation-error-regexp-alist 'ekr-perl-ignore-ruby-rspec)
(add-to-list 'compilation-error-regexp-alist-alist
 	     '(ekr-perl-ignore-ruby-rspec "RSpec::Expectations::ExpectationNotMetError"
		     1 1 nil 0))

;             ^^^^^^^^^^ 
(add-to-list 'compilation-error-regexp-alist 'ekr-perl-ignore-rubocop)
(add-to-list 'compilation-error-regexp-alist-alist
 	     '(ekr-perl-ignore-rubocop " \\^+"
		     1 1 nil 0))

; cucumber undefined step
; ./bin/rake:8:in `<top (required)>'
(add-to-list 'compilation-error-regexp-alist 'ekr-cucumber-ignore-junk)
(add-to-list 'compilation-error-regexp-alist-alist
 	     '(ekr-cucumber-ignore-junk "\./bin/rake:"
                                       1 1 nil 0))

(add-to-list 'compilation-error-regexp-alist 'ekr-cucumber-ignore-junk2)
(add-to-list 'compilation-error-regexp-alist-alist
 	     '(ekr-cucumber-ignore-junk2 "-e:1:"
                                       1 1 nil 0))


;; ; line wrap in compilation mode (avoid 100% CPU for long li
;; (defun my-compilation-mode-hook ()
;;   (setq truncate-lines nil) ;; automatically becomes buffer local
;;   (set (make-local-variable 'truncate-partial-width-windows) nil))
;; (add-hook 'compilation-mode-hook 'my-compilation-mode-hook)

(setenv "TERM" "dumb")  ; for perldoc etc.
(setenv "PAGER" "cat")

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
(add-to-list 'load-path "~/.emacs.d/elisp/feature-mode")
(setq feature-default-language "fi")
;(setq feature-default-i18n-file "/path/to/gherkin/gem/i18n.yml")
(require 'feature-mode)
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
  (feature-run-cucumber '("--tags @dev") :feature-file (buffer-file-name)))

; hiermit erkennt u.a. der Compilation Buffer (=> cucumber) utf8 korrekt
;(prefer-coding-system 'utf-8)
; ... wird aber in .dir-locals.el eingestellt (auf andere Weise - genügt das?)

;; ANSI coloring in compilation buffers
(require 'ansi-color)
(defun ff/ansi-colorize-buffer ()
  (setq buffer-read-only nil)
  (ansi-color-apply-on-region (point-min) (point-max))
  (setq buffer-read-only t))
(add-hook 'compilation-filter-hook 'ff/ansi-colorize-buffer)

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

;(setenv "http_proxy" "http://A.B.C.D:8080")
;(setenv "https_proxy" "http://A.B.C.D:8080")

(require 'package)
(add-to-list 'package-archives
             '("melpa-stable" . "http://stable.melpa.org/packages/") t)
(when (< emacs-major-version 24)
  ;; For important compatibility libraries like cl-lib
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))
(package-initialize) ;; You might already have this line

(projectile-global-mode)

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

(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\.yml$" . yaml-mode))

(require 'scss-mode)
(add-to-list 'auto-mode-alist '("\.scss$" . scss-mode))

; messes with M-n...
;(add-hook 'after-init-hook #'global-flycheck-mode)

;; .html.erb etc.

;(add-to-list 'auto-mode-alist '("\.html.erb$" . ruby-mode))
;(add-to-list 'auto-mode-alist '("\.xml.erb$" . xml-mode))
;(add-to-list 'auto-mode-alist '("\.js.erb$" . javascript-mode))

(require 'web-mode)
(add-to-list 'auto-mode-alist '("\.erb$" . web-mode))
(add-to-list 'auto-mode-alist '("\.jsp$" . web-mode))
(add-to-list 'auto-mode-alist '("\.php$" . web-mode))

(add-to-list 'auto-mode-alist '("\.builder$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\.axlsx$" . ruby-mode))

(load-library "sql-indent/sql-indent")
;(eval-after-load "sql"
;  '(load-library "sql-indent/sql-indent"))

; (require 'git)

(require 'multiple-cursors)
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)

; Ctrl-j etc. für "div" => "<div></div>"
(add-to-list 'load-path "~/.emacs.d/elisp/emmet-mode")
(require 'emmet-mode)
(add-hook 'web-mode-hook 'emmet-mode)


;(setq electric-indent-functions-without-reindent (remove 'indent-line-function electric-indent-functions-without-reindent))

(setq-default electric-indent-inhibit t) 

(require 'recentf)
(recentf-mode 1)
(setq recentf-max-menu-items 25)
(global-set-key "\C-x\ \C-r" 'recentf-open-files)
;;;
;;; magit
;;;

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
  (progn
    (message "Standard face...")
    (message default-directory)))
  
; maximise on windows

(run-at-time "1" nil '(lambda () (toggle-frame-maximized)))

(require 'string-inflection)
(global-unset-key (kbd "C-q"))
(global-set-key (kbd "C-q C-u") 'my-string-inflection-cycle-auto)
(defun my-string-inflection-cycle-auto ()
  "switching by major-mode"
  (interactive)
  (cond
   ;; for emacs-lisp-mode
   ((eq major-mode 'emacs-lisp-mode)
    (string-inflection-all-cycle))
   ;; for java
   ((eq major-mode 'java-mode)
    (string-inflection-java-style-cycle))
   (t
    ;; default
    (string-inflection-ruby-style-cycle))))

; avoid extreme pauses on long compilation lines
(require 'truncated-compilation-mode)
(truncated-compilation-mode)

