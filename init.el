; If this does not load at all (no error message), maybe remove ~/.emacs.

; vimacs (emacs mode for vim)
; ---------------------------
; wget http://www.algorithm.com.au/downloads/vimacs/vimacs-0.95.tar.gz
; mkdir -p $HOME/.vim/doc
; mkdir -p $HOME/.vim/plugin
; cp -R doc plugin $HOME/.vim
; vim --cmd "helptags $HOME/.vim/doc" --cmd "q"
;
; or
;
; cat vimacs.tgz.uue | uudecode | tar -C ~/.vim -xzf -


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
 '(ahk-indentation 4)
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
 '(file-coding-system-alist
   (quote
    (("\\.dz\\'" no-conversion . no-conversion)
     ("\\.txz\\'" no-conversion . no-conversion)
     ("\\.xz\\'" no-conversion . no-conversion)
     ("\\.lzma\\'" no-conversion . no-conversion)
     ("\\.lz\\'" no-conversion . no-conversion)
     ("\\.g?z\\'" no-conversion . no-conversion)
     ("\\.\\(?:tgz\\|svgz\\|sifz\\)\\'" no-conversion . no-conversion)
     ("\\.tbz2?\\'" no-conversion . no-conversion)
     ("\\.bz2\\'" no-conversion . no-conversion)
     ("\\.Z\\'" no-conversion . no-conversion)
     ("\\.elc\\'" . utf-8-emacs)
     ("\\.el\\'" . prefer-utf-8)
     ("\\.utf\\(-8\\)?\\'" . utf-8)
     ("\\.xml\\'" . xml-find-file-coding-system)
     ("\\(\\`\\|/\\)loaddefs.el\\'" raw-text . raw-text-unix)
     ("\\.tar\\'" no-conversion . no-conversion)
     ("\\.po[tx]?\\'\\|\\.po\\." . po-find-file-coding-system)
     ("\\.\\(tex\\|ltx\\|dtx\\|drv\\)\\'" . latexenc-find-file-coding-system)
     ("" undecided)
     ("\\.rb\\'" . utf-8)
     ("\\.yml\\'" . utf-8)
     ("\\.erb\\'" . utf-8)
     ("\\.feature\\'" . utf-8))))
 '(git-commit-summary-max-length 2000)
 '(grep-find-ignored-files
   (quote
    (".#*" "*.o" "*~" "*.bin" "*.lbin" "*.so" "*.a" "*.ln" "*.blg" "*.bbl" "*.elc" "*.lof" "*.glo" "*.idx" "*.lot" "*.fmt" "*.tfm" "*.class" "*.fas" "*.lib" "*.mem" "*.x86f" "*.sparcf" "*.dfsl" "*.pfsl" "*.d64fsl" "*.p64fsl" "*.lx64fsl" "*.lx32fsl" "*.dx64fsl" "*.dx32fsl" "*.fx64fsl" "*.fx32fsl" "*.sx64fsl" "*.sx32fsl" "*.wx64fsl" "*.wx32fsl" "*.fasl" "*.ufsl" "*.fsl" "*.dxl" "*.lo" "*.la" "*.gmo" "*.mo" "*.toc" "*.aux" "*.cp" "*.fn" "*.ky" "*.pg" "*.tp" "*.vr" "*.cps" "*.fns" "*.kys" "*.pgs" "*.tps" "*.vrs" "*.pyc" "*.pyo" "*.gz" "cucumber.json")))
 '(grep-find-template "find . <X> -type f <F> -print0 | xargs -0 grep <C> -n <R>")
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
 '(package-selected-packages (quote (inhibit-startup-screen t)))
 '(projectile-globally-ignored-files (quote ("TAGS" "#*#")))
 '(ruby-insert-encoding-magic-comment nil)
 '(safe-local-variable-values
   (quote
    ((buffer-file-coding-system . iso-8859-1)
     (buffer-file-coding-system . utf-8))))
 '(scroll-error-top-bottom t)
 '(server-mode t)
 '(show-paren-mode t)
 '(smerge-command-prefix "d")
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
 '(url-proxy-services nil)
 '(use-file-dialog nil)
 '(vc-handled-backends (quote (RCS SVN SCCS Bzr Git Hg Arch)))
 '(vc-svn-diff-switches "-x -b")
 '(with-editor-emacsclient-executable nil)
 '(xslt-process-fop-log-level (quote (debug)))
 '(xslt-process-xml-xslt-associations nil)
 '(xterm-mouse-mode t))

;; (defun arrange-frame (w h x y)
;;   "Set the width, height, and x/y position of the current frame"
;;   (let ((frame (selected-frame)))
;;     (delete-other-windows)
;;     (set-frame-position frame x y)
;;     (set-frame-size frame w h)))
;; (defun arrange-my-frame ()
;;   ""
;;   (arrange-frame 148 68 494 22))
;; (add-hook 'emacs-startup-hook 'arrange-my-frame)
;; (arrange-my-frame)

(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)

(setq default-buffer-file-coding-system 'iso-latin-1-unix)

;(setq special-display-buffer-names
;           '("*Async Shell Command*" "*grep*" "*compilation*" "*vc-dir*"))
(setq special-display-buffer-names nil)

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
  (interactive)
  "Save all files, wait a little bit, then call (recompile). For compilations that watch file-change-times (RotTestHelper...)"
  (progn (save-some-buffers t)
	 ;(sleep-for 1.5)
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
  (execute-kbd-macro (kbd "<return>")))

(defun my-previous-scenario ()
  (interactive)
  "Jump to the previous cucumber scenario in the compilation buffer"
  (switch-to-buffer-other-window "*compilation*")
  (search-backward "Szenario:")
  (execute-kbd-macro (kbd "<return>")))

;; (global-set-key (kbd "<f1>") 'ekr-wiki-update)
(global-set-key (kbd "M-a") 'ekr-recompile)
(global-set-key (kbd "M-c") 'ekr-recompile)
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

; also jump between Szenarios...
;  Szenario: xxxxx           # /home/xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx.feature:242
(add-to-list 'compilation-error-regexp-alist 'ekr-cucumber-scenario)
(add-to-list 'compilation-error-regexp-alist-alist
 	     '(ekr-cucumber-scenario "# \\(.+\\.feature\\):\\([0-9]+\\)"
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

; cucumber output => ignore "<pre><code>" on first line of HTML stacktrace
;            <pre><code>app/models/concerns/gp_asp.rb:60:in `_update_kundenbetreuer_direktvertrieb&#39;
(add-to-list 'compilation-error-regexp-alist 'ekr-cucumber-html-pre)
(add-to-list 'compilation-error-regexp-alist-alist
 	     '(ekr-cucumber-html-pre " *<pre><code>\\([a-zA-Z0-9_./-]*?\\.rb\\):\\([0-9]+\\):"
		     1 2 nil 2))

; build in docker... /app/ entfernen
; /app/src/emil/build.xml:226: Javadoc failed: java.io.IOException: Cannot run program "javadoc": error=2, No such file or directory
(add-to-list 'compilation-error-regexp-alist 'ekr-container-remove-app)
(add-to-list 'compilation-error-regexp-alist-alist
 	     '(ekr-container-remove-app "^/app/\\(.+?\\):\\([0-9]+\\):"
                                     1 2 nil 2))

; ignore API warnings...
; /app/src/emil/src/de/edag/fps/emil/AnwendungEMIL.java:54: warning: Signal is internal proprietary API and may be removed in a future release
(add-to-list 'compilation-error-regexp-alist 'ekr-ignore-sunapi)
(add-to-list 'compilation-error-regexp-alist-alist
 	     '(ekr-ignore-sunapi ".*is internal proprietary API and may be removed in a future release.*"
		     nil nil nil 0))

; erstes (also letztes ;) ) entfernen, beim Entwickeln
;(setq compilation-error-regexp-alist-alist (cdr compilation-error-regexp-alist-alist))

;; (add-to-list 'compilation-error-regexp-alist 'ekr-imp-log)
;; (add-to-list 'compilation-error-regexp-alist-alist
;;  	     '(ekr-imp-log "IMPORT: Log-Datei \\(/opt/rot/dump/.*\.sql\\)<br>"
;; 		     1 nil nil 0))
;;
;; (add-to-list 'compilation-error-regexp-alist 'ekr-rot-perl-lib)
;; (add-to-list 'compilation-error-regexp-alist-alist
;;  	     '(ekr-rot-perl-lib " at \\(/opt/rot/app/project.*?/vendor/rot-perl-lib/src/Rot/.*?\\) line \\([0-9]+\\)"
;; 		     1 2 nil 0))

;; (add-to-list 'compilation-error-regexp-alist 'ekr-rails-test)
;; (add-to-list 'compilation-error-regexp-alist-alist
;;  	     '(ekr-rails-test "\\[\\(/[^: ]+\\):\\([0-9]+\\)\\]"
;; 		     1 2 nil 1))
;;
;; (add-to-list 'compilation-error-regexp-alist 'ekr-perl-ignore)
;; (add-to-list 'compilation-error-regexp-alist-alist
;;  	     '(ekr-perl-ignore " at \\(library \\)?\\(/.*?\\) line \\([0-9]+\\)"
;; 		     2 3 nil 0))
;;
;; ; undefined method `create_table?' for #<CreateDelayedJobs:0x00555f85d0b698>/home/ekr/src/akp/db/migrate/20150715091448_create_delayed_jobs.rb:4:in `up'
;; (add-to-list 'compilation-error-regexp-alist 'ekr-ruby-classfile)
;; (add-to-list 'compilation-error-regexp-alist-alist
;;  	     '(ekr-ruby-classfile "#<.*?>\\(/.*?\\):\\([0-9]+\\)"
;; 		     1 2 nil 0))
;;
;; (add-to-list 'compilation-error-regexp-alist 'ekr-perl-ignore-http)
;; (add-to-list 'compilation-error-regexp-alist-alist
;;  	     '(ekr-perl-ignore-http "\\(http://\\)"
;; 		     1 nil nil 0))
;;
;; (add-to-list 'compilation-error-regexp-alist 'ekr-perl-ignore-c-in-so)
;; (add-to-list 'compilation-error-regexp-alist-alist
;;  	     '(ekr-perl-ignore-c-in-so "\.c:[0-9]+:in [^ ]+\.so"
;; 		     1 1 nil 0))
;;
;; (add-to-list 'compilation-error-regexp-alist 'ekr-perl-ignore-timestamp)
;; (add-to-list 'compilation-error-regexp-alist-alist
;;  	     '(ekr-perl-ignore-timestamp "[0-9]+/[0-9]+/[0-9]+ [0-9]+:[0-9]+:[0-9]+"
;; 		     1 1 nil 0))
;;
;; ; Compilation started at Mon Feb 17 14:05:36
;; (add-to-list 'compilation-error-regexp-alist 'ekr-perl-ignore-timestamp2)
;; (add-to-list 'compilation-error-regexp-alist-alist
;;  	     '(ekr-perl-ignore-timestamp2 "Com.* started at .* [0-9]+:[0-9]+:[0-9]+"
;; 		     1 1 nil 0))
;;
;; ;            got: "06.01.2014 10:42:14"
;; (add-to-list 'compilation-error-regexp-alist 'ekr-perl-ignore-timestamp3)
;; (add-to-list 'compilation-error-regexp-alist-alist
;;  	     '(ekr-perl-ignore-timestamp3 "[0-9]+\.[0-9]+\.[0-9]+ [0-9]+:[0-9]+:[0-9]+"
;; 		     1 1 nil 0))
;;
;; ;   ..... RSpec::Expectations::ExpectationNotMetError ....
;; (add-to-list 'compilation-error-regexp-alist 'ekr-perl-ignore-ruby-rspec)
;; (add-to-list 'compilation-error-regexp-alist-alist
;;  	     '(ekr-perl-ignore-ruby-rspec "RSpec::Expectations::ExpectationNotMetError"
;; 		     1 1 nil 0))
;;
;; ;             ^^^^^^^^^^
;; (add-to-list 'compilation-error-regexp-alist 'ekr-perl-ignore-rubocop)
;; (add-to-list 'compilation-error-regexp-alist-alist
;;  	     '(ekr-perl-ignore-rubocop " \\^+"
;; 		     1 1 nil 0))
;;
;; ; cucumber undefined step
;; ; ./bin/rake:8:in `<top (required)>'
;; (add-to-list 'compilation-error-regexp-alist 'ekr-cucumber-ignore-junk)
;; (add-to-list 'compilation-error-regexp-alist-alist
;;  	     '(ekr-cucumber-ignore-junk "\./bin/rake:"
;;                                        1 1 nil 0))
;;
;; (add-to-list 'compilation-error-regexp-alist 'ekr-cucumber-ignore-junk2)
;; (add-to-list 'compilation-error-regexp-alist-alist
;;  	     '(ekr-cucumber-ignore-junk2 "-e:1:"
;;                                        1 1 nil 0))
;;
;; ; ruby "ap" stacktrace
;; ; [  0] "/Users/KRAEME/.rbenv/versions/2.3.3/lib/ruby/gems/2.3.0/gems/activemodel-4.0.6/lib/active_model/attribute_methods.rb:439:in `method_missing'",
;; (add-to-list 'compilation-error-regexp-alist 'ekr-ruby-ap-trace)
;; (add-to-list 'compilation-error-regexp-alist-alist
;;  	     '(ekr-ruby-ap-trace "\"\\(.+\\):\\([0-9]+\\):"
;; 		     1 2 nil 0))
;;
;; ; pytest in Docker -> files are not reachable
;; ; /usr/local/lib/python3.6/site-packages/werkzeug/wrappers/json.py:119:
;; (add-to-list 'compilation-error-regexp-alist 'ekr-ignore-pytest)
;; (add-to-list 'compilation-error-regexp-alist-alist
;;  	     '(ekr-ignore-pytest "\\(/usr/local/lib/python.+\\):\\([0-9]+\\):"
;; 		     1 2 nil 0))
;;
;; ; Python tests
;; ; E     File "/app/application/models/emil_schema.py", line 209
;; (add-to-list 'compilation-error-regexp-alist 'ekr-python-test)
;; (add-to-list 'compilation-error-regexp-alist-alist
;;  	     '(ekr-python-test "File \"/app/\\(.*?\\)\", line \\([0-9]+\\)"
;; 		     1 2 nil 0))
;;
;; ; gatling compilation
;; ; 09:16:27.407 [ERROR] i.g.c.ZincCompiler$ - /opt/gatling/user-files/simulations/BrowsefilterSimulation.scala:107:34: value mean is not a member of io.gatling.core.assertion.AssertionWithPath
;; (add-to-list 'compilation-error-regexp-alist 'ekr-gatling-error)
;; (add-to-list 'compilation-error-regexp-alist-alist
;;  	     '(ekr-gatling-error " - /opt/gatling/\\([^:]+\\):\\([0-9]+\\):\\([0-9]+\\):"
;; 		     1 2 nil 0))

;; ; standard ruby/python error
;; ;      ./lib/interfaces/magentaforce.rb:129:in `execute'
;; (add-to-list 'compilation-error-regexp-alist 'ekr-ruby-error)
;; (add-to-list 'compilation-error-regexp-alist-alist
;;  	     '(ekr-ruby-error "^[ \"]+\\([a-zA-Z0-9/._-]+?\\):\\([0-9]+\\):"
;; 		     1 2 nil 2))
;;
;; ; cucumber compressed mode
;; ; cucumber -p rerun /Users/KRAEME/Documents/src/akp/acnneu/features/api_mobilfunk_vertrage.feature:16 # Szenario: Der Abruf der Verträge eines GP funktioniert
;; (add-to-list 'compilation-error-regexp-alist 'ekr-ruby-cucumber-rerun)
;; (add-to-list 'compilation-error-regexp-alist-alist
;;  	     '(ekr-ruby-cucumber-rerun "^cucumber -p rerun +\\([a-zA-Z0-9/._-]+?\\):\\([0-9]+\\) #"
;; 		     1 2 nil 2))

;; ; line wrap in compilation mode (avoid 100% CPU for long lines)
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

;;;; in PUTTY, not X11:
;; (custom-set-faces
;;  ;; custom-set-faces was added by Custom.
;;  ;; If you edit it by hand, you could mess it up, so be careful.
;;  ;; Your init file should contain only one such instance.
;;  ;; If there is more than one, they won't work right.
;;  '(default ((t (:inherit nil :stipple nil :background "#181818" :foreground "gray80" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 1 :width normal :foundry "default" :family "default"))))
;;  '(font-lock-comment-delimiter-face ((t (:inherit font-lock-comment-face :foreground "gray55"))))
;;  '(font-lock-comment-face ((t (:foreground "gray55")))))

(require 'package)
(add-to-list 'package-archives
             '("melpa-stable" . "http://stable.melpa.org/packages/") t)
(when (< emacs-major-version 24)
  ;; For important compatibility libraries like cl-lib
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))
(package-initialize) ;; You might already have this line

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

; frame commands
;(require 'frame-cmds)

; debugger
(unless (ignore-errors
          (require 'realgud)
          (add-to-list 'load-path "~/.emacs.d/elisp/realgud-byebug")
          (require 'realgud-byebug))
  (message "Warning: error loading realgud, ignoring"))


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
 )


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

; auto syntax error check
(unless (ignore-errors
          (require 'flymake-ruby)
          (add-hook 'ruby-mode-hook 'flymake-ruby-load))
  (message "Warning: error loading flymake-ruby, ignoring"))

; ruby shell
(global-set-key (kbd "C-c r r") 'inf-ruby-console-auto)

;;;;;; projectile

(projectile-global-mode)

; many commands like C-c r m  (Model...)
(unless (ignore-errors
          (projectile-rails-global-mode))
  (message "Warning: ignoring errors loading projectile-rails-global-mode"))

(unless (ignore-errors
          (require 'robe)
          (add-hook 'ruby-mode-hook 'robe-mode))
  (message "Warning: ignoring errors loading robe"))

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
      (setenv "DOCKER_HOST" (shell-command-to-string "docker-machine env default | grep DOCKER_HOST | sed -e 's/export DOCKER_HOST=.//' -e 's/.$//' | tr -d '\n'"))
      (setenv "DOCKER_TLS_VERIFY" "1")
      (setenv "DOCKER_CERT_PATH" "/Users/KRAEME/.docker/machine/machines/default")
      (setenv "DOCKER_MACHINE_NAME" "default")))


;;; folding in xml

(require 'hideshow)
(require 'sgml-mode)
(require 'nxml-mode)
(add-to-list 'hs-special-modes-alist
             '(nxml-mode
               "<!--\\|<[^/>]*[^/]>"
               "-->\\|</[^/>]*[^/]>"

               "<!--"
               sgml-skip-tag-forward
               nil))
(add-hook 'nxml-mode-hook 'hs-minor-mode)
(define-key nxml-mode-map (kbd "C-c h") 'hs-toggle-hiding)

(server-start)
