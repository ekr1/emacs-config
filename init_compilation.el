;;; init_compulation.el --- compilation mode specifics
;;; -*- lexical-binding: t; -*-
                                        ;
(my-banner "Compilation...")

(require 'compile)
(require 'feature-mode)

; fix for very slow compiling:  (see docs, a little bit confusing?)
; (setopt process-adaptive-read-buffering nil)

; (setopt special-display-buffer-names
;           '("*Async Shell Command*" "*grep*" "*compilation*" "*vc-dir*"))

; note: special-display-buffer-names is deprecated, use display-buffer-alist instead
; (setopt special-display-buffer-names nil)

(defun my-recompile ()
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

(defun my-compilation-finished (buf result)
  "Play a beep when compilation finishes."
  ; if the "afplay" exec exists...
  (if (executable-find "afplay")
      (start-process "*Compilation Finished Beep*" nil "afplay" "/Users/KRAEME/.emacs.d/short_beep.m4a")))
(remove-hook 'compilation-finish-functions 'my-compilation-finished)
(add-hook 'compilation-finish-functions 'my-compilation-finished)

(defun my-git-gui ()
  "Run 'git gui' without a buffer."
  (interactive)
  (start-process "git gui" nil "git" "gui")
  (message "'git gui' started"))

(defun my-next-scenario ()
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

;; (global-set-key (kbd "<f1>") 'my-wiki-update)
(global-set-key (kbd "M-a") 'my-recompile)
(global-set-key (kbd "M-c") 'my-recompile)
;; (global-set-key (kbd "<f3>") 'my-compile-plsql)
(global-set-key (kbd "<f5>") 'my-git-gui)
(global-set-key (kbd "M-n") 'next-error)
(global-set-key (kbd "M-p") 'previous-error)
(global-set-key (kbd "C-M-n") 'my-next-scenario)
(global-set-key (kbd "C-M-p") 'my-previous-scenario)
(global-set-key (kbd "C-/") 'comment-or-uncomment-region)
(global-set-key (kbd "C-#") 'comment-or-uncomment-region)

; hiermit erkennt u.a. der Compilation Buffer (=> cucumber) utf8 korrekt
;(prefer-coding-system 'utf-8)
; ... wird aber in .dir-locals.el eingestellt (auf andere Weise - genügt das?)

;; included in 28.2, not required anymore
;; ANSI coloring in compilation buffers
;; (require 'ansi-color)
;; (defun ff/ansi-colorize-buffer ()
;; ;  (setopt buffer-read-only nil)
;; ;  (ansi-color-apply-on-region (point-min) (point-max))
;; ;  (setopt buffer-read-only t)
;;   )
;; (add-hook 'compilation-filter-hook 'ff/ansi-colorize-buffer)

;; ; ANSI coloring in compilation buffers
(add-hook 'compilation-filter-hook 'ansi-color-compilation-filter)

(defun my-compilation-start-add-fold (args)
  "Add '| fold -w <visible-width>' to the command passed to compilation-start.
If the *compilation* buffer is not visible or does not exist, default to 100."
  (let* ((command (nth 0 args)))
    (setf (nth 0 args)
          (concat "( " command " ) 2>&1 | LC_ALL=C   awk '{LEN=400; while (length($0) > LEN) {print substr($0, 1, LEN), \"↩\"; $0 = substr($0, LEN+1)} print $0}' # see init.el"))
    args))

(advice-add 'compilation-start :filter-args #'my-compilation-start-add-fold)

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

; avoid extreme pauses on long compilation lines
;; (require 'truncated-compilation-mode)
;; (truncated-compilation-mode)

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
;;                                         ; (setopt current-frame (car (car (cdr (current-frame-configuration)))))
;;                                         ; (select-frame-set-input-focus current-frame)
;;     ))
;;
;; (add-to-list 'compilation-finish-functions
;; 	     'notify-compilation-result)

; DOCKER_BUILDKIT does not look good in *compilation* buffers...
(setenv "BUILDKIT_PROGRESS" "plain")

(add-to-list 'savehist-additional-variables 'compile-command)
(add-to-list 'savehist-additional-variables 'compile-history)
(add-to-list 'savehist-additional-variables 'compilation-directory)

(global-set-key (kbd "C-c C-k") 'kill-compilation)

; cucumber mode
;(add-to-list 'load-path "~/.emacs.d/elisp/feature-mode")
;(setopt feature-default-i18n-file "/path/to/gherkin/gem/i18n.yml")
(setopt feature-default-language "fi")
(add-to-list 'auto-mode-alist '("\.feature$" . feature-mode))
;; Keybinding	Description
;; C-c ,v	Verify all scenarios in the current buffer file.
;; C-c ,s	Verify the scenario under the point in the current buffer.
;; C-c ,f	Verify all features in project. (Available in feature and ruby files)
;; C-c ,r	Repeat the last verification process.
;; C-c ,g	Go to step-definition under point (requires ruby_parser gem >= 2.0.5)
(define-key feature-mode-map  (kbd "C-c ,v") 'my-feature-verify-dev-scenarios-in-buffer)
(define-key feature-mode-map  (kbd "C-c ,V") 'feature-verify-all-scenarios-in-buffer)

(defun my-feature-verify-dev-scenarios-in-buffer ()
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

(defun starts-with-my (symb)
      "Returns non-nil if symbol symb starts with 'my-'.  Else nil."
      (let ((s (symbol-name symb)))
        (cond ((>= (length s) (length "my-"))
               (string-equal (substring s 0 (length "my-")) "my-"))
              (t nil))))

(setopt compilation-error-regexp-alist-alist
      (cl-remove-if (lambda (item) (starts-with-my (car item)))
                 compilation-error-regexp-alist-alist))
(setopt compilation-error-regexp-alist
      (cl-remove-if 'starts-with-my
                 (mapcar 'car compilation-error-regexp-alist-alist)))

; M-x re-builder
; (setopt compilation-debug t) ; => then M-x describe-text-properties

; (REGEXP FILE [LINE COLUMN TYPE HYPERLINK HIGHLIGHT...])
; TYPE is 2 or nil for a real error or 1 for warning or 0 for info.
;
; Later entries seem to have higher priority...

; ignore spurious output
;      stmt.c:243:in oci8lib_230.bundle
(add-to-list 'compilation-error-regexp-alist 'my-oci8-stmt)
(add-to-list 'compilation-error-regexp-alist-alist
 	     '(my-oci8-stmt "^ *\\(stmt.c\\):\\([0-9]+\\):"
		     1 2 nil 0))

; comments on every cucumber line
; Wenn die REST-API für Mobilfunkverträge aufgerufen wird # features/step_definitions/api_mobilfunk_vertraege_steps.rb:17
(add-to-list 'compilation-error-regexp-alist 'my-cucumber-comment)
(add-to-list 'compilation-error-regexp-alist-alist
 	     '(my-cucumber-comment "# \\(.+rb\\):\\([0-9]+\\)$"
		     1 2 nil 0))

; build in docker... /app/ entfernen
; /app/src/emil/build.xml:226: Javadoc failed: java.io.IOException: Cannot run program "javadoc": error=2, No such file or directory
(add-to-list 'compilation-error-regexp-alist 'my-container-remove-app)
(add-to-list 'compilation-error-regexp-alist-alist
 	     '(my-container-remove-app "^/app/\\(.+?\\):\\([0-9]+\\):"
                                     1 2 nil 2))

; pytest/stacktrace
; file /app/tests/test_basics.py, line 14
(add-to-list 'compilation-error-regexp-alist 'my-pytest-container-trace)
(add-to-list 'compilation-error-regexp-alist-alist
 	     '(my-pytest-container-trace "^file /app/\\(.+?\\), line \\([0-9]+\\)$"
                                     1 2 nil 2))

; pytest / yet another format
;  File "/app/application/routes/halerium.py", line 28
(add-to-list 'compilation-error-regexp-alist 'my-pytest-app-error)
(add-to-list 'compilation-error-regexp-alist-alist
 	     '(my-pytest-app-error "^ *File \"/app/\\(.+?\\)\", line \\([0-9]+\\)$"
                                     1 2 nil 2))

; ignore pytest timing
; #13 3.878 test_main.py:79:4: E0602: Undefined variable '_clear_all' (undefined-variable)
(add-to-list 'compilation-error-regexp-alist 'my-pytest-timing)
(add-to-list 'compilation-error-regexp-alist-alist
 	     '(my-pytest-timing "^#[0-9]+ [0-9]+.[0-9]+ \\(.+?\\):\\([0-9]+\\):"
                                     1 2 nil 2))

; ignore /usr/local...
; /usr/local/lib/python3.7/site-packages/urllib3/connection.py:170:
(add-to-list 'compilation-error-regexp-alist 'my-ignore-usr-local)
(add-to-list 'compilation-error-regexp-alist-alist
 	     '(my-ignore-usr-local "^/usr/local/"
                                     nil nil nil 0))

; also jump between Szenarios...
;  Szenario: xxxxx           # /home/xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx.feature:242
(add-to-list 'compilation-error-regexp-alist 'my-cucumber-scenario)
(add-to-list 'compilation-error-regexp-alist-alist
 	     '(my-cucumber-scenario "Szenario:.*# \\(.+\\.feature\\):\\([0-9]+\\)"
                                     1 2 nil 1))

; stacktrace
;    [  0] "/Users/KRAEME/Documents/src/akp/acnneu/app/controllers/gps_controller.rb:128:in `block (2 levels) in show_analytics'",
(add-to-list 'compilation-error-regexp-alist 'my-ruby-stacktrace)
(add-to-list 'compilation-error-regexp-alist-alist
 	     '(my-ruby-stacktrace "^ *.[ 0-9]+. \"\\(.+\\.rb\\):\\([0-9]+\\):"
                                     1 2 nil 2))

; ruby stack trace -> rbenv are low prio
;    [  1] "/Users/KRAEME/.rbenv/versions/2.3.3/lib/ruby/gems/2.3.0/gems/actionpack-4.0.6/lib/action_controller/metal/mime_responds.rb:191:in `respond_to'",
(add-to-list 'compilation-error-regexp-alist 'my-rbenv)
(add-to-list 'compilation-error-regexp-alist-alist
 	     '(my-rbenv "\\([a-zA-Z0-9_./-]*/.rbenv/.*?\\):\\([0-9]+\\):"
		     1 2 nil 0))

; cucumber output => ignore HTML on first line of HTML stacktrace
;            <pre><code>app/models/concerns/gp_asp.rb:60:in `_update_kundenbetreuer_direktvertrieb&#39;
;        <h2>/Users/KRAEME/Documents/src/akp/acnneu/app/views/doculife_api/akte_index.json.jbuilder:14: syntax error, unexpected keyword_ensure, expecting end-of-input</h2>
(add-to-list 'compilation-error-regexp-alist 'my-cucumber-html-pre)
(add-to-list 'compilation-error-regexp-alist-alist
 	     '(my-cucumber-html-pre ">\\([a-zA-Z0-9_./-]*?\\.[a-z]+\\):\\([0-9]+\\):"
		     1 2 nil 2))

; weird capybara output
;          Showing <i>/Users/KRAEME/Documents/src/akp/acnneu/app/views/doculife_api/akte_index.json.jbuilder</i> where line <b>#5</b> raised:
(add-to-list 'compilation-error-regexp-alist 'my-cucumber-showing)
(add-to-list 'compilation-error-regexp-alist-alist
 	     '(my-cucumber-showing "Showing <i>\\(.*?\\)</i> where line <b>.\\([0-9]+\\)</b>"
		     1 2 nil 2))

;; ; rspec... # entfernen
;; ;      # ./spec/acceptance/doculife_api_spec.rb:17:in `block (3 levels) in <top (required)>'
;; (add-to-list 'compilation-error-regexp-alist 'my-remove-hash)
;; (add-to-list 'compilation-error-regexp-alist-alist
;;  	     '(my-remove-hash "# \\(.+?\\):\\([0-9]+\\)$"
;;                                      1 2 nil 2))

; Yet another style of ruby errors...
;	 3: from /Users/xx/Documents/src/akp/acn_neu_tools/bin/acn_tools_jira.rb:146:in `add_watcher'
(add-to-list 'compilation-error-regexp-alist 'my-ruby-acntools)
(add-to-list 'compilation-error-regexp-alist-alist
 	     '(my-ruby-acntools "from \\([^:]+\\):\\([0-9]+\\):" 1 2 nil 2))

; ignore API warnings...
; /app/src/emil/src/de/edag/fps/emil/AnwendungEMIL.java:54: warning: Signal is internal proprietary API and may be removed in a future release
(add-to-list 'compilation-error-regexp-alist 'my-ignore-sunapi)
(add-to-list 'compilation-error-regexp-alist-alist
 	     '(my-ignore-sunapi ".*is internal proprietary API and may be removed in a future release.*"
		     nil nil nil 0))

; ignore cucumber artifact...
; -e:1:in `<main>'
(add-to-list 'compilation-error-regexp-alist 'my-ignore-minuse)
(add-to-list 'compilation-error-regexp-alist-alist
 	     '(my-ignore-minuse "^-e:1:in"
		     nil nil nil 0))

; saxon / xslt errors
; Error at xsl:param on line 770 of ipp_measurement.xsl:
(add-to-list 'compilation-error-regexp-alist 'my-saxon-error)
(add-to-list 'compilation-error-regexp-alist-alist
 	     '(my-saxon-error "^Error at .*? on line \\([0-9]+\\) of \\(.+\\):$"
                                     2 1 nil 2))

; saxon / xslt error with column
; Error at xsl:call-template on line 598 column 78 of ipp_measurement.xsl:
(add-to-list 'compilation-error-regexp-alist 'my-saxon-error-col)
(add-to-list 'compilation-error-regexp-alist-alist
 	     '(my-saxon-error-col "^Error at .*? on line \\([0-9]+\\) column \\([0-9]+\\) of \\(.+\\):$"
                                     3 1 2 2))

; saxon / backtrace
;  at xsl:call-template name="tolerances" (file:/Users/KRAEME/Documents/src/doorfitting/process-adapter/config/ipp_measurement.xsl#598)
(add-to-list 'compilation-error-regexp-alist 'my-saxon-backtrace)
(add-to-list 'compilation-error-regexp-alist-alist
 	     '(my-saxon-backtrace " at .*? (file:\\(.+\\)#\\([0-9]+\\))$"
                                     1 2 nil 2))

; PHP with docker prefix
; #1 /opt/app-root/src/webroot/php/application/AjaxConnector.php(2063): Tools::convertTimestampToDate('@1662716581', false)
(add-to-list 'compilation-error-regexp-alist 'my-php-approot)
(add-to-list 'compilation-error-regexp-alist-alist
 	     '(my-php-approot "#[0-9]+ /opt/app-root/src/\\(.+?.php\\)(\\([0-9]+\\)): "
                                     1 2 nil 2))

; #2  /opt/app-root/src/tests/unit/KafkaTest.php:134
(add-to-list 'compilation-error-regexp-alist 'my-php-codeception-backtrace)
(add-to-list 'compilation-error-regexp-alist-alist
 	     '(my-php-codeception-backtrace "/opt/app-root/src/\\(.+?.php\\):\\([0-9]+\\)"
                                     1 2 nil 2))

; PHP Codeception snapshots
;  [Snapshot Saved] file:///opt/app-root/src/tests/_output/debug/2023-01-18_14-02-03_63c7fbdb9f1029.50598734.html
(add-to-list 'compilation-error-regexp-alist 'my-php-codeception-snapshot)
(add-to-list 'compilation-error-regexp-alist-alist
 	     '(my-php-codeception-snapshot ".*\\[Snapshot Saved\\] file:///opt/app-root/src/\\(.+?.html\\)"
                                     1 nil nil 2))

; PHP Codeception HTML
;Html: /opt/app-root/src/tests/_output/ArchivedMeasurementPlanCest.showListOfArchivedPlans.fail.html
(add-to-list 'compilation-error-regexp-alist 'my-php-codeception-html)
(add-to-list 'compilation-error-regexp-alist-alist
 	     '(my-php-codeception-html "Html: /opt/app-root/src/\\(.+?.html\\)"
                                     1 nil nil 2))

; PHP Codeception Response
;Response: /opt/app-root/src/tests/_output/ArchivedMeasurementPlanCest.showListOfArchivedPlans.fail.html
(add-to-list 'compilation-error-regexp-alist 'my-php-codeception-response)
(add-to-list 'compilation-error-regexp-alist-alist
 	     '(my-php-codeception-response "Response: /opt/app-root/src/\\(.+?.html\\)"
                                     1 nil nil 2))

; PHP Codeception Png
;Png: /opt/app-root/src/tests/_output/ArchivedMeasurementPlanCest.showListOfArchivedPlans.fail.png
(add-to-list 'compilation-error-regexp-alist 'my-php-codeception-png)
(add-to-list 'compilation-error-regexp-alist-alist
 	     '(my-php-codeception-png "Png: /opt/app-root/src/\\(.+?.png\\)"
                                     1 nil nil 2))

; rspec error
;     # ./spec/models/dtag_spec.rb:9:in `block (3 levels) in <top (required)>'
(add-to-list 'compilation-error-regexp-alist 'my-rspec-error)
(add-to-list 'compilation-error-regexp-alist-alist
 	     '(my-rspec-error "# \\(.+?\\):\\([0-9]+\\):"
                                     1 2 nil 2))

; "helm template" error
; Error: template: legacy-webserver-backup/templates/pvc.yaml:10:25:
(add-to-list 'compilation-error-regexp-alist 'my-helm-template-error)
(add-to-list 'compilation-error-regexp-alist-alist
 	     '(my-helm-template-error "Error: template: .*/\\(templates/.+?\\):\\([0-9]+\\):\\([0-9]+\\):"
                                     1 2 3 2))

; "helm YAML parse" error
; Error: YAML parse error on legacy-webserver-backup/templates/cronjob.yaml: error converting YAML to JSON: yaml: line 28: did not find expected '-' indicator
(add-to-list 'compilation-error-regexp-alist 'my-helm-yaml-parse-error)
(add-to-list 'compilation-error-regexp-alist-alist
 	     '(my-helm-yaml-parse-error "Error: YAML parse error on .*/\\(templates/.+?\\): error converting YAML to JSON: yaml: line \\([0-9]+\\):"
                                     1 2 nil 2))

; internal helm go errors, ignore
; install.go:222: [debug] Original chart version: ""
(add-to-list 'compilation-error-regexp-alist 'my-helm-ignore-go)
(add-to-list 'compilation-error-regexp-alist-alist
 	     '(my-helm-ignore-go "[^.:]+\\.go:[0-9]+:"
                                     nil nil nil 0))

; yaml image tag, ignore
;            image: mtr.devops.telekom.de/akp/legacy-webserver-backup:1.0.0
(add-to-list 'compilation-error-regexp-alist 'my-yaml-image-tag-ignore)
(add-to-list 'compilation-error-regexp-alist-alist
 	     '(my-yaml-image-tag-ignore "[\t ]image:[\t ]"
                                     nil nil nil 0))

; Dockerfiles
; Dockerfile.akp-web:98
(add-to-list 'compilation-error-regexp-alist 'my-dockerfile)
(add-to-list 'compilation-error-regexp-alist-alist
 	     '(my-dockerfile "^\\(Dockerfile[^:]+\\):\\([0-9]+\\)$"
                                     1 2 nil 2))

;  File "/.../xxx.py", line 43, in parse_under_cursor
(add-to-list 'compilation-error-regexp-alist 'my-python-parse)
(add-to-list 'compilation-error-regexp-alist-alist
             '(my-python-parse "^ *File \"\\(.*?\.py\\)\", line \\([0-9]+\\)"
                                1 2 nil 2))

; undo the last add-to-list:
; (setopt compilation-error-regexp-alist-alist (cdr compilation-error-regexp-alist-alist))

; REGEXP FILE [LINE COLUMN TYPE HYPERLINK HIGHLIGHT...]
; TYPE is 2 or nil for a real error or 1 for warning or 0 for info.

; erstes (also letztes ;) ) entfernen, beim Entwickeln
;(setopt compilation-error-regexp-alist-alist (cdr compilation-error-regexp-alist-alist))
