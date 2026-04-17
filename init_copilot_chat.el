;;; init_copilot_chat.el --- Copilot Chat configuration
;;; -*- lexical-binding: t; -*-

(my-banner "Copilot, ...")

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
        (let ((hook (intern (concat (symbol-name mode) "-mode-hook"))))
          (add-hook hook 'copilot-mode)
          ;; (add-hook hook 'highlight-indent-guides-mode))
          ))

      ;; (add-hook 'after-change-major-mode-hook 'copilot-turn-on-unless-buffer-read-only)


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

;;; copilot-chat afterwards, or it will get some functions overridden by copilot.el

(my-banner "copilot-chat")

(use-package copilot-chat
  :straight (:host github :repo "chep/copilot-chat.el" :files ("*.el"))
  :after (request org markdown-mode shell-maker))

(global-set-key (kbd "Ì") 'copilot-chat-display)

(define-prefix-command 'copilot-chat-prefix)

(global-set-key (kbd (if (eq system-type 'gnu/linux) "s-c" "ç")) 'copilot-chat-prefix)

(define-key copilot-chat-prefix (if (eq system-type 'gnu/linux) "s-c" "ç") 'copilot-chat-transient)
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

; simplify the instances - only use one
(with-eval-after-load 'copilot-chat
  (defun copilot-chat--ask-for-instance ()
    "Reuse an existing Copilot Chat instance, or create one if none exist."
    (if copilot-chat--instances
        (copilot-chat--choose-instance)
      (copilot-chat--create-instance))))

; always use home dir
(with-eval-after-load 'copilot-chat
  (defun copilot-chat--create-instance ()
    "Create a new copilot chat instance for your home directory."
    (let* ((directory (expand-file-name "~")) ; Always use home directory
           (found (copilot-chat--find-instance directory))
           (instance
            (if found
                found
              (copilot-chat--create directory))))
      (unless found
        (push instance copilot-chat--instances))
      instance)))

(defun my-copilot-chat-ask-and-return-string (prompt)
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

;;; commit messages

(defun my-try-insert-branch-name (branch-name reps commit-buffer)
  "Wait until copilot has finished (by busy waiting on the *Messages* buffer) and insert the BRANCH-NAME into the commit message.  REPS is the countdown to timeout."

  (if (< reps 0)
      (message "my-try-insert-branch-name: did not find completion message, giving up.")
    (progn
      ;; extract the last 3 lines from the buffer *Messages* into a string
      (let ((finished nil))
        (progn
          ;; check if copilot is finished
          (with-current-buffer commit-buffer
            (if (not (string-match "Generating commit message"
                                   (buffer-substring-no-properties (point-min) (point-max))))
                (setq finished t)))
          (if finished
              (with-current-buffer commit-buffer
                ;; insert the branch name at the beginning of the buffer
                (goto-char (point-min))
                (insert (concat branch-name ": ")))
            (progn
              (run-at-time "0.5 sec" nil 'my-try-insert-branch-name branch-name (- reps 1) commit-buffer))))))))

(defun my-insert-commit-msg ()
  "Run copilot to figure out a commit message.  Make sure the branch name is included."
  (copilot-mode -1)
  (let ((branch-name (string-trim (shell-command-to-string "git rev-parse --abbrev-ref HEAD 2>/dev/null"))))
    (message "flush 1")
    (message "flush 2")
    (message "flush 3")
    (message "flush 4")
    ;; (copilot-chat-insert-commit-message)   ; has a 1 sec timer
    (copilot-chat-insert-commit-message-when-ready)   ; is async with aio
    (run-at-time "1 sec" nil 'my-try-insert-branch-name branch-name 20 (current-buffer))))

(defun my-run-good-auto ()
  "Execute a Good-Auto query."
  (interactive)
  ;; if the current buffer is the git commit message, then run copilot-chat-insert-commit-message
  (if (string-prefix-p "COMMIT_EDITMSG" (buffer-name))
      (my-insert-commit-msg)
    (error "This function is deprecated and only to be used in COMMIT_EDITMSG.")))

; © is option-g
(global-set-key (kbd "©") 'my-run-good-auto)

; aidermacs

(my-banner "Aider, aidermacs...")

(global-set-key (kbd "C-c a") 'aidermacs-transient-menu)

(defun my-switch-to-aidermacs-buffer ()
  "Display a list of *aidermacs:* buffers and switch to the selected one."
  (interactive)
  (let ((bufs (cl-remove-if-not
               (lambda (b) (string-prefix-p "*aidermacs:" (buffer-name b)))
               (buffer-list))))
    (if (null bufs)
        (message "No *aidermacs:* buffers found.")
      (let ((name (completing-read "Switch to aidermacs buffer: "
                                   (mapcar #'buffer-name bufs)
                                   nil t)))
        (switch-to-buffer name)))))

(with-eval-after-load 'aidermacs
  (transient-append-suffix 'aidermacs-transient-menu '(0 0 -1)
    '("C-a" "Switch Aidermacs Buffer" my-switch-to-aidermacs-buffer)))

(defun my-next-aidermacs-buffer ()
  "Switch to the next *aidermacs:* buffer, cycling through them.
If the current buffer is not an aidermacs buffer, switch to the first one."
  (interactive)
  (let ((bufs (cl-remove-if-not
               (lambda (b) (string-prefix-p "*aidermacs:" (buffer-name b)))
               (buffer-list))))
    (if (null bufs)
        (message "No *aidermacs:* buffers found.")
      (let* ((cur (current-buffer))
             (pos (cl-position cur bufs))
             (next (if pos
                       (nth (mod (1+ pos) (length bufs)) bufs)
                     (car bufs))))
        (switch-to-buffer next)))))

(with-eval-after-load 'aidermacs
  (transient-append-suffix 'aidermacs-transient-menu '(0 0 -1)
    '("TAB" "Next Aidermacs Buffer" my-next-aidermacs-buffer)))

(defun my-akp-unclone ()
  "If the current buffer is under ~/Documents/src/akp/AKPPUB-NNNN/, save all buffers and run akp-unclone."
  (interactive)
  (let* ((buf-file (or (buffer-file-name) default-directory))
         (expanded (expand-file-name buf-file))
         (akp-root (expand-file-name "~/Documents/src/akp/"))
         (rel (and (string-prefix-p akp-root expanded)
                   (substring expanded (length akp-root)))))
    (unless rel
      (user-error "Current buffer is not under %s" akp-root))
    (let* ((ticket (car (split-string rel "/")))
           (ticket-dir (expand-file-name ticket akp-root)))
      (unless (and ticket (string-match-p "\\`AKPPUB-[0-9]+\\'" ticket))
        (user-error "Directory %s does not match AKPPUB-NNNN pattern" ticket))
      (save-some-buffers t)
      (if (file-directory-p ticket-dir)
          (progn
            (message "Running akp-unclone %s ..." ticket)
            (let* ((output-buf (get-buffer-create "*akp-unclone*"))
                   (ret (progn
                          (with-current-buffer output-buf (erase-buffer))
                          (call-process "~/bin/akp-unclone" nil output-buf nil ticket))))
              (unless (= ret 0)
                (display-buffer output-buf)
                (user-error "akp-unclone %s failed with exit code %d" ticket ret)))
            (when (file-directory-p ticket-dir)
              (user-error "akp-unclone succeeded but %s still exists" ticket-dir)))
        (message "Directory %s does not exist, skipping akp-unclone." ticket-dir))
      ;; Kill all buffers whose file is under the removed tree
      (let ((killed 0))
        (dolist (buf (buffer-list))
          (let ((f (or (buffer-file-name buf)
                       (buffer-local-value 'default-directory buf))))
            (when (and f (string-prefix-p ticket-dir (expand-file-name f)))
              (message "  killing buffer: %s" (buffer-name buf))
              (kill-buffer buf)
              (cl-incf killed))))
        (message "akp-unclone %s done, killed %d buffer(s)." ticket killed)))))

(with-eval-after-load 'aidermacs
  (transient-append-suffix 'aidermacs-transient-menu '(0 0 -1)
    '("C-k" "AKP Unclone" my-akp-unclone)))


;; aider-ce
;; ========
;;
;; uv tool install --python python3.12 aider-ce
;; Installed 5 executables: aider-ce, ce, ce-cli, ce.cli, cecli
;;
;; uv tool upgrade --python python3.12 aider-ce
;;
;; Github Copilot key
;; ==================
;;
;; https://aider.chat/docs/llms/github.html (OPENAI_API_BASE,
;; OPENAI_API_KEY in .bashrc) But then we get
;; "forbidden". Instead: set aidermacs-default-model to
;; github_copilot/gpt-4.1, aider will prompt for Github login,
;; then set those variables from those files (see below)
;;
;; https://www.reddit.com/r/ChatGPTCoding/comments/1lk2mvv/aider_anyone_have_success_with_gh_copilot_oauth/
;; https://github.com/Aider-AI/aider/issues/2227#issuecomment-3141551921
;;
;; Also ~/.aider.conf.yml for model defaults etc.
;;
;; Test with
;;
;;   curl -s $OPENAI_API_BASE/models -H "Authorization: Bearer $OPENAI_API_KEY" | jq -r '.data[].id'
;;
;;       gpt-4.1
;;       gpt-5-mini
;;       gpt-5
;;       gpt-3.5-turbo
;;       gpt-3.5-turbo-0613
;;       gpt-4o-mini
;;       gpt-4o-mini-2024-07-18
;;       gpt-4
;;       gpt-4-0613
;;       gpt-4-0125-preview
;;       gpt-4o
;;       gpt-4o-2024-11-20
;;       gpt-4o-2024-05-13
;;       gpt-4-o-preview
;;       gpt-4o-2024-08-06
;;       grok-code-fast-1
;;       text-embedding-ada-002
;;       text-embedding-3-small
;;       text-embedding-3-small-inference
;;       claude-sonnet-4
;;       claude-sonnet-4.5
;;       gemini-2.5-pro
;;       gpt-4.1-2025-04-14
;;
;;
;; github_copilot Setup
;; ====================
;;
;; github_copilot/gpt-5 seems not to work (doesn't have editor auth):
;;   litellm.BadRequestError: Github_copilotException - bad request: missing
;;   Editor-Version header for IDE auth
;;
;; Experiment with https://github.com/Aider-AI/aider/issues/2227#issuecomment-3141551921 :
;;
;; * OK: Current aider seems to use litellm==1.75.0
;;
;; * Create ~/.aider.model.settings.yml:
;;
;;   - name: github_copilot/gpt-4.1
;;     # edit_format: diff
;;     extra_params:
;;       max_tokens: 80000
;;       extra_headers:
;;         User-Agent: GithubCopilot/1.155.0
;;         Editor-Plugin-Version: copilot/1.155.0
;;         Editor-Version: vscode/1.85.1
;;         Copilot-Integration-Id: vscode-chat
;;
;; * But this is enough as default:
;;
;;   - name: aider/extra_params
;;     extra_params:
;;       extra_headers:
;;         Editor-Version: vscode/42
;;
;; * Works again, this fixes the missing editor header.
;;
;; Quota
;; =====
;;
;; Github Copilot has a "premium" quota (-> Web GUI, right at the top).
;;
;; Remove confusing/old tokens, re-login everything
;; ================================================
;;
;;   find ~/.config -type f | xargs grep -l ghu_ |grep -v \~ | grep -v \.old
;;   -rw-r--r--@ 1 xx  staff  41 Jun  5  2025 .config/copilot-chat/github-token
;;     ghu_...
;;   -rw-r--r--@ 1 xx  staff  90 Jun  4  2025 .config/github-copilot/hosts.json
;;     {"github.com":{"user":"...","oauth_token":"ghu_..."}}
;;   -rw-r--r--@ 1 xx  staff  40 Dec 10 14:14 .config/litellm/github_copilot/access-token
;;     ghu_...

;;
;; mv ~/.config/copilot-chat ~/.config/copilot-chat.old
;; mv ~/.config/github-copilot ~/.config/github-copilot.old
;; mv ~/.config/litellm/github_copilot ~/.config/litellm/github_copilot.old
;;
;; unset OPENAI_API_BASE   # not needed for aider with gpt-4/gpt-5, but later with gpt-5-codex
;; unset OPENAI_API_KEY
;; aider
;;  # github login procedure -> everything works, info stored in ~/.config/litellm/github_copilot
;;
;; M-x aidermacs works
;;
;; M-x copilot-chat does its own github_copilot login; works
;;
;; M-x copilot-login does its own login, works.
;;
;; The above 3 files are recreated.
;;
;; Get gpt-5-codex to work
;; =======================
;;
;; .bash_profile ->
;;
;; export OPENAI_API_BASE=$(jq -r '.endpoints.api' ~/.config/litellm/github_copilot/api-key.json)
;; -> https://api.business.githubcopilot.com
;; export OPENAI_API_KEY=$(cat ~/.config/litellm/github_copilot/access-token)

(setopt aidermacs-program (expand-file-name "emacs_cecli.sh" user-emacs-directory))

(defun my-get-aider-main-model (&optional interactive)
  "Extract the GPT model name from the ~/.emacs.d/aider.conf.yml file."
  (interactive)
    (let* ((aider-config-file (expand-file-name "aider.conf.yml" user-emacs-directory))
           (model-line (with-temp-buffer
                         (insert-file-contents aider-config-file)
                         (goto-char (point-min))
                         (if (re-search-forward "^model:[ \t]*\\(.*\\)$" nil t)
                             (match-string 1)
                             nil))))
        (if model-line
            (let* ((model-name (string-trim model-line))
                   (slash-pos (string-match "/" model-name))
                   (model-short-name (if slash-pos
                                         (substring model-name (1+ slash-pos))
                                       model-name)))
              (progn
                (message "Aider GPT model: %s" model-short-name)
                model-short-name))
          (message "No model found in %s" aider-config-file))))

(defun my-get-aider-editor-model (&optional interactive)
  "Extract the GPT model name from the ~/.emacs.d/aider.conf.yml file."
  (interactive)
    (let* ((aider-config-file (expand-file-name "aider.conf.yml" user-emacs-directory))
           (model-line (with-temp-buffer
                         (insert-file-contents aider-config-file)
                         (goto-char (point-min))
                         (if (re-search-forward "^editor-model:[ \t]*\\(.*\\)$" nil t)
                             (match-string 1)
                             nil))))
        (if model-line
            (let* ((model-name (string-trim model-line))
                   (slash-pos (string-match "/" model-name))
                   (model-short-name (if slash-pos
                                         (substring model-name (1+ slash-pos))
                                       model-name)))
              (progn
                (message "Aider GPT model: %s" model-short-name)
                model-short-name))
          (message "No model found in %s" aider-config-file))))

(progn
  (setopt copilot-lsp-settings `(:copilot.model ,(my-get-aider-main-model)))
  (setopt copilot-chat-default-model (my-get-aider-main-model))
  (setopt copilot-chat-commit-model (my-get-aider-editor-model)))
