;;; init_copilot_chat.el --- Copilot Chat configuration
;;; -*- lexical-binding: t; -*-

(ekr-banner "copilot-chat")

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
