;; M-x straight-pull-recipe-repositories   # restart emacs after that...
;; M-x straight-pull-all           # updates packages, not only the lists...

;; write lockfile: straight-freeze-versions -> straight/versions/default.el, copy to straight/versions/default.el-YYYY-MM-DD

(defun ekr-compare-straight-version-files (file1 file2)
  "Compare two straight.el version files and print keys whose hashes differ.
FILE1 and FILE2 should be paths to the version files."
  (let* ((alist1 (with-temp-buffer
                   (insert-file-contents file1)
                   (read (current-buffer))))
         (alist2 (with-temp-buffer
                   (insert-file-contents file2)
                   (read (current-buffer))))
         (ht1 (make-hash-table :test 'equal))
         (ht2 (make-hash-table :test 'equal)))
    ;; Populate hash tables for fast lookup
    (dolist (pair alist1)
      (puthash (car pair) (cdr pair) ht1))
    (dolist (pair alist2)
      (puthash (car pair) (cdr pair) ht2))
    ;; Collect all keys from both files
    (let ((all-keys (delete-dups (append (mapcar #'car alist1)
                                         (mapcar #'car alist2)))))
      (dolist (key all-keys)
        (let ((val1 (gethash key ht1))
              (val2 (gethash key ht2)))
          (unless (equal val1 val2)
            (message "%s: %s (file1) vs %s (file2)" key val1 val2)))))))

(defun ekr-compare-straight-version-files-with-git-log (file1 file2)
  "Compare two straight.el version files and show git log for differing hashes.
FILE1 and FILE2 should be paths to the version files."
  (let* ((alist1 (with-temp-buffer
                   (insert-file-contents file1)
                   (read (current-buffer))))
         (alist2 (with-temp-buffer
                   (insert-file-contents file2)
                   (read (current-buffer))))
         (ht1 (make-hash-table :test 'equal))
         (ht2 (make-hash-table :test 'equal)))
    ;; Populate hash tables for fast lookup
    (dolist (pair alist1)
      (puthash (car pair) (cdr pair) ht1))
    (dolist (pair alist2)
      (puthash (car pair) (cdr pair) ht2))
    ;; Collect all keys from both files
    (let ((all-keys (delete-dups (append (mapcar #'car alist1)
                                         (mapcar #'car alist2)))))
      (dolist (key all-keys)
        (let ((val1 (gethash key ht1))
              (val2 (gethash key ht2)))
          (unless (equal val1 val2)
            (let* ((repo-dir (expand-file-name (format "straight/repos/%s" key)
                                               user-emacs-directory))
                   (git-log-cmd (format "git log --oneline %s..%s"
                                        (or val2 "") (or val1 ""))))
              (message "Package: %s\n  file1: %s\n  file2: %s"
                       key val1 val2)
              (message "Git log cmd: %s" git-log-cmd)
              (if (file-directory-p repo-dir)
                  (let ((default-directory repo-dir))
                    (message "Git log for %s:" key)
                    (message "%s"
                             (with-temp-buffer
                               (let ((exit-code (call-process-shell-command
                                                 git-log-cmd nil t)))
                                 (if (eq exit-code 0)
                                     (buffer-string)
                                   (format "Error running git log in %s" repo-dir))))))
                (message "Repo directory not found: %s" repo-dir)))))))))

(require 'seq)

(defun ekr--git-current-branch ()
  "Return the current branch name in `default-directory`, or \"\" on error."
  (string-trim
   (with-output-to-string
     (with-current-buffer standard-output
       (call-process "git" nil t nil
                     "rev-parse" "--abbrev-ref" "HEAD")))))

(defun ekr--insert-remote-only-commits-section (branch)
  "Insert the \"Remote-only commits\" section for BRANCH into the current buffer.
Return non-nil iff there are any remote-only commits.
Assumes `default-directory` is a git repo and point is at the place
to insert Org content."
  (insert "** Remote-only commits\n")
  (let ((start (point)))
    (let ((log-exit-code
           (call-process
            "git" nil t nil "log" "--oneline"
            (format "HEAD..origin/%s" branch))))
      (if (eq log-exit-code 0)
          (let ((log-output
                 (buffer-substring-no-properties start (point))))
            (if (string-empty-p (string-trim log-output))
                (progn
                  ;; No commits: clean up this whole section and signal nil
                  (delete-region (save-excursion
                                   (search-backward "** Remote-only commits")
                                   (line-beginning-position))
                                 (point))
                  nil)
              ;; We have commits: reformat region as list and return t
              (delete-region start (point))
              (insert
               (format "- git log HEAD..origin/%s :: success (exit %d)\n\n"
                       branch log-exit-code))
              (dolist (line (split-string log-output "\n" t))
                ;; line is "<sha> <message>" from --oneline
                (insert (format "  - %s\n" line)))
              (insert "\n")
              (message "DEBUG: ekr--insert-remote-only-commits-section: found remote-only commits for branch %s"
                       branch)
              t))
        ;; log failed: keep error info, but signal nil so caller
        ;; can still decide what to do.
        (delete-region start (point))
        (insert
         (format "- git log HEAD..origin/%s :: *ERROR* (exit %d)\n\n"
                 branch log-exit-code))
        nil))))

(defun ekr--get-changelog-diff (branch)
  "Return plist describing the changelog diff for BRANCH, or nil.

The return value is either nil (no relevant changelog diff)
or a plist of the form:

  (:file REL-NAME :diff DIFF-TEXT)

where REL-NAME is the changelog file name relative to
`default-directory` and DIFF-TEXT is the cleaned diff text.

A changelog file is any regular file in `default-directory`
whose basename starts with one of:

  CHANGELOG, Changelog, changelog, CHANGES, Changes, changes

If `git diff HEAD..origin/BRANCH -- <file>` has non-empty output,
the diff is cleaned line-by-line and returned. Otherwise nil is
returned.

Assumes `default-directory` is a git repo."
  (let* ((changelog-file
          (car
           (seq-filter
            (lambda (f)
              (and (file-regular-p f)
                   (string-match-p
                    (rx string-start
                        (or "CHANGELOG" "Changelog" "changelog"
                            "CHANGES" "Changes" "changes"))
                    (file-name-nondirectory f))))
            (directory-files default-directory t "^[^.].*")))))
    (when changelog-file
      (with-temp-buffer
        (let* ((rel-name (file-relative-name changelog-file default-directory))
               (diff-exit-code
                (call-process
                 "git" nil t nil "diff"
                 (format "HEAD..origin/%s" branch)
                 "--" rel-name)))
          (when (eq diff-exit-code 0)
            (let ((raw (buffer-substring-no-properties (point-min) (point-max))))
              (unless (string-empty-p (string-trim raw))
                (let* ((lines (split-string raw "\n"))
                       (cleaned
                        (string-join
                         (mapcar
                          (lambda (line)
                            (let ((clean-line line))
                              ;; 1. Drop leading two spaces from diff hunks.
                              (when (string-prefix-p "  " clean-line)
                                (setq clean-line (substring clean-line 2)))
                              ;; 2. Strip leading '+' (added lines).
                              (when (string-match "^\\s-*\\+\\s-*" clean-line)
                                (setq clean-line (replace-match "" t t clean-line)))
                              ;; 3. If line now starts with '*', demote it by adding '**'.
                              (when (string-prefix-p "*" clean-line)
                                (setq clean-line (concat "**" clean-line)))
                              clean-line))
                          lines)
                         "\n")))
                  (list :file rel-name :diff cleaned))))))))))

(defun ekr--insert-changelog-diff-section (branch)
  "Compute and insert the changelog diff section for BRANCH into current buffer.
Returns non-nil iff a changelog diff was found and inserted.

Assumes `default-directory` is a git repo and point is where
the Org section should be inserted."
  (let ((result (ekr--get-changelog-diff branch)))
    (when result
      (let ((rel-name (plist-get result :file))
            (diff-text (plist-get result :diff)))
        (insert "** Changelog diff\n")
        (insert (format "- File: =%s=\n" rel-name))
        (insert (format "- git diff HEAD..origin/%s -- %s :: success\n\n"
                        branch rel-name))
        (dolist (line (split-string diff-text "\n"))
          (insert line "\n"))
        (insert "\n")
        (message "DEBUG: ekr--insert-changelog-diff-section: real diff for branch %s"
                 branch)
        t))))

(defun ekr-fetch-remote-main-branches-and-diff-with-local-main-branch ()
  "For each straight repo, fetch the remote for the current branch and
show commits that are on origin but not on the local branch, in an Org buffer."
  (interactive)
  (message "DEBUG: entering ekr-fetch-remote-main-branches-and-diff-with-local-main-branch")
  (let* ((buf-name "*straight-remote-diff*")
         (output-buf (get-buffer-create buf-name))
         (repos-dir (expand-file-name "straight/repos/" user-emacs-directory)))
    (message "DEBUG: let* bound buf-name=%S output-buf=%S repos-dir=%S"
             buf-name output-buf repos-dir)
    (with-current-buffer output-buf
      (message "DEBUG: inside with-current-buffer, buffer=%S" (current-buffer))
      (let ((inhibit-read-only t))
        (erase-buffer)
        (org-mode)
        (insert "#+TITLE: straight.el remote diffs\n\n")
        (insert (format "Checking straight repos under: =%s=\n\n" repos-dir))
        (message "DEBUG: starting repo iteration over %s" repos-dir)

        (dolist (repo (sort (directory-files repos-dir t "^[^.]")
                            #'string<))
          (message "DEBUG: considering repo path=%S" repo)
          (when (file-directory-p repo)
            (message "DEBUG: repo %S is a directory, processing" repo)
            (let* ((default-directory repo)
                   (repo-name (file-name-nondirectory repo))
                   (branch (ekr--git-current-branch))
                   (repo-buf (generate-new-buffer
                              (format " *ekr-straight-repo-%s*" repo-name)))
                   (had-changes nil))
              (message "DEBUG: [%s] initial had-changes=%S" repo-name had-changes)
              (unwind-protect
                  (with-current-buffer repo-buf
                    (org-mode)
                    ;; Heading for this repo (we assume a normal branch)
                    (insert
                     (format "* %s (%s)\n:PROPERTIES:\n:DIR: %s\n:BRANCH: %s\n:END:\n"
                             repo-name branch repo branch))
                    ;; --- Fetch section ---
                    (insert "** Fetch\n")
                    (let ((fetch-exit-code
                           (call-process "git" nil nil nil "fetch" "origin" branch)))
                      (if (eq fetch-exit-code 0)
                          (insert
                           (format "- git fetch origin %s :: success (exit %d)\n"
                                   branch fetch-exit-code))
                        (insert
                         (format "- git fetch origin %s :: *ERROR* (exit %d)\n\n"
                                 branch fetch-exit-code)))
                      ;; Only compute further sections if fetch succeeded.
                      (when (eq fetch-exit-code 0)
                        (message "DEBUG: [%s] before diff helpers, had-changes=%S"
                                 repo-name had-changes)
                        (let ((changelog-had-diff
                               (ekr--insert-changelog-diff-section branch)))
                          (message "DEBUG: [%s] changelog-had-diff=%S (had-changes was %S)"
                                   repo-name changelog-had-diff had-changes)
                          (when changelog-had-diff
                            (message "DEBUG: had-changes set by changelog diff for %s (branch %s)\n%s"
                                     repo-name branch changelog-had-diff))
                          (setq had-changes (or had-changes changelog-had-diff))
                          (message "DEBUG: [%s] after changelog, had-changes=%S"
                                   repo-name had-changes)
                          (unless changelog-had-diff
                            (let ((commits-had-diff
                                   (ekr--insert-remote-only-commits-section branch)))
                              (message "DEBUG: [%s] commits-had-diff=%S (had-changes was %S)"
                                       repo-name commits-had-diff had-changes)
                              (when commits-had-diff
                                (message "DEBUG: had-changes set by remote-only commits for %s (branch %s)"
                                         repo-name branch))
                              (setq had-changes (or had-changes commits-had-diff))
                              (message "DEBUG: [%s] after commits, had-changes=%S"
                                       repo-name had-changes)))))))
                (message "DEBUG: [%s] before append, final had-changes=%S"
                         repo-name had-changes)
                ;; After building repo-buf, decide whether to copy it.
                (when had-changes
                  (message "DEBUG: repo %s had changes, appending to main buffer" repo-name)
                  (with-current-buffer output-buf
                    (goto-char (point-max))
                    (insert-buffer-substring repo-buf)))
                (when (buffer-live-p repo-buf)
                  (kill-buffer repo-buf)))))))
        (goto-char (point-min))
        (message "DEBUG: before outline-hide-sublevels, buffer contents length=%d"
                 (buffer-size))
        (when (fboundp 'outline-hide-sublevels)
          (outline-hide-sublevels 1)))
      ;; end with-current-buffer
      (message "DEBUG: about to display-buffer, output-buf=%S (live=%S)"
               output-buf (buffer-live-p output-buf)))
    ;; Call display-buffer after the with-current-buffer form, so the
    ;; number of closing parens matches the bindings above.
    (display-buffer output-buf)))

;; Usage example (output in *Messages*):
;; (ekr-compare-straight-version-files-with-git-log "straight/versions/default.el"
;;                                                  "straight/versions/default.el-2025-07-16")
