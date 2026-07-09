;;; straight_versions.el --- Inspect straight.el package versions -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Two workflows for keeping an eye on straight.el-managed packages:
;;
;; 1. Compare two frozen lockfiles under straight/versions/
;;      M-x my-straight-compare-lockfiles
;;
;;    Typical flow:
;;      M-x straight-freeze-versions
;;        -> writes straight/versions/default.el
;;      cp straight/versions/default.el straight/versions/default.el-YYYY-MM-DD
;;      ... time passes, more freezes ...
;;      M-x my-straight-compare-lockfiles OLD NEW
;;
;;    The Org buffer shows added / removed / changed packages, and for
;;    every changed package a `git log --oneline OLD..NEW' run inside
;;    the corresponding straight/repos/<pkg>/ checkout.
;;
;; 2. See what `straight-pull-all' would pull, without pulling
;;      M-x my-straight-fetch-and-diff        ; fetches, then diffs
;;      C-u M-x my-straight-fetch-and-diff    ; skip fetch, just diff
;;
;;    For each straight repo whose HEAD is on a branch, `git fetch
;;    origin <branch>' is run.  If the branch's changelog file has
;;    diffs against origin, or origin has commits HEAD doesn't, the
;;    repo is included in the Org report.  Detached HEADs are skipped
;;    silently (straight often pins revisions).
;;
;; Refresh recipe repositories (MELPA / GNU ELPA mirrors etc.):
;;      M-x straight-pull-recipe-repositories   ; restart Emacs afterwards
;;
;; How to upgrade all packages?
;;
;;   1. Preview what would be pulled:
;;        M-x my-straight-fetch-and-diff
;;   2. Snapshot the current lockfile before upgrading:
;;        cp straight/versions/default.el \
;;           straight/versions/default.el-YYYY-MM-DD
;;   3. Pull latest commits for every package:
;;        M-x straight-pull-all
;;   4. Rebuild everything (or restart Emacs):
;;        M-x straight-rebuild-all
;;   5. Write the new lockfile:
;;        M-x straight-freeze-versions
;;        -> overwrites straight/versions/default.el
;;   6. Review what actually changed:
;;        M-x my-straight-compare-lockfiles
;;           OLD = default.el-YYYY-MM-DD  (the snapshot from step 2)
;;           NEW = default.el

;;; Code:

(require 'seq)
(require 'subr-x)
(require 'cl-lib)
(require 'pcase)

;;;; Paths

(defun my-straight--repos-dir ()
  "Absolute path to straight/repos/, with trailing slash."
  (expand-file-name "straight/repos/" user-emacs-directory))

(defun my-straight--versions-dir ()
  "Absolute path to straight/versions/, with trailing slash."
  (expand-file-name "straight/versions/" user-emacs-directory))

;;;; Git helpers

(defun my-straight--git (dir &rest args)
  "Run git in DIR with ARGS.
Return cons (EXIT-CODE . OUTPUT-STRING) where OUTPUT-STRING has no
trailing whitespace."
  (with-temp-buffer
    (let* ((default-directory (file-name-as-directory dir))
           (exit (apply #'call-process "git" nil t nil args)))
      (cons exit (string-trim (buffer-string))))))

(defun my-straight--current-branch (dir)
  "Return the current branch name of git repo DIR, or nil if detached / error."
  (pcase-let ((`(,exit . ,out)
               (my-straight--git dir "symbolic-ref" "--short" "--quiet" "HEAD")))
    (and (eq exit 0) (not (string-empty-p out)) out)))

;;;; Lockfile helpers

(defun my-straight--read-lockfile (file)
  "Read straight.el lockfile FILE and return its alist of (NAME . HASH).
Trailing metadata (e.g. `:epsilon') is ignored."
  (with-temp-buffer
    (insert-file-contents file)
    (goto-char (point-min))
    (let ((form (read (current-buffer))))
      (unless (consp form)
        (error "Unexpected lockfile shape in %s" file))
      form)))

(defun my-straight--lockfile-completing-read (prompt &optional default)
  "Read a lockfile name from straight/versions/ with completion.
PROMPT and DEFAULT are passed to `completing-read'."
  (let* ((dir (my-straight--versions-dir))
         (files (and (file-directory-p dir)
                     (seq-filter (lambda (f)
                                   (and (not (string-prefix-p "." f))
                                        (string-match-p "\\`default\\.el" f)))
                                 (directory-files dir)))))
    (unless files
      (error "No lockfiles matching default.el* in %s" dir))
    (completing-read prompt (sort files #'string<) nil t nil nil default)))

;;;; Repo helpers

(defun my-straight--repo-dir-for (pkg)
  "Return absolute path of straight repo for PKG, or nil if not present."
  (let ((d (expand-file-name pkg (my-straight--repos-dir))))
    (and (file-directory-p d) d)))

(defun my-straight--repo-dirs ()
  "Sorted list of absolute paths of all straight repo directories."
  (let ((base (my-straight--repos-dir)))
    (and (file-directory-p base)
         (sort (seq-filter #'file-directory-p
                           (directory-files base t "^[^.]"))
               #'string<))))

(defun my-straight--find-changelog (dir)
  "Return the first changelog file in DIR, or nil.
Matches basenames starting with CHANGELOG, Changelog, changelog, CHANGES,
Changes, or changes."
  (car (seq-filter
        (lambda (f)
          (and (file-regular-p f)
               (string-match-p
                (rx string-start
                    (or "CHANGELOG" "Changelog" "changelog"
                        "CHANGES"   "Changes"   "changes"))
                (file-name-nondirectory f))))
        (directory-files dir t "^[^.]"))))

;;;; Compare two lockfiles

(defun my-straight--classify-lockfile-diff (old new)
  "Return (ADDED REMOVED CHANGED) from lockfile alists OLD and NEW.
ADDED and REMOVED are lists of package names (sorted).
CHANGED is a list of (NAME OLD-HASH NEW-HASH) (sorted by NAME)."
  (let ((old-h (make-hash-table :test 'equal))
        (new-h (make-hash-table :test 'equal))
        added removed changed)
    (dolist (p old) (puthash (car p) (cdr p) old-h))
    (dolist (p new) (puthash (car p) (cdr p) new-h))
    (dolist (name (sort (delete-dups (append (mapcar #'car old)
                                             (mapcar #'car new)))
                        #'string<))
      (let ((o (gethash name old-h))
            (n (gethash name new-h)))
        (cond
         ((and o (null n))  (push name removed))
         ((and n (null o))  (push name added))
         ((not (equal o n)) (push (list name o n) changed)))))
    (list (nreverse added) (nreverse removed) (nreverse changed))))

(defun my-straight--insert-changed-package (name old-hash new-hash)
  "Insert an Org subsection for changed package NAME (OLD-HASH -> NEW-HASH)."
  (insert (format "** %s\n" name))
  (insert (format "- Old :: =%s=\n" old-hash))
  (insert (format "- New :: =%s=\n" new-hash))
  (let ((repo (my-straight--repo-dir-for name)))
    (cond
     ((not repo)
      (insert "- Repo not found under straight/repos/.\n\n"))
     (t
      (pcase-let ((`(,exit . ,log)
                   (my-straight--git repo "log" "--oneline"
                                     (format "%s..%s" old-hash new-hash))))
        (cond
         ((not (eq exit 0))
          (insert (format "- git log %s..%s :: *ERROR* (exit %d) -- one side may be missing locally; try `git fetch' first\n\n"
                          old-hash new-hash exit)))
         ((string-empty-p log)
          ;; Empty forward log: try the reverse in case the branch was rewound.
          (pcase-let ((`(,e2 . ,log2)
                       (my-straight--git repo "log" "--oneline"
                                         (format "%s..%s" new-hash old-hash))))
            (cond
             ((and (eq e2 0) (not (string-empty-p log2)))
              (insert (format "- git log %s..%s (reverse) :: rewound / divergent\n"
                              old-hash new-hash))
              (dolist (line (split-string log2 "\n" t))
                (insert (format "  - %s\n" line)))
              (insert "\n"))
             (t
              (insert (format "- git log %s..%s :: (no direct ancestry)\n\n"
                              old-hash new-hash))))))
         (t
          (dolist (line (split-string log "\n" t))
            (insert (format "- %s\n" line)))
          (insert "\n"))))))))

;;;###autoload
(defun my-straight-compare-lockfiles (old-name new-name)
  "Compare two straight.el lockfiles OLD-NAME and NEW-NAME.
Both are looked up under straight/versions/.  Show added / removed /
changed packages; for each changed package show `git log --oneline
OLD..NEW' inside its straight/repos/<pkg>/ checkout.  Output is an Org
buffer `*straight-lockfile-diff*'."
  (interactive
   (let* ((old (my-straight--lockfile-completing-read "Old lockfile: "))
          (new (my-straight--lockfile-completing-read "New lockfile: "
                                                      "default.el")))
     (list old new)))
  (let* ((dir (my-straight--versions-dir))
         (old-path (expand-file-name old-name dir))
         (new-path (expand-file-name new-name dir))
         (old (my-straight--read-lockfile old-path))
         (new (my-straight--read-lockfile new-path)))
    (pcase-let* ((`(,added ,removed ,changed)
                  (my-straight--classify-lockfile-diff old new))
                 (out-buf (get-buffer-create "*straight-lockfile-diff*")))
      (with-current-buffer out-buf
        (let ((inhibit-read-only t))
          (erase-buffer)
          (org-mode)
          (insert "#+TITLE: straight.el lockfile diff\n\n")
          (insert (format "- Old :: =%s=\n"   old-path))
          (insert (format "- New :: =%s=\n\n" new-path))
          (insert (format "- Added   :: %d\n"   (length added)))
          (insert (format "- Removed :: %d\n"   (length removed)))
          (insert (format "- Changed :: %d\n\n" (length changed)))
          (when added
            (insert "* Added packages\n")
            (dolist (n added) (insert (format "- %s\n" n)))
            (insert "\n"))
          (when removed
            (insert "* Removed packages\n")
            (dolist (n removed) (insert (format "- %s\n" n)))
            (insert "\n"))
          (when changed
            (insert "* Changed packages\n")
            (let ((reporter (make-progress-reporter
                             "Comparing repos... " 0 (length changed)))
                  (i 0))
              (dolist (entry changed)
                (pcase-let ((`(,name ,o ,n) entry))
                  (progress-reporter-update reporter i (format "[%s]" name))
                  (my-straight--insert-changed-package name o n))
                (cl-incf i))
              (progress-reporter-done reporter)))
          (goto-char (point-min))
          (when (fboundp 'outline-hide-sublevels)
            (outline-hide-sublevels 1))))
      (display-buffer out-buf))))

;;;; Fetch and diff each straight repo against its origin

(defun my-straight--changelog-diff (dir branch)
  "Return `git diff HEAD..origin/BRANCH -- <changelog>' for DIR, or nil.
Returns a plist (:file REL-NAME :diff TEXT) if the diff is non-empty."
  (when-let* ((changelog (my-straight--find-changelog dir))
              (rel (file-relative-name changelog dir)))
    (pcase-let ((`(,exit . ,out)
                 (my-straight--git dir "diff"
                                   (format "HEAD..origin/%s" branch)
                                   "--" rel)))
      (and (eq exit 0)
           (not (string-empty-p out))
           (list :file rel :diff out)))))

(defun my-straight--remote-only-commits (dir branch)
  "Return `git log --oneline HEAD..origin/BRANCH' output for DIR, or nil.
Nil if there are no remote-only commits or the command fails."
  (pcase-let ((`(,exit . ,out)
               (my-straight--git dir "log" "--oneline"
                                 (format "HEAD..origin/%s" branch))))
    (and (eq exit 0)
         (not (string-empty-p out))
         out)))

(defun my-straight--insert-repo-section (name branch repo clog log)
  "Insert an Org section for repo NAME on BRANCH in REPO with CLOG or LOG.
CLOG is a plist from `my-straight--changelog-diff' or nil.
LOG is the string output from `my-straight--remote-only-commits' or nil."
  (insert (format "* %s (%s)\n" name branch))
  (insert ":PROPERTIES:\n")
  (insert (format ":DIR:    %s\n" repo))
  (insert (format ":BRANCH: %s\n" branch))
  (insert ":END:\n")
  (cond
   (clog
    (insert (format "** Changelog diff (=%s=)\n" (plist-get clog :file)))
    (insert "#+BEGIN_SRC diff\n")
    (insert (plist-get clog :diff))
    (insert "\n#+END_SRC\n\n"))
   (log
    (insert "** Remote-only commits\n")
    (dolist (line (split-string log "\n" t))
      (insert (format "- %s\n" line)))
    (insert "\n"))))

;;;###autoload
(defun my-straight-fetch-and-diff (&optional no-fetch)
  "For each straight repo, fetch and report what would come in from origin.
With prefix argument or non-nil NO-FETCH, skip `git fetch' and just
inspect the existing origin refs.

Detached-HEAD repos are skipped silently (straight often pins revisions
to a specific SHA).  A repo only appears in the report if it has a
remote-only commit or a diff in its changelog file.

Output goes to Org buffer `*straight-remote-diff*'."
  (interactive "P")
  (let* ((repos (my-straight--repo-dirs))
         (out-buf (get-buffer-create "*straight-remote-diff*"))
         (reporter (make-progress-reporter
                    (if no-fetch
                        "Diffing straight repos... "
                      "Fetching + diffing straight repos... ")
                    0 (length repos)))
         (i 0)
         (detached 0)
         (with-changes 0))
    (unless repos
      (user-error "No straight repos under %s" (my-straight--repos-dir)))
    (with-current-buffer out-buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (org-mode)
        (insert "#+TITLE: straight.el remote diffs\n\n")
        (insert (format "- Repos scanned :: %d\n" (length repos)))
        (insert (format "- Fetch         :: %s\n"
                        (if no-fetch "skipped" "yes")))
        ;; Placeholder lines; filled in at the end so we can report totals.
        (insert "- Detached HEADs :: ...\n")
        (insert "- With changes   :: ...\n\n")))
    (dolist (repo repos)
      (let* ((name (file-name-nondirectory repo))
             (branch (my-straight--current-branch repo)))
        (progress-reporter-update reporter i (format "[%s]" name))
        (cond
         ((null branch)
          (cl-incf detached))
         (t
          (unless no-fetch
            ;; Discard fetch output; success/failure is reflected by later commands.
            (my-straight--git repo "fetch" "origin" branch))
          (let* ((clog (my-straight--changelog-diff repo branch))
                 (log  (unless clog
                         (my-straight--remote-only-commits repo branch))))
            (when (or clog log)
              (cl-incf with-changes)
              (with-current-buffer out-buf
                (let ((inhibit-read-only t))
                  (goto-char (point-max))
                  (my-straight--insert-repo-section name branch repo clog log))))))))
      (cl-incf i))
    (progress-reporter-done reporter)
    (with-current-buffer out-buf
      (let ((inhibit-read-only t))
        ;; Fill in the placeholders.
        (goto-char (point-min))
        (when (re-search-forward "^- Detached HEADs :: \\.\\.\\.$" nil t)
          (replace-match (format "- Detached HEADs :: %d" detached) t t))
        (goto-char (point-min))
        (when (re-search-forward "^- With changes   :: \\.\\.\\.$" nil t)
          (replace-match (format "- With changes   :: %d" with-changes) t t))
        (goto-char (point-min))
        (when (fboundp 'outline-hide-sublevels)
          (outline-hide-sublevels 1))))
    (display-buffer out-buf)))

(provide 'straight_versions)
;;; straight_versions.el ends here
