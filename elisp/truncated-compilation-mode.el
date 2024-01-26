; https://gist.githubusercontent.com/spacebat/994f42291352397bfb48945b3a48f4ac/raw/a868121a8f5d8dff9766cec9d1e5a5d5bcb66db1/truncated-compilation-mode.el

(defvar truncated-compilation-line-limit 300)

(defvar truncated-compilation-line-trailer "…")

;; TODO: convert this from a post filter hook to advice on a
;; configured set of filter functions to prevent the insertion of text
;; constituting overlong lines in the first place

;;;###autoload
(defun truncate-compilation-long-lines ()
  "Emacs doesn't cope well with extremely long
lines. Unfortunately some processes like grep, ack, ag, rg are
prone to matching minified files or otherwise extremely long
lines. Once Added to compilation-filter-hook, this function
truncates lines returned by the compilation process."
  (cl-flet ((truncate-line (pos)
                           (let* ((beginning (progn (beginning-of-line) (point)))
                                  (ending (progn (end-of-line) (point)))
                                  (length (- ending beginning))
                                  (excess (max (- length truncated-compilation-line-limit))))
                             (when (plusp excess)
                               (delete-region (- ending excess) ending)
                               (when truncated-compilation-line-trailer
                                 (insert truncated-compilation-line-trailer))))))
    (goto-char compilation-filter-start)
    (cl-loop do (truncate-line (point))
             (forward-line 1)
             (end-of-line)
             (when (= (point) (point-max))
               (cl-return)))))

;;;###autoload
(define-minor-mode truncated-compilation-mode
  "Limit the length of lines in compilation buffers"
  :global t
  (cond
   (truncated-compilation-mode
    (add-hook 'compilation-filter-hook 'truncate-compilation-long-lines))
   (t
    (remove-hook 'compilation-filter-hook 'truncate-compilation-long-lines))))

(provide 'truncated-compilation-mode)
