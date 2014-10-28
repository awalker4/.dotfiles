;;; Personal functions
;;; Mostly copied from this guy - https://github.com/hans/dotfiles

;; Thanks http://thorne.posterous.com/load-a-directory-full-of-emacs-lisp-files-in
(defvar file-loadable-regexp
  (replace-regexp-in-string "\\." "\\\\."
			    (let (string
				  ;; A list of extensions which Emacs can load
				  (suffix-list (get-load-suffixes)))
			      (concat (car suffix-list) "$"
				      (dolist (extension (cdr suffix-list) string)
					(setq string (concat "\\|" extension "$" string))))))
  "Regular expression that matches any file name with a file extension returned by `get-load-suffixes'.")

(defun file-loadable-p (file)
  "Return t if FILE is an Emacs lisp file.
More precisely, return t if the file name extension matches
`file-loadable-regexp'"
  (string-match file-loadable-regexp file))

(defun load-dir (dir)
  "Load all files in a directory (non-recursive)."
  (let ((files
	 ;; Load a list .el and .elc scripts in the given directory
	 (directory-files (expand-file-name dir) t "^[^.].*\.elc?$")))
    (dolist (file files)
      (when
          (and
           (file-loadable-p file)

           ;; Only load real files, not symlinks
           (file-exists-p file)

           ;; Don't try to load directories matching the regex
           (not (file-directory-p file))

           ;; Don't load the .el version of a file if there exists an .elc version
           ;; as well.
           (not (and (string= (file-name-extension file t) ".el")
                     (member
                      (concat (file-name-sans-extension file) ".elc")
                      files))))
        (load file)))))

;; Quickly jump back and forth between matching parens/brackets
(defun match-paren (arg)
  "Go to the matching parenthesis if on parenthesis."
  (interactive "p")
  (cond ((looking-at "\\s\(") (forward-list 1) (backward-char 1))
        ((looking-at "\\s\)") (forward-char 1) (backward-list 1))))

;; Make the whole buffer pretty and consistent
(defun iwb()
  "Indent Whole Buffer"
  (interactive)
  (delete-trailing-whitespace)
  (indent-region (point-min) (point-max) nil)
  (untabify (point-min) (point-max)))

(defun delete-window-replacement (&optional p)
  "Kill current window.  If called with PREFIX, kill the buffer too."
  (interactive "P")
  (if p
      (kill-buffer nil))
  (delete-window))

(defun delete-other-windows-replacement (&optional p)
  "Make the selected window fill its frame.  If called with PREFIX,
kill all other visible buffers."
  (interactive "P")
  (if p
      (dolist (window (window-list))
        (unless (equal (window-buffer window) (current-buffer))
          (kill-buffer (window-buffer window)))))
  (delete-other-windows))

(defun lorem ()
  (interactive)
  (insert "Lorem ipsum dolor sit amet, consectetuer adipiscing elit. Praesent libero orci, auctor sed, faucibus vestibulum, gravida vitae, arcu. Nunc posuere. Suspendisse potenti. Praesent in arcu ac nisl ultricies ultricies. Fusce eros. Sed pulvinar vehicula ante. Maecenas urna dolor, egestas vel, tristique et, porta eu, leo. Curabitur vitae sem eget arcu laoreet vulputate. Cras orci neque, faucibus et, rhoncus ac, venenatis ac, magna. Aenean eu lacus. Aliquam luctus facilisis augue. Nullam fringilla consectetuer sapien. Aenean neque augue, bibendum a, feugiat id, lobortis vel, nunc. Suspendisse in nibh quis erat condimentum pretium. Vestibulum tempor odio et leo. Sed sodales vestibulum justo. Cras convallis pellentesque augue. In eu magna. In pede turpis, feugiat pulvinar, sodales eget, bibendum consectetuer, magna. Pellentesque vitae augue."))

;; Use the text around point as a cue what it is that we want from the
;; editor. Allowance has to be made for the case that point is at the
;; edge of a buffer.
(defun indent-or-expand (arg)
  "Either indent according to mode, or expand the word preceding
point."
  (interactive "*P")
  (if (and
       (or (bobp) (= ?w (char-syntax (char-before))))
       (or (eobp) (not (= ?w (char-syntax (char-after))))))
      (dabbrev-expand arg)
    (indent-according-to-mode)))

;; Normally, killing the newline between indented lines doesn’t remove any extra
;; spaces caused by indentation. That is, placing the cursor (symbolized by [])
;; at
;;
;;         AAAAAAAAAA[]
;;         AAAAAAAAAA
;; and pressing C-k (bound to kill-line) results in
;;
;;         AAAAAAAAAA[]        AAAAAAAAAA
;; when it might be more desirable to have
;;
;;         AAAAAAAAAA[]AAAAAAAAAA
;; which is what would have happened if the lines were not indented.
;;
;; http://www.emacswiki.org/emacs/AutoIndentation
;;
;; This function is bound to C-k to replace the default kill-line command: see
;; bindings.el.
(defun kill-and-join-forward (&optional arg)
      (interactive "P")
      (if (and (eolp) (not (bolp)))
          (progn (forward-char 1)
                 (just-one-space 0)
                 (backward-char 1)
                 (kill-line arg))
        (kill-line arg)))

(defun comment-or-uncomment-line (&optional lines)
  "Comment current line. Argument gives the number of lines forward to comment"
  (interactive "P")
  (comment-or-uncomment-region
   (line-beginning-position)
   (line-end-position lines)))

(defun comment-or-uncomment-region-or-line (&optional lines)
  "If a region is marked, comment or uncomment it. Otherwise comment/uncomment
   the current line."
  (interactive "P")
  (if mark-active
      (if (< (mark) (point))
          (comment-or-uncomment-region (mark) (point))
        (comment-or-uncomment-region (point) (mark)))
    (comment-or-uncomment-line lines)))

(defun delete-this-file ()
  "Delete the file displayed in the current buffer."
  (interactive)
  (delete-file (buffer-file-name))
  (kill-this-buffer))

(defun goto-line-with-feedback ()
  "Show line numbers temporarily, while prompting for the line number input"
  (interactive)
  (unwind-protect
      (progn
        (linum-mode 1)
        (call-interactively 'goto-line))
    (linum-mode -1)))

(defun open-line-below ()
  (interactive)
  (end-of-line)
  (newline)
  (indent-for-tab-command))

(defun open-line-above ()
  (interactive)
  (beginning-of-line)
  (newline)
  (forward-line -1)
  (indent-for-tab-command))

;; Toggle between single and double quotes in the string at point.
;; Thanks https://github.com/magnars/.emacs.d/blob/master/defuns/editing-defuns.el
(defun current-quotes-char ()
  (nth 3 (syntax-ppss)))

(defalias 'point-is-in-string-p 'current-quotes-char)

(defun move-point-forward-out-of-string ()
  (while (point-is-in-string-p) (forward-char)))

(defun move-point-backward-out-of-string ()
  (while (point-is-in-string-p) (backward-char)))

(defun alternate-quotes-char ()
  (if (eq ?' (current-quotes-char)) ?\" ?'))

(defun toggle-quotes ()
  (interactive)
  (if (point-is-in-string-p)
      (let ((old-quotes (char-to-string (current-quotes-char)))
            (new-quotes (char-to-string (alternate-quotes-char)))
            (start (make-marker))
            (end (make-marker)))
        (save-excursion
          (move-point-forward-out-of-string)
          (backward-delete-char 1)
          (set-marker end (point))
          (insert new-quotes)
          (move-point-backward-out-of-string)
          (delete-char 1)
          (insert new-quotes)
          (set-marker start (point))
          (replace-string new-quotes (concat "\\" new-quotes) nil start end)
          (replace-string (concat "\\" old-quotes) old-quotes nil start end)))
    (error "Point is not in a string")))

(defun recompile-config ()
  "Recompile Emacs config files"
  (interactive)

  (let* ((avoid '(".." "auto-save-list" "backups" "elpa"))
	 (files (directory-files-and-attributes config-directory))
         (targets (--filter (and (not (-contains? avoid (car it)))
				 (or
				  ;; is valid directory
				  (eq t (cadr it))

				  ;; is .el file
				  (s-ends-with? ".el" (car it)))) ;; .el file
                            files))
	 (clean (--map (cons (expand-file-name (car it) config-directory)
			     (cadr it))
		       targets)))

    (--each clean
      (if (eq t (cdr it))
	  (byte-recompile-directory (car it))
	(byte-recompile-file (car it))))))

;; Thanks http://www.masteringemacs.org/articles/2010/12/22/fixing-mark-commands-transient-mark-mode/
(defun push-mark-no-activate ()
  "Pushes `point' to `mark-ring' and does not activate the
  region. Equivalent to \\[set-mark-command] when
  \\[transient-mark-mode] is disabled."
  (interactive)
  (push-mark (point) t nil)
  (message "Pushed mark to ring"))

(defun jump-to-mark ()
  "Jumps to the local mark, respecting the `mark-ring' order.
  This is the same as using \\[set-mark-command] with the prefix
  argument."
  (interactive)
  (set-mark-command 1))

(defun exchange-point-and-mark-no-activate ()
  "Identical to \\[exchange-point-and-mark] but will not activate
  the region."
  (interactive)
  (exchange-point-and-mark)
  (deactivate-mark nil))

(defun toggle-fullscreen ()
  (interactive)
  (set-frame-parameter
   nil 'fullscreen
   (when (not (frame-parameter nil 'fullscreen)) 'fullboth)))

(defun find-file-custom (arg)
  "Try to find a file with Projectile first, and default to general ido."

  (interactive "P")
  (condition-case nil
      (projectile-find-file arg)
    (error (ido-find-file))))

(defun string-starts-with (string prefix)
  "Returns non-nil if string STRING starts with PREFIX, otherwise nil."
  (and (>= (length string) (length prefix))
       (string-equal (substring string 0 (length prefix)) prefix)))

(defadvice display-warning
    (around no-warn-.emacs.d-in-load-path (type message &rest unused) activate)
  "Ignore the warning about the `.emacs.d' directory being in `load-path'."
  (unless (and (eq type 'initialization)
               (string-starts-with message "Your `load-path' seems to contain\nyour `.emacs.d' directory"))
    ad-do-it))
