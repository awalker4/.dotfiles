(provide 'setup-editing)

;; GROUP: Editing -> Editing Basics
(setq global-mark-ring-max 5000		; increase mark ring to 5000 entries
      mark-ring-max 5000		; increase killl ring to 5000 entries
      mode-require-final-newline t	; add newline to the end fo files
      tab-width 4			; default to display tab as 4 spaces
      )

(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-language-environment "UTF-8")
(prefer-coding-system 'utf-8)

(setq-default indent-tabs-mode nil)
(delete-selection-mode)
(global-set-key (kbd "RET") 'newline-and-indent)

;; GROUP: Editing -> Killing
(setq
 kill-ring-max 5000 ; increase kill-ring capacity
 kill-whole-line t  ; if NIL, kill whole line and move next line up
 )

;; show important white space in diff mode
(add-hook 'diff-mode-hook (lambda()
			    (setq-local whitespace-style
					'(face
					  tabs
					  tab-mark
					  spaces
					  space-mark
					  trailing
					  indentation::space
					  indentation::tab
					  newline
					  newline-mark))
			    (whitespace-mode 1)))
