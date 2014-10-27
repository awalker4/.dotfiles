(provide 'setup-convenience)

; Yes or no prompts are shorter
(fset 'yes-or-no-p 'y-or-n-p)

; Don't prompt to kill running processes
(setq kill-buffer-query-functions
      (remq 'process-kill-buffer-query-function
	    kill-buffer-query-functions))

; Make scripts executable
(add-hook 'after-save-hook
	  'executable-make-buffer-file-executable-if-script-p)
