;;;(eval-after-load 'flycheck (setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc)))

(add-hook 'after-init-hook #'global-flycheck-mode)

