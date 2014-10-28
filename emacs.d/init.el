;; Add modules path
(add-to-list 'load-path "~/.emacs.d/custom/")

(setq config-directory "~/.emacs.d")

;; Load all packages
(require 'package)

(add-to-list 'package-archives
             '("melpa" . "http://stable.melpa.org/packages/") t)

(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/") t)
(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))

(defvar required-packages
  '( magit yasnippet yasnippet-bundle
    solarized-theme flycheck powerline projectile)
  "Packages which should be installed upon launch")

(dolist (p required-packages)
  (when (not (package-installed-p p))
    (package-install p)))


;; Load modules
(require 'setup-applications)
(require 'setup-communication)
(require 'setup-convenience)
(require 'setup-data)
(require 'setup-development)
(require 'setup-editing)
(require 'setup-environment)
(require 'setup-external)
(require 'setup-faces-and-ui)
(require 'setup-files)
(require 'setup-help)
(require 'setup-programming)
(require 'setup-text)
(require 'setup-local)

(savehist-mode 1)
(setq savehist-file "~/.emacs.d/tmp/savehist")

(add-hook 'after-init-hook #'global-flycheck-mode)
