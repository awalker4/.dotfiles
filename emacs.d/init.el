;; Add modules path
(add-to-list 'load-path "~/.emacs.d/custom/")

(setq config-directory "~/.emacs.d")

(load "packages")

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
