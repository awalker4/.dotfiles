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
  '( helm magit yasnippet yasnippet-bundle
    solarized-theme flycheck powerline projectile)
  "Packages which should be installed upon launch")

(dolist (p required-packages)
  (when (not (package-installed-p p))
    (package-install p)))
