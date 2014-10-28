(setq config-directory "~/.emacs.d")

(load "packages")
(load "scripts/defuns")
(load-dir "~/.emacs.d/scripts")

(savehist-mode 1)
(setq savehist-file "~/.emacs.d/tmp/savehist")
