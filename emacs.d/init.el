
;; Meta

;;    Emacs can only load =.el=-files. We can use =C-c C-v t= to run
;;    =org-babel-tangle=, which extracts the code blocks from the current file
;;    into a source-specific file (in this case a =.el=-file).

;;    To avoid doing this each time a change is made we can add a function to
;;    the =after-save-hook= ensuring to always tangle and byte-compile the
;;    =org=-document after changes.

(defun tangle-init ()
   "If the current buffer is 'init.org' the code-blocks are
 tangled, and the tangled file is compiled."
   (when (equal (buffer-file-name)
                "/home/austin/.dotfiles/emacs.d/init.org")
     ;; Avoid running hooks when tangling.
     (let ((prog-mode-hook nil))
       (org-babel-tangle)
       (byte-compile-file (concat user-emacs-directory "init.el")))))

 (add-hook 'after-save-hook 'tangle-init)

;; I'm adding code blocks to this file all the time. Org-mode provides a few
;;    [[http://orgmode.org/manual/Easy-Templates.html][structure templates]] for quickly adding new blocks, but I can make it even
;;    better. Everything in here is Emacs lisp, so let's alter the source code
;;    template a bit when I'm in this file.

(defun aw/init-org-elisp-template ()
    (when (equal (buffer-file-name)
                 "/home/austin/.dotfiles/emacs.d/init.org")
      (setq-local org-structure-template-alist
                  '(("s" "#+BEGIN_SRC emacs-lisp\n?\n#+END_SRC" "<src lang="emacs lisp">\n?\n</src>")))))

(add-hook 'org-mode-hook 'aw/init-org-elisp-template)

;; Package

;;    Managing extensions for Emacs is simplified using =package= which is
;;    built in to Emacs 24 and newer. To load downloaded packages we need to
;;    initialize =package=. =cl= is a library that contains many functions from
;;    Common Lisp, and comes in handy quite often, so we want to make sure it's
;;    loaded, along with =package=, which is obviously needed.

(require 'cl)
(require 'package)
(setq package-enable-at-startup nil)
(package-initialize)

;; Packages can be fetched from different mirrors, [[http://melpa.org][melpa]] is the largest
;;    archive and is well maintained.

(setq package-archives
      '(("gnu" . "http://elpa.gnu.org/packages/")
        ("MELPA" . "http://melpa.org/packages/")))

;; Keep a list of packages we want to have installed.

(defvar required-packages
       '(ac-octave           ; Auto-completion for octave
         auto-compile        ; Automatically compile Emacs Lisp libraries
         cider               ; Clojure repl
         clojure-mode        ; Mode for .clj files
         csharp-mode         ; Mode for C# files
         company             ; Auto-completion engine
         evil                ; Vi and Emacs, in harmony
         evil-leader         ; Bring back the leader key
         evil-nerd-commenter ; Quickly comment out lines
         evil-surround       ; Tim Pope's vim plugin to surround objects
         expand-region       ; Increase selected region by semantic units
         flx-ido             ; flx integration for ido
         flycheck            ; On-the-fly syntax checking
         helm                ; Super powerful completion tool
         helm-gtags          ; Use gtags for semantic completion
         helm-projectile     ; Projectile as a helm completion source
         idle-require        ; load elisp libraries while Emacs is idle
         ido-vertical-mode   ; Makes ido-mode display vertically.
         jedi                ; Python auto-completion for Emacs
         key-chord           ; Run commands with multiple key strokes (Helpful for Evil)
         magit               ; Git integration for Emacs
         markdown-mode       ; Emacs Major mode for Markdown-formatted files.
         move-text           ; Move current line or region with M-up or M-down
         multi-term          ; Better terminals
         multiple-cursors    ; Multiple cursors for Emacs.
         org-trello
         paredit             ; minor mode for editing parentheses
         powerline           ; Rewrite of Powerline
         projectile          ; Easy navigation for files in a project
         slime               ; Superior Lisp Interaction Mode for Emacs
         solarized-theme     ; Great color theme
         smex                ; M-x interface with Ido-style fuzzy matching.
         undo-tree           ; Treat undo history as a tree
         yasnippet           ; Snippet engine
         zenburn-theme       ; Nice looking low-contrast theme
         )
      "Packages which should be installed upon launch")

;; We can define a predicate that tells us whether or not the newest version
;;    of a package is installed.

(defun newest-package-installed-p (package)
  "Return true if the newest available PACKAGE is installed."
  (when (package-installed-p package)
    (let* ((local-pkg-desc (or (assq package package-alist)
                               (assq package package--builtins)))
           (newest-pkg-desc (assq package package-archive-contents)))
      (and local-pkg-desc newest-pkg-desc
           (version-list-= (package-desc-vers (cdr local-pkg-desc))
                           (package-desc-vers (cdr newest-pkg-desc)))))))

;; Let's write a function to install a package if it is not installed or
;;    upgrades it if a new version has been released. Here our predicate comes
;;    in handy.

(defun upgrade-or-install-package (package)
  "Unless the newest available version of PACKAGE is installed
PACKAGE is installed and the current version is deleted."
  (unless (newest-package-installed-p package)
    (let ((get-desc (if (version< emacs-version "24.4") 'cdr 'cadr))
          (pkg-desc (assq package package-alist)))
      (when pkg-desc
        (if (version< emacs-version "24.4")
            (package-delete (symbol-name package)
                            (package-version-join
                             (package-desc-vers (get-desc pkg-desc))))
          (package-delete pkg-desc)))
      (and (assq package package-archive-contents)
           (package-install package)))))

;; Also, we will need a function to find all dependencies from a given package.

(defun dependencies (package)
  "Returns a list of dependencies from a given PACKAGE."
  (let* ((pkg-desc (assq package package-alist))
         (reqs (and pkg-desc (package-desc-reqs (cdr pkg-desc)))))
    (mapcar 'car reqs)))

;; The =package-refresh-contents= function downloads archive descriptions,
;;    this is a major bottleneck in this configuration. To avoid this we can
;;    try to only check for updates once every day or so. Here are three
;;    variables. The first specifies how often we should check for updates. The
;;    second specifies whether one should update during the initialization. The
;;    third is a path to a file where a time-stamp is stored in order to check
;;    when packages were updated last.

(defvar days-between-updates 7)
(defvar do-package-update-on-init t)
(defvar package-last-update-file
  (expand-file-name (concat user-emacs-directory ".package-last-update")))

;; The tricky part is figuring out when packages were last updated. Here is
;;    a hacky way of doing it, using [[http://www.gnu.org/software/emacs/manual/html_node/emacs/Time-Stamps.html][time-stamps]]. By adding a time-stamp to the
;;    a file, we can determine whether or not to do an update. After that we
;;    must run the =time-stamp=-function to update the time-stamp.

(require 'time-stamp)
;; Open the package-last-update-file
(with-temp-file package-last-update-file
  (if (file-exists-p package-last-update-file)
      (progn
        ;; Insert it's original content's.
        (insert-file-contents package-last-update-file)
        (let ((start (re-search-forward time-stamp-start nil t))
              (end (re-search-forward time-stamp-end nil t)))
          (when (and start end)
            ;; Assuming we have found a time-stamp, we check determine if it's
            ;; time to update.
            (setq do-package-update-on-init
                  (<= days-between-updates
                      (days-between
                       (current-time-string)
                       (buffer-substring-no-properties start end))))
            ;; Remember to update the time-stamp.
            (when do-package-update-on-init
              (time-stamp)))))
    ;; If no such file exists it is created with a time-stamp.
    (insert "Time-stamp: <>")
    (time-stamp)))

(when (and do-package-update-on-init
           (y-or-n-p "Update all packages?"))
  (package-refresh-contents)

  (dolist (p required-packages)
    (when (not (package-installed-p p))
      (package-install p))))

;; Sane defaults

;;    These are what /I/ consider to be saner defaults.

;;    We can set variables to whatever value we'd like using =setq=.

(setq default-input-method "TeX"    ; Use TeX when toggling input method.
      doc-view-continuous t         ; At page edge goto next/previous.
      echo-keystrokes 0.1           ; Show keystrokes asap.
      inhibit-startup-message t     ; No splash screen please.
      initial-scratch-message nil   ; Clean scratch buffer.
      electric-pair-mode 1          ; Insert brackets, parentheses in pairs
      ring-bell-function 'ignore    ; Quiet.
      byte-compile-warnings nil     ; Don't show warnings when compiling elisp
      ;; Save undo history between sessions, if you have an undo-dir
      undo-tree-auto-save-history
      (file-exists-p
       (concat user-emacs-directory "undo"))
      undo-tree-history-directory-alist
      ;; Put undo-history files in a directory, if it exists.
      (let ((undo-dir (concat user-emacs-directory "undo")))
        (and (file-exists-p undo-dir)
             (list (cons "." undo-dir)))))

;; Some variables are buffer-local, so changing them using =setq= will only
;;    change them in a single buffer. Using =setq-default= we change the
;;    buffer-local variable's default value.

(setq-default fill-column 80                    ; Maximum line width.
              indent-tabs-mode nil              ; Use spaces instead of tabs.
              split-width-threshold 100         ; Split verticly by default.
              compilation-scroll-output 1       ; Follow compilation buffer
              compilation-ask-about-save nil    ; Automatically save when compiling
              auto-fill-function 'do-auto-fill) ; Auto-fill-mode everywhere.

;; Answering /yes/ and /no/ to each question from Emacs can be tedious, a
;;    single /y/ or /n/ will suffice.

(fset 'yes-or-no-p 'y-or-n-p)

;; To avoid file system clutter we put all auto saved files in a single
;;    directory.

(defvar emacs-autosave-directory
  (concat user-emacs-directory "autosaves/")
  "This variable dictates where to put auto saves. It is set to a
  directory called autosaves located wherever your .emacs.d/ is
  located.")

;; Sets all files to be backed up and auto saved in a single directory.
(setq backup-directory-alist
      `((".*" . ,emacs-autosave-directory))
      auto-save-file-name-transforms
      `((".*" ,emacs-autosave-directory t)))

;; The scratch buffer is a useful place to test out bits of elisp or store some
;;    text temporarily. It would be nice if it was persistent, though. The
;;    following code will save the buffer every 5 minutes, and reload it on startup.

(defun save-persistent-scratch ()
  "Save the contents of *scratch*"
       (with-current-buffer (get-buffer-create "*scratch*")
         (write-region (point-min) (point-max)
                       (concat user-emacs-directory "scratch"))))

(defun load-persistent-scratch ()
  "Reload the scratch buffer"
  (let ((scratch-file (concat user-emacs-directory "scratch")))
    (if (file-exists-p scratch-file)
        (with-current-buffer (get-buffer "*scratch*")
          (delete-region (point-min) (point-max))
          (insert-file-contents scratch-file)))))

(add-hook 'emacs-startup-hook 'load-persistent-scratch)
(add-hook 'kill-emacs-hook 'save-persistent-scratch)

(run-with-idle-timer 300 t 'save-persistent-scratch)

;; Set =utf-8= as preferred coding system.

(set-language-environment "UTF-8")

;; By default the =narrow-to-region= command is disabled and issues a
;;    warning, because it might confuse new users. I find it useful sometimes,
;;    and don't want to be warned.

(put 'narrow-to-region 'disabled nil)

;; Call =auto-complete= default configuration, which enables =auto-complete=
;;    globally.

(eval-after-load 'auto-complete-config `(ac-config-default))

;; Automaticly revert =doc-view=-buffers when the file changes on disk.

(add-hook 'doc-view-mode-hook 'auto-revert-mode)

(when (fboundp 'windmove-default-keybindings)
   (windmove-default-keybindings))

;; Visual

;;    Change the color-theme to =solarized=.

(setq solarized-scale-org-headlines nil)
(load-theme 'solarized-dark t)

;; Use the [[http://www.levien.com/type/myfonts/inconsolata.html][Inconsolata]] font if it's installed on the system.

(when (member "Inconsolata" (font-family-list))
  (set-face-attribute 'default nil :font "Inconsolata-12"))

;; Modes

;;    There are some modes that are enabled by default that I don't find
;;    particularly useful. We create a list of these modes, and disable all of
;;    these.

(dolist (mode
         '(tool-bar-mode                ; No toolbars, more room for text.
           menu-bar-mode                ; No menu bar
           scroll-bar-mode              ; No scroll bars either.
           blink-cursor-mode))          ; The blinking cursor gets old.
  (funcall mode 0))

;; Let's apply the same technique for enabling modes that are disabled by
;;    default.

(dolist (mode
         '(column-number-mode         ; Show column number in mode line.
           delete-selection-mode      ; Replace selected text.
           dirtrack-mode              ; directory tracking in *shell*
           recentf-mode               ; Recently opened files.
           show-paren-mode))          ; Highlight matching parentheses.
  (funcall mode 1))

(when (version< emacs-version "24.4")
  (eval-after-load 'auto-compile
    '((auto-compile-on-save-mode 1))))  ; compile .el files on save.

;; This makes =.md=-files open in =markdown-mode=.

(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

;; We want to have autocompletion by default. Load company mode everywhere.

(add-hook 'after-init-hook 'global-company-mode)
(setq company-idle-delay 0)

;; Evil-leader
   
;;    We can bring back the leader key with the =evil-leader= package. I've always
;;    been a fan of , for my leader.

(global-evil-leader-mode)
(evil-leader/set-leader ",")
(evil-leader/set-key
  "0" 'delete-window
  "1" 'delete-other-windows
  "2" 'split-window-below
  "3" 'split-window-right
  "f" 'helm-find-files
  "m" 'compile
  "p" 'projectile-find-file
  "t" 'multi-term-dedicated-toggle
  "ei" 'my-edit-init-org
  "es" 'my-switch-to-scratch
  "x" 'helm-M-x)

;; Buffer Stuff
(evil-leader/set-key
  "bb" 'helm-mini
  "bk" 'kill-buffer
  "bs" 'save-buffer
  )

;; Evil-surround

;;     This awesome Vim plugin will let you surround text objects with various
;;     items. Luckily, there's an Emacs port.

(global-evil-surround-mode 1)

;; Evil Functions

(defun aw/open-line-above ()
  (interactive)
  (save-excursion
    (previous-line)
    (end-of-line)
    (open-line 1)))

(defun aw/open-line-below ()
  (interactive)
  (save-excursion
    (end-of-line)
    (open-line 1)))

;; Initialization

;;    Once everything is set up, we can start evil-mode.

(evil-mode 1)

(define-key evil-normal-state-map "H" 'windmove-left)
(define-key evil-normal-state-map "J" 'windmove-down)
(define-key evil-normal-state-map "K" 'windmove-up)
(define-key evil-normal-state-map "L" 'windmove-right)

(key-chord-define evil-insert-state-map "jk" 'evil-normal-state)
(key-chord-define evil-insert-state-map "kj" 'evil-normal-state)

;; I was really starting to miss some of these bindings from TPope's vim-unimpaired.

(key-chord-define evil-normal-state-map "[e" 'move-text-up)
(key-chord-define evil-normal-state-map "]e" 'move-text-down)
(key-chord-define evil-normal-state-map "[ " 'aw/open-line-above)
(key-chord-define evil-normal-state-map "] " 'aw/open-line-below)
(key-chord-define evil-normal-state-map "[b" 'previous-buffer)
(key-chord-define evil-normal-state-map "]b" 'next-buffer)

;; Snippets

;;    Start yasnippet

(require 'yasnippet)
(yas-global-mode 1)

;; Helm

;;    Helm is an amazing completion tool for finding almost anything. We can
;;    replace many default functions with the helm equivalent.

(eval-after-load 'helm
  '(progn
     (global-set-key (kbd "M-y") 'helm-show-kill-ring)
     (global-set-key (kbd "C-x b") 'helm-mini)
     (global-set-key (kbd "C-x C-f") 'helm-find-files)
     (global-set-key (kbd "M-x") 'helm-M-x)
     (define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action)
     (define-key helm-map (kbd "C-z")  'helm-select-action) ; list actions using C-z

     (add-to-list 'helm-sources-using-default-as-input 'helm-source-man-pages)))

(require 'helm-config)

;; The default "C-x c" is quite close to "C-x C-c", which quits Emacs.
;; Changed to "C-c h". Note: We must set "C-c h" globally, because we
;; cannot change `helm-command-prefix-key' once `helm-config' is loaded.
(global-set-key (kbd "C-c h") 'helm-command-prefix)
(global-unset-key (kbd "C-x c"))

(when (executable-find "curl")
  (setq helm-google-suggest-use-curl-p t))

(setq helm-quick-update                     t ; do not display invisible candidates
      helm-split-window-in-side-p           t ; open helm buffer inside current window, not occupy whole other window
      helm-M-x-fuzzy-match                  t ; fuzzy matching M-x
      helm-buffers-fuzzy-matching           t ; fuzzy matching buffer names when non--nil
      helm-recentf-fuzzy-match              t ; fuzzy matching recent files
      helm-move-to-line-cycle-in-source     t ; move to end or beginning of source when reaching top or bottom of source.
      helm-ff-search-library-in-sexp        t ; search for library in `require' and `declare-function' sexp.
      helm-scroll-amount                    8 ; scroll 8 lines other window using M-<next>/M-<prior>
      helm-ff-file-name-history-use-recentf t)

(helm-mode 1)

;; Helm-gtags

(setq
 helm-gtags-ignore-case t
 helm-gtags-auto-update t
 helm-gtags-use-input-at-cursor t
 helm-gtags-pulse-at-cursor t
 helm-gtags-prefix-key "\C-cg"
 helm-gtags-suggested-key-mapping t
 )

(require 'helm-gtags)
;; Enable helm-gtags-mode
(add-hook 'dired-mode-hook 'helm-gtags-mode)
(add-hook 'eshell-mode-hook 'helm-gtags-mode)
(add-hook 'c-mode-hook 'helm-gtags-mode)
(add-hook 'c++-mode-hook 'helm-gtags-mode)
(add-hook 'asm-mode-hook 'helm-gtags-mode)

(define-key helm-gtags-mode-map (kbd "C-c g a") 'helm-gtags-tags-in-this-function)
(define-key helm-gtags-mode-map (kbd "C-j") 'helm-gtags-select)
(define-key helm-gtags-mode-map (kbd "M-.") 'helm-gtags-dwim)
(define-key helm-gtags-mode-map (kbd "M-,") 'helm-gtags-pop-stack)
(define-key helm-gtags-mode-map (kbd "C-c <") 'helm-gtags-previous-history)
(define-key helm-gtags-mode-map (kbd "C-c >") 'helm-gtags-next-history)

;; Projectile

;;    Projectile makes it easy to navigate files in a single project. A project
;;    is defined as any directory containing a .git/ or other VCS
;;    repository. We can manually define a project by adding an empty
;;    =.projectile= file to our directory.

(projectile-global-mode) ; Load Projectile everywhere
(setq projectile-completion-system 'helm)
(setq projectile-enable-caching t)
(helm-projectile-on)

;; Interactive functions
   
;;    I want to be able to quickly jump back to certain files or buffers in just a few key
;;    presses. I'll call these interactive functions with =evil=leader= later on.

(defun my-edit-init-org ()
  (interactive)
  (find-file (concat user-emacs-directory "init.org")))

(defun my-switch-to-scratch ()
  (interactive)
  (switch-to-buffer "*scratch*"))

;; Advice

;;    An advice can be given to a function to make it behave differently. This
;;    advice makes =eval-last-sexp= (bound to =C-x C-e=) replace the sexp with
;;    the value.

(defadvice eval-last-sexp (around replace-sexp (arg) activate)
  "Replace sexp when called with a prefix argument."
  (if arg
      (let ((pos (point)))
        ad-do-it
        (goto-char pos)
        (backward-kill-sexp)
        (forward-sexp))
    ad-do-it))

;; When interactively changing the theme (using =M-x load-theme=), the
;;    current custom theme is not disabled. This often gives weird-looking
;;    results; we can advice =load-theme= to always disable themes currently
;;    enabled themes.

(defadvice load-theme
  (before disable-before-load (theme &optional no-confirm no-enable) activate)
  (mapc 'disable-theme custom-enabled-themes))

;; Key-chord-mode

;;    =key-chord-mode= allows me to use sequences of key presses to do things. It
;;    will come in handy when setting up =evil-mode=

(setq key-chord-two-keys-delay 2)
(key-chord-mode 1)

;; Semantic

(require 'cc-mode)
(require 'semantic)

(global-semanticdb-minor-mode 1)
(global-semantic-idle-scheduler-mode 1)

(semantic-mode 1)

;; Java and C

;;     The =c-mode-common-hook= is a general hook that work on all C-like
;;     languages (C, C++, Java, etc...). I like being able to quickly compile
;;     using =C-c C-c= (instead of =M-x compile=), a habit from =latex-mode=.

(defun c-setup ()
  (local-set-key (kbd "C-c C-c") 'compile)
  (setq c-default-style "linux"
        c-basic-offset 4))

(add-hook 'c-mode-common-hook 'c-setup)

;; Some statements in Java appear often, and become tedious to write
;;     out. We can use abbrevs to speed this up.

(define-abbrev-table 'java-mode-abbrev-table
  '(("psv" "public static void main(String[] args) {" nil 0)
    ("sopl" "System.out.println" nil 0)
    ("sop" "System.out.printf" nil 0)))

;; To be able to use the abbrev table defined above, =abbrev-mode= must be
;;     activated.

(defun java-setup ()
  (abbrev-mode t)
  (setq-local compile-command (concat "javac " (buffer-name))))

(add-hook 'java-mode-hook 'java-setup)

;; C#

;;     Omnisharp gives us IDE capabilities for C#. Let's enable it for
;;     =csharp-mode=

(add-hook 'csharp-mode-hook 'omnisharp-mode)

;;(omnisharp-imenu-support t)

;; Allow company to use OmniSharp for autocompletion.

(eval-after-load 'company
  '(add-to-list 'company-backends 'company-omnisharp))

;; LaTeX

;;     =.tex=-files should be associated with =latex-mode= instead of
;;     =tex-mode=.

(add-to-list 'auto-mode-alist '("\\.tex\\'" . latex-mode))

;; I like using the [[https://code.google.com/p/minted/][Minted]] package for source blocks in LaTeX. To make org
;;     use this we add the following snippet.

(eval-after-load 'org
  '(add-to-list 'org-latex-packages-alist '("" "minted")))
(setq org-latex-listings 'minted)

;; Because [[https://code.google.com/p/minted/][Minted]] uses [[http://pygments.org][Pygments]] (an external process), we must add the
;;     =-shell-escape= option to the =org-latex-pdf-process= commands. The
;;     =tex-compile-commands= variable controls the default compile command for
;;     Tex- and LaTeX-mode, we can add the flag with a rather dirty statement
;;     (if anyone finds a nicer way to do this, please let me know).

(eval-after-load 'ox-latex
  '(setq org-latex-pdf-process
         (mapcar
          (lambda (str)
            (concat "pdflatex -shell-escape "
                    (substring str (string-match "-" str))))
          org-latex-pdf-process)))

(eval-after-load 'tex-mode
  '(setcar (cdr (cddaar tex-compile-commands)) " -shell-escape "))

;; Python

;;      [[http://tkf.github.io/emacs-jedi/released/][Jedi]] offers very nice auto completion for =python-mode=. Mind that it is
;;      dependent on some python programs as well, so make sure you follow the
;;      instructions from the site.

(require 'jedi)
(add-hook 'python-mode-hook 'jedi:setup)
(setq jedi:server-command
     (cons "python3" (cdr jedi:server-command))
     python-shell-interpreter "python3")
(setq jedi:complete-on-dot t)
;;(add-hook 'python-mode-hook 'jedi:ac-setup)

;; Matlab

;;     =Matlab-mode= works pretty good out of the box, but we can do without the
;;     splash screen.

(eval-after-load 'matlab
  '(add-to-list 'matlab-shell-command-switches "-nosplash"))

;; Octave

;;     Make it so =.m= files are loaded in =octave-mode=.

(autoload 'octave-mode "octave-mod" nil t)
(setq auto-mode-alist
      (cons '("\\.m$" . octave-mode) auto-mode-alist))

;; Emacs Lisp

(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            ;; Use spaces, not tabs.
            (setq indent-tabs-mode nil)
            (define-key emacs-lisp-mode-map
              "\r" 'reindent-then-newline-and-indent)))
(add-hook 'emacs-lisp-mode-hook 'eldoc-mode)
(add-hook 'emacs-lisp-mode-hook 'paredit-mode)
(add-hook 'emacs-lisp-mode-hook 'flyspell-prog-mode) ;; Requires Ispell

;; FSP

;;     FSP (Finite state processes) is a notation that formally describes concurrent
;;     systems as described in the book Concurrency by Magee and Kramer. Someday
;;     I want to make a fully featured mode for FSP. Someone by the name of
;;     Esben Andreasen made a mode with basic syntax highlighting, so that will
;;     have to do for now.

;;     We'll add it manually until I have time to play around with it.

;; Load fsp-mode.el from its own directory
(add-to-list 'load-path "~/Dropbox/fsp-mode/")
(require 'fsp-mode)

;; Term
   
;;    Multi-term makes working with many terminals a bit nicer. I can easily create
;;    and cycle through any number of terminals. There's also a "dedicated terminal"
;;    that I can pop up when needed. I'll make use of this all in the key bindings section.
   
;;    From the emacs wiki:

(defun last-term-buffer (l)
  "Return most recently used term buffer."
  (when l
    (if (eq 'term-mode (with-current-buffer (car l) major-mode))
        (car l) (last-term-buffer (cdr l)))))

(defun get-term ()
  "Switch to the term buffer last used, or create a new one if
    none exists, or if the current buffer is already a term."
  (interactive)
  (let ((b (last-term-buffer (buffer-list))))
    (if (or (not b) (eq 'term-mode major-mode))
        (multi-term)
      (switch-to-buffer b))))

(setq multi-term-dedicated-select-after-open-p t)

;; Some modes don't need to be in the terminal.

(add-hook 'term-mode-hook (lambda()
                            (yas-minor-mode -1)))

;; Shell

;;    To be able to quickly switch back and forth between a shell I make use of this little function.

(defun toggle-shell ()
  "Jumps to eshell or back."
  (interactive)
  (if (string= (buffer-name) "*shell*")
      (switch-to-prev-buffer)
    (eshell)))

;; I'd like the =C-l= to work more like the standard terminal (which works
;;    like running =clear=), and resolve this by simply removing the
;;    buffer-content. Mind that this is not how =clear= works, it simply adds a
;;    bunch of newlines, and puts the prompt at the top of the window, so it
;;    does not remove anything. In Emacs removing stuff is less of a worry,
;;    since we can always undo!

(defun clear-shell ()
  "Runs `comint-truncate-buffer' with the
`comint-buffer-maximum-size' set to zero."
  (interactive)
  (let ((comint-buffer-maximum-size 0))
   (comint-truncate-buffer)))

;; Lastly we should bind our functions. The =toggle-shell= should be a
;;    global binding (because we want to be able to switch to a shell from any
;;    buffer), but the =clear-shell= should only affect =shell-mode=.

(add-hook 'shell-mode-hook (lambda () (local-set-key (kbd "C-l") 'clear-shell)))

;; Config files
   
;;    Let's add some color to these files.

(add-to-list 'auto-mode-alist '("\\.service\\'" . conf-unix-mode))
(add-to-list 'auto-mode-alist '("\\.timer\\'" . conf-unix-mode))
(add-to-list 'auto-mode-alist '("\\.target\\'" . conf-unix-mode))
(add-to-list 'auto-mode-alist '("\\.mount\\'" . conf-unix-mode))
(add-to-list 'auto-mode-alist '("\\.automount\\'" . conf-unix-mode))
(add-to-list 'auto-mode-alist '("\\.slice\\'" . conf-unix-mode))
(add-to-list 'auto-mode-alist '("\\.socket\\'" . conf-unix-mode))
(add-to-list 'auto-mode-alist '("\\.path\\'" . conf-unix-mode))

;; Proced

(defun proced-settings ()
  (proced-toggle-auto-update t))

(add-hook 'proced-mode-hook 'proced-settings)

;; Task Tracking

;;    I keep my schedule with =org=agenda=.

(setq org-agenda-start-on-weekday nil              ; Show agenda from today.
      org-agenda-files '("~/Dropbox/org")          ; A list of agenda files.
      org-agenda-default-appointment-duration 60   ; 1 hour appointments
      org-agenda-span 1)                           ; Show only today by default

;; When editing org-files with source-blocks, we want the source blocks to
;;    be themed as they would in their native mode.

(setq org-src-fontify-natively t
      org-confirm-babel-evaluate nil)

;; This is quite an ugly fix for allowing code markup for expressions like
;;    ="this string"=, because the quotation marks causes problems.

(setcar (nthcdr 2 org-emphasis-regexp-components) " \t\n,")
(custom-set-variables `(org-emphasis-alist ',org-emphasis-alist))

;; MobileOrg
;;    MobileOrg will let me sync my agenda to my phone, which will then sync
;;    with my calendar.

;; Set to the location of your Org files on your local system
(setq org-directory "~/Dropbox/org")
;; Set to the name of the file where new notes will be stored
(setq org-mobile-inbox-for-pull "~/Dropbox/org/flagged.org")
;; Set to <your Dropbox root directory>/MobileOrg.
(setq org-mobile-directory "~/Dropbox/Apps/MobileOrg")

;; We can use =idle-timer= to push and pull to MobileOrg when there's no
;;    other activity.

(defvar my-org-mobile-sync-timer nil)

(defvar my-org-mobile-sync-secs (* 60 20))

(defun my-org-mobile-sync-pull-and-push ()
  (org-mobile-pull)
  (org-mobile-push)
  (when (fboundp 'sauron-add-event)
    (sauron-add-event 'my 3 "Called org-mobile-pull and org-mobile-push")))

(defun my-org-mobile-sync-start ()
  "Start automated `org-mobile-push'"
  (interactive)
  (setq my-org-mobile-sync-timer
        (run-with-idle-timer my-org-mobile-sync-secs t
                             'my-org-mobile-sync-pull-and-push)))

(defun my-org-mobile-sync-stop ()
  "Stop automated `org-mobile-push'"
  (interactive)
  (cancel-timer my-org-mobile-sync-timer))

(my-org-mobile-sync-start)

;; Keybindings

;;    Org-mode uses Shift + arrow keys to change things like timestamps, TODO
;;    keywords, priorities, and so on. This is nice, but it gets in the way of
;;    windmove. The following hooks will allow shift+<arrow> to use windmove if
;;    there are no special org-mode contexts under the point.

(add-hook 'org-shiftup-final-hook 'windmove-up)
(add-hook 'org-shiftleft-final-hook 'windmove-left)
(add-hook 'org-shiftdown-final-hook 'windmove-down)
(add-hook 'org-shiftright-final-hook 'windmove-right)

;; Key bindings

;;   Inspired by [[http://stackoverflow.com/questions/683425/globally-override-key-binding-in-emacs][this StackOverflow post]] I keep a =custom-bindings-map= that
;;   holds all my custom bindings. This map can be activated by toggling a
;;   simple =minor-mode= that does nothing more than activating the map. This
;;   inhibits other =major-modes= to override these bindings. I keep this at
;;   the end of the init-file to make sure that all functions are actually
;;   defined.

(defvar custom-bindings-map (make-keymap)
  "A keymap for custom bindings.")

;; Binding to use Ibuffer over BufferMenu

(define-key custom-bindings-map (kbd "C-x C-b")  'ibuffer)

;; Bindings for [[https://github.com/magnars/expand-region.el][expand-region]].

(define-key custom-bindings-map (kbd "C-'")  'er/expand-region)
(define-key custom-bindings-map (kbd "C-;")  'er/contract-region)

;; Bindings for multi-term

(define-key custom-bindings-map (kbd "C-c t")  'multi-term-dedicated-toggle)
(define-key custom-bindings-map (kbd "C-c T")  'get-term)

;; Bindings for [[http://magit.github.io][Magit]].

(define-key custom-bindings-map (kbd "C-c m") 'magit-status)

;; Bindings for [[http://emacs-helm.github.io/helm/][Helm]].

(define-key custom-bindings-map (kbd "C-c h g") 'helm-google-suggest)

;; Bind some native Emacs functions.

(define-key custom-bindings-map (kbd "C-x p")    'proced)
(define-key custom-bindings-map (kbd "C-c r")    'rename-buffer)
(define-key custom-bindings-map (kbd "C-c s")    'ispell-word)
(define-key custom-bindings-map (kbd "C-c a")    'org-agenda-list)

;; Lastly we need to activate the map by creating and activating the
;;   =minor-mode=.

(define-minor-mode custom-bindings-mode
  "A mode that activates custom-bindings."
  t nil custom-bindings-map)
