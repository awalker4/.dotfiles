
;; Meta
   
;;    Let's see how long everything takes to load.

(defconst emacs-start-time (current-time))

;; Emacs can only load =.el=-files. We can use =C-c C-v t= to run
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
       (org-babel-tangle))))

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

;; Package List
   
;;     This is a list of everything that should be installed using package.el.

(defvar required-packages
  '(ac-octave             ; Auto-completion for octave
    auto-compile          ; Automatically compile Emacs Lisp libraries
    cider                 ; Clojure repl
    clojure-mode          ; Mode for .clj files
    csharp-mode           ; Mode for C# files
    company               ; Auto-completion engine
    diminish              ; Clean up the status line a but
    evil                  ; Vi and Emacs, in harmony
    evil-leader           ; Bring back the leader key
    evil-nerd-commenter   ; Quickly comment out lines
    evil-surround         ; Tim Pope's vim plugin to surround objects
    evil-visualstar       ; Use * and # to search for the visual selection
    expand-region         ; Increase selected region by semantic units
    flycheck              ; On-the-fly syntax checking
    fullframe             ; Make certain modes take up the whole frame
    gist                  ; Quickly post code snippets to Github
    helm                  ; Super powerful completion tool
    helm-ag
    helm-gtags            ; Use gtags for semantic completion
    helm-projectile       ; Projectile as a helm completion source
    helm-spotify          ; Spotify's entire library at my fingertips
    idle-require          ; load elisp libraries while Emacs is idle
    ibuffer-tramp         ; sort ibuffer based on tramp connection
    impatient-mode        ; Edit html in realtime
    jedi                  ; Python auto-completion for Emacs
    key-chord             ; Run commands with multiple key strokes (Helpful for Evil)
    magit                 ; Git integration for Emacs
    markdown-mode         ; Emacs Major mode for Markdown-formatted files.
    move-text             ; Move current line or region with M-up or M-down
    multi-term            ; Better terminals
    multiple-cursors      ; Multiple cursors for Emacs.
    org-trello            ; two-way sync between org and Trello
    paredit               ; minor mode for editing parentheses
    powerline             ; Rewrite of Powerline
    projectile            ; Easy navigation for files in a project
    skewer-mode           ; Use the browser as a Javascript repl
    slime                 ; Superior Lisp Interaction Mode for Emacs
    solarized-theme       ; Great color theme
    undo-tree             ; Treat undo history as a tree
    yasnippet             ; Snippet engine
    zenburn-theme         ; Nice looking low-contrast theme
    )
  "Packages which should be installed upon launch")

;; Package Setup

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

;; Force =list-packages= to use the whole frame.

(fullframe list-packages quit-window)

;; Requires

(require 'fullframe)

;; Sane defaults

;;    These are what /I/ consider to be saner defaults.

(setq default-input-method "TeX"    ; Use TeX when toggling input method.
      doc-view-continuous t         ; At page edge goto next/previous.
      echo-keystrokes 0.1           ; Show keystrokes asap.
      inhibit-startup-message t     ; No splash screen please.
      initial-scratch-message nil   ; Clean scratch buffer.
      electric-pair-mode 1          ; Insert brackets, parentheses in pairs
      ring-bell-function 'ignore    ; Quiet.
      byte-compile-warnings nil     ; Don't show warnings when compiling elisp
      require-final-newline t       ; End files with \n
      ;; Save undo history between sessions, if you have an undo-dir
      undo-tree-auto-save-history
      (file-exists-p
       (concat user-emacs-directory "undo"))
      undo-tree-history-directory-alist
      ;; Put undo-history files in a directory, if it exists.
      (let ((undo-dir (concat user-emacs-directory "undo")))
        (and (file-exists-p undo-dir)
             (list (cons "." undo-dir)))))

(require 'undo-tree)
(diminish 'undo-tree-mode)

;; Some variables are buffer-local, so changing them using =setq= will only
;;    change them in a single buffer. Using =setq-default= we change the
;;    buffer-local variable's default value.

(setq-default fill-column 80                    ; Maximum line width.
              indent-tabs-mode nil              ; Use spaces instead of tabs.
              split-width-threshold 100         ; Split verticly by default.
              compilation-scroll-output 1       ; Follow compilation buffer
              compilation-ask-about-save nil    ; Automatically save when compiling
              auto-fill-function 'do-auto-fill) ; Auto-fill-mode everywhere.
(diminish 'auto-fill-function)

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
;;    following code will save the buffer every 5 minutes, and reload it on
;;    startup. ([[http://dorophone.blogspot.com/2011/11/how-to-make-emacs-scratch-buffer.html][Source]])

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

;; Use Shift+arrow keys to jump around windows.

(when (fboundp 'windmove-default-keybindings)
   (windmove-default-keybindings))

;; Since I'm using a daemon, I rarely kill emacs, which means bookmarks will
;;    never get saved on quit. Just save them on every update.

(setq bookmark-save-flag 1)

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

(eval-after-load "helm-regexp"
  '(setq helm-source-moccur
         (helm-make-source "Moccur"
             'helm-source-multi-occur :follow 1)))
;;(diminish 'helm-mode)

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

;; I'd like to easily run helm-occur on all buffers that are backed by files. ([[http://stackoverflow.com/questions/14726601/sublime-text-2s-goto-anything-or-instant-search-for-emacs][Source]])

(defun aw/helm-do-grep-all-buffers ()
  "multi-occur in all buffers backed by files."
  (interactive)
  (helm-multi-occur
   (delq nil
         (mapcar (lambda (b)
                   (when (buffer-file-name b) (buffer-name b)))
                 (buffer-list)))))

;; When you press backspace in a helm buffer and there's nothing left to delete,
;;    helm will complain by saying ~Text is read only~. A much better default is to just
;;    close the buffer. ([[http://oremacs.com/2014/12/21/helm-backspace/][Source]])

(defun helm-backspace ()
  (interactive)
  (condition-case nil
      (backward-delete-char 1)
    (error
     (helm-keyboard-quit))))

(define-key helm-map (kbd "DEL") 'helm-backspace)

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

;; Keybindings
   
;;    I keep my global key bindings in a custom keymap. By loading this map in its
;;    very own minor mode, I can make sure they ovverride any major mode
;;    bindings. I'll keep adding keys to this and then load it at the end.

(defvar custom-bindings-map (make-keymap)
  "A keymap for custom bindings.")

;; Some bindings that I haven't categorized yet:

(define-key custom-bindings-map (kbd "C-'") 'er/expand-region)
(define-key custom-bindings-map (kbd "C-;") 'er/contract-region)
(define-key custom-bindings-map (kbd "C-c h g") 'helm-google-suggest)
(define-key custom-bindings-map (kbd "C-c s") 'ispell-word)

;; Key-chord-mode
    
;;     =key-chord-mode= allows me to use sequences of key presses to do things. It
;;     will come in handy when setting up =evil-mode=

(setq key-chord-two-keys-delay 2)
(key-chord-mode 1)

;; Evil-leader
    
;;      We can bring back the leader key with the =evil-leader= package. I've always
;;      been a fan of , for my leader.

(global-evil-leader-mode)
(evil-leader/set-leader ",")
(evil-leader/set-key
  "f" 'helm-find-files
  "m" 'compile
  "t" 'multi-term-dedicated-toggle
  "ei" 'aw/edit-init-org
  "eI" 'aw/edit-init-el
  "eo" 'aw/edit-org-calendar
  "es" 'aw/switch-to-scratch
  "x" 'helm-M-x)

;; Window stuff
(evil-leader/set-key
  "0" 'delete-window
  "1" 'delete-other-windows
  "2" 'split-window-below
  "@" 'aw/split-window-below-and-switch
  "3" 'split-window-right
  "#" 'aw/split-window-right-and-switch
  "=" 'balance-windows)

;; Buffer and file stuff
(evil-leader/set-key
  "bb" 'helm-mini
  "bg" 'aw/helm-do-grep-all-buffers
  "bh" 'ff-find-other-file
  "bk" 'kill-buffer
  "bl" 'ibuffer
  "bm" 'bookmark-jump
  "bo" 'helm-occur
  "bs" 'save-buffer
  "bw" 'write-file)

;; Help stuff
(evil-leader/set-key
  "hc" 'describe-key-briefly
  "hf" 'describe-function
  "hv" 'describe-variable
  "hm" 'man)

;; Git stuff
(evil-leader/set-key
  "gb" 'magit-blame-mode
  "gs" 'magit-status)

;; Projectile/Helm stuff
(evil-leader/set-key
  "pf" 'helm-projectile-find-file
  "pg" 'helm-projectile-grep
  "pp" 'projectile-switch-project
  "ps" 'helm-spotify)

;; Org stuff
(evil-leader/set-key
  "oa" 'org-agenda-list
  "oc" 'org-capture)

;; Misc
(evil-leader/set-key
  "vb" 'eval-buffer
  "vv" 'eval-last-sexp)

;; Evil-surround

;;      This awesome Vim plugin will let you surround text objects with various
;;      items. Luckily, there's an Emacs version.

(global-evil-surround-mode 1)

;; Evil Functions

(defun aw/edit-init-org ()
  (interactive)
  (find-file (concat user-emacs-directory "init.org")))

(defun aw/edit-init-el ()
  (interactive)
  (find-file (concat user-emacs-directory "init.el")))

(defun aw/edit-org-calendar ()
  (interactive)
  (find-file (concat org-directory "/calendar.org")))

(defun aw/switch-to-scratch ()
  (interactive)
  (switch-to-buffer "*scratch*"))

(defun aw/split-window-right-and-switch ()
  (interactive)
  (split-window-right)
  (other-window 1))

(defun aw/split-window-below-and-switch ()
  (interactive)
  (split-window-below)
  (other-window 1))

(defun aw/open-line-above ()
  (interactive)
  (save-excursion
    (beginning-of-line)
    (open-line 1)))

(defun aw/open-line-below ()
  (interactive)
  (save-excursion
    (end-of-line)
    (open-line 1)))

;; Initialization

;;     Once everything is set up, we can start evil-mode.

;; Nerd commenter
(evilnc-default-hotkeys)
(global-evil-visualstar-mode t)

(evil-mode 1)

(define-key evil-normal-state-map "H" 'windmove-left)
(define-key evil-normal-state-map "J" 'windmove-down)
(define-key evil-normal-state-map "K" 'windmove-up)
(define-key evil-normal-state-map "L" 'windmove-right)

(key-chord-define evil-insert-state-map "jk" 'evil-normal-state)
(key-chord-define evil-insert-state-map "kj" 'evil-normal-state)

;; Make tab work in terminal emacs
(setq evil-want-C-i-jump nil)

;; I was really starting to miss some of these bindings from TPope's vim-unimpaired.

(key-chord-define evil-normal-state-map "[e" 'move-text-up)
(key-chord-define evil-normal-state-map "]e" 'move-text-down)
(key-chord-define evil-normal-state-map "[ " 'aw/open-line-above)
(key-chord-define evil-normal-state-map "] " 'aw/open-line-below)
(key-chord-define evil-normal-state-map "[b" 'previous-buffer)
(key-chord-define evil-normal-state-map "]b" 'next-buffer)

;; Modes

;;    There are some modes that are enabled by default that I don't find
;;    particularly useful. We create a list of these modes, and disable all of
;;    these.


;;    Let's apply the same technique for enabling modes that are disabled by
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
;; (diminish 'company-mode)
 (setq company-idle-delay 0)

;; Visual

;;    First, get rid of a few things.

(dolist (mode
         '(tool-bar-mode                ; No toolbars, more room for text.
           menu-bar-mode                ; No menu bar
           scroll-bar-mode              ; No scroll bars either.
           blink-cursor-mode))          ; The blinking cursor gets old.
  (funcall mode 0))

;; TODO: This doesn't work with emacsclient
;;    Change the color-theme to =solarized=. Use =wombat= in the terminal, since
;;    solarized doesn't play very nicely.

(load-theme 'solarized-dark t)

(setq solarized-scale-org-headlines nil)

;; Use the [[http://www.levien.com/type/myfonts/inconsolata.html][Inconsolata]] font if it's installed on the system.

(when (member "Inconsolata" (font-family-list))
  (set-face-attribute 'default nil :font "Inconsolata-12")
  (add-to-list 'default-frame-alist
               '(font . "Inconsolata-12")))

;; When interactively changing the theme (using =M-x load-theme=), the
;;    current custom theme is not disabled. This often gives weird-looking
;;    results; we can advice =load-theme= to always disable themes currently
;;    enabled themes.

(defadvice load-theme
  (before disable-before-load (theme &optional no-confirm no-enable) activate)
  (mapc 'disable-theme custom-enabled-themes))

;; I like how Vim shows you empty lines using tildes. Emacs can do something
;;    similar with the variable =indicate-empty-lines=, but I'll make it look a bit
;;    more familiar. ([[http://www.reddit.com/r/emacs/comments/2kdztw/emacs_in_evil_mode_show_tildes_for_blank_lines/][Source]])

(setq-default indicate-empty-lines t)
(define-fringe-bitmap 'tilde [0 0 0 113 219 142 0 0] nil nil 'center)
(setcdr (assq 'empty-line fringe-indicator-alist) 'tilde)
(set-fringe-bitmap-face 'tilde 'font-lock-function-name-face)

;; Windows
   
;;    =Winner-mode= allows you to jump back to previously used window
;;    configurations. The following massive function will ignore unwanted buffers
;;    when returning to a particular layout. ([[https://github.com/thierryvolpiatto/emacs-tv-config/blob/master/.emacs.el#L1706][Source]])

(setq winner-boring-buffers '("*Completions*"
                              "*Compile-Log*"
                              "*inferior-lisp*"
                              "*Fuzzy Completions*"
                              "*Apropos*"
                              "*Help*"
                              "*cvs*"
                              "*Buffer List*"
                              "*Ibuffer*"
                              ))
(defvar winner-boring-buffers-regexp "\\*[hH]elm.*")
(defun winner-set1 (conf)
  (let* ((buffers nil)
         (alive
          ;; Possibly update `winner-point-alist'
          (cl-loop for buf in (mapcar 'cdr (cdr conf))
                   for pos = (winner-get-point buf nil)
                   if (and pos (not (memq buf buffers)))
                   do (push buf buffers)
                   collect pos)))
    (winner-set-conf (car conf))
    (let (xwins) ; to be deleted
      ;; Restore points
      (dolist (win (winner-sorted-window-list))
        (unless (and (pop alive)
                     (setf (window-point win)
                           (winner-get-point (window-buffer win) win))
                     (not (or (member (buffer-name (window-buffer win))
                                      winner-boring-buffers)
                              (string-match winner-boring-buffers-regexp
                                            (buffer-name (window-buffer win))))))
          (push win xwins))) ; delete this window
      ;; Restore marks
      (letf (((current-buffer)))
        (cl-loop for buf in buffers
                 for entry = (cadr (assq buf winner-point-alist))
                 for win-ac-reg = (winner-active-region)
                 do (progn (set-buffer buf)
                           (set-mark (car entry))
                           (setf win-ac-reg (cdr entry)))))
      ;; Delete windows, whose buffers are dead or boring.
      ;; Return t if this is still a possible configuration.
      (or (null xwins)
          (progn
            (mapc 'delete-window (cdr xwins)) ; delete all but one
            (unless (one-window-p t)
              (delete-window (car xwins))
              t))))))
(defalias 'winner-set 'winner-set1)
(winner-mode 1)

;; Buffer Management
   
;;    =Ibuffer= mode is a built-in replacement for the stock =BufferMenu=. It offers
;;    fancy things like filtering buffers by major mode or sorting by size. The
;;    [[http://www.emacswiki.org/emacs/IbufferMode][wiki]] offers a number of improvements.
   
;;    The size column is always listed in bytes. We can make it a bit more human
;;    readable by creating a custom column.

;; (eval-after-load 'ibuffer
;;   (define-ibuffer-column size-h
;;     (:name "Size" :inline t)
;;     (cond
;;      ((> (buffer-size) 1000000) (format "%7.1fM" (/ (buffer-size) 1000000.0)))
;;      ((> (buffer-size) 1000) (format "%7.1fk" (/ (buffer-size) 1000.0)))
;;      (t (format "%8d" (buffer-size)))))

;;   ;; Modify the default ibuffer-formats
;;   (setq ibuffer-formats
;;         '((mark modified read-only " "
;;                 (name 18 18 :left :elide) " "
;;                 (size-h 9 -1 :right) " "
;;                 (mode 16 16 :left :elide) " "
;;                 filename-and-process))))

(add-hook 'ibuffer-hook 'ibuffer-tramp-set-filter-groups-by-tramp-connection)

;; (fullframe ibuffer ibuffer-quit)
(define-key custom-bindings-map (kbd "C-x C-b")  'ibuffer)
(define-key custom-bindings-map (kbd "C-c r") 'rename-buffer)

(evil-set-initial-state 'ibuffer-mode 'normal)

;; Snippets

;;    Start yasnippet

(require 'yasnippet)
(yas-global-mode 1)

;; Base Environment

;;    Only use line numbering when programming. For opening large files, this may add some
;;    overhead, so we can delay rendering a bit.

(setq linum-delay t linum-eager nil)
(add-hook 'prog-mode-hook 'linum-mode)
(add-hook 'prog-mode-hook 'flycheck-mode)

;; White space stuff ([[http://www.reddit.com/r/emacs/comments/2keh6u/show_tabs_and_trailing_whitespaces_only/][Source]])

(require 'whitespace)
(setq whitespace-display-mappings
   ;; all numbers are Unicode codepoint in decimal. try (insert-char 182 ) to see it
  '(
    (space-mark 32 [183] [46]) ; 32 SPACE, 183 MIDDLE DOT 「·」, 46 FULL STOP 「.」
    (newline-mark 10 [182 10]) ; 10 LINE FEED
    (tab-mark 9 [187 9] [9655 9] [92 9]) ; 9 TAB, 9655 WHITE RIGHT-POINTING TRIANGLE 「▷」
    ))
(setq whitespace-style '(face tabs trailing tab-mark))
(set-face-attribute 'whitespace-tab nil
                    :background "#f0f0f0"
                    :foreground "#00a8a8"
                    :weight 'bold)
(set-face-attribute 'whitespace-trailing nil
                    :background "#e4eeff"
                    :foreground "#183bc8"
                    :weight 'normal)
(add-hook 'prog-mode-hook 'whitespace-mode)

;; C#

;;     Omnisharp gives us IDE capabilities for C#. Let's enable it for
;;     =csharp-mode=

(add-hook 'csharp-mode-hook 'omnisharp-mode)

;;(omnisharp-imenu-support t)

;; Allow company to use OmniSharp for autocompletion.

;;(eval-after-load 'company
  ;;'(add-to-list 'company-backends 'company-omnisharp))

;; C++

;;     By default, .h files are opened in C mode. I'll mostly be using them for C++
;;     projects, though.

(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))

;; FSP

;;     FSP (Finite state processes) is a notation that formally describes concurrent
;;     systems as described in the book Concurrency by Magee and Kramer. Someday
;;     I want to make a fully featured mode for FSP. Someone by the name of
;;     Esben Andreasen made a mode with basic syntax highlighting, so that will
;;     have to do for now.

;;     We'll add it manually until I have time to play around with it.

;; Load fsp-mode.el from its own directory
;; (add-to-list 'load-path "~/Dropbox/fsp-mode/")
;; (require 'fsp-mode)

;; Java and C

;;     The =c-mode-common-hook= is a general hook that work on all C-like
;;     languages (C, C++, Java, etc...). I like being able to quickly compile
;;     using =C-c C-c= (instead of =M-x compile=), a habit from =latex-mode=.

(defun c-setup ()
  (local-set-key (kbd "C-c C-c") 'compile)
  (setq c-default-style "linux"
        c-basic-offset 4))

(add-hook 'c-mode-common-hook 'c-setup)

(defun java-setup ()
  (setq-local compile-command (concat "javac " (buffer-name))))

(add-hook 'java-mode-hook 'java-setup)

;; LaTeX

;;     =.tex=-files should be associated with =latex-mode= instead of
;;     =tex-mode=.

(add-to-list 'auto-mode-alist '("\\.tex\\'" . latex-mode))

;; TODO flycheck

(evil-leader/set-key-for-mode 'latex-mode
  "at" 'tex-compile)

;; Lisps

;;      This advice makes =eval-last-sexp= (bound to =C-x C-e=) replace the sexp with
;;      the value.

(defadvice eval-last-sexp (around replace-sexp (arg) activate)
  "Replace sexp when called with a prefix argument."
  (if arg
      (let ((pos (point)))
        ad-do-it
        (goto-char pos)
        (backward-kill-sexp)
        (forward-sexp))
    ad-do-it))

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

;; Flycheck gets to be a bit much when warning about checkdoc issues, so we
;;      should disable those.

(setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc))

;; Octave

;;     Make it so =.m= files are loaded in =octave-mode=.

(autoload 'octave-mode "octave-mod" nil t)
(setq auto-mode-alist
      (cons '("\\.m$" . octave-mode) auto-mode-alist))

;; Python

;;      [[http://tkf.github.io/emacs-jedi/released/][Jedi]] offers very nice auto completion for =python-mode=. Mind that it is
;;      dependent on some python programs as well, so make sure you follow the
;;      instructions from the site.

(require 'jedi)
;; (add-hook 'python-mode-hook 'jedi:setup)
(setq jedi:server-command
     (cons "python3" (cdr jedi:server-command))
     python-shell-interpreter "python3")
(setq jedi:complete-on-dot t)
;;(add-hook 'python-mode-hook 'jedi:ac-setup)

;; TODO Web Editing

;;      TODO: start httpd in correct directory

;;     =impatient-mode= is an amazing tool for live-editing web pages. When paired with
;;     =simple-httdp=, you can point your browser to =http://localhost:8080/imp= to
;;     see a live copy of any buffer that has impatient-mode enabled. If that buffer happens to contain HTML, CSS, or Javascript, it will be evaluated on the fly. No need to save or refresh
;;     anything. It's almost like they knew that I'm very... impatient.

;;     Let's start impatient mode for all HTML, CSS, and Javascript buffers, and
;;     run =httpd-start= when needed.

(require 'simple-httpd)

(defun aw/imp-setup ()
  (setq httpd-root "/home/austin/Dropbox/school/cis467/hw3/") ;; I'd like to set this based on the current buffer's working directory
  (httpd-start)
  (impatient-mode))

;; (add-hook 'html-mode-hook 'aw/imp-setup)
;; (add-hook 'css-mode-hook 'aw/imp-setup)
;; (add-hook 'js-mode-hook 'aw/imp-setup)

;; JavaScript

(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
(add-hook 'js2-mode-hook 'ac-js2-mode)
(setq js2-highlight-level 1)

;; Semantic

(require 'cc-mode)
(require 'semantic)

(global-semanticdb-minor-mode 1)
(global-semantic-idle-scheduler-mode 1)

(semantic-mode 1)

;; Source Control

;;    Magit is awesome. Open it up with the entire frame.

(fullframe magit-status magit-mode-quit-window)
(define-key custom-bindings-map (kbd "C-c m") 'magit-status)

;; Diffs

;;     =ediff= is a powerful tool for dealing with changes to a file. You can diff
;;     two files or diff the current buffer against the version that's on disk. I
;;     haven't had to use it too much yet, but here are some tweaks that I've
;;     picked up.

;;     By default, ediff compares two buffers in a vertical split. Horizontal would
;;     make it a lot easier to compare things.

(custom-set-variables
 '(ediff-window-setup-function 'ediff-setup-windows-plain)
 '(ediff-diff-options "-w")
 '(ediff-split-window-function 'split-window-horizontally))

;; Don't screw up my window configuration after I leave ediff.

(add-hook 'ediff-after-quit-hook-internal 'winner-undo)

;; It's hard to diff org files when everything is collapsed. These functions
;;     will expand each hunk as I jump to it, and collapse the rest. ([[http://permalink.gmane.org/gmane.emacs.orgmode/75211][Source]])

;; Check for org mode and existence of buffer
(defun aw/ediff-org-showhide(buf command &rest cmdargs)
  "If buffer exists and is orgmode then execute command"
  (if buf
      (if (eq (buffer-local-value 'major-mode (get-buffer buf)) 'org-mode)
          (save-excursion (set-buffer buf) (apply command cmdargs)))))

(defun aw/ediff-org-unfold-tree-element ()
  "Unfold tree at diff location"
  (aw/ediff-org-showhide ediff-buffer-A 'org-reveal)
  (aw/ediff-org-showhide ediff-buffer-B 'org-reveal)
  (aw/ediff-org-showhide ediff-buffer-C 'org-reveal))
;;
(defun aw/ediff-org-fold-tree ()
  "Fold tree back to top level"
  (aw/ediff-org-showhide ediff-buffer-A 'hide-sublevels 1)
  (aw/ediff-org-showhide ediff-buffer-B 'hide-sublevels 1)
  (aw/ediff-org-showhide ediff-buffer-C 'hide-sublevels 1))

(add-hook 'ediff-select-hook 'aw/ediff-org-unfold-tree-element)
(add-hook 'ediff-unselect-hook 'aw/ediff-org-fold-tree)

;; We can use a function to toggle how whitespace is treated in the
;;     diff. ([[http://www.reddit.com/r/emacs/comments/2513zo/ediff_tip_make_vertical_split_the_default/][Source]])

(defun ediff-toggle-whitespace-sensitivity ()
  "Toggle whitespace sensitivity for the current EDiff run.

This does not affect the global EDiff settings.  The function
automatically updates the diff to reflect the change."
  (interactive)
  (let ((post-update-message
         (if (string-match " ?-w$" ediff-actual-diff-options)
             (progn
               (setq ediff-actual-diff-options
                     (concat ediff-diff-options " " ediff-ignore-case-option)
                     ediff-actual-diff3-options
                     (concat ediff-diff3-options " " ediff-ignore-case-option3))
               "Whitespace sensitivity on")
           (setq ediff-actual-diff-options
                 (concat ediff-diff-options " " ediff-ignore-case-option " -w")
                 ediff-actual-diff3-options
                 (concat ediff-diff3-options " " ediff-ignore-case-option3 " -w"))
           "Whitespace sensitivity off")))
    (ediff-update-diffs)
    (message post-update-message)))

(add-hook 'ediff-keymap-setup-hook
          #'(lambda () (define-key ediff-mode-map [?W] 'ediff-toggle-whitespace-sensitivity)))

;; Projectile

;;    Projectile makes it easy to navigate files in a single project. A project
;;    is defined as any directory containing a .git/ or other VCS
;;    repository. We can manually define a project by adding an empty
;;    =.projectile= file to our directory.

(projectile-global-mode) ; Load Projectile everywhere
(setq projectile-completion-system 'helm)
(setq projectile-enable-caching t)
;;(diminish 'projectile-mode " P" )
(helm-projectile-on)

;; Terminals
   
;;    Multi-term makes working with many terminals a bit nicer. I can easily create
;;    and cycle through any number of terminals. There's also a "dedicated terminal"
;;    that I can pop up when needed.
   
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

(define-key custom-bindings-map (kbd "C-c t") 'multi-term-dedicated-toggle)
(define-key custom-bindings-map (kbd "C-c T") 'get-term)

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
(define-key custom-bindings-map (kbd "C-x p") 'proced)

;; Org-mode
  
;;   =Org-mode= makes up a massive part of my emacs usage.

(add-to-list 'auto-mode-alist '("\.txt\\'" . org-mode))

;; Expand a fold when trying to edit it.

(setq org-catch-invisible-edits 'show)

;; Agenda
   
;;    I keep my schedule with =org=agenda=.

(setq org-agenda-start-on-weekday nil              ; Show agenda from today.
      org-agenda-files '("~/Dropbox/org")          ; A list of agenda files.
      org-agenda-default-appointment-duration 60   ; 1 hour appointments
      org-agenda-span 1)                           ; Show only today by default

(define-key custom-bindings-map (kbd "C-c a") 'org-agenda-list)

;; Show the agenda buffer in a full frame.

;; (fullframe org-agenda-mode org-agenda-quit)

;; When editing org-files with source-blocks, we want the source blocks to
;;    be themed as they would in their native mode.

(setq org-src-fontify-natively t
      org-confirm-babel-evaluate nil)

;; This is quite an ugly fix for allowing code markup for expressions like
;;    ="this string"=, because the quotation marks causes problems.

(setcar (nthcdr 2 org-emphasis-regexp-components) " \t\n,")
(custom-set-variables `(org-emphasis-alist ',org-emphasis-alist))

;; TODO: Make =o= start a new header.



;; Babel

;;    Org-babel is awesome for literate programming, and it even works with
;;    compiled languages. To create C source blocks we just need to enable

(add-to-list 'org-babel-load-languages
             '(C . t))

(advice-add 'org-babel-C-ensure-main-wrap :override #'aw/org-c-src-main)

(defun aw/org-c-src-main (body)
  "Wrap BODY in a \"main\" function call if none exists."
  (if (string-match "^[ \t]*[intvod]+[ \t\n\r]*main[ \t]*(.*)" body)
      body
    (format "int main(int argc, char* argv[]) {\n%s\nreturn 0;\n}\n" body)))

;; Capturing

(setq org-default-notes-file (concat org-directory "/calendar.org"))
(define-key custom-bindings-map (kbd "C-c o") 'org-capture)

;; The capture buffer should start in insert state. Note that the usual function
;;    =evil-set-initial-state= doesn't work for this case. I'm pretty sure it's
;;    because =org-capture-mode= is only a minor mode, but I could be wrong.

(add-hook 'org-capture-mode-hook 'evil-insert-state)

;; Capture templates

;;     The list of templates should be empty to begin with.

(setq org-capture-templates '())



(add-to-list 'org-capture-templates
             '("s" "Scheduled Action"
               entry (file+datetree+prompt "~/Dropbox/org/calendar.org")
               "* %?\n%T\n" ))

(add-to-list 'org-capture-templates
             '("t" "Todo" entry (file+headline "~/Dropbox/org/calendar.org" "Todos")
              "* TODO %?\n  SCHEDULED: %t\n"))

;; One of the most common captures will be school assignments.

(add-to-list 'org-capture-templates
             '("1" "Assignment (CIS 381)"
               entry (file+datetree+prompt "~/Dropbox/org/calendar.org" "CIS 381")
               "* TODO %? :cis381:\nDEADLINE: %T\n" ))
(add-to-list 'org-capture-templates
             '("2" "Assignment (CIS 467)"
               entry (file+datetree+prompt "~/Dropbox/org/calendar.org" "CIS 467")
               "**** TODO %? :cis467:\nDEADLINE: %T\n" ))
(add-to-list 'org-capture-templates
             '("3" "Assignment (CIS 481)"
               entry (file+datetree+prompt "~/Dropbox/org/calendar.org" "CIS 481")
               "**** TODO %? :cis481:\nDEADLINE: %T\n" ))
(add-to-list 'org-capture-templates
             '("4" "Assignment (CIS 499)"
               entry (file+datetree+prompt "~/Dropbox/org/calendar.org" "CIS 499")
               "**** TODO %? :cis499:\nDEADLINE: %T\n" ))

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

;; Some default org keybindings could be a bit more evil.

(evil-define-key 'normal org-mode-map
  (kbd "M-h") 'org-metaleft
  (kbd "M-j") 'org-metadown
  (kbd "M-k") 'org-metaup
  (kbd "M-l") 'org-metaright)

;; Wrap-up
  
;;   We're ready to load the minor mode containing my global keybindings.

(define-minor-mode custom-bindings-mode
  "A mode that activates custom-bindings."
  t nil custom-bindings-map)

;; The moment of truth. How did we do on load time?

(defun aw/get-init-time ()
    (when window-system
      (let ((elapsed
             (float-time (time-subtract (current-time) emacs-start-time))))
        (message "Loading init.el...done (%.3fs)" elapsed))))

(add-hook 'after-init-hook 'aw/get-init-time)
