;;; fsp-mode.el --- major mode for editing FSP descriptions

;; Copyright (C) 2009 ESBEN Andreasen <esbenandreasen@gmail.com>

;; Authors: Esben Andreasen <esbenandreasen@gmail.com>

;; Keywords: FSP LTSA FSP Labeled Transition System

;; This file is not an official part of Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; if not, you can either send email to this
;; program's maintainer or write to: The Free Software Foundation,
;; Inc.; 59 Temple Place, Suite 330; Boston, MA 02111-1307, USA.

;;; Commentary:

;; Created to supply a better editing option than the one
;; supplied by the LTSA-tool and the somewhat broken
;; Eclipse-plugin.

;; INSTALLATION

;; place this file somewhere, edit the line below
;; accordingly, and insert both of the lines below into your
;; ~/.emacs -file. Files ending in .fsp should now be using
;; the fsp-mode.


;;(autoload 'fsp-mode "ABSOLUTE-DIRECTORY-OF-THE-FILE/fsp-mode.el")
;;(add-to-list 'auto-mode-alist '("\\.fsp$" . fsp-mode))

;; FEATURES
;; * indentation
;; * highlightning
;; * minor shortcuts
;; * * copy whole buffer to OS clipboard: \Cc-c
;; * * insert balanced parens and set point in the middle: \Cc-p

(defvar fsp-indent-offset 4
  "Indentation offset for `fsp-mode'.")
(defvar fsp-mode-map nil "Keymap for fsp-mode")

(defun generic-indent-line ()
  "Indent current line for any balanced-paren-mode'."
  (interactive)
  (let ((indent-col 0)
        (indentation-increasers "[[{(]")
        (indentation-decreasers "[]})]")
        )
    (save-excursion
      (beginning-of-line)
      (condition-case nil
          (while t
            (backward-up-list 1)
            (when (looking-at indentation-increasers)
              (setq indent-col (+ indent-col fsp-indent-offset))))
        (error nil)))
    (save-excursion
      (back-to-indentation)
      (when (and (looking-at indentation-decreasers) (>= indent-col fsp-indent-offset))
        (setq indent-col (- indent-col fsp-indent-offset))))
    (indent-line-to indent-col)))



(define-generic-mode 'fsp-mode
  '("//" ("/*" . "*/")) ;; comments
  '("END" "STOP" "ERROR" "const" "property" "range" "if" "then" "else" "forall" "when" "progress" 
    "fluent" "initially" "U" "W" "X" "<>" "[]" "rigid" "assert" "exists" "set") ;; keywords
  '(
    ("||" . 'font-lock-builtin-face)
    ("|" . 'font-lock-builtin-face)
    ("\\.\\." . 'font-lock-builtin-face)
    ("::" . 'font-lock-builtin-face)
    (":" . 'font-lock-builtin-face)
    ("<->" . 'font-lock-builtin-face)
    ("->" . 'font-lock-builtin-face)
    (">>" . 'font-lock-builtin-face)
    ("<<" . 'font-lock-builtin-face)
    ("&&" . 'font-lock-builtin-face)
    ("!" . 'font-lock-builtin-face)
    ("[[:upper:]]?[[:lower:]]+"  . 'font-lock-warning-face)
    ("[[:upper:]]+"  . 'font-lock-builtin-face)
    ("+{.*}" . 'font-lock-preprocessor-face)
    ("\\\\{.*}" . 'font-lock-preprocessor-face)
    ("/{.*}" . 'font-lock-preprocessor-face)
    ("@{.*}" . 'font-lock-preprocessor-face)
    ("@(.*)" . 'font-lock-preprocessor-face)
    ("#(.*)" . 'font-lock-preprocessor-face)
    )
  '("\\.fsp\\'") ;; filename suffixes
  '( ;; env setup
    (lambda () 
      (make-local-variable 'fsp-indent-offset)
      (make-local-variable 'generic-indent-line)
      (set 'indent-line-function 'generic-indent-line)
      (when (not fsp-mode-map) ; if it is not already defined
        (setq fsp-mode-map (make-sparse-keymap))
        (define-key fsp-mode-map  (kbd "RET") 'newline-and-indent)
        (define-key fsp-mode-map  (kbd "M-RET") 
          (lambda () (interactive) (newline-and-indent) (insert "| ")) 
          )
        (define-key fsp-mode-map  "\C-cp"
          (lambda () (interactive) 
            (insert "(") 
            (newline-and-indent) 
            (newline-and-indent) (insert ")") 
            (end-of-line 0)
            ) 
          )
        (define-key fsp-mode-map  "\C-cc"
          (lambda () (interactive) 
            (clipboard-kill-ring-save (point-min) (point-max))
            (message "Buffer copied to clipboard")
            )
          )
        )
      (use-local-map fsp-mode-map)
      ))
  "Major mode for FSP highlighting.")

(provide 'fsp-mode)
