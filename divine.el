;;; divine.el --- Modal interface with text objects, or something close enough  -*- lexical-binding: t; coding: utf-8 -*-

;; Copyright (c) 2020-2021 Thibault Polge <thibault@thb.lt>

;; Author: Thibault Polge <thibault@thb.lt>
;; Maintainer: Thibault Polge <thibault@thb.lt>
;;
;; Keywords: convenience
;; Homepage: https://github.com/thblt/divine
;; Version: 0.0

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This module provides the standard Divine interface.

;;; Code:

(require 'divine-core)
(require 'divine-commands)

;;; Misc

(defun divine-toggle-pending-operator-indicator ()
  "Blink the cursor faster if Divine is in pending operator
state, restore blink speed otherwise."
  (if (divine-pending-operator-p)
      (setq-local blink-cursor-interval .05)
    (setq-local blink-cursor-interval .5)))

;;; Control

(defcustom divine-initial-mode-rules
  '((magit-status-mode divine-off-mode)
    (pdf-view-mode divine-off-mode)
    (mu4e-view-mode divine-off-mode)
    (mu4e-headers-mode divine-off-mode)
    (string= (buffer-name) "*mu4e-headers" divine-off-mode)
    ;; mu4e headers buffers don't start in mu4e-headers-mode, so we
    ;; also need the above rule
    (mu4e-main-mode divine-off-mode)
    ((and (not buffer-read-only)
          (eq (point-min) (point-max)))
     divine-insert-mode)
    (buffer-read-only divine-insert-mode)
    (t divine-normal-mode))
  "TODO"
  :group 'divine )

(defun divine--start-eval-rule (rule)
  "Evaluate a single entry in the form of
  `divine-initial-mode-rules'."
  (let ((pred (car rule)))
    (when (eval
           (if (symbolp pred)
               `(bound-and-true-p ,pred)
             pred))
      (cdr rule))))

(defun divine-start (&optional buffer)
  "Pick an appropriate initial mode for BUFFER.

Interactively, or if BUFFER isn't specified, default to (current-buffer)."
  (interactive)

  (setq-local blink-cursor-blinks 0
              blink-cursor-delay 0)

  (add-hook 'divine-pending-operator-hook 'divine-toggle-pending-operator-indicator)

  (with-current-buffer (or buffer (current-buffer))
    (if-let ((func (cl-some 'divine--start-eval-rule divine-initial-mode-rules)))
        (funcall (car func))
      (error "Traversal of `divine-initial-mode-rules' ended without an initial mode."))))

(defun divine-abort ()
  "Do the first of these things whose condition holds:
 - If region is active: deactivate the mark.
 - If a transient mode is activated, return to the caller mode. @FIXME Unimplemented.
 - If an operator is pending: unset it, and clear state.
 - If a non-normal mode is activated, return to normal mode.
 - Othewise, clear state."
  (interactive)
  (cond ((region-active-p) (deactivate-mark))
        ((divine-pending-operator-p) (divine-operator-abort))
        ;; @FIXME Implement transient mode deactivation.
        ((not divine-normal-mode) (divine-normal-mode))
        (t (divine-clear-state))))

;;; Utility for normal modes

(defun divine-init-normal-keymap (mode)
  (define-key mode [remap self-insert-command] 'undefined)
  (define-key mode "0" 'divine-zero)
  (define-key mode "0" 'beginning-of-line)
  (define-key mode "1" 'digit-argument)
  (define-key mode "2" 'digit-argument)
  (define-key mode "3" 'digit-argument)
  (define-key mode "4" 'digit-argument)
  (define-key mode "5" 'digit-argument)
  (define-key mode "6" 'digit-argument)
  (define-key mode "7" 'digit-argument)
  (define-key mode "8" 'digit-argument)
  (define-key mode "9" 'digit-argument)
  (define-key mode "-" 'negative-argument)
  (define-key mode "\"" 'divine-select-register)
  (define-key mode [remap keyboard-quit] 'divine-abort))

;;; Normal mode

(divine-defmode normal
  "Normal mode for Divine."
  :cursor 'box)

;;;; Default keymap

(define-key divine-normal-mode-map "o"
  (defun divine-o ()
    "Exchange point and mark if region is active and non-empty, otherwise open line after point."
    (if (and (region-active-p)
             (not (eq (point) (mark))))
        (exchange-point-and-mark)
      (divine-open-line))))

(divine-init-normal-keymap divine-normal-mode-map)
;; Eat characters.
;; Character motion
(define-key divine-normal-mode-map (kbd "B") 'backward-char)
(define-key divine-normal-mode-map (kbd "<left>") 'left-char)
(define-key divine-normal-mode-map (kbd "F") 'forward-char)
(define-key divine-normal-mode-map (kbd "<right>") 'right-char)
(define-key divine-normal-mode-map (kbd "b") 'divine-word-backward)
(define-key divine-normal-mode-map (kbd "f") 'divine-word-forward)
;; (define-key divine-normal-mode-map (kbd "<spc>") 'activate-mark)
;; Lines
(define-key divine-normal-mode-map (kbd "^") 'divine-line-beginning)
;;(divine-define-key 'normal "a" 'divine-scope-around-select :state 'accept-scope)
(define-key divine-normal-mode-map (kbd "$") 'end-of-line)
(define-key divine-normal-mode-map (kbd "p") 'previous-line)
(define-key divine-normal-mode-map (kbd "<up>") 'divine-line-backward)
(define-key divine-normal-mode-map (kbd "<down>") 'divine-line-forward)
(define-key divine-normal-mode-map (kbd "n") 'divine-line-forward)
;; Paragraph motion
(define-key divine-normal-mode-map (kbd "P") 'backward-paragraph)
(define-key divine-normal-mode-map (kbd "N") 'forward-paragraph)
;; Buffer motion
;; FIXME (define-key divine-normal-mode-map (kbd "g") 'divine-goto-line :state 'numeric-argument)
(define-key divine-normal-mode-map (kbd "g") (defun divine-goto-or-g-mode ()
                                               (interactive)
                                               (message "NA? %s" (divine-numeric-argument-p 'noconsume))
                                               (if (divine-numeric-argument-p 'noconsume)
                                                   (divine-then 'divine-goto-line)
                                                 (divine-g-mode))))
(define-key divine-normal-mode-map (kbd "G") 'end-of-buffer)
;; Other basic motion
(define-key divine-normal-mode-map (kbd "a") 'backward-sentence)
(define-key divine-normal-mode-map (kbd "e") 'forward-sentence)
;; Search
(define-key divine-normal-mode-map (kbd "t") 'divine-find-char-forward-before)
(define-key divine-normal-mode-map (kbd "T") 'divine-find-char-backward-before)
(define-key divine-normal-mode-map (kbd "r") 'divine-find-char-forward-after)
(define-key divine-normal-mode-map (kbd "R") 'divine-find-char-backward-after)
(define-key divine-normal-mode-map (kbd "s") 'isearch-forward)
;; (divine-define-key 'normal "S" 'isearch-forward-regexp)
(define-key divine-normal-mode-map (kbd "S") 'isearch-backward)
                                        ; (divine-define-key 'normal "R" 'isearch-backward-regexp)
;; Insertion
(define-key divine-normal-mode-map (kbd "RET") 'divine-line-open-forward)
(define-key divine-normal-mode-map (kbd "M-RET") 'divine-line-open-backward)
(define-key divine-normal-mode-map (kbd "c") 'divine-change)
(define-key divine-normal-mode-map (kbd "i") 'divine-insert-mode)
;; @FIXME (define-key divine-normal-mode-map (kbd "i") 'divine-scope-step)
;; Killing and yanking text
(define-key divine-normal-mode-map (kbd "k") 'divine-kill)
(define-key divine-normal-mode-map (kbd "d") 'delete-char)
(define-key divine-normal-mode-map (kbd "w") 'divine-text-save)
(define-key divine-normal-mode-map (kbd "y") 'divine-yank)
(define-key divine-normal-mode-map (kbd "r") 'divine-char-replace)
;; Complex manipulations
(define-key divine-normal-mode-map (kbd "j") 'divine-line-join)
(define-key divine-normal-mode-map (kbd "=") 'divine-indent-region)
(define-key divine-normal-mode-map (kbd "SPC w") 'divine-wrap)
(define-key divine-normal-mode-map (kbd "/") 'undo)
;; (define-key divine-normal-mode-map (kbd "U") 'redo)
;; Switch to Sexp
(define-key divine-normal-mode-map (kbd "l") 'divine-transient-sexp-mode)
(define-key divine-normal-mode-map (kbd "L") 'divine-sexp-mode)
;; Folds and Outline
(define-key divine-normal-mode-map (kbd "z") 'divine-transient-folds-mode)
(define-key divine-normal-mode-map (kbd "Z") 'divine-folds-mode)
;; Region editing
(define-key divine-normal-mode-map (kbd "m") 'divine-mark-toggle)
(define-key divine-normal-mode-map (kbd "M") 'divine-mark-rectangle-toggle)
;; Macros
(define-key divine-normal-mode-map (kbd "Q") 'divine-macro-start)
;; @FIXME (define-key divine-normal-mode-map (kbd "Q") 'divine-macro-end :mode 'defining-kbd-macro)
(define-key divine-normal-mode-map (kbd "q") 'divine-macro-call)
(define-key divine-normal-mode-map (kbd "M-i") 'counsel-imenu)
;ish bindings
(define-key divine-normal-mode-map (kbd "SPC s") 'save-buffer)
(define-key divine-normal-mode-map (kbd "SPC o") 'divine-sort-lines)
(define-key divine-normal-mode-map (kbd "SPC p p") 'projectile-switch-project)
(define-key divine-normal-mode-map (kbd "SPC p f") 'projectile-find-file)

;;; “g” mode

(divine-defmode g
  "A purely transient mode for the `g' command."
  :cursor 'hollow)

(divine-init-normal-keymap divine-g-mode-map)

(define-key divine-g-mode-map (kbd "g") 'divine-buffer-beginning)
(define-key divine-g-mode-map (kbd "G") 'divine-buffer-end)
(define-key divine-g-mode-map (kbd "l") 'divine-transient-folds-mode)

;;; folds mode (z)

(divine-defmode folds
  "A mode to work on folds and outlines."
  :cursor-color "ForestGreen")

(divine-init-normal-keymap divine-folds-mode-map)

(define-key divine-folds-mode-map (kbd "p") 'outline-previous-visible-heading)
(define-key divine-folds-mode-map (kbd "n") 'outline-next-visible-heading)
(define-key divine-folds-mode-map (kbd "P") 'outline-move-subtree-up)
(define-key divine-folds-mode-map (kbd "N") 'outline-move-subtree-down)
(define-key divine-folds-mode-map (kbd "f") 'outline-demote)
(define-key divine-folds-mode-map (kbd "b") 'outline-promote)
(define-key divine-folds-mode-map (kbd "r") 'outshine-narrow-to-subtree)
(define-key divine-folds-mode-map (kbd "c") 'outshine-cycle)
(define-key divine-folds-mode-map (kbd "C") 'outshine-cycle-buffer)
(define-key divine-folds-mode-map (kbd "o") 'outline-hide-other)

;;; Insert mode

(divine-defmode insert
  "Emacs-like mode for Divine."
  :cursor 'bar)

(define-key divine-insert-mode-map [remap keyboard-quit] 'divine-abort)

;;; Off mode

(divine-defmode off
  "Stricly like Emacs Divine mode."
  :lighter "<Off>"
  :cursor 'bar)

;;; Sexp mode

(divine-defmode sexp
  "S-Expressions mode for Divine."
  :cursor 'box
  :cursor-color "MediumVioletRed")

(divine-init-normal-keymap divine-sexp-mode-map)

(define-key divine-sexp-mode-map (kbd "s") 'divine-wrap)
(define-key divine-sexp-mode-map (kbd "f") 'sp-forward-sexp)
(define-key divine-sexp-mode-map (kbd "b") 'sp-backward-sexp)
(define-key divine-sexp-mode-map (kbd "n") 'sp-up-sexp)
(define-key divine-sexp-mode-map (kbd "p") 'sp-down-sexp)
(define-key divine-sexp-mode-map (kbd "N") 'sp-backward-up-sexp)
(define-key divine-sexp-mode-map (kbd "P") 'sp-backward-down-sexp)
(define-key divine-sexp-mode-map (kbd "a") 'sp-end-of-previous-sexp)
(define-key divine-sexp-mode-map (kbd "e") 'sp-beginning-of-next-sexp)

;;; Conclusion

(provide 'divine)

;;; divine.el ends here
