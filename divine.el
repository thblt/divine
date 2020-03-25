;;; divine.el --- Modal interface with text objects, or something close enough  -*- lexical-binding: t; coding: utf-8 -*-

;; Copyright (c) 2020 Thibault Polge <thibault@thb.lt>

;; Author: Thibault Polge <thibault@thb.lt>
;; Maintainer: Thibault Polge <thibault@thb.lt>
;;
;; Keywords: convenience
;; Homepage: https://github.com/thblt/divine
;; Version: pre-alpha

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

(defun divine--toggle-pending-operator-indicator ()
  ""
  (if (divine-pending-operator-p)
      (setq-local blink-cursor-interval .05)
    (setq-local blink-cursor-interval .5)))

;;; Control

(defun divine-start (&optional buffer)
  "Pick an appropriate initial mode for BUFFER.

Interactively, or if BUFFER isn't specified, default to (current-buffer)."
  (interactive)

  (setq-local blink-cursor-blinks 0
              blink-cursor-delay 0)

  (add-hook 'divine-pending-operator-hook 'divine--toggle-pending-operator-indicator)

  (with-current-buffer (or buffer (current-buffer))
    (cond ((eq (point-min) (point-max))
           (divine-insert-mode t))
          ;; Major modes that deactivate Divine
          ((member major-mode '(magit-status-mode pdf-view-mode mu4e-view-mode mu4e-headers-mode mu4e-main-mode))
           (divine-off-mode t))
          ;; Buffer names that deactivate Divine
          ((member (buffer-name) '("*mu4e-headers"))
           (divine-off-mode t))
          (t (divine-normal-mode t)))))

(defun divine-abort ()
  "Abort what needs to be aborted."
  (interactive)
  (cond ((region-active-p) (deactivate-mark))
        ((divine-abort-pending-operator))
        ((not divine-normal-mode) (divine-normal-mode t)))
  (divine--finalize)) ; @TODO Check for transient mode.

;;; Normal mode

(divine-defmode normal
  "Normal mode for Divine."
  :cursor 'box)

(defun divine-init-normal-keymap (&optional keymap)
  (unless keymap (setq keymap (make-keymap)))

  (define-key keymap [remap self-insert-command] 'divine-fail)
  ;; Fundamentals
  (define-key keymap (kbd "0") 'divine-zero)
  (define-key keymap (kbd "1") 'digit-argument)
  (define-key keymap (kbd "2") 'digit-argument)
  (define-key keymap (kbd "3") 'digit-argument)
  (define-key keymap (kbd "4") 'digit-argument)
  (define-key keymap (kbd "5") 'digit-argument)
  (define-key keymap (kbd "6") 'digit-argument)
  (define-key keymap (kbd "7") 'digit-argument)
  (define-key keymap (kbd "8") 'digit-argument)
  (define-key keymap (kbd "9") 'digit-argument)
  (define-key keymap (kbd "-") 'negative-argument)
  (define-key keymap (kbd "\"") 'divine-select-register)
  (define-key keymap [remap keyboard-quit] 'divine-abort)
  keymap)

;;;; Utilities

;;;; Default keymap

(divine-define-key 'normal "o" 'divine-open-line)
(divine-define-key 'normal "o" 'exchange-point-and-mark :state 'region-active)

(divine-init-normal-keymap divine-normal-mode-map)
;; Eat characters.
;; Character motion
(divine-define-key 'normal "b" 'backward-char)
(divine-define-key 'normal "<left>" 'left-char)
(divine-define-key 'normal "f" 'forward-char)
(divine-define-key 'normal "<right>" 'right-char)
(divine-define-key 'normal "B" 'divine-word-backward)
(divine-define-key 'normal "F" 'divine-word-forward)
;; (define-key divine-normal-mode-map (kbd "<spc>") 'activate-mark)
;; Lines
(divine-define-key 'normal "p" 'previous-line)
(divine-define-key 'normal "e" 'end-of-line)
(divine-define-key 'normal "<down>" 'next-line)
(divine-define-key 'normal "n" 'next-line)
(divine-define-key 'normal "<up>" 'previous-line)
;; Paragraph motion
(divine-define-key 'normal "P" 'divine-paragraph-backward)
(divine-define-key 'normal "N" 'divine-paragraph-forward)
;; Buffer motion
(divine-define-key 'normal "g" 'divine-goto-line :state 'numeric-argument)
(divine-define-key 'normal "g" 'divine-transient-g-mode)
(divine-define-key 'normal "G" 'divine-end-of-buffer)
;; Searche
(divine-define-key 'normal "t" 'divine-find-char-forward-before)
(divine-define-key 'normal "T" 'divine-find-char-backward-before)
(divine-define-key 'normal "l" 'divine-find-char-forward-after)
(divine-define-key 'normal "L" 'divine-find-char-backward-after)
;; Insertion
(divine-define-key 'normal "a" 'divine-bol-or-around)
(divine-define-key 'normal "o" 'divine-open-line)
(divine-define-key 'normal "c" 'divine-change)
(divine-define-key 'normal "i" 'divine-insert-or-inside)
;; Killing and yanking text
(divine-define-key 'normal "d" 'divine-kill)
(divine-define-key 'normal "d" 'divine-whole-line :state 'repeated-binding)
(divine-define-key 'normal "x" 'delete-char)
(divine-define-key 'normal "w" 'divine-text-save)
(divine-define-key 'normal "w" 'divine-text-save)
(divine-define-key 'normal "w" 'divine-whole-line :state 'repeated-binding)
(divine-define-key 'normal "y" 'divine-yank)
(divine-define-key 'normal "r" 'divine-char-replace)
;; Complex manipulations
(divine-define-key 'normal "j" 'divine-line-join)
(divine-define-key 'normal "=" 'divine-indent-region)
(divine-define-key 'normal "SPC w" 'divine-wrap)

;; History
(divine-define-key 'normal "u" 'undo)
(divine-define-key 'normal "U" 'redo)
;; Switch to Sexp
(divine-define-key 'normal "l" 'divine-transient-sexp-mode)
(divine-define-key 'normal "L" 'divine-sexp-mode)
;; Folds and Outline
(divine-define-key 'normal "z" 'divine-transient-folds-mode)
(divine-define-key 'normal "Z" 'divine-folds-mode)
;; Region editing
(divine-define-key 'normal "m" 'divine-mark-toggle)
(divine-define-key 'normal "M" 'divine-mark-rectangle-toggle)

;; Macros
(divine-define-key 'normal "Q" 'divine-start-or-end-macro)
(divine-define-key 'normal "q" 'divine-play-macro)
(divine-define-key 'normal "M-i" 'counsel-imenu)

;; Leader-ish bindings
(divine-define-key 'normal "SPC s" 'save-buffer)
(divine-define-key 'normal "SPC f" 'ffap)
(divine-define-key 'normal "SPC p p" 'projectile-switch-project)
(divine-define-key 'normal "SPC p f" 'projectile-find-file)

;;; “g” mode

(divine-defmode g
  "A purely transient mode for the `g' command."
  :cursor 'hollow)

(divine-init-normal-keymap divine-g-mode-map)

(divine-define-key 'g "g" 'divine-beginning-of-buffer)
(divine-define-key 'g "l" 'divine-transient-folds-mode)

;;; “z” mode

(divine-defmode folds
  "A mode to work on folds and outlines."
  :cursor-color "ForestGreen")

(divine-init-normal-keymap divine-folds-mode-map)
(divine-define-key 'folds "p" 'outline-previous-visible-heading)
(divine-define-key 'folds "n" 'outline-next-visible-heading)
(divine-define-key 'folds "P" 'outline-move-subtree-up)
(divine-define-key 'folds "N" 'outline-move-subtree-down)
(divine-define-key 'folds "f" 'outline-demote)
(divine-define-key 'folds "b" 'outline-promote)
(divine-define-key 'folds "r" 'outshine-narrow-to-subtree)
(divine-define-key 'folds "c" 'outshine-cycle)
(divine-define-key 'folds "C" 'outshine-cycle-buffer)
(divine-define-key 'folds "o" 'outline-hide-other)

;;; Insert mode

(divine-defmode insert
  "Emacs-like mode for Divine."
  :cursor 'bar)

;;; Off mode

(divine-defmode off
  "Stricly like Emacs Divine mode."
  :lighter "<Off>"
  :cursor 'bar)

;;;; Default keymap

(divine-define-key 'insert [remap keyboard-quit] 'divine-normal-mode)

;;; Sexp mode

(divine-defmode sexp
  "S-Expressions mode for Divine."
  :cursor 'box
  :cursor-color "MediumVioletRed")

(divine-init-normal-keymap divine-sexp-mode-map)

(divine-define-key 'sexp "w" 'divine-wrap)
(divine-define-key 'sexp "f" 'sp-forward-sexp)
(divine-define-key 'sexp "b" 'sp-backward-sexp)
(divine-define-key 'sexp "n" 'sp-up-sexp)
(divine-define-key 'sexp "p" 'sp-down-sexp)
(divine-define-key 'sexp "N" 'sp-backward-up-sexp)
(divine-define-key 'sexp "P" 'sp-backward-down-sexp)

;;; Conclusion

(provide 'divine)

;;; divine.el ends here
