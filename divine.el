;;; divine.el --- Modal interface with text objects, or something close enough  -*- lexical-binding: t; coding: utf-8 -*p

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

;;; Control

(defun divine-choose-initial-mode (&optional buffer)
  "Pick an appropriate initial mode for BUFFER.

Interactively, or if BUFFER isn't specified, default to (current-buffer)."
  (interactive)
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
        ((not divine-normal-mode) (divine-normal-mode t)))
  (divine--finalize)) ; @TODO Check for transient mode.

;;; Normal mode

(divine-defmode divine-normal-mode
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

(divine-init-normal-keymap divine-normal-mode-map)
;; Eat characters.
;; Character motion
(define-key divine-normal-mode-map (kbd "b") 'divine-char-backward)
(define-key divine-normal-mode-map (kbd "<left>") 'divine-char-left)
(define-key divine-normal-mode-map (kbd "f") 'divine-char-forward)
(define-key divine-normal-mode-map (kbd "<right>") 'divine-char-right)
(define-key divine-normal-mode-map (kbd "B") 'divine-word-backward)
(define-key divine-normal-mode-map (kbd "F") 'divine-word-forward)
;; (define-key divine-normal-mode-map (kbd "<spc>") 'activate-mark)
;; Lines
(define-key divine-normal-mode-map (kbd "p") 'divine-line-backward)
(define-key divine-normal-mode-map (kbd "$") 'divine-line-end)
(define-key divine-normal-mode-map (kbd "a") 'divine-line-beginning)
(define-key divine-normal-mode-map (kbd "<down>") 'divine-line-forward)
(define-key divine-normal-mode-map (kbd "n") 'divine-line-forward)
(define-key divine-normal-mode-map (kbd "<up>") 'divine-line-backward)
(define-key divine-normal-mode-map (kbd "j") 'divine-join-line)
;; Paragraph motion
(define-key divine-normal-mode-map (kbd "P") 'divine-paragraph-backward)
(define-key divine-normal-mode-map (kbd "N") 'divine-paragraph-forward)
;; Buffer motion
(define-key divine-normal-mode-map (kbd "g") 'divine-goto-line-or-g-mode)
;; Searche
(define-key divine-normal-mode-map (kbd "t") 'divine-find-char-forward-before)
(define-key divine-normal-mode-map (kbd "T") 'divine-find-char-backward-before)
(define-key divine-normal-mode-map (kbd "l") 'divine-find-char-forward-after)
(define-key divine-normal-mode-map (kbd "L") 'divine-find-char-backward-after)

;; Insertion
(define-key divine-normal-mode-map (kbd "a") 'divine-bol-or-around)
(define-key divine-normal-mode-map (kbd "o") 'divine-open-line)
(define-key divine-normal-mode-map (kbd "c") 'divine-change)
(define-key divine-normal-mode-map (kbd "i") 'divine-insert-or-inside)
;; Killing and yanking text
(define-key divine-normal-mode-map (kbd "d") 'divine-kill)
(define-key divine-normal-mode-map (kbd "x") 'delete-char)
(define-key divine-normal-mode-map (kbd "w") 'divine-text-save)
(define-key divine-normal-mode-map (kbd "w") 'divine-text-save)
(define-key divine-normal-mode-map (kbd "y") 'divine-yank)
(define-key divine-normal-mode-map (kbd "r") 'divine-char-replace)

;; History
(define-key divine-normal-mode-map (kbd "u") 'undo)
(define-key divine-normal-mode-map (kbd "U") 'redo)
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
(define-key divine-normal-mode-map (kbd "Q") 'divine-start-or-end-macro)
(define-key divine-normal-mode-map (kbd "q") 'divine-play-macro)

(define-key divine-normal-mode-map (kbd "M-i") 'counsel-imenu)

(define-key divine-normal-mode-map (kbd "SPC s") 'save-buffer)


;;; “g” mode

(divine-defmode divine-g-mode
  "A purely transient mode for the `g' command.")

(divine-init-normal-keymap divine-g-mode-map)

(define-key divine-g-mode-map (kbd "l") 'divine-transient-folds-mode)

;;; “z” mode

(divine-defmode divine-folds-mode
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

(divine-defmode divine-insert-mode
  "Emacs-like mode for Divine."
  :cursor 'bar)

;;; Off mode

(divine-defmode divine-off-mode
  "Stricly like Emacs Divine mode."
  :lighter "<Off>"
  :cursor 'bar)

;;;; Default keymap

(define-key divine-insert-mode-map [remap keyboard-quit] 'divine-normal-mode)

;;; Sexp mode

(divine-defmode divine-sexp-mode
  "S-Expressions mode for Divine."
  :cursor 'box
  :cursor-color "MediumVioletRed")

(divine-init-normal-keymap divine-sexp-mode-map)

;;; Conclusion

(provide 'divine)

;;; divine.el ends here
