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

;;; Control modes

(defun divine-choose-initial-mode (&optional buffer)
  "Pick an appropriate initial mode for BUFFER.

If BUFFER has zero length, activate `divine-insert-mode',
otherwise `divine-normal-mode'.

Interactively, or if BUFFER isn't specified, default to (current-buffer)."
  (interactive)
  (unless buffer (setq buffer (current-buffer)))
  (save-excursion
    (set-buffer buffer)
    (if (eq (point-min) (point-max))
        (divine-insert-mode t)
      (divine-normal-mode t))))

;;; Normal mode

(divine-defmode divine-normal-mode
  "Normal mode for Divine."
  :cursor 'box)

;;;; Default keymap

;; Eat characters.
(define-key divine-normal-mode-map [remap self-insert-command] 'divine-fail)

;; Fundamentals
(define-key divine-normal-mode-map (kbd "0") 'divine-zero)
(define-key divine-normal-mode-map (kbd "1") 'digit-argument)
(define-key divine-normal-mode-map (kbd "2") 'digit-argument)
(define-key divine-normal-mode-map (kbd "3") 'digit-argument)
(define-key divine-normal-mode-map (kbd "4") 'digit-argument)
(define-key divine-normal-mode-map (kbd "5") 'digit-argument)
(define-key divine-normal-mode-map (kbd "6") 'digit-argument)
(define-key divine-normal-mode-map (kbd "7") 'digit-argument)
(define-key divine-normal-mode-map (kbd "8") 'digit-argument)
(define-key divine-normal-mode-map (kbd "9") 'digit-argument)
(define-key divine-normal-mode-map (kbd "-") 'digit-argument)
;; Character motion
(define-key divine-normal-mode-map (kbd "b") 'divine-char-backward)
(define-key divine-normal-mode-map (kbd "<left>") 'divine-char-left)
(define-key divine-normal-mode-map (kbd "f") 'divine-char-forward)
(define-key divine-normal-mode-map (kbd "<right>") 'divine-char-right)
(define-key divine-normal-mode-map (kbd "B") 'divine-word-backward)
(define-key divine-normal-mode-map (kbd "F") 'divine-word-forward)
;; (define-key divine-normal-mode-map (kbd "<spc>") 'activate-mark)
;; Line motion
(define-key divine-normal-mode-map (kbd "p") 'divine-line-backward)
(define-key divine-normal-mode-map (kbd "$") 'divine-line-end)
(define-key divine-normal-mode-map (kbd "a") 'divine-line-beginning)
(define-key divine-normal-mode-map (kbd "<down>") 'divine-line-forward)
(define-key divine-normal-mode-map (kbd "n") 'divine-line-forward)
(define-key divine-normal-mode-map (kbd "<up>") 'divine-line-backward)
;; Paragraph motion
(define-key divine-normal-mode-map (kbd "P") 'divine-paragraph-backward)
(define-key divine-normal-mode-map (kbd "N") 'divine-paragraph-forward)
;; Insertion
(define-key divine-normal-mode-map (kbd "a") 'divine-around-or-append)
(define-key divine-normal-mode-map (kbd "o") 'divine-open-line)
(define-key divine-normal-mode-map (kbd "c") 'divine-change)
(define-key divine-normal-mode-map (kbd "i") 'divine-insert-mode)
;; Killing
(define-key divine-normal-mode-map (kbd "d") 'divine-kill)
;; History
(define-key divine-normal-mode-map (kbd "u") 'undo)
(define-key divine-normal-mode-map (kbd "U") 'redo)
;; Switch to visual
(define-key divine-normal-mode-map (kbd "v") 'divine-visual-mode)
(define-key divine-normal-mode-map (kbd "V") 'divine-visual-line-mode)
(define-key divine-normal-mode-map (kbd "C-v") 'divine-visual-rect-mode)
;; Switch to Sexp
(define-key divine-normal-mode-map (kbd "l") 'divine-transient-sexp-mode)
(define-key divine-normal-mode-map (kbd "L") 'divine-sexp-mode)
;; Folds and Outline
"TODO"
;; Macros
(define-key divine-normal-mode-map (kbd "q") 'divine-start-or-end-macro)
(define-key divine-normal-mode-map (kbd "Q") 'divine-start-or-end-anonymous-macro)
(define-key divine-normal-mode-map (kbd "@") 'divine-play-macro)

(define-key divine-normal-mode-map (kbd "M-i") 'counsel-imenu)

;;; “g” mode

(divine-defmode divine-g-mode
  "A purely transient mode for the `g' command."
  :lighter divine-normal-mode-lighter)

;;; Insert mode

(divine-defmode divine-insert-mode
  "Emacs-like mode for Divine."
  :cursor 'bar)

;;;; Default keymap

(define-key divine-insert-mode-map [remap kill-word] 'divine-kill)
(define-key divine-insert-mode-map [remap keyboard-quit] 'divine-normal-mode)

;;; Emacs mode

(divine-defmode divine-emacs-mode
  "Strictly Emacs mode for Divine."
  :cursor 'bar)

;;;; Default keymap

(define-key divine-emacs-mode-map [remap keyboard-quit] 'divine-normal-mode)

;;; Visual mode

(divine-defmode divine-visual-mode
  "Emacs-like mode for Divine."
  :cursor 'hollow
  :cursor-color "LightSkyBlue")

(define-key divine-visual-mode-map (kbd "i") 'divine-insert-mode)

;;; Sexp mode

(divine-defmode divine-sexp-mode
  "S-Expressions mode for Divine."
  :cursor 'box
  :cursor-color "MediumVioletRed")

;;; Conclusion

(provide 'divine)

;;; divine.el ends here
