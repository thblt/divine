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

(define-minor-mode divine-mode
  "Divine, a modal interface with text objects, or something
  close enough."
  :lighter " DivineControl"
  (if divine-mode
      ;; Enter
      (divine-choose-initial-mode)
    ;; Leave
    (divine--disable-modes nil)))
(defvar divine-mode-line " INSERT » ")
(define-globalized-minor-mode divine-global-mode divine-mode divine-mode)

(defun divine-start ()
  "Start Divine in the current buffer."
  (funcall divine-initial-mode-function))

(defun divine-p (&optional buffer)
  "Return non-nil if Divine is active in BUFFER."
  (unless buffer (setq buffer (current-buffer)))
  (save-excursion
    (set-buffer buffer)
    divine--active-mode))

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

;;;; Predicates


;;;; Default keymap

(define-key divine-normal-mode-map (kbd "M-i") 'divine-insert-mode)
;; Fundamentals

;; <oni-on-ion> thblt, i think your digit-argument thing can be macro'd to a "0123456789" loop.
;; <oni-on-ion> really all of them -- why not (let ((x divine-normal-map)) (loop... (define-key x ...)))) ?
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
;; Eat characters.
(define-key divine-normal-mode-map [remap self-insert-command] 'divine-fail)
;; Character motion
(define-key divine-normal-mode-map (kbd "b") 'divine-backward-char)
(define-key divine-normal-mode-map (kbd "B") 'backward-word)
(define-key divine-normal-mode-map (kbd "f") 'divine-forward-char)
(define-key divine-normal-mode-map (kbd "F") 'forward-word)
(define-key divine-normal-mode-map (kbd "<spc>") 'activate-mark)
;; Line motion
(define-key divine-normal-mode-map (kbd "p") 'previous-line)
(define-key divine-normal-mode-map (kbd "n") 'next-line)
(define-key divine-normal-mode-map (kbd "$") 'divine-end-of-line)
(define-key divine-normal-mode-map (kbd "a") 'divine-beginning-of-line)

;; Paragraph motion
(define-key divine-normal-mode-map (kbd "P") 'backward-paragraph)
(define-key divine-normal-mode-map (kbd "N") 'forward-paragraph)
;; Killing
(define-key divine-normal-mode-map (kbd "d") 'divine-kill)
;; History
(define-key divine-normal-mode-map (kbd "u") 'undo)
(define-key divine-normal-mode-map (kbd "U") 'redo)
;; Switch to insert (direct switch or through deletion)
(define-key divine-normal-mode-map (kbd "o") 'divine-open-line)
(define-key divine-normal-mode-map (kbd "c") 'divine-change)
(define-key divine-normal-mode-map (kbd "i") 'divine-insert-mode)
(define-key divine-normal-mode-map (kbd "<esc>") 'divine-insert-mode)
;; Switch to visual
(define-key divine-normal-mode-map (kbd "v") 'divine-visual-mode)
(define-key divine-normal-mode-map (kbd "V") 'divine-visual-line-mode)
(define-key divine-normal-mode-map (kbd "C-v") 'divine-visual-rect-mode)
;; Switch to Lisp
(define-key divine-normal-mode-map (kbd "l") 'divine-lisp-transient-mode)
(define-key divine-normal-mode-map (kbd "L") 'divine-lisp-mode)
;; Folds
"TODO"
;; Macros
(define-key divine-normal-mode-map (kbd "q") 'divine-start-or-end-macro)
(define-key divine-normal-mode-map (kbd "Q") 'divine-start-or-end-anonymous-macro)
(define-key divine-normal-mode-map (kbd "@") 'divine-play-macro)

(define-key divine-normal-mode-map (kbd "M-i") 'counsel-imenu)

;;; “g” mode

(divine-defmode divine-g-mode
  "A purely transient mode for the `g' command."
  :lighter divine-normal-mode-lighter
  )

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

;;; Lisp mode

(divine-defmode divine-lisp-mode
  "Lisp mode for Divine."
  :cursor 'box
  :cursor-color "MediumVioletRed")

;;; Conclusion

(provide 'divine)

;;; divine.el ends here
