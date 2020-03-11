;;; Divine --- Emacs modal interface, with text objecs or something close enough

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

;; See README.md

;;; Code:

;;; Core

;;;; Variables

(defvar divine-modes nil
  "List of known divine modes")

(defvar divine-default-cursor-color nil
  "Default cursor color for modes that don't specify it.  If nil, use the foreground color of the default face.")

(defvar divine-default-cursor 'box
  "Default cursor style for modes that don't specify it.")

(defvar divine-empty-buffers-start-in-insert-mode t
  "When non-nil, buffer of zero length start in insert mode instead of normal")

;;;; Macros

(cl-defmacro divine-defmode (name lighter docstring &key cursor cursor-color init finalize)
  "Define the Divine mode NAME, with documentation DOCSTRING."
  (declare (indent 2))
  (let ((cursor-variable (intern (format "%s-cursor" name)))
        (cursor-color-variable (intern (format "%s-cursor-color" name)))
        (map-variable (intern (format "%s-map" name))))
    `(progn
       (defvar ,cursor-variable ,cursor
         ,(format "Cursor style for %s." name))
       (defvar ,cursor-color-variable ,cursor-color
         ,(format "Cursor color for %s." name))
       (defvar ,map-variable (make-keymap)
         ,(format "Keymap for %s" name))
       (add-to-list 'divine-modes ',name)
       (define-minor-mode ,name
         ,docstring
         :lighter ,lighter
         :keymap ,map-variable
         (when ,name
           (setq-local cursor-type (or ,cursor-variable ,divine-default-cursor)))
         (set-cursor-color (or ,cursor-color-variable divine-default-cursor-color (face-attribute 'default :foreground))))
       (divine--disable-others ',name))))

;;;; Functions

(defun divine (arg)
  "Start Divine.

With an argument, start in insert mode instead of normal mode."
  (interactive "p")
  (if (and divine-empty-buffers-start-in-insert-mode
           (eq (point-max) (point-min)))
      (divine-insert-mode)
    (divine-normal-mode)))

(defun divine--disable-others (except)
  "Disable all modes in `divine-modes' except EXCEPT."
  (dolist (mode divine-modes)
    (unless (eq mode except)
      (funcall mode 0))))

;;; Normal mode

;;;; Declaration

(divine-defmode divine-normal-mode " Divine <N>"
  "Normal mode for Divine."
  :cursor 'box)

;;;; Macros

(cl-defmacro divine-defcommand (name docstring object &body body)
  "Define a Divine action NAME with doc DOCSTRING.

If OBJECT is non-nil, and this action is invoked twice in an
immmediate sequence, it is run to create the region.  This allows
to create commands like vim's dd.

BODY is the code of the action.  It should always operate on an active region."
  (declare (indent defun))
  `(defun ,name (arg)
     (interactive "p")
     ,docstring
     (when (eq divine-command ,name)
       (activate-mark)
       (call-interactively ,object))
     (if (and (region-active-p)
              (not (eq (region-beginning) (region-end))))
         (progn ,@body
                (setq divine--command nil))
       (unless (region-active-p)
         (push-mark)
         (activate-mark))
       (setq divine--command ',name))))

(defmacro divine-def-object (name docstring &rest body)
  "Define a Divine text object NAME with doc DOCSTRING.

When invoked, BODY is evaluated with ARG set to the value of the
digit argument."
  (declare (indent defun))
  `(defun ,name (arg)
     ,docstring
     (interactive "p")
     ,@body
     (when divine--command
       (funcall divine--command nil))
     )
  )

(defmacro divine-wrap-object (name motion-command)
  "Create a function called NAME that will run MOTION-COMMAND
and, if necessary, execute the activated divine command."

  `(defun ,name (arg)
     ,(format "Divine textobject wrapper around `%s', which see." name)
     (interactive "p")
     (call-interactively ',motion-command)
     (when divine--command
       (call-interactively divine--command))))

;;;; Objects

;; A text object, in divine, is roughly a motion command.

(divine-def-object divine-whole-line-object
  "Set mark at the first character of the current-line, and point at the last.

This is not meant for use as a direct command, but as the default object for repeatable commands."
  (beginning-of-line)
  (set-mark (point))
  (end-of-line arg))


(divine-wrap-object divine-end-of-line end-of-line)
(divine--defobject divine-beginning-of-line beginning-of-line)

(divine--defaction divine-kill
                   (call-interactively 'kill-region))

;;;; Commands

(divine-defcommand divine-kill-command divine-whole-line-object
  "Kill the selected object"
  (kill-region (region-beginning) (region-end)))

;;;; Default keymap

(define-key divine-normal-mode-map (kbd "M-i") 'divine-insert-mode)
;; Fundamentals
; <oni-on-ion> thblt, i think your digit-argument thing can be macro'd to a "0123456789" loop.
; <oni-on-ion> really all of them -- why not (let ((x divine-normal-map)) (loop... (define-key x ...)))) ?

(define-key divine-normal-mode-map (kbd "0") 'digit-argument)
(define-key divine-normal-mode-map (kbd "1") 'digit-argument)
(define-key divine-normal-mode-map (kbd "2") 'digit-argument)
(define-key divine-normal-mode-map (kbd "3") 'digit-argument)
(define-key divine-normal-mode-map (kbd "4") 'digit-argument)
(define-key divine-normal-mode-map (kbd "5") 'digit-argument)
(define-key divine-normal-mode-map (kbd "6") 'digit-argument)
(define-key divine-normal-mode-map (kbd "7") 'digit-argument)
(define-key divine-normal-mode-map (kbd "8") 'digit-argument)
(define-key divine-normal-mode-map (kbd "9") 'digit-argument)
;; Character motion
(define-key divine-normal-mode-map (kbd "b") 'backward-char)
(define-key divine-normal-mode-map (kbd "B") 'backward-word)
(define-key divine-normal-mode-map (kbd "f") 'forward-char)
(define-key divine-normal-mode-map (kbd "F") 'forward-word)
(define-key divine-normal-mode-map (kbd "<spc>") 'activate-mark)
;; Line motion
(define-key divine-normal-mode-map (kbd "p") 'previous-line)
(define-key divine-normal-mode-map (kbd "n") 'next-line)
(define-key divine-normal-mode-map (kbd "$") 'divine-end-of-line)
(define-key divine-normal-mode-map (kbd "a") 'divine-beginning-of-line)
;; Killing
(define-key divine-normal-mode-map (kbd "d") 'divine-kill)
;; History
(define-key divine-normal-mode-map (kbd "u") 'undo)
(define-key divine-normal-mode-map (kbd "U") 'redo)
;; imenu
(define-key divine-normal-mode-map (kbd "M-i") 'counsel-imenu)

;;; Insert mode

(divine-defmode divine-insert-mode " Divine <I>"
  "Emacs-like mode for Divine."
  :cursor 'bar)

;;;; Default keymap

(define-key divine-insert-mode-map  (kbd "M-i") 'divine-normal-mode)

;;; Visual mode

(divine-defmode divine-visual-mode " Divine <V>"
  "Emacs-like mode for Divine."
  :cursor 'hollow
  :cursor-color "LightSkyBlue")

;;; Conclusion

(provide 'divine)

;;; divine.el ends here
