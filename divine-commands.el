;;; divine-commands.el --- The Divine command set  -*- lexical-binding: t; coding: utf-8 -*-

;; Copyright (c) 2020 Thibault Polge <thibault@thb.lt>

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

;; This module provides commands for Divine.

;;; Code:

(require 'divine-core)
(require 'outline)
(require 'rect)

;;; Customizations

(defcustom divine-pair-aliases '((?p . ?\()
                                 (?P . ?\))
                                 (?b . ?\[)
                                 (?B . ?\])
                                 (?c . ?{)
                                 (?C . ?}))

  "Configure bindings for pair definitions.  This is used for
  the (re)wrapping commands to let you more easily enter pair
  prefixes."
  :type '(alist :key-type character :value-type character))

;;; Variables

(defvar divine--macro-register nil
  "Register to save the current macro to.")

;;; Utilities

(defconst divine--blank-line-regexp (rx bol (* space) eol))

(defalias 'divine-commands-switch-to-insert-mode 'divine-insert-mode
  "To use divine-commands.el as a library in a divine interface
that doesn't provide `divine-insert-mode', override this
alias to use your function instead.")

(defun divine--text-to-register-helper (&optional delete)
  "A generic helper to move text regions to REGISTER, or to kill-ring.

If DELETE, region is deleted from buffer."
  (if (divine-register-p)
      ;; otherwise
      (copy-to-register (divine-register) (region-beginning) (region-end) delete t))
  (kill-ring-save (region-beginning) (region-end) t)
  (when delete
    (delete-region (region-beginning) (region-end))))

(defun divine-read-pair (&optional prompt)
  "Like `divine-read-char', but translates the result with
  `divine-pair-aliases'."
  (interactive)
  (let ((char (divine-read-char prompt)))
    (alist-get char divine-pair-aliases char)))

;;; Operators

;;;; Killing

(divine-defoperator divine-kill
  "Kill REGION into REGISTER."
  (divine--text-to-register-helper t))

(divine-defoperator divine-change
  "Kill REGION, then enter insert mode."
  (divine--text-to-register-helper t)
  (divine-commands-switch-to-insert-mode))

(divine-defaction divine-char-replace
  "Replace char at point without leaving normal mode."
  (let ((char (char-to-string (divine-read-char))))
    (save-excursion
      (divine-dotimes
       (if (and (eq count 1) (looking-at "$"))
           ;; * Special case: we're at the end of a line, and count is
           ;; * 1: we append without replacing the \n
           (insert char)
         ;; Ignore newlines.
         ;; * Base case
         (while (looking-at "$")
           (forward-char plus1))
         (delete-char plus1)
         (insert char)
         (when negative (backward-char)))))))

;;; Motions

;;;; Scopes

(divine-defscope around)
(divine-defscope inside)

(defmacro divine-with-numeric-argument-and-scope (&rest body)
  `(divine-with-numeric-argument
    (let ((scope (divine-scope-p))
          (around (divine-scope-around-flag))
          (inside (divine-scope-inside-flag)))
      ,@body)))

;;;; Character motion

(divine-defmotion divine-char-forward
  "Move forward ARG character(s)."
  (forward-char (divine-numeric-argument)))

(divine-reverse-command 'divine-char-forward)

(divine-defmotion divine-char-left
  "Move backward ARG character(s), on the same line."
  (left-char (divine-numeric-argument)))

(divine-reverse-command 'divine-char-left)

;;;; Line motion

(divine-defmotion divine-line-beginning
  "Go to the first non-space character of current line."
  (beginning-of-line)
  (unless (looking-at "^[[:space:]]*$")
    (search-forward-regexp (rx (not space)))
    (backward-char)))

(divine-defmotion divine-goto-line
  "Go to the line indicated by the prefix argument, or fail."
  (if (divine-numeric-argument-p)
      (progn (goto-char (point-min))
             (forward-line (1- (divine-numeric-argument))))
    (divine-fail)))

(divine-defmotion divine-whole-line
  "Set mark at the first character of the current-line, and point
at the last, including the final \n."
  (beginning-of-line)
  (push-mark (point))
  (end-of-line (divine-numeric-argument))
  (unless (eobp) (forward-char)))

(divine-defmotion divine-line-contents
  "Set mark at the first character of the current-line, and point
at the last, including the final \n."
  (beginning-of-line)
  (push-mark (point))
  (end-of-line (divine-numeric-argument)))

;;;; Word motion

(divine-defmotion divine-word-forward
  "Move COUNT word(s) forward."
  (divine-with-numeric-argument
   (forward-word count)
   ;; With a scope.
   (when (divine-scope-flag)
     (push-mark (point) t t)
     (backward-word count))))

(divine-reverse-command 'divine-word-forward)

;;;; Buffer motion

(divine-defmotion divine-beginning-of-buffer
  "Move point to (point-min).

Use this if you want to move to the absolute beginning of buffer."
  (goto-char (point-min)))

(divine-defmotion divine-end-of-buffer
  "Move point to (point-max).

Use this if you want to move to the absolute end of buffer."
  (goto-char (point-max)))

;;;; Search

(defun divine--find-char-helper (after)
  "Helper for divine-find-char-*[-after]"
  (divine-with-numeric-argument
   (search-forward (char-to-string (divine-read-char)) nil nil count)
   (unless (eq positive after) (forward-char minus1))))

(divine-defmotion divine-find-char-forward
  "Before the COUNTh occurence of CHAR forward, after if SCOPE."
  (divine--find-char-helper (divine-scope-flag)))

(divine-reverse-command 'divine-find-char-forward)

(divine-defmotion divine-find-char-forward-before
  "Prompt for a character, then move point forward before the COUNTh occurence."
  (divine--find-char-helper nil))

(divine-reverse-command 'divine-find-char-forward-before)

(divine-defmotion divine-find-char-forward-after
  "Prompt for a character, then move point forward after the COUNTh occurence."
  (divine--find-char-helper t))

(divine-reverse-command 'divine-find-char-forward-after)

;;;; Buffer motion

(divine-defmotion divine-buffer-beginning
  "Go to beginning of buffer."
  (goto-char (point-min)))

(divine-defmotion divine-buffer-end
  "Go to beginning of buffer."
  (goto-char (point-max)))

;;;; Folds and outline

(divine-defmotion divine-visible-heading-backward
  "Move COUNT visible headings, backwards."
  (outline-previous-visible-heading (divine-numeric-argument)))

(divine-defmotion divine-visible-heading-forward
  "Move COUNT visible headings, backwards."
  (outline-next-visible-heading (divine-numeric-argument)))

(divine-defaction divine-subtree-move-up
  "Move the subtree up."
  (outline-move-subtree-up (divine-numeric-argument)))

(divine-defaction divine-subtree-move-down
  "Move the subtree down."
  (outline-move-subtree-down (divine-numeric-argument)))

;;;; Balanced expressions (Lisp-mode)

;;;; Join lines?

(divine-defaction divine-line-join
  "Join COUNT lines."
  (divine-dotimes
   (join-line positive)))

(divine-defaction divine-line-open-forward
  "Open COUNT lines before point, and move the point at the first."
  (divine-with-numeric-argument
   (if positive (end-of-line)
     (beginning-of-line)
     (backward-char))
   (insert "\n")
   (if positive (end-of-line) (beginning-of-line))
   (divine-commands-switch-to-insert-mode)))

(divine-reverse-command 'divine-line-open-forward)

;;;; Saving and restoring stuff

(divine-defoperator divine-text-save
  "Save text to REGISTER or kill-ring."
  (divine--text-to-register-helper))

(divine-defaction divine-yank
  "Yank text from register or kill-ring.
If no register is specified, yank COUNTieth elements from
kill-ring."
  (if (divine-register-p)
      (insert-register (divine-register))
    (yank (divine-numeric-argument))))

(divine-defaction divine-point-save
  "Save point to REGISTER or mark stack."
  (divine--text-to-register-helper))

;;;; Region editing

(divine-defaction divine-mark-activate
  "Activate the mark."
  (push-mark (point) t t))

(divine-defaction divine-mark-deactivate
  "Deactivate the mark."
  (deactivate-mark))

(divine-defaction divine-mark-toggle
  "Toggle the mark"
  (if (region-active-p)
      (divine-mark-deactivate)
    (divine-mark-activate)))

(divine-defaction divine-mark-rectangle-activate
  "Activate the rectangular mark."
  (rectangle-mark-mode t))

(divine-defaction divine-mark-rectangle-deactivate
  "Activate the rectangular mark."
  (rectangle-mark-mode 0))

(divine-defaction divine-mark-rectangle-toggle
  "If mark is inactive, activate it in rectangle mode.  If mark
is active, toggle rectangle mode"
  (if (region-active-p)
      (if rectangle-mark-mode
          (divine-mark-rectangle-deactivate)
        (divine-mark-rectangle-activate))
    (divine-mark-activate)
    (divine-mark-rectangle-activate)))

;;; @FIXME Unsorted

(divine-wrap-operator indent-region)
(divine-wrap-operator sort-lines)

;;; Macros

(divine-defaction divine-macro-start
  "Begin recording macro into REGISTER."
  (assert (not defining-kbd-macro))
  (setq divine--macro-register (divine-register))
  (kmacro-start-macro nil))

(divine-defaction divine-macro-end
  "Stop recording macro."
  (kmacro-end-macro nil)
  (when divine--macro-register
    (kmacro-to-register divine--macro-register)
    (setq divine--macro-register nil)))

;; @FIXME Support calling from register.
(divine-defaction divine-macro-call
  "Execute macro from REGISTER COUNT times."
  (kmacro-call-macro (divine-numeric-argument nil) nil ))

;;; Smartparens

(divine-defoperator divine-wrap
  "Prompt for character, then wrap region with it.")

;;; Hybrids

(divine-defcommand divine-zero
  "Multiply current numeric argument by 10."
  (if (divine-numeric-argument-p)
      (setq prefix-arg (* (divine-numeric-argument t) 10))
    (divine-line-beginning)))

(divine-defcommand divine-goto-line-or-g-mode
  "Goto line COUNT if COUNT, otherwise enter transient g mode."
  (if (divine-numeric-argument-p)
      (divine-goto-line)
    (divine-transient-g-mode)))

;;; Conclusion

(provide 'divine-commands)
