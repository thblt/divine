;;; divine-commands.el --- The Divine command set  -*- lexical-binding: t; coding: utf-8 -*-

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

;; This module provides commands for Divine.

;;; Code:

(require 'divine-core)
(require 'outline)
(require 'rect)

;;; Customizations

(defcustom divine-paragraphs-are-defuns 'divine-prog-mode-p
  "Determine whether P and N should move by defuns instead of paragraphs.

This can be either t, nil, or a function that returns a boolean value."
  :type '(choice (const :tag "Always" t)
                 (const :tag "Never" nil)
                 (function :tag "Predicate")))

;;; Utilities

(defconst divine--blank-line-regexp (rx bol (* space) eol))

(defun divine--reversed-one (count)
  "Return -1 if arg is positive, 1 otherwise."
  (if (< count 0) +1 -1))

(defun divine--text-to-register-helper (&optional delete)
  "A generic helper to move text regions to REGISTER, or to kill-ring.

If DELETE, region is deleted from buffer."
  (if (divine-register-p)
      (copy-to-register (divine-register) (region-beginning) (region-end) delete t))
  ;; otherwise
  (kill-ring-save (region-beginning) (region-end) t)
  (when delete
    (delete-region (region-beginning) (region-end))))

;;; Operators

;;;; Killing

(divine-defoperator divine-kill divine-whole-line
  "Kill the relevant something."
  (divine--text-to-register-helper t))

(divine-defoperator divine-change divine-whole-line
  "Kill the relevant something, then enter insert mode."
  (divine--text-to-register-helper t)
  (divine-insert-mode))

(divine-defaction divine-char-replace
  "Kill the relevant something, then enter insert mode."
   (let ((char (char-to-string (divine-read-char))))
     (divine-dotimes
      (if (and (eq count 1) (looking-at "$"))
          ;; * Special case: we're at the end of a line, and count is
          ;; * 1: we append without replacing the \n
          (insert char)
        ;; * Base case
        ;; Ignore newlines.
        (while (looking-at "$")
          (forward-char plus1))
        (delete-char plus1)
        (insert char)
        (when negative (backward-char))))))

;;; Motions

;;;; Scopes

(divine-defscope around)
(divine-defscope inside)

;;;; Character motion

(divine-reverse-command
 (divine-defmotion divine-char-forward
   "Move forward ARG character(s)."
   (forward-char (divine-numeric-argument))))

(divine-reverse-command
 (divine-defmotion divine-char-left
   "Move backward ARG character(s), on the same line."
   (left-char (divine-numeric-argument))))

(divine-defmotion divine-paragraph-forward
  "Move forward ARG paragraph(s)."
  (search-forward-regexp divine--blank-line-regexp nil nil (divine-numeric-argument))
  (while (looking-at divine--blank-line-regexp) (forward-line)))

(divine-defmotion divine-paragraph-backward
  "Move backward ARG paragraph(s)."
  (search-backward-regexp divine--blank-line-regexp nil nil (1+ (divine-numeric-argument)))
  (while (looking-at divine--blank-line-regexp) (forward-line -1))
  (start-of-paragraph-text))

;;;; Line motion

(divine-reverse-command
 (divine-defmotion divine-line-forward
   "Move forward COUNT line(s)."
   (divine-with-numeric-argument
    (message "%s" count)
    (forward-line count))))


(divine-defmotion divine-line-beginning
  "Go to the beginning of current line."
  (beginning-of-line))

(divine-defmotion divine-line-end
  "Go to the end of current line."
  (end-of-line))

(divine-defmotion divine-goto-line
  "Go to the line indicated by the prefix argument, or fail."
  (if (divine-numeric-argument-p)
      (progn (goto-char (point-min))
             (forward-line (1- (divine-numeric-argument))))
    (divine-fail)))

(divine-defmotion divine-whole-line
  "Set mark at the first character of the current-line, and point at the last."
  (beginning-of-line)
  (push-mark (point))
  (end-of-line (divine-numeric-argument)))

;;;; Word motion

(divine-reverse-command
 (divine-defmotion divine-word-forward
   "Move COUNT word(s) forward."
   (divine-with-numeric-argument
    (message "Run: %s %s" (divine-scope-p) count)
    (forward-word count)
    ;; With a scope.
    (when (divine-scope-flag)
      (push-mark (point) t t)
      (backward-word count)))))

;;;; Search

(defun divine--find-char-helper (after)
  "Helper for divine-find-char-*[-after]"
  (divine-with-numeric-argument
   (search-forward (char-to-string (divine-read-char)) nil nil count)
   (unless (eq positive after) (forward-char minus1))))

(divine-reverse-command
 (divine-defmotion divine-find-char-forward-before
   "Prompt for a character, then move point forward before the COUNTh occurence."
   (divine--find-char-helper nil)))

(divine-reverse-command
 (divine-defmotion divine-find-char-forward-after
   "Prompt for a character, then move point forward after the COUNTh occurence."
   (divine--find-char-helper t)))

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

;; ("p" outline-previous-visible-heading "prev")
;; ("<down>" outline-previous-visible-heading "prev")
;; ("n" outline-next-visible-heading "next")
;; ("<up>" outline-next-visible-heading "prev")
;; ("P" outline-move-subtree-up "up")
;; ("N" outline-move-subtree-down "down")
;; ("b" outline-promote "+")
;; ("f" outline-demote "-")
;; ("w" outshine-narrow-to-subtree "Narrow")
;; ("c" outshine-cycle "Cycle")
;; ("<tab>" outshine-cycle "Cycle")
;; ("C" outshine-cycle-buffer "Cycle buffer")
;; ("M-<tab>" outshine-cycle-buffer)
;; ("o" outline-hide-other "Hide others"))

;;;; Balanced expressions (Lisp-mode)

;;;; Join lines?

(divine-defaction divine-line-join
  "Join COUNT lines."
  (join-line (divine-numeric-argument)))

;;;; Saving and restoring stuff

(divine-defoperator divine-text-save divine-whole-line
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

;;; Hybrids

(divine-defcommand divine-bol-or-around
  "If there's a pending operator, maybe select the 'around scope,
 otherwise go to beginning of line."
  (cond ((divine-accept-scope-p)
         (divine-scope-around-select))
        ((divine-accept-motion-p)
         (beginning-of-line))
        (t (divine-fail))))

(divine-defcommand divine-insert-or-inside
  "If there's a pending operator, maybe set the motion flag to 'around,
  otherwise append at point."
  (cond ((divine-accept-scope-p)
         (divine-scope-inside-select))
        ((divine-accept-operator-p)
         (divine-insert-mode))
        (t (divine-fail))))

(divine-defcommand divine-zero
  "If there's a digit argument, multiply it by 10, otherwise go
to the first character in line."
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

;;; divine.el ends here
