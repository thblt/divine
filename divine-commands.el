;;; divine-commands.el --- The Divine command set  -*- lexical-binding: t; coding: utf-8 -*-

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

(defcustom divine-insert-mode-command 'divine-insert-mode
  "Function to enter insert mode.")

;;; Variables

(defvar divine--macro-register nil
  "Register to save the current macro to.")


;;; Helper commands

;;;; Scope support

(defmacro divine-defscope (name)
  "Define the Divine scope modifier NAME.

This macro generates three functions: `divine-scope-NAME-select'
to activate the scope if no other scope is already selected,
`divine-scope-NAME-force' to activate it, optionally replacing an
existing scope, and `divine-scope-NAME-flag', to read and consume
the scope."
  (let ((enter-fn (intern (format "divine-scope-%s-select" name)))
        (force-fn (intern (format "divine-scope-%s-force" name)))
        (pred-fn (intern (format "divine-scope-%s-p" name)))
        (peek-fn (intern (format "divine-scope-%s-p" name))))
    `(progn
       (defun ,enter-fn ()
         ,(format "Select '%s as the scope for the next text object.

This won't have any effect is another scope is already selected." name)
         (interactive)
         (unless divine--object-scope
           (setq divine--object-scope ',name)))
       (defun ,force-fn ()
         ,(format "Replace currently selected scope with '%s." name)
         (interactive)
         (setq divine--object-scope ',name))
       (defun ,pred-fn (&optional noconsume)
         ,(format "Return non-nil if Divine scope %s is selected, then consume it.

This won't consume any other scope.

If NOCONSUME it non-nil, don't consume the scope." name)
         (when (eq divine--object-scope ',name)
           (unless noconsume
             (setq divine--object-scope nil))
           t)))))

(defvar-local divine--object-scope nil
  "A text motion transformation tag.  In the standard Divine
  interface, this accepts 'around or 'inside, like Vim's `a' and
  `i'.")

(defun divine-scope-p (&optional noconsume)
  "Return the value of the selected scope, then consume it.

If NOCONSUME, only peek at the scope without consuming it"
  (prog1
      divine--object-scope
    (unless noconsume
      (setq divine--object-scope nil))))

;;;; Register argument support

(defvar-local divine--register nil
  "The register chosen by the `divine-register-prefix' function.")

(defun divine-select-register (reg)
  "Select REG as the default register for the next operation.

Interactively, prompt the user using `divine-read-char'."
  (interactive (list (divine-read-char "Register?")))
  (setq divine--register reg))

(defun divine-register (&optional noconsume)
  "Return the selected register and consume it.

If NOCONSUME is non-nil, don't consume the value."
  (prog1
      divine--register
    (unless noconsume (setq divine--register nil))))

(defun divine-register-p ()
  "Non-nil if there's a register selected."
  divine--register)

;;; Base features

;;;; Killing and yanking

(defun divine--text-to-register-helper (&optional delete)
  "A generic helper to move text regions to REGISTER, or to kill-ring.

If DELETE, region is also deleted from buffer."
  (if (divine-register-p)
      (copy-to-register (divine-register) (region-beginning) (region-end) delete t)
    (kill-ring-save (region-beginning) (region-end) t))
  (when delete
    (delete-region (region-beginning) (region-end))))

(divine-defoperator divine-kill
  "Kill REGION into REGISTER."
  (divine--text-to-register-helper t))

(divine-defoperator divine-change
  "Kill REGION, then enter insert mode."
  (divine--text-to-register-helper t)
  (funcall divine-insert-mode-command))

(defun divine-char-replace ()
  "Replace char at point without leaving normal mode."
  (interactive)
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

(defun divine-yank ()
  "Yank the COUNTh entry from KILL-RING, or the contents of
  REGISTER.

If text to yank ends with \n, go to the beginning of next line
before yanking.  If COUNT is negative, go to the beginning of
previous line instead.

If pasting from the kill-ring, this function pretends to be
`yank' by setting `this-command', so `yank-pop' is happy."
  (interactive)
  (divine-with-numeric-argument-and-register
   (when (string-suffix-p "\n"
                          (if register
                              (register-get register)
                            (current-kill times t)))
     (when positive (forward-line))
     (beginning-of-line))
   (if (divine-register-p)
       (insert-register (divine-register))
     (setq this-command 'yank)
     (yank (divine-numeric-argument)))))

;;; Actions

(defun divine-append ()
  "Move COUNT characters forward, then switch to insert mode."
  (interactive)
  (forward-char (divine-numeric-argument))
  (divine-commands-switch-to-insert-mode))

(defun divine-append-line ()
  "Move to end of line, then switch to insert mode."
  (interactive)
  (end-of-line)
  (divine-commands-switch-to-insert-mode))

;;; Motions

;;;; Scopes

(divine-defscope around)
(divine-defscope inside)

(defun divine-scope-step ()
  "If not SCOPE, select inside scope.  Otherwise, select around."
  (interactive)
  (if (divine-scope-p)
      (divine-scope-around-force)
    (divine-scope-around-select)))

(defmacro divine-with-numeric-argument-and-scope (&rest body)
  `(divine-with-numeric-argument
    (let ((scope (divine-scope-p))
          (around (divine-scope-around-flag))
          (inside (divine-scope-inside-flag)))
      ,@body)))

;;;; Character motion
;;;; Line motion

(defun divine-goto-line ()
  "Goto line ARG."
  (interactive)
  ;; We test, because (divine-numeric-argument) returns 1 if the user
  ;; gave no arg.
  (when (divine-numeric-argument-p 'noconsume)
    (goto-line (divine-numeric-argument))))


;;;; Buffer motion


;;;; Word motion

;; @FIXME We need linewise motion.

(defun divine-word-start ()
  "Move point to the beginning of word at point, or the previous word if point isn't on a word."
  (interactive)
  (let ((beg (car (bounds-of-thing-at-point 'word))))
    (if beg
        (goto-char beg)
      (backward-word))))

(defun divine-word-end ()
  "Move point to the beginning of word at point, or the previous word if point isn't on a word."
  (interactive)
  (let ((beg (cdr (bounds-of-thing-at-point 'word))))
    (if beg
        (goto-char beg)
      (forward-word))))

(divine-defobject word "Move by words."
                  :forward forward-word
                  :backward backward-word
                  :beginning divine-word-start
                  :end divine-word-end)

;;;; Search

(defun divine--find-char-helper (after)
  "Helper for divine-find-char-*[-after]"
  (interactive)
  (divine-with-numeric-argument
   (search-forward (char-to-string (divine-read-char)) nil nil count)
   (unless (eq positive after) (forward-char minus1))))

(defun divine-find-char-forward ()
  "Before the COUNTh occurence of CHAR forward, after if SCOPE."
  (interactive)
  (divine--find-char-helper (divine-scope-flag)))

(divine-reverse-command 'divine-find-char-forward)

(defun divine-find-char-forward-before ()
  "Prompt for a character, then move point forward before the COUNTh occurence."
  (interactive)
  (divine--find-char-helper nil))

(divine-reverse-command 'divine-find-char-forward-before)

(defun divine-find-char-forward-after ()
  "Prompt for a character, then move point forward after the COUNTh occurence."
  (interactive)
  (divine--find-char-helper t))

(divine-reverse-command 'divine-find-char-forward-after)

;;;; Buffer motion

(defun divine-buffer-beginning ()
  "Go to beginning of buffer."
  (interactive)
  (goto-char (point-min)))

(defun divine-buffer-end ()
  "Go to beginning of buffer."
  (interactive)
  (goto-char (point-max)))

;;;; Folds and outline


;;;; Balanced expressions (Smartparens)

(defun divine-sexp-motion ()
  "S-exp motion.

Select the NTH-level balanced expression around point, delimited
by CHAR.  If INSIDE, select everything inside the delimiters.  If
AROUND, select the delimiters as well.

This is a magical-ish motion that acts on balanced s-expressions,
courtesy of smartparens.

Without a scope, pending `divine-kill' or `divine-change' are
replaced by `divine-sexp-kill' and `divine-sexp-change',
respectively."
  (unless (divine-scope-p t)
    (divine-operator-swap-pending
     '((divine-kill . divine-delimiter-kill)
       (divine-change . divine-delimiter-change))))
  ;; @TODO
  )

(divine-defoperator divine-delimiter-kill
  "Kill the outermost delimiters of REGION, effectively unwrapping it."
  ;; @TODO
  )

(divine-defoperator divine-delimiter-change
  "Prompt for delimiters and insert them instead of the current
  delimiters of REGION, effectively rewrapping it."
;; @TODO
  )

;;;; Join lines?

(defun divine-line-join ()
  "Join COUNT lines."
  (interactive)
  (divine-dotimes
   (join-line positive)))

(defun divine-line-open-forward ()
  "Open COUNT lines before point, and move the point at the first."
  (interactive)
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

(defun divine-point-save ()
  "Save point to REGISTER or mark stack."
  (interactive)
  (divine--text-to-register-helper))

;;;; Region editing

(defun divine-mark-activate ()
  "Activate the mark."
  (interactive)
  (push-mark (point) t t))

(defun divine-mark-deactivate ()
  "Deactivate the mark."
  (interactive)
  (deactivate-mark))

(defun divine-mark-toggle ()
  "Toggle the mark"
  (interactive)
  (if (region-active-p)
      (divine-mark-deactivate)
    (divine-mark-activate)))

(defun divine-mark-rectangle-activate ()
  "Activate the rectangular mark."
  (interactive)
  (rectangle-mark-mode t))

(defun divine-mark-rectangle-deactivate ()
  "Activate the rectangular mark."
  (interactive)
  (rectangle-mark-mode 0))

(defun divine-mark-rectangle-toggle ()
  "If mark is inactive, activate it in rectangle mode.  If mark
is active, toggle rectangle mode"
  (interactive)
  (if (region-active-p)
      (if rectangle-mark-mode
          (divine-mark-rectangle-deactivate)
        (divine-mark-rectangle-activate))
    (divine-mark-activate)
    (divine-mark-rectangle-activate)))


;;; Macros

(defun divine-macro-start ()
  "Begin recording macro into REGISTER."
  (interactive)
  (assert (not defining-kbd-macro))
  (setq divine--macro-register (divine-register))
  (kmacro-start-macro nil))

(defun divine-macro-end ()
  "Stop recording macro."
  (interactive)
  (kmacro-end-macro nil)
  (when divine--macro-register
    (kmacro-to-register divine--macro-register)
    (setq divine--macro-register nil)))

;; @FIXME Support calling from register.
(defun divine-macro-call ()
  "Execute macro from REGISTER COUNT times."
  (interactive)
  (kmacro-call-macro (divine-numeric-argument nil) nil ))

;;; Misc utilities

(defconst divine--blank-line-regexp (rx bol (* space) eol))

(defun divine-read-pair (&optional prompt)
  "Like `divine-read-char', but translates the result with
  `divine-pair-aliases'."
  (interactive)
  (let ((char (divine-read-char prompt)))
    (alist-get char divine-pair-aliases char)))

;;; Conclusion

(provide 'divine-commands)
