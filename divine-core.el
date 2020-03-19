;;; divine-core.el --- Core infrastructure for Divine or your own modal editor  -*- lexical-binding: t; coding: utf-8 -*-

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

;; This module provides the core Divine framework.

;;; Code:

(require 'cl-lib)

;;; Constants

(defconst divine-version "0.0 alpha")

(defconst divine-custom-cursor-type
  '(choice
    (const :tag "Frame default" t)
    (const :tag "Filled box" box)
    (const :tag "Hollow cursor" hollow)
    (const :tag "Vertical bar" bar)
    (cons :tag "Vertical bar with specified width" (const bar) integer)
    (const :tag "Horizontal bar" hbar)
    (const :tag "Horizontal bar with specified width" (const hbar) integer)
    (const :tag "None " nil))) ; To update, C-u eval (custom-variable-type 'cursor-type)

(defconst divine-custom-cursor-color-type '(choice (const :tag "Default" nil)
                                                   (color :tag "Color")))

;;; Customizations

(defgroup divine nil
  "Modal interface with text objects, or something close enough."
  :group 'convenience)

(defcustom divine-default-cursor 'box
  "Default cursor style for modes that don't specify it."
  :type divine-custom-cursor-type)

(defcustom divine-read-char-cursor 'hbar
  "Default cursor style for modes that don't specify it."
  :type divine-custom-cursor-type)

(defcustom divine-default-cursor-color nil
  "Default cursor color for modes that don't specify it.

If nil, use the foreground color of the default face."
  :type divine-custom-cursor-color-type)

(defcustom divine-initial-mode-function 'divine-choose-initial-mode
  "The function that runs the initial divine mode.

Can be either `divine-choose-initial-mode', for a reasonably
smart selection, one of the divine mode functions, eg
function `divine-normal-mode', or your own function."
  :type 'function)

;; Variables

(defvar-local divine--lighter nil
  "The current mode lighter.")

(defvar divine--mode-lighters nil
  "An alist of (MODE . LIGHTER)")

(defvar divine--modes nil
  "List of known divine modes")

(defvar-local divine--active-mode nil
  "The currently active Divine mode.")

(defvar divine--next-mode nil
  "If non nil, `divine-finalize will activate this mode after the
  next command has completed.'")

(defvar-local divine--ready-for-operator nil
  "Whether Divine region is ready, that is, the operator can act
  over it.")

(defvar-local divine--pending-operator nil
  "The operator waiting for a motion.")

(defvar-local divine--transient-stack nil
  "Stack of modes to restore after a transient operation.")

;;; Core infrastructure

;;;; Control modes

(define-minor-mode divine-mode
  "Divine, a modal interface with text objects, or something
  close enough."
  :lighter (:eval divine--lighter)
  (if divine-mode
      ;; Enter
      (progn
        (divine--finalize) ; Clear state variables, just in case.
        (add-hook 'pre-command-hook 'divine-pre-command-hook)
        (add-hook 'post-command-hook 'divine-post-command-hook)
        (funcall divine-initial-mode-function))
    ;; Leave
      (divine--disable-modes nil)
      (divine--finalize)))

(define-globalized-minor-mode divine-global-mode divine-mode divine-mode)

;;;; Command hooks

(defvar-local divine--point nil
  "Saved value of point")

(defun divine-pre-command-hook ()
  (setq divine--point (point)))

(defun divine-post-command-hook ()
  (unless (eq divine--point (point))
    (divine-motion-done)))

;;;; Finalizers

(defun divine--finalize ()
  "Restore base state."
  (setq divine--object-scope nil
        divine--pending-operator nil
        divine--ready-for-operator nil
        divine--register nil
        prefix-arg nil)
  ;; Quit transient modes
  (while divine--transient-stack
    (funcall (pop divine--transient-stack) t)))

(defun divine-operator-done ()
  "Finalize the current operator."
  (divine--finalize))

(defun divine-motion-done ()
  "Finalize the current motion."
  (when divine--pending-operator
    (setq divine--ready-for-operator t)
    (call-interactively divine--pending-operator))
  (divine--finalize))

(defun divine-fail ()
  "Do whatever makes sense when a binding is unusable in the current context.

This function should be used as a base case for hybrid commands,
and can be bound to override binding."
  (interactive)
  (ding)
  (divine-flash " --- UNBOUND ---"))

;;;; Numeric argument support

(defun divine-numeric-argument-p ()
  "Return non-nil if the numeric argument is defined.

This predicate is only to be used to determine the relevant
function in an hybrid command.  To consume the numeric argument,
even if you ignore the actual value, use
`divine-numeric-argument' or `divine-numeric-argument-flag'."
  current-prefix-arg)

(defun divine-numeric-argument (&optional noconsume)
  "Return the current numeric argument or a reasonable default.

If no argument was provided, return 1.  The negative argument is
-1.

The numeric argument is consumed after it's been read, which
means subsequent invocations will always return 1.  If NOCONSUME
is non-nil, the argument isn't consumed.  This is probably not a
good idea.

To check for the presence of a user-provide numeric argument,
use `divine-numeric-argument-p' instead."
  (divine--numeric-argument-normalize
   (prog1
       current-prefix-arg
     (unless noconsume (setq current-prefix-arg nil)))))

(defun divine-numeric-argument-flag (&optional noconsume)
  "Like `divine-numeric-argument-p', but actually consume the argument.

This is useful to use the argument as a flag and ignore its
value.

The numeric argument is consumed after it's been read, which
means subsequent invocations will always return 1.  If NOCONSUME
is non-nil, the argument isn't consumed.  This is probably not a
good idea."
    (prog1
        current-prefix-arg
    (unless noconsume (setq current-prefix-arg nil))))

(defun divine--numeric-argument-normalize (arg)
  "Normalize ARG as an integer.

ARG can be any possible value of `prefix-arg', that is: nil, the
symbol `-', a one-element list whose car is an integer, or a
non-null integer."
  (cond
   ((null arg) 1)
   ((eq '- arg) -1)
   ((listp arg) (car arg))
    (  arg)))

;;;;; Utility macros


(defmacro divine-with-numeric-argument (&rest body)
  "Evaluate BODY in an environment where:

 - COUNT is bound to the value of the numeric argument
 - TIMES is bound to the absolute value of the numeric argument
 - POSITIVE is non-nil if COUNT is positive or null
 - NEGATIVE is (not positive)
 - PLUS1 is 1 if POSITIVE, -1 otherwise.
 - MINUS1 is minus PLUS1."
  `(let* ((count (divine-numeric-argument))
          (times (abs count))
          (positive (>= count 0))
          (negative (not positive))
          (plus1 (if positive +1 -1))
          (minus1 (- plus1)))
     ,@body))

(defmacro divine-dotimes (&rest body)
  "Execute BODY COUNT times, in the same environment as
  `divine-with-numeric-argument'."
  `(divine-with-numeric-argument
    (dotimes (_ times)
      ,@body)))

(defmacro divine-reverse-command (command &optional name)
  "Create a command called NAME that runs COMMAND with the
  numeric argument reversed.

COMMAND must be quoted.

If NAME isn't provided, it's calculated with
`divine--reverse-direction-words'."
  (setq command (eval command))
  (unless (symbolp command) (error "COMMAND must be a symbol."))
  (unless name ; Guess name
    (setq name (intern (divine--reverse-direction-words (symbol-name command))))
    (when (eq command name) (error "Cannot magically reverse `%s', please pass a name." name)))
  `(defun ,name ()
     ,(divine--reverse-direction-words (documentation command))
     (interactive)
     (setq current-prefix-arg (- (divine-numeric-argument)))
     (,command)))

(defun divine--word-replace-both-ways (word other-word &optional noswap)
  "Replace the first occurence of WORD by OTHER-WORD, or conversely.

If WORD is found, replace all occurences with OTHER-WORD.

If WORD isn't found and NOSWAP is nil, repeat with WORD and
OTHER-WORD swapped"
  (save-excursion
    (cond ((re-search-forward (rx word-boundary (literal word) word-boundary) nil t)
           (replace-match other-word))
          ((not noswap) (divine--word-replace-both-ways other-word word t)))))

(defun divine--reverse-direction-words (STRING)
  "In STRING replace forward by backward, next by prev,
and conversely, and return the modified symbol."
  (with-temp-buffer
    (insert STRING)
    (dolist (pair '(("next" . "previous")
                    ("forward" . "backward")
                    ("left" . "right")))
      (goto-char (point-min))
      (divine--word-replace-both-ways (car pair) (cdr pair)))
    (buffer-string)))

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

;;;; Scope support

(defvar-local divine--object-scope nil
  "A text motion transformation tag.  In the standard Divine
  interface, this accepts 'around or 'inside, like Vim's `a' and
  `i'.")

(defun divine-scope-p ()
  "Return non-nil if a scope has been selected.  Don't consume the scope.

Don't use this function to consume the scope, even if you have
  only one."
  divine--object-scope)

(defun divine-scope-flag ()
  "Like `divine-scope-p', but consume the scope."
  (prog1
      divine--object-scope
    (setq divine--object-scope nil)))

;;;; Messages

(defun divine-flash (msg)
  "Display MSG with `message'."
  (message "%s" msg))

;;;; Internal

(defun divine--disable-modes (&optional except)
  "Disable all modes in `divine--modes' except EXCEPT."
  (dolist (mode divine--modes)
    (unless (eq mode except)
      (funcall mode 0)))
  (setq-local divine--active-mode except))

;;; Low-level command interface

;;;; Predicates

(defun divine-accept-motion-p ()
  "Return non-nil in a motion command can be entered."
  t)

(defun divine-accept-object-p ()
  "Return non-nil in a text object command can be entered.  This
  is more narrow that `divine-accept-motion-p'"
  (or (region-active-p)
      (divine-pending-operator-p)))

(defun divine-accept-operator-p ()
  "Return non-nil in an operator can be entered."
  (not (divine-pending-operator-p)))

(defun divine-accept-scope-p ()
  "Return non-nil in an object scope can be entered."
  (and (not (divine-scope-p))
            (divine-accept-object-p)))

(defun divine-pending-operator-p ()
  "Return non-nil in Divine is waiting for a text motion to run on operator."
  divine--pending-operator)

(defun divine-run-operator-p ()
  "Return non-nil if there's an active region an operator can work on."
  (or divine--ready-for-operator
      (and (region-active-p)
           (not (eq (region-beginning)
                    (region-end))))))

;;; High-level user interface
;;;; Mode definition interface

(cl-defmacro divine-defmode (name docstring &key cursor cursor-color lighter transient-fn rname &aux (fname (symbol-name name)))
  "Define the Divine mode NAME, with documentation DOCSTRING.

The following keyword arguments are accepted.

 - `:cursor' The cursor style for this mode, as a valid argument
   for `set-cursor', which see.
 - `:cursor-color' The cursor color for this mode, as an hex
   string or en Emacs color name.
 - `:lighter' The mode lighter.
 - `:rname' A readable name for the mode, as a string.
 - `:transient-fn' The name of the function used to temporarily activate the mode."
  (declare (indent defun))
  ;; Guess :rname
  (unless rname
    (unless (and (string-prefix-p "divine" fname)
                 (string-suffix-p "-mode" fname))  ; Sanity check.
      (error "I cannot guess :rname if name doesn't look like `divine-*-mode'"))
    (setq rname (capitalize (substring fname 7 -5))))
  ;; Guess :lighter
  (unless lighter
    (setq lighter (format "<%s>" (substring rname 0 1))))
  ;; Guess :transient-fn
  (unless transient-fn
    (let ((pos (cl-search "-" fname)))
      (unless pos (error "I cannot guess :transient-fn if name doesn't contain at least one dash")) ; Sanity check.
      (setq transient-fn (intern (format "%s-transient-%s" (substring fname 0 pos) (substring fname (1+ pos)))))))

  ;; Body
  (let ((cursor-variable (intern (format "%s-cursor" name)))
        (cursor-color-variable (intern (format "%s-cursor-color" name)))
        (map-variable (intern (format "%s-map" name))))
    `(progn
       ;; Customization group
       (defgroup ,name nil
         ,(format "Options for Divine %s mode." rname)
	       :group 'divine)
       ;; Cursor style
       (defcustom ,cursor-variable ,cursor
         ,(format "Cursor style for Divine %s mode." rname)
         :type ',divine-custom-cursor-type)
       (defcustom ,cursor-color-variable ,cursor-color
         ,(format "Cursor color for Divine %s mode." rname)
         :type ',divine-custom-cursor-color-type)
       ;; Variables
       (defvar ,map-variable (make-keymap)
         ,(format "Keymap for Divine %s mode." rname))
       (add-to-list 'divine--modes ',name)
       ;; Transient activation function
       (defun ,transient-fn ()
         ,(format "Transient activation function for Divine %s mode." rname)
         (interactive)
         (push divine--active-mode divine--transient-stack)
         (,name))
       ;; Definition
       (define-minor-mode ,name
         ,docstring
         :lighter nil
         :keymap ,map-variable
         (when ,name
	         (setq-local cursor-type (or ,cursor-variable divine-default-cursor))
           (set-cursor-color (or ,cursor-color-variable divine-default-cursor-color (face-attribute 'default :foreground)))
           (setq divine--lighter (format " Divine%s" ,lighter))
           (divine--disable-modes ',name)
           (force-mode-line-update))))))

;;;; Command definition interface

(defmacro divine-defcommand (name docstring &rest body)
  "Define an interactive command NAME for Divine."
  (declare (indent defun))
  `(defun ,name ()
     ,docstring
     (interactive)
     ,@body
     ;; Preserve prefix argument
     (when (called-interactively-p 'any)
       (setq prefix-arg current-prefix-arg))))

;;;; Action definition interface

(defmacro divine-defaction (name docstring &rest body)
  "Define an action NAME for Divine.

An action is similar to an operator that doesn't need a region.
It is legal whenever an operator is, but is never pending."
  (declare (indent defun))
  `(divine-defcommand ,name ,docstring
     ,@body
     (divine--finalize)))

;;;; Operator definition interface

(defmacro divine-defoperator (name motion docstring &rest body)
  "Define a Divine operator NAME with doc DOCSTRING.

If MOTION is non-nil, and this action is invoked twice in an
immmediate sequence, it is run to create the region.  This allows
to create commands like vim's dd.

BODY is the code of the operator.  It's expected to work between
point and mark.  It can read the current prefix argument, exactly
once, by calling `divine-argument'."
  (declare (indent defun))
  `(divine-defcommand ,name ,docstring
     (cond
      ;; There's a region, act on it
      ((divine-run-operator-p)
       (progn
         ,@body
         (divine-operator-done)))
      ;; No region, and nothing pending: register ourselves
      ((not (divine-pending-operator-p))
       (divine-flash "Pending")
       (push-mark (point) t nil)
       (setq divine--pending-operator ',name))
      ;; We're pending: execute default motion and wait for
      ;; repetition.
      ((eq divine--pending-operator ',name)
       (,motion))
      (  (divine-fail)))))

;;;; Motion definition interface

(defmacro divine-defmotion (name docstring &rest body)
  "Define a Divine text motion NAME with doc DOCSTRING.
BODY should move the point for a regular motion, or both the
point and the mark, as needed for a text object.  Neither motions
nor objects must activate or deactivate the region."
  (declare (indent defun))
  `(divine-defcommand ,name ,docstring
     ,@body))

;;;; Scope definition interface

(defmacro divine-defscope (name)
  "Define the Divine scope modifier NAME.

This macro generates three functions: `divine-scope-NAME-select'
to activate the scope if no other scope is already selected,
`divine-scope-NAME-force' to activate it, optionally replacing an
existing scope, and `divine-scope-NAME-flag', to read and consume
the scope."
  (let ((enter-fn (intern (format "divine-scope-%s-select" name)))
        (force-fn (intern (format "divine-scope-%s-force" name)))
        (pred-fn (intern (format "divine-scope-%s-flag" name))))
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

;;; Misc utilities

(defun divine-read-char (&optional prompt)
  "Show PROMPT, read a single character interactively, and return it."
  (let ((ct cursor-type))
    (when divine-read-char-cursor (setq cursor-type divine-read-char-cursor)
          (if prompt (message "%s" prompt))
          (let ((char (read-char)))
            (if prompt (message "%s%c" prompt char))
            (setq cursor-type ct)
            char))))

(defcustom divine-flash-function 'divine-flash
  "The function used to display mode changes."
  :type 'function)

(defun divine-describe-state (arg)
  "Print a message describing the Divine state for the active buffer.

If ARG is non-nil, copy the message to the kill ring."
  (interactive "P")
  (let ((message
         (format
          "Divine: %s
Controller: %s
Active mode: %s
Actually active modes: %s
Known modes: %s
Transient mode stack: %s
Pending operator: %s
Ready for operator: %s
current-prefix-arg: %s
selected register: %s
Point and mark: (%s %s)
Emacs region active: %s
Motion scope: %s"
          divine-version
          divine-mode
          divine--active-mode
          (seq-filter (lambda (x) (symbol-value x)) divine--modes)
          divine--modes
          divine--transient-stack
          divine--pending-operator
          divine--ready-for-operator
          current-prefix-arg
          divine--register
          (point) (mark)
          (region-active-p)
          divine--object-scope)))
    (message message)
    (when arg
      (with-temp-buffer
        (insert message)
        (kill-ring-save (point-min) (point-max))))))

;;; Conclusion

(provide 'divine-core)

;;; divine-core.el ends here
