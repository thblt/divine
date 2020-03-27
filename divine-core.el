;;; divine-core.el --- Core infrastructure for Divine or your own modal editor  -*- lexical-binding: t; coding: utf-8 -*-

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

;; This module provides the core Divine framework.

;;; Code:

(require 'cl-lib)

;;; Constants

(defconst divine-version "0.0"
  "Divine version number.")

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

;;; Variables

(defvar-local divine--lighter nil
  "The current mode lighter.")

(defvar divine--mode-functions nil
  "List of known divine modes")

(defvar divine--modes-symbols-alist nil
  "A list of pairs associating short mode names to their
  corresponding emacs symbol.")

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

(defvar divine-pending-operator-hook nil
  "Hook run when Divine enters or leaves pending operator state.")

;;; Core infrastructure

;;;; Control modes

(define-minor-mode divine-mode
  "Divine, a modal interface with text objects, or something
  close enough."
  :lighter (:eval divine--lighter)
  :group 'convenience
  (if divine-mode
      ;; Enter
      (progn
        (divine--finalize) ; Clear state variables, just in case.
        (add-hook 'pre-command-hook 'divine-pre-command-hook)
        (add-hook 'post-command-hook 'divine-post-command-hook)
        (if (fboundp 'divine-start)
            (divine-start)
          (error "Function `divine-start' undefined.  See Divine manual.")))
    ;; Leave
    (divine--disable-modes nil)
    (divine--finalize)))

(define-globalized-minor-mode divine-global-mode divine-mode divine-mode)

;;;; Command hooks

(defvar-local divine--point nil
  "Previous value of point")

(defun divine-pre-command-hook ()
  (setq divine--point (point)))

(defun divine-post-command-hook ()
  (unless (eq divine--point (point))
    (divine-motion-done)))

;;;; Finalizers

(defun divine--finalize ()
  "Restore base state."
  (setq divine--object-scope nil
        divine--ready-for-operator nil
        divine--register nil
        prefix-arg nil)
  (divine-abort-pending-operator)
  (divine-quit-transient-modes))

(defun divine-abort-pending-operator ()
  ""
  (when (divine-pending-operator-p)
    (setq divine--pending-operator nil)
    (run-hooks 'divine-pending-operator-hook)
    t))

(defun divine-quit-transient-modes ()
  "Terminatate all transient modes."
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
  "Disable all modes in `divine--mode-functions' except EXCEPT."
  (dolist (mode divine--mode-functions)
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

(cl-defmacro divine-defmode (name docstring &key cursor cursor-color lighter mode-name transient-fn rname)
  "Define the Divine mode ID, with documentation DOCSTRING.

NAME is a short identifier, like normal or insert.

The following optional keyword arguments are accepted.

 - `:cursor' The cursor style for this mode, as a valid argument
   for `set-cursor', which see.
 - `:cursor-color' The cursor color for this mode, as an hex
   string or en Emacs color name.
 - `:lighter' The mode lighter.
 - `:mode-name' The actual Emacs mode name. This defaults to divine-NAME-mode.
 - `:rname' A readable name for the mode, as a string.
 - `:transient-fn' The name of the function used to temporarily activate the mode."
  (declare (indent defun))
  ;; Guess :rname
  (unless rname
    (setq rname (capitalize (symbol-name name))))
  ;; Guess :lighter
  (unless lighter
    (setq lighter (format "<%s>" (substring rname 0 1))))
  ;; Guess :mode-name
  (unless mode-name
    (setq mode-name (intern (format "divine-%s-mode" name))))
  ;; Guess :transient-fn
  (unless transient-fn
    (setq transient-fn (intern (format "divine-transient-%s-mode" name))))

  ;; Body
  (let ((cursor-variable (intern (format "%s-cursor" mode-name)))
        (cursor-color-variable (intern (format "%s-cursor-color" mode-name)))
        (map-variable (intern (format "%s-map" mode-name))))
    `(progn
       ;; Customization group
       (defgroup ,mode-name nil
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
       (add-to-list 'divine--mode-functions ',mode-name)
       (push '(,name . ,mode-name) divine--modes-symbols-alist)
       ;; Transient activation function
       (defun ,transient-fn ()
         ,(format "Transient activation function for Divine %s mode." rname)
         (interactive)
         (push divine--active-mode divine--transient-stack)
         (,mode-name))
       ;; Definition
       (define-minor-mode ,mode-name
         ,docstring
         :lighter nil
         :keymap ,map-variable
         (when ,mode-name
	         (setq-local cursor-type (or ,cursor-variable divine-default-cursor))
           (set-cursor-color (or ,cursor-color-variable divine-default-cursor-color (face-attribute 'default :foreground)))
           (setq divine--lighter (format " Divine%s" ,lighter))
           (divine--disable-modes ',mode-name)
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

(defmacro divine-defoperator (name docstring &rest body)
  "Define a Divine operator NAME with doc DOCSTRING.

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
      ;; No region, and nothing pending: register ourselves.
      ((not (divine-pending-operator-p))
       (divine-flash "Pending")
       (push-mark (point) t nil)
       (setq divine--pending-operator ',name)
       (divine-quit-transient-modes)
       (run-hooks 'divine-pending-operator-hook))
      ;; Fail, probably because there's a pending operator already.
      (  (divine-fail)))))

(cl-defmacro divine-wrap-operator (command &key)
  "Wrap the Emacs command COMMAND as a Divine operator.

The resulting operotar is called divine-NAME."
  (let ((name (intern (format "divine-%s" command))))
    `(divine-defoperator ,name
       ,(format "Divine operator wrapper around `%s', which see." command)
       (call-interactively ',command))))

;;;; Motion definition interface

(defmacro divine-defmotion (name docstring &rest body)
  "Define a Divine text motion NAME with doc DOCSTRING.
BODY should move the point for a regular motion, or both the
point and the mark, as needed for a text object.  Neither motions
nor objects must activate or deactivate the region."
  (declare (indent defun))
  `(divine-defcommand ,name ,docstring
     ,@body
     ;; divine-motion-done may be called, again, in the
     ;; post-command-hook. This is unavoidable: some Divine motions
     ;; may not move the point (for example, divine-line-contents on
     ;; an empty line) but should run the pending operator regardless.
     (divine-motion-done)))

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
          (seq-filter (lambda (x) (symbol-value x)) divine--mode-functions)
          divine--mode-functions
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

;;; Key binding interface

(defconst divine--binding-states '(region-active
                                   numeric-argument
                                   repeated-operator
                                   operator-pending
                                   t)
  "Valid states for divine conditional bindings, by order of evaluation.")

(defun divine--make-binding-function-name (mode key)
  "Make a unique symbol from MODE and KEY."
  (intern (format "divine--%s-in-%s-mode" (key-description key) mode)))

(cl-defun divine-define-key (mode key command &key ((:mode emacs-mode) 't) ((:state state) 't))
  "Bind KEY to COMMAND in Divine mode MODE.

 - MODE is the short name of a Divine mode, like 'normal or 'insert.

 - STATE is a predicate that depends of the current
   interactive state of Divine.  It usually corresponds to the
   type of the command.


 - EMACS-MODE is a symbol identifying an Emacs major or minor
   mode.

Bindings are compiled by `divine-compile-bindings', which see."
  ;; Sanity checks
  (unless (symbolp mode) (error "MODE must be a symbol"))
  (unless (symbolp command) (error "COMMAND must be a symbol"))
  (unless (symbolp emacs-mode) (error "In `:mode m', m must be a symbol"))
  (unless (member state divine--binding-states)
    (error "In `:state s', s must be one of %s, not %s" divine--binding-states state))
  ;; Normalize key
  (when (stringp key) (setq key (kbd key)))

  (let ((name (divine--make-binding-function-name mode key))
        (binding (list emacs-mode state command)))

    ;; Create binding variable if necessary
    (unless (boundp name)
      (set name nil))

    ;; Delete existing value, if any
    (set name
         (cl-delete-if
          (lambda (b) (and
                       (eq (car b) emacs-mode)
                       (eq (cadr b) state)))
          (symbol-value name)))

    (when command
      ;; Insert binding
      (push binding (symbol-value name))

      ;; Sort bindings
      (set name (sort (symbol-value name) 'divine--binding<))

      ;; Create function
      (fset name (lambda () (interactive)
                   "@TODO Docstring generation not implemented."
                   (divine--run-binding (symbol-value name))))

      ;; Create binding
      (define-key
        (symbol-value (intern (format "%s-map" (alist-get mode divine--modes-symbols-alist))))
        key
        (if (and
             (eq 1 (length (symbol-value name)))
             (eq t emacs-mode)
             (eq t state))
            command
          name)))))

(defun divine--eval-binding-predicate (pred)
  (pcase pred
    ('region-active (region-active-p))
    ('numeric-argument (divine-numeric-argument-p))
    ('repeated-operator (and (divine-pending-operator-p)
                             (eq this-command last-command)))
    ;; @FIXME ^ This is broken. It will hold whenever a binding is
    ;; repeated with a pending operator.
    ('operator-pending (divine-pending-operator-p))
    (_ t)))

(cl-defun divine--run-binding (candidates &aux command)
  (interactive)
  (while (and candidates (not command))
    (let ((cand (car candidates)))
      (setq command (and (eval (car cand))
                         (divine--eval-binding-predicate (cadr cand))
                         (caddr cand))
            candidates (cdr candidates))))
  (if command
      (funcall-interactively command)
    (divine-fail)))

(defun divine--string< (a b)
  "Utility sort helper.  Like `string<', but sorts the symbol t
after everything."
  "Comparison function for bindings Emacs Mode value."
  (cond ((string= a b) nil)
        ((eq t a) nil)
        ((eq t b) t)
        (t (string< a b))))

(defun divine--binding< (a b)
  "Utility sort helper.  Compare bindings to sort them in a way
that makes sense."
  "Comparison function for bindings Emacs Mode value."
  (let ((emode-a (car a))
        (emode-b (car b))
        (state-a (cadr a))
        (state-b (cadr b))
        (command-a (cadr a))
        (command-b (cadr b)))
    (if (eq emode-a emode-b)
        (if (eq state-a state-b)
            (string= command-a command-b)
          (divine--state< state-a state-b))
      (divine--string< emode-a emode-b))))

(defun divine--state< (a b &optional list)
  "Return t if a comes in LIST before B, nil otherwise.

A and B are symbols, and should not be equal."
  (unless list (setq list divine--binding-states))
  (let ((x (car list)))
    (cond
     ((null list) nil)
     ((eq x a) t)
     ((eq x b) nil)
     (t (divine--state< a b (cdr list))))))

;;; Conclusion

(provide 'divine-core)

;;; divine-core.el ends here
