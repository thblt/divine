;;; divine-core.el --- Core infrastructure for Divine or your own modal editor  -*- lexical-binding: t; coding: utf-8 -*-

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

;; This module provides the core Divine framework, over which Divine
;; is built.

;; This core can also be reused as the foundation for your own modal
;; interface.  To get started, (info "(divine)The Divine framework")
;; is a recommended reading.

;;; Code:

(eval-when-compile
  (require 'cl-lib))

;;; Constants

(defconst divine-version (list 0 0)
  "The Divine version number.

This is a list of the form (major minor patch pre-release).  To
  get the version as a string, call `divine-version'.")

(defconst divine-custom-cursor-type
  '(choice
    (const :tag "Frame default" t)
    (const :tag "Filled box" box)
    (const :tag "Hollow cursor" hollow)
    (const :tag "Vertical bar" bar)
    (cons :tag "Vertical bar with specified width"
          (const bar) integer)
    (const :tag "Horizontal bar" hbar)
    (const :tag "Horizontal bar with specified width"
           (const hbar) integer)
    (const :tag "None " nil)) ; To update, C-u eval (custom-variable-type 'cursor-type)
  "A customize :type for `cursor-type'.")

(defconst divine-custom-cursor-color-type '(choice (const :tag "Default" nil)
                                                   (color :tag "Color"))
  "A customize :type for `set-cursor-color'.")

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

(defcustom divine-operator-default-objects-alist nil
  "An alist of (OPERATOR . OBJECT), mapping operators to the
  default object to call when the operator is called on itself.")

;;; Variables

;;;; Global

(defvar divine-modes nil
  "List of known divine modes.")

(defvar divine-pending-operator-hook nil
  "Hook run when Divine enters or leaves pending operator state.")

(defvar divine-clear-state-functions (list 'divine--clear-state)
  "Functions to call for dropping buffer state.

If you add state variables for your custom commands, create a
function to clear them (return them to their initial value) and
add it to this list.  Don't remove `divine-clear-state' or you
will break Divine.

Functions in this list should accept, and ignore, any number of
arguments.")

;;;; Buffer runtime state

(defvar-local divine--transient-stack nil
  "Stack of modes to restore after a transient operation.")

(defvar-local divine--active-mode nil
  "The currently active Divine mode.")

(defvar-local divine--ready-for-operator nil
  "Whether Divine region is ready, that is, the operator can act
over it.  This is set by all motions and text objects, and can be
non-nil even if the region has no length.

The point of this variable is to make sure that even if a
motion/object command produced an empty region, the operator will
run over it (probably doing nothing) and the pending operator
state will be terminated.")
;; @FIXME Make this behavior configurable. This can happen, for
;; example, using the end-of-line motion when already at the end of
;; the line.  What should happen in such case should be
;; user-configurable.
;; @FIXME Does this make sense?  Two cases:
;;  - A basic Emacs motion kept the point where it is.
;;  - A Divine object created an empty region.
;;
;; This can be useful for functions like [c]hange
;; @FIXME I don't believe we actually use this variable.

(defvar-local divine--pending-operator nil
  "The operator waiting for a motion.")

(defvar-local divine--transient-cursor-color-stack nil
  "Stack of cursor styles to restore in
`divine-post-command-restore-cursor'.")

(defvar-local divine--continue nil
  "Whether the Divine state must be preserved at the end of the
command loop.  This variable should be set by `divine-continue',
which see.

When inspected interactively, this variable is always nil.")

;;;;; Cosmetics

(defvar-local divine--lighter nil
  "The minor mode lighter.")

;;; Core infrastructure

;;;; Control mode

;;; ### autoload
(define-minor-mode divine-mode
  "Divine, a modal interface with text objects, or something
close enough."
  :lighter (:eval divine--lighter)
  :group 'divine
  (if divine-mode
      ;; Enter
      (progn
        (divine-clear-state)

        ;; (add-hook 'pre-command-hook 'divine-pre-command-hook)
        (add-hook 'post-command-hook 'divine-post-command-hook)

        ;; Make digit/universal arguments force Divine to preserve its
        ;; state.  I tried with a special list of functions that would
        ;; prevent state clearing in post-command-hook, but
        ;; `digit-argument' and `universal-argument' set
        ;; `this-command' and `real-this-command' to `last-command'
        ;; and `real-last-command', respectively, so they're
        ;; impossible to detect.
        (add-hook 'prefix-command-preserve-state-hook 'divine-continue)

        (if (fboundp 'divine-start)
            (divine-start)
          (error "Function `divine-start' undefined.  See Divine manual")))
    ;; Leave
    (remove-hook 'post-command-hook 'divine-post-command-hook)
    (remove-hook 'prefix-command-preserve-state-hook 'divine-continue)
    (divine--disable-modes nil)
    (divine-clear-state)))

;;; ### autoload
(define-globalized-minor-mode divine-global-mode divine-mode divine-mode)

;;;; Command loop

(defun divine-post-command-hook ()
  "Finalize pending operators."
  ;; Run pending operator, if any.
  (when (and (divine-pending-operator-p)
             (not (eq (point) (mark))))
    (divine-operator-run-pending))
  ;; Drop or persist state
  (if (or divine--continue (eq this-command 'undefined))
      (progn
        (setq current-prefix-arg prefix-arg)
        (setq divine--continue nil))
    (divine-clear-state)))

;;;; Buffer state manipulation

(defun divine-continue ()
  "Make unconsumed Divine state variables persist after the current command."
  (setq divine--continue t))

(defun divine-terminate ()
  "Cancel divine-continue."
  (setq divine--continue nil))

(defun divine-clear-state ()
  "Restore base state by running `divine-clear-state-functions'."
  (run-hooks 'divine-clear-state-functions))

(defun divine--clear-state ()
  "Restore base state."
  ;; @FIXME I removed the code that reset the prefix arg. I'm not sure
  ;; what to do with this.
  (mapc (lambda (m) (funcall m t))
        divine--transient-stack)
  (setq divine--transient-stack nil
        divine--object-scope nil
        divine--ready-for-operator nil)
  (when divine--pending-operator
    (divine-operator-set-pending nil)))

(defun divine-quit-transient-modes ()
  "Terminate all transient modes."
  (while divine--transient-stack
    (funcall (pop divine--transient-stack) t)))

;;;; Pending operators

(defun divine-operator-set-pending (operator)
  "Set OPERATOR as pending.  If nil, unset the pending operator.

If the pending operator was modified, set or unset, run
`divine-pending-operator-hook'."
  (unless (eq divine--pending-operator
              (setq divine--pending-operator operator))
    (run-hooks 'divine-pending-operator-hook)))

(defun divine-operator-select (operator)
  "Select OPERATOR.

If region is active, execute it immediately.  Otherwise, install
it as pending if no operator already is.  If OPERATOR is already
pending, execute the default object from
`divine-operator-default-objects-alist', which see."
  (message "Select %s" operator)
  (cond
   ;; There's already a region.
   ((divine-run-operator-p)
    (funcall operator :run))
   ;; There's no region: install the operator.
   ((not (divine-pending-operator-p))
    (divine-operator-set-pending operator)
    (push-mark (point) t )
    (divine-continue))
   ;; This operator is already pending: run default motion.
   ((divine-pending-operator-p operator)
    (funcall (alist-get operator divine-operator-default-objects-alist))
    (divine-operator-run-pending))
   ;; Another operator is pending: panic.
   ((divine-pending-operator-p)
    (error "@FIXME Decide what to do.")
    (divine-operator-abort))))

(defun divine-operator-run-pending ()
  "Execute pending operator."
  (when divine--pending-operator
    (setq divine--ready-for-operator t)
    (call-interactively divine--pending-operator)
    (divine-clear-state)
    (divine-terminate)))

(defun divine-operator-swap-pending (repl)
  "Replace the pending operator with an operator picked from REPL.

REPL is either a list of pair of symbols (FROM . TO), where FROM
is the active operator, and TO its replacement, or a function
that takes an operator symbol and returns a replacement
operator, or nil.

If no replacement is found, the currently pending operator remains."
  (when-let (op (divine-pending-operator-p))
    (divine-operator-select
     (or
      (when (functionp repl)
        (funcall repl))
      (when (listp repl)
        (alist-get operator repl))
      op))))

(defun divine-operator-abort ()
  "Abort the pending operator, if any."
  ;; @FIXME Do we use this?
  (when (divine-pending-operator-p)
    (divine-operator-set-pending nil)
    (divine-clear-state)))

(defun divine-operator-done ()
  "Finalize the current operator."
  (divine-terminate))

;;;; Numeric argument support

(defun divine-numeric-argument-p (&optional noconsume)
  "Return non-nil if the numeric argument is defined.

This predicate is only to be used to determine the relevant
function in an hybrid command.  To consume the numeric argument,
even if you ignore the actual value, use
`divine-numeric-argument' or `divine-numeric-argument-flag'."
  (prog1
      current-prefix-arg
    (unless noconsume (setq current-prefix-arg nil))))

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

- NAFLAG is non-nil if the argument was provided by the user.
- COUNT is the value of the numeric argument
- TIMES is the absolute value of the numeric argument
- POSITIVE is non-nil if COUNT is positive or null
- NEGATIVE is (not positive)
- PLUS1 is 1 if POSITIVE, -1 otherwise.
- MINUS1 is minus PLUS1.

This is useful for writing motions and objects. In most of the
cases, you can assume a forward direction provided you use PLUS1
and MINUS1 instead of literal quantities, and your function will
adapt to negative arguments."
  `(let* ((naflag (divine-numeric-argument-p 'noconsume))
          (count (divine-numeric-argument 'noconsume))
          (times (abs count))
          (positive (>= count 0))
          (negative (not positive))
          (plus1 (if positive +1 -1))
          (minus1 (- plus1)))
     ,@body))

(defmacro divine-with-register (&rest body)
  "Evaluate BODY in an environment where REGISTER is bound to the
selected REGISTER, and consume it."

  `(let* ((register (divine-register)))
     ,@body))

(defmacro divine-with-numeric-argument-and-register (&rest body)
  "Wrap BODY in `divine-with-numeric-argument' and
`divine-with-register', which see."
  `(divine-with-numeric-argument
    (divine-with-register
     ,@body)))

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

If NAME is nil, it's generated with
`divine--reverse-direction-words', which see."
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
left by right, and conversely, and return the modified symbol."
  (with-temp-buffer
    (insert STRING)
    (dolist (pair '(("next" . "previous")
                    ("forward" . "backward")
                    ("left" . "right")))
      (goto-char (point-min))
      (divine--word-replace-both-ways (car pair) (cdr pair)))
    (buffer-string)))

;;;; Messages

(defun divine-flash (msg &rest args)
  "Display MSG with `message'."
  (apply 'message msg args))

;;;; Internal

(defun divine--disable-modes (&optional except)
  "Disable all modes in `divine-modes' except EXCEPT."
  (dolist (mode divine-modes)
    (unless (eq mode except)
      (funcall mode 0)))
  (setq-local divine--active-mode except))

;;; Low-level command interface

;;;; Predicates

(defun divine-accept-action-p ()
  "Return non-nil in an action command can be entered."
  (not (or (region-active-p)
           (divine-pending-operator-p))))

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

(defun divine-pending-operator-p (&optional operator)
  "Return non-nil if an operator is pending.

If OPERATOR is provided, only return non-nil if OPERATOR is the pending operator."
  (if operator
      (eq operator divine--pending-operator)
    divine--pending-operator))

(defun divine-run-operator-p ()
  "Return non-nil if there's an active region an operator can work on."
  (or divine--ready-for-operator
      (and (region-active-p)
           (not (eq (region-beginning)
                    (region-end))))))

;;; High-level programming interface
;;;; Mode definition interface

(cl-defmacro divine-defmode (name docstring &key cursor cursor-color lighter mode-name transient-fn rname)
"Define the Divine mode NAME, with documentation DOCSTRING.

NAME is a short identifier, like normal or insert.

The following optional keyword arguments are accepted.

 - `:cursor' The cursor style for this mode, as a valid argument
   for `set-cursor', which see.
 - `:cursor-color' The cursor color for this mode, as an hex
   string or en Emacs color name.
 - `:lighter' The mode lighter.
 - `:mode-name' The actual Emacs mode name.   This defaults to divine-NAME-mode.
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
     (add-to-list 'divine-modes ',mode-name)
     ;; Transient activation function
     (defun ,transient-fn ()
       ,(format "Transient activation function for Divine %s mode." rname)
       (interactive)
       (push divine--active-mode divine--transient-stack)
       (divine-continue)
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

;; ;;;; Command definition interface
;; @FIXME Remove entirely
;; (defmacro divine-defcommand (name docstring &rest body)
;;   "Define an interactive command NAME for Divine."
;;   (declare (indent defun))
;;   `(defun ,name ()
;;      ,docstring
;;      (interactive)
;;      ,@body
;;      ;; Preserve prefix argument
;;      (when (called-interactively-p 'any)
;;        (setq prefix-arg current-prefix-arg))))

;; ;;;; Action definition interface

;; (defmacro divine-defaction (name docstring &rest body)
;;   "Define an action NAME for Divine.

;; An action is similar to an operator that doesn't need a region.
;; It is legal whenever an operator is, but is never pending."
;;   (declare (indent defun))
;;   `(divine-defcommand ,name ,docstring
;;      ,@body
;;      (divine--finalize)))


;;;; Operator definition interface

(defmacro divine-defoperator (name docstring &rest body)
  "Define a Divine operator NAME-operator, implemented as NAME,
with doc DOCSTRING.

BODY is the code of the operator.  It should work on the region,
whether it's active or not, by reading `(region-beginning)'
and `(region-end)'.

The optional keyword argument INSTALL is evaluated whenever the
operator gets activated, even if it's made pending.

BODY will only be executed if and when the operator runs.

The region may be empty, if this is an issue for your command,
wrap in `(unless (eq (point) (mark)) ...)'.

Operators need not check anything about the runtime Divine state,
although they can of course access the state variables they need
to adjust their behavior.  To do so, please only use the divine-
accessors, like `divine-numeric-argument', which will correctly
consume the state variables.  Notice that if the operator runs
after having been pending, it will read state after the motion
that created the region, so part of the state may have already
been consumed."
  (declare (indent defun))
  `(defun ,name (&optional run)
     ,docstring
     (interactive)
     (if run
         (progn ,@body)
       (divine-operator-select ',name))))

;;;; Motion/objects definition interface

;; FIXME This works more or less, but is a bit broken.  With a scope
;; modifier, if we start *between* words with forward, it selects the
;; word before.  To fix this:
;;
;; - Add a :predicate argument to determine if we're actually on a
;;   THING, and decide what to do from here.  bounds-of-thing-at-point
;;   works (it returns nil or a pair).
;;   => DON'T bounds-of-thing-at-point is too broad, it returns something even on a space between words.

(defmacro divine--defobject1 (name docstring reversed forward backward beginning end extend-before extend-after)
  "Generate a function body for divine-defobject.

If REVERSED is non-nil, we behave as a backward function."
  `(defun ,name ()
     (interactive)
     (divine-with-numeric-argument
      (let ((the-right-way (eq ,reversed negative))
            (point))
        (if (not (divine-scope-p 'noconsume))
            ;; Just a motion
            (if the-right-way
                (,forward times)
              (,backward times))
          ;; There's a scope.

          ;; We move to the first end
          (if the-right-way
              (,beginning)
            (,end))

          ;; Are we *around*? If so, we get there.
          (save-excursion
            (when (divine-scope-around-p 'noconsume)
              (if the-right-way (,extend-before) (,extend-after)))
            (push-mark)
            (activate-mark))
          ;; Moving
          (if the-right-way (,forward times) (,backward times))
          (if the-right-way (,end) (,beginning))
          ;;
          (when (divine-scope-around-p)
            (if the-right-way (,extend-after) (,extend-before))))))))

(cl-defmacro divine-defobject
    (name docstring &key forward backward beginning end (extend-before 'ignore) (extend-after 'ignore) (special 'identity))
  "Define the object NAME documented with DOCSTRING.

Two functions will be created: divine-NAME-forward and
divine-NAME-backward.  They will behave as a regular motion,
unless the 'inside or 'around scope is activated.

The object is constructed with the following keyword arguments:

- FORWARD is a command that moves the point to the next object
of this motion.
- BACKWARD is FORWARD in reverse; if absent, forward is called
with a reversed argument.
- BEGINNING moves point to the beginning of the object at point.
- END moves point to the end of the object at point.
- EXTEND-BEFORE moves the point from the beginning of the object
in a way that makes sense in the context of the 'around
modifier.  If empty, it defaults to (BACKWARD) (END).
- EXTEND-AFTER is the same in reverse, and defaults
to (FORWARD) (BEGINNING).
- SPECIAL is a function that receives the pending operator (as a
  symbol) and return another symbol that gets run as the actual
  operator.  Default is `identity'." ; TODO Verify that we document special objecs somewhere.
  `(progn
     (divine--defobject1
      ,(intern (format "divine-%s-forward" name))
      ,docstring
      nil
      ,forward
      ,backward
      ,beginning
      ,end
      ,extend-before
      ,extend-after)
     (divine--defobject1
      ,(intern (format "divine-%s-backward" name))
      ,docstring
      t
      ,forward
      ,backward
      ,beginning
      ,end
      ,extend-before
      ,extend-after)))

(defun divine-create-standard-scopes ()
  "Create the standard scopes required by motions created by `divine-defobject'."
  ;; @FIXME @XXX Duplicated from divine-commands, also make core depend on
  ;; command, which defines defscope. BAD.
  (divine-defscope inside)
  (divine-defscope around)
  )

;;;; Cursor handling

(defun divine--set-cursor (&optional style color)
  "Set cursor to STYLE and COLOR, if set, and install a hook to
restore them after current command returns."
  ) ; @FIXME

;;; Misc utilities

(defun divine-read-char (&optional prompt)
  "Show PROMPT, read a single character interactively, and return it."
  (let ((inhibit-quit t)
        (ct cursor-type)
        (char))
    (when divine-read-char-cursor (setq cursor-type divine-read-char-cursor))
    (when prompt (message "%s" prompt))
    (with-local-quit
      (setq char (read-char))
      (when prompt (message "%s%c" prompt char)))
    (setq cursor-type ct
          inhibit-quit nil)
    char))

(defun divine-then (command)
  "Execute COMMAND interactively, as if it was called by the user."
  (let ((command (command-remapping command)))
    (setq this-command command)
    (call-interactively command)))

(defun divine-const (x)
  "Returns a function that ignores all arguments and return X."
  (lambda (&rest _) x))

;;; Debug and information

(defun divine-version (&optional show)
  "Divine version number."
  (interactive (list t))
  (let* ((major (car divine-version))
         (minor (cadr divine-version))
         (patch (caddr divine-version))
         (pre (cadddr divine-version))
         (version (seq-concatenate
                   'string
                   (format "%s.%s" major minor)
                   (if patch (format ".%s" patch) "")
                   (if pre (format "-%s" pre) ""))))
    (when show
      (message "Divine %s (%s)" version (symbol-file 'divine-version)))
    version))

(defun divine-describe-state (arg)
  "Print a message describing the Divine state for the active buffer.

If ARG is non-nil, copy the message to the kill ring."
  ;; @FIXME This uses variables from divine.el, move it there?
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
prefix-arg: %s
selected register: %s
Point and mark: (%s %s)
Emacs region active: %s
Motion scope: %s"
          divine-version
          divine-mode
          divine--active-mode
          (seq-filter (lambda (x) (symbol-value x)) divine-modes)
          divine-modes
          divine--transient-stack
          divine--pending-operator
          divine--ready-for-operator
          current-prefix-arg
          prefix-arg
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
