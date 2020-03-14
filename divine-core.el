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
(require 'dash)

;;; Constants

(defconst divine-version "0.0 alpha")

;;; Customizations

(defgroup divine nil
  "Modal interface with text objects, or something close enough.") ; @DESC

(defcustom divine-default-cursor 'box
  "Default cursor style for modes that don't specify it.")

(defcustom divine-default-cursor-color nil
  "Default cursor color for modes that don't specify it.  If nil,
use the foreground color of the default face."
  :group 'divine
  :type 'bool)

(defcustom divine-initial-mode-function 'divine-choose-initial-mode
  "The function that runs the initial divine mode.

Can be either `divine-choose-initial-mode', for a reasonably
smart selection, or one of the divine mode functions, eg
`divine-normal-mode'.")

(defcustom divine-flash-function 'divine-flash
  "The function used to display mode changes.")

;;; Variables

(defvar divine--modes nil
  "List of known divine modes")

(defvar-local divine--pending-operator nil
  "The operator waiting for a motion.")

(defvar-local divine--active-mode nil
  "The currently active Divine mode.")

(defvar-local divine--transient-stack nil
  "Stack of modes to restore after a transient operation.")

(defvar-local divine--motion-scope nil
  "A text motion transformation tag.  In the standard Divine
  interface, this accepts 'around or 'inside, like Vim's `a' and
  `i'.")

(defvar-local divine--region-ready nil
  "Whether Divine region is ready, that it, the operator can act
  over it.")

;;; Core infrastructure

;;;; Finalizers

(defun divine-restore-base-state (&optional nomsg)
  "Restore base state."
  (interactive)
  (setq divine--motion-scope nil
        divine--pending-operator nil
        divine--ready-for-operator nil)
  (unless nomsg
    (divine-flash " --- ABORT ---")))

(defun divine-fail (&optional nomsg)
  "Signal to the user that a binding cannot do anything in that context."
  (interactive)
    (divine-flash " --- UNUSABLE ---"))

(defun divine-abort (&optional nomsg)
  "Abort current operation and restore base state."
  (interactive)
  (divine-restore-base-state)
  (unless nomsg
    (divine-flash " --- ABORT ---")))

;;;; Numeric argument support

(defvar-local divine--numeric-argument-consumed nil
  "Whether the numeric argument has already been consumed")

(defun divine-numeric-argument-p (&optional consumed)
  "Return non-nil if the numeric argument is defined.

If CONSUMED isn't nil, return a value even if the argument was
consumed."
  (and current-prefix-arg
       (or consumed (not divine--numeric-argument-consumed))))

(defun divine-numeric-argument (&optional noconsume)
  "Return the current numeric argument, and consume it.  Further invocations will return nil.

If NOCONSUME is non-nil, the argument isn't consumed.

To check for the presence of a digit argument, use
`divine-digit-argument-p' instead."
  (prog1
      (unless divine--numeric-argument-consumed current-prefix-arg)
      (setq divine--numeric-argument-consumed (not noconsume))))


(defun divine-preserve-numeric-argument (&optional newvalue)
  "Preserve or set the numeric argument.

If NEWVALUE is not nil, the numeric argument is set to that value."
  (setq divine--numeric-argument-consumed nil)
  (when newvalue
    (setq prefix-arg newvalue
          current-prefix-arg newvalue)))

;;;; Messages

(defun divine-flash (msg)
  "Display MSG with `message'."
  (message "%s" msg))

(defun divine-describe-state (arg)
  "Print a message describing the Divine state for the active buffer."
  (interactive "P")
  (let ((message
         (format
          "Divine: %s
Active mode: %s
Actually active modes: %s
All known modes: %s
Transient mode stack: %s
Pending operator: %s
Point and mark: (%s %s)
Divine region usable: %s
Emacs region active: %s
Motion scope: %s"
          divine-version
          divine--active-mode
          (seq-filter (lambda (x) (symbol-value x)) divine--modes)
          divine--modes
          divine--transient-stack
          divine--pending-operator
          (point) (mark)
          (divine-has-region-p)
          (region-active-p)
          divine--motion-scope)))
    (message message)
    (when arg
      (with-temp-buffer
        (insert message)
        (kill-ring-save (point-min) (point-max))))))

;;;; Internal

(defun divine--disable-modes (&optional except)
  "Disable all modes in `divine--modes' except EXCEPT."
  (dolist (mode divine--modes)
    (unless (eq mode except)
      (funcall mode 0)))
  (setq-local divine--active-mode except))

;;; Mode definition interface

;;;; Macros

(cl-defmacro divine-defmode (name docstring &key cursor cursor-color flash lighter transient-fn rname &aux (fname (symbol-name name)))
  "Define the Divine mode NAME, with documentation DOCSTRING.

The following keyword arguments are accepted.

 - `:cursor' The cursor style for this mode, as a valid argument
   for `set-cursor', which see.
 - `:cursor-color' The cursor color for this mode, as an hex
   string or en Emacs color name.
 - `:flash' The string to flash when entering the mode, eg ' ---
   INSERT ---'.
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
  ;; Guess :flash
  (unless flash
    (setq flash (format " --- %s ---" rname)))
  ;; Guess :lighter
  (unless lighter
    (setq lighter (format " <%s>" (substring rname 0 1))))
  ;; Guess :transient-fn
  (unless transient-fn
    (--if-let (cl-search "-" fname) ; Sanity check.
        (setq transient-fn (intern (format "%s-transient-%s" (substring fname 0 it) (substring fname (1+ it)))))
      (error "I cannot guess :transient-fn if name doesn't contain at least one dash.")))

  ;; Body
  (let ((flash-variable (intern (format "%s-flash" name)))
        (cursor-variable (intern (format "%s-cursor" name)))
        (cursor-color-variable (intern (format "%s-cursor-color" name)))
        (map-variable (intern (format "%s-map" name))))
    `(progn
       ;; Customization group
       (defgroup ,name nil
         ,(format "Options for Divine %s mode." rname)
	       :group 'divine)
       ;; Cursor style
       (defcustom ,cursor-variable ,cursor
         ,(format "Cursor style for Divine %s mode." rname))
       (defcustom ,cursor-color-variable ,cursor-color
         ,(format "Cursor color for Divine %s mode." rname))
       ;; Variables
       (defvar ,flash-variable ,flash
         ,(format "Message to flash when activating Divine %s mode." rname))
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
         :lighter ,lighter
         :keymap ,map-variable
         (when ,name
           (funcall divine-flash-function ,flash-variable)
	         (setq-local cursor-type (or ,cursor-variable ,divine-default-cursor))
           (set-cursor-color (or ,cursor-color-variable divine-default-cursor-color (face-attribute 'default :foreground)))
           (divine--disable-modes ',name))))))

;;; Low-level interface for user commands

;;;; Functions

(defun divine-operator-done ()
  "Finalize the current operator."
  (setq divine--pending-operator nil ; Clear the pending operator
        divine--ready-for-operator nil) ; Clear the region
  ;; Quit transient state, if there's one
  (when divine--transient-stack
    (funcall (pop divine--transient-stack) t)))

(defun divine-motion-done ()
  "Finalize the current motion."
  (setq divine--ready-for-operator t)
  (when divine--pending-operator
    (call-interactively divine--pending-operator)))

;;;; Predicates

(defun divine-accept-motion-p ()
  "Return non-nil in a motion command can be entered."
  t)

(defun divine-accept-operator-p ()
  "Return non-nil in an operator can be entered."
  (not (divine-has-pending-operator-p)))

(defun divine-accept-scope-p ()
  "Return non-nil in an operator can be entered."
  (and (divine-accept-motion-p)
       (not divine--motion-scope)))

(defun divine-has-pending-operator-p ()
  "Return non-nil in Divine is waiting for a text motion to run on
  operator."
  divine--pending-operator)

(defun divine-has-prefix-argument-p ()
  "Return non-nil if a prefix argument has been set."
  current-prefix-arg)

(defun divine-run-operator-p ()
  "Return non-nil if there's an active region an operator can
  work on."
  (and divine--ready-for-operator
       (not (eq (region-beginning)
                (region-end)))))

;;; Operator definition interface

(cl-defmacro divine-defoperator (name motion docstring &body body)
  "Define a Divine operator NAME with doc DOCSTRING.

If MOTION is non-nil, and this action is invoked twice in an
immmediate sequence, it is run to create the region.  This allows
to create commands like vim's dd.

BODY is the code of the operator.  It's expected to work between
point and mark.  It can read the current prefix argument, exactly
once, by calling `divine-argument'."
  (declare (indent defun))
  `(defun ,name ()
     (interactive)
     ,docstring
     (cond
      ;; There's a region, act on it
      ((divine-run-operator-p)
       (progn
         ,@body
         (divine-operator-done)))
      ;; No region, and nothing pending: register ourselves
      ((not divine--pending-operator)
       (divine-flash "Pending")
       (setq divine--pending-operator ',name))
      ;; We're pending: execute default motion and wait for repetition.
      ((eq divine--pending-operator ',name)
       (push-mark t)
       (call-interactively ',motion)
       (if (divine-run-operator-p)
           (call-interactively ',name)
         (divine--abort))))))

(defmacro divine-defscope (name)
  "Define the Divine scope modifier NAME."
  (let ((enter-name (intern (format "divine-enter-%s-scope" name)))
        (force-name (intern (format "divine-force-%s-scope" name)))
        (pred-name (intern (format "divine-%s-scope-p" name))))
    `(progn
       (defun ,enter-name ()
         ,(format "Activate Divine %s scope if no other scope is active." name)
         (interactive)
         (unless divine--motion-scope
           (setq divine--motion-scope ',name)))
       (defun ,force-name ()
         ,(format "Activate Divine %s scope even if another scope is active." name)
         (interactive)
         (setq divine--motion-scope ',name))
       (defun ,pred-name ()
         ,(format "Return non-nil if Divine scope %s is active" name)
         (eq divine--motion-scope ',name)))))

;;; Motion definition interface

;;;; Macros

(defmacro divine-defmotion (name docstring &rest body)
  "Define a Divine text motion NAME with doc DOCSTRING."
  (declare (indent defun))
  `(defun ,name (arg)
     ,docstring
     (interactive "P")
     ,@body
     (divine-motion-done)))

(defmacro divine-wrap-motion (command &optional motion)
  "Wrap Emacs command COMMAND as a Divine text motion MOTION.

If omitted or nil, MOTION is COMMAND prefixed with `divine-'."
  (unless motion
    (setq motion (intern (format "divine-%s" command))))
  `(defun ,motion (arg)
     ,(format "Divine motion wrapper around `%s', which see." command)
     (interactive "P")
     (call-interactively ',command)
     (when divine--pending-operator
       (call-interactively divine--pending-operator))))

;;; Hydrid interface

;; A lower-level interface for commands whose type depends on context.

;;; Conclusion

(provide 'divine-core)

;;; divine-core.el ends here
