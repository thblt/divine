;;; divine-experimental.el --- Experimental Divine features  -*- lexical-binding: t; coding: utf-8 -*-

;;; Repetition

;; Repetition reproduces Vim's . command.

;; TODO:

;;;; Customizations

(defcustom divine-repeatable-history-size 16
  "The maximum number of repeatable operations to store for each Divine buffer.

Modifying this value will only affect new buffers."
  :type 'integer)

;;;; Types

(defstruct (divine-repeatable
            (:constructor divine-repeatable-make))
  "A repeatable command.

This stores a restorable Divine state that can be pushed to the
repetition ring."
  (register
   nil
   :documentation "The user-selected register, or nil.")
  (numeric-argument
   nil
   :documentation "The value of the numeric argument.")
  (scope
   nil
   :documentation "The user-selected scope, or nil.")
  (operator
   nil
   :documentation "The operator or action to repeat.")
  (motion
   nil
   :documentation "The motion to apply if the command is an
   operator."))

;;;; Variables

(defvar-local divine--repeatable-ring (make-ring divine-repeatable-history-size)
  "The ring of `divine-repeatable' objects used by `divine-repeat'.")

(defvar-local divine--repeating nil
  "Automatically set to t when Divine is running a repeated
  command.")

;;;; Functions

;;;;; Infrastructure

(defun divine-repeatable-run (r)
  "Run R, a `divine-repeatable'."
  (assert (divine-repeatable-p r))
  ;; Init state.
  (setq divine--repeating t
        prefix-arg (if (divine-numeric-argument-p) (divine-numeric-argument) (divine-repeatable-count r))
        divine--register (divine-repeatable-register r)
        divine--object-scope (divine-repeatable-scope r))
        divine--pending-operator (divine-repeatable-command r))

;;;;; Interactive interface

(divine-defaction divine-repeat
  "Repeat last command COUNT times."
  (when (ring-empty-p) (divine-fail "Nothing to repeat."))
  (divine-repeatable-run (ring-) )
  @TODO)

(divine-defaction divine-repeatable-ring-rotate
  "Rotate the repeatable ring by COUNT position(s).

the last defined element.
If the ring is partially empty, this will position the ring to

If the ring is totally empty, this has no effect."
  (unless (ring-empty-p divine--repetition-ring)
    (dotimes (_ (mod (divine-numeric-argument) (ring-size divine--repetition-ring)))))
  @TODO)
