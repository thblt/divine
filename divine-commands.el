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

;;; Operators

;;;; Killing

(divine-defoperator divine-kill divine-whole-line-object
  "Kill the relevant something."
  (kill-region (region-beginning) (region-end)))

(divine-defoperator divine-change divine-whole-line-object
  "Kill the relevant something, then enter insert mode."
  (kill-region (region-beginning) (region-end))
  (divine-insert-mode))

;;; Motions

;;;; Scopes

(divine-defscope around)
(divine-defscope inside)

;;;; Character motion

(divine-wrap-motion forward-char)
(divine-wrap-motion backward-char)

;;;; Line motion

(divine-defmotion divine-whole-line-motion
  "Set mark at the first character of the current-line, and point at the last.

This is not meant for use as a direct command, but as the default motion for repeatable commands."
  (beginning-of-line)
  (set-mark (point))
  (end-of-line arg))

(divine-defmotion divine-goto-line
  "Go to the line indicated by the prefix argument, or fail."
  (if (divine-numeric-argument-p)
      (goto-line (divine-numeric-argument))
    (divine-fail)))

(divine-wrap-motion end-of-line)
(divine-wrap-motion beginning-of-line)

;;; Hybrids

(defun divine-around-or-append (arg)
  "If there's a command, set the motion flag to 'around, otherwise append at point.")

(defun divine-zero (arg)
  "If there's a digit argument, multiply it by 10, otherwise go
to the first character in line."
  (interactive "P")
  (message "Working with %s" arg)
  (if arg
      (setq prefix-arg (* arg 10))
    (divine-beginning-of-line)))

;;; Conclusion

(provide 'divine-commands)

;;; divine.el ends here
