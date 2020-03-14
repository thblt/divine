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

(divine-defoperator divine-kill divine-whole-line
  "Kill the relevant something."
  (kill-region (region-beginning) (region-end)))

(divine-defoperator divine-change divine-whole-line
  "Kill the relevant something, then enter insert mode."
  (kill-region (region-beginning) (region-end))
  (divine-insert-mode))

;;; Motions

;;;; Scopes

(divine-defscope around)
(divine-defscope inside)

;;;; Character motion

(divine-defmotion divine-char-forward
  "Move forward ARG character(s)."
  (forward-char (divine-numeric-argument)))

(divine-defmotion divine-char-right
  "Move forward ARG character(s), on the same line."
  (right-char (divine-numeric-argument)))

(divine-defmotion divine-char-left
  "Move backward ARG character(s), on the same line."
  (left-char (divine-numeric-argument)))

(divine-defmotion divine-char-backward
  "Move backward ARG character(s)."
  (backward-char (divine-numeric-argument)))

(divine-defmotion divine-forward-paragraph
  "Move forward ARG paragraph(s)."
  (backward-paragraph (divine-numeric-argument)))

(divine-defmotion divine-backward-paragraph
  "Move backward ARG paragraph(s)."
  (backward-paragraph (divine-numeric-argument)))

;;;; Line motion

(divine-defmotion divine-line-forward
  "Move forward ARG line(s)."
  (next-line (divine-numeric-argument)))

(divine-defmotion divine-line-backward
  "Move forward ARG line(s)."
  (previous-line (divine-numeric-argument)))

(divine-defmotion divine-line-beginning
  "Go to the beginning of current line."
  (beginning-of-line))

(divine-defmotion divine-line-end
  "Go to the end of current line."
  (end-of-line))

(divine-defmotion divine-goto-line
  "Go to the line indicated by the prefix argument, or fail."
  (if (divine-numeric-argument-p)
      (goto-line (divine-numeric-argument))
    (divine-fail)))

(divine-defmotion divine-whole-line
  "Set mark at the first character of the current-line, and point at the last.

This is not meant for use as a direct command, but as the default motion for repeatable commands."
  (beginning-of-line)
  (set-mark (point))
  (end-of-line (divine-numeric-argument)))

;;;; S-exp motion

;;; Hybrids

(divine-defcommand divine-around-or-append
  "If there's a pending operator, maybe set the motion flag to 'around,
  otherwise append at point."
  (cond ((divine-accept-scope-p)
         (divine-enter-around-scope))
        ((divine-accept-operator-p)
         (right-char)
         (divine-insert-mode))))

(divine-defcommand divine-zero
  "If there's a digit argument, multiply it by 10, otherwise go
to the first character in line."
  (if (divine-numeric-argument-p)
      (setq prefix-arg (* arg 10))
    (divine-line-beginning)))

;;; Conclusion

(provide 'divine-commands)

;;; divine.el ends here
