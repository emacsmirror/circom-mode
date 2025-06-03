;;; circom-mode.el --- Major mode for editing Circom circuit -*- lexical-binding: t -*-

;; Copyright © 2025, by Ta Quang Trung

;; Author: Ta Quang Trung
;; Version: 0.1
;; Created: 25 March 2025
;; Keywords: languages
;; Package-Requires: ((emacs "24.3"))
;; Homepage: https://github.com/taquangtrung/emacs-circom-mode

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Emacs major mode for editing Circom circuits.

;;; Code:

(require 'rx)
(require 'imenu)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Syntax highlighting

(defconst circom-keywords
  '("assert"
    "bus"
    "circom"
    "component"
    "custom_templates"
    "do"
    "else"
    "for"
    "function"
    "if"
    "include"
    "log"
    "parallel"
    "pragma"
    "public"
    "return"
    "signal"
    "template"
    "var"
    "while")
  "List of Circom keywords.")

(defconst circom-operators
  '("<--"
    "<=="
    "-->"
    "==>"
    "===")
  "List of Circom operators.")

(defconst circom-types
  '("input"
    "output")
  "List of Circom types.")

(defvar circom-keyword-regexp
  (concat
   (rx symbol-start)
   (regexp-opt circom-keywords t)
   (rx symbol-end))
  "Regular expression to match keywords.")

(defvar circom-type-regexp
  (concat
   (rx symbol-start)
   (regexp-opt circom-types t)
   (rx symbol-end))
  "Regular expression to match data types.")

(defvar circom-template-declaration-regexp
  "^\s*template\s+\\([a-zA-Z0-9_]+\\)\s*{"
  "Regular expression to match template declaration types.")

(defun circom-match-regexp (regexp bound)
  "Generic regular expression matching wrapper for REGEXP until a BOUND position."
  (re-search-forward regexp bound t nil))

(defun circom-match-operators (bound)
  "Search the buffer forward until the BOUND position to match operators.
The operators are matched in the 1st group."
  (circom-match-regexp
   (concat (rx (or alnum space "(" ")"))
           (regexp-opt circom-operators t)
           (rx (or alnum space "(" ")")))
   bound))

(defun circom-match-template-declaration (bound)
  "Search the buffer forward until BOUND to match template declaration names."
  (circom-match-regexp circom-template-declaration-regexp bound))

(defconst circom-font-lock-keywords
  (list
   `(,circom-keyword-regexp . font-lock-keyword-face)
   `(,circom-type-regexp . font-lock-type-face)
   `(circom-match-operators (1 font-lock-operator-face keep))
   `(circom-match-template-declaration (1 font-lock-function-name-face)))
  "Font lock keywords of `circom-mode'.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Indentation

(defun circom-indent-line (&optional indent)
  "Indent the current line according to the Circom syntax, or supply INDENT."
  (interactive "P")
  (let ((pos (- (point-max) (point)))
        (indent (or indent (circom-calculate-indentation)))
        (shift-amount nil)
        (beg (line-beginning-position)))
    (skip-chars-forward " \t")
    (if (null indent)
        (goto-char (- (point-max) pos))
      (setq shift-amount (- indent (current-column)))
      (unless (zerop shift-amount)
        (delete-region beg (point))
        (indent-to indent))
      (when (> (- (point-max) pos) (point))
        (goto-char (- (point-max) pos))))))

(defun circom-calculate-indentation ()
  "Calculate the indentation of the current line."
  (let (indent)
    (save-excursion
      (back-to-indentation)
      (let* ((ppss (syntax-ppss))
             (depth (car ppss))
             (base (* tab-width depth)))
        (unless (= depth 0)
          (setq indent base)
          (cond ((looking-at "\s*[})]")
                 ;; closing a block or a parentheses pair
                 (setq indent (- base tab-width)))
                ((looking-at "\s*:=")
                 ;; indent for multiple-line assignment
                 (setq indent (+ base (* 2 tab-width))))
                ((looking-back "\s*:=\s*\n\s*" nil nil)
                 ;; indent for multiple-line assignment
                 (setq indent (+ base (* 2 tab-width))))))))
    indent))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Imenu settings

(defvar circom-imenu-generic-expression
  `(("Function" ,circom-template-declaration-regexp 1))
  "Regular expression to generate Imenu outline.")

(defun circom-imenu-create-index ()
  "Generate outline of Circom circuit for imenu-mode."
  (save-excursion
    (imenu--generic-function circom-imenu-generic-expression)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Define major mode

(defvar circom-syntax-table
  (let ((syntax-table (make-syntax-table)))
    ;; C++ style comment "// ..."
    (modify-syntax-entry ?\/ ". 124" syntax-table)
    (modify-syntax-entry ?* ". 23b" syntax-table)
    (modify-syntax-entry ?\n ">" syntax-table)
    syntax-table)
  "Syntax table for `circom-mode'.")

;;;###autoload
(define-derived-mode circom-mode prog-mode
  "circom-mode"
  "Major mode for editing Circom circuit."
  :syntax-table circom-syntax-table

  ;; Syntax highlighting
  (setq font-lock-defaults '(circom-font-lock-keywords))

  ;; Indentation
  (setq-local indent-tabs-mode nil)
  (setq-local indent-line-function #'circom-indent-line)

  ;; Set comment command
  (setq-local comment-start "//")
  (setq-local comment-end "")
  (setq-local comment-multi-line nil)
  (setq-local comment-use-syntax t)

  ;; Configure imenu
  (setq-local imenu-create-index-function #'circom-imenu-create-index))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.circom\\'" . circom-mode))

;; Finally export the `circom-mode'
(provide 'circom-mode)

;;; circom-mode.el ends here
