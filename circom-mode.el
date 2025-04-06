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

(defconst circom--keywords
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
    "input"
    "log"
    "output"
    "parallel"
    "pragma"
    "public"
    "return"
    "signal"
    "template"
    "var"
    "while")
  "Circom keywords.")

(defvar circom--regexp-keyword
  (concat
   (rx symbol-start)
   (regexp-opt circom--keywords t)
   (rx symbol-end))
  "Regular expression to match Circom keywords.")

(defvar circom--regexp-template-name
  "[a-zA-Z0-9_]+"
  "Regular expression to match Circom template names.")

(defvar circom--regexp-template-decl-name
  (concat "\s*template\s+\\(" circom--regexp-template-name "\\)\s*\(")
  "Regular expression to match Circom template declaration names.")

(defun circom--match-template-decl-name (limit)
  "Search the buffer forward until LIMIT to match template declaration names."
  (re-search-forward circom--regexp-template-decl-name limit t nil))

(defconst circom-font-lock-keywords
  (list
   `(,circom--regexp-keyword . font-lock-keyword-face)
   '(circom--match-template-decl-name (1 font-lock-function-name-face)))
  "Font lock keywords of `circom-mode'.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Indentation

(defun circom-indent-line (&optional indent)
  "Indent the current line according to the Circom syntax, or supply INDENT."
  (interactive "P")
  (let ((pos (- (point-max) (point)))
        (indent (or indent (circom--calculate-indentation)))
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

(defun circom--calculate-indentation ()
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

(defvar circom--imenu-generic-expression
  `(("Function" ,circom--regexp-template-decl-name 1))
  "Regular expression to generate Imenu outline.")

(defun circom--imenu-create-index ()
  "Generate outline of Circom circuit for imenu-mode."
  (save-excursion
    (imenu--generic-function circom--imenu-generic-expression)))

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
  (setq-local imenu-create-index-function #'circom--imenu-create-index))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.circom\\'" . circom-mode))

;; Finally export the `circom-mode'
(provide 'circom-mode)

;;; circom-mode.el ends here
