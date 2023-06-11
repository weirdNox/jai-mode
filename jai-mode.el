;;; jai-mode.el --- Major mode for JAI  -*- lexical-binding: t; -*-

;; Copyright (C) 2015-2023  Kristoffer Grönlund

;; Author: Kristoffer Grönlund <k@ziran.se>
;; Maintainer: Kristoffer Grönlund <k@ziran.se>
;; URL: https://github.com/krig/jai-mode
;; Version: 0.0.1
;; Package-Requires: ((emacs "26.1"))
;; Keywords: languages

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; Major mdoe for JAI
;;

;;; Code:

(require 'cc-mode)
(require 'cc-langs)
(require 'js)

(require 'newcomment)
(require 'rx)

(defconst jai-mode-syntax-table
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?\" "\"" table)
    (modify-syntax-entry ?\\ "\\" table)

    (modify-syntax-entry ?_ "_" table)

    (modify-syntax-entry ?' "." table)
    (modify-syntax-entry ?: "." table)
    (modify-syntax-entry ?+  "." table)
    (modify-syntax-entry ?-  "." table)
    (modify-syntax-entry ?%  "." table)
    (modify-syntax-entry ?&  "." table)
    (modify-syntax-entry ?|  "." table)
    (modify-syntax-entry ?^  "." table)
    (modify-syntax-entry ?!  "." table)
    (modify-syntax-entry ?$  "/" table)
    (modify-syntax-entry ?=  "." table)
    (modify-syntax-entry ?<  "." table)
    (modify-syntax-entry ?>  "." table)
    (modify-syntax-entry ??  "." table)

    ;; Modify some syntax entries to allow nested block comments
    (modify-syntax-entry ?/ ". 124b" table)
    (modify-syntax-entry ?* ". 23n" table)
    (modify-syntax-entry ?\n "> b" table)
    (modify-syntax-entry ?\^m "> b" table)

    table))

(defconst jai-builtins
  '("cast" "it" "type_info" "size_of"))

(defconst jai-keywords
  '("if" "ifx" "else" "then" "while" "for" "switch" "case" "struct" "enum"
    "return" "new" "remove" "continue" "break" "defer" "inline" "no_inline"
    "using" "SOA"))

(defconst jai-constants
  '("null" "true" "false"))

(defconst jai-typenames
  '("int" "u64" "u32" "u16" "u8" "s64" "s32" "s16" "s8" "float" "float32" "float64" "string" "bool"))

(defsubst jai-wrap-symbol-rx (s) (concat "\\_<" s "\\_>"))
(defsubst jai-keywords-rx (keywords) (jai-wrap-symbol-rx (regexp-opt keywords t)))

(defconst jai-hat-type-rx (rx (group (and "^" (1+ word)))))
(defconst jai-dollar-type-rx (rx (group "$" (or (1+ word) (opt "$")))))
(defconst jai-number-rx
  (rx (and
       symbol-start
       (or (and (+ digit) (opt (and (any "eE") (opt (any "-+")) (+ digit))))
           (and "0" (any "xX") (+ hex-digit)))
       (opt (and (any "_" "A-Z" "a-z") (* (any "_" "A-Z" "a-z" "0-9"))))
       symbol-end)))

(defconst jai-font-lock-defaults
  `(;; Keywords
    (,(jai-keywords-rx jai-keywords) 1 font-lock-keyword-face)

    ;; single quote characters
    ("\\('[[:word:]]\\)\\>" 1 font-lock-constant-face)

    ;; Variables
    (,(jai-keywords-rx jai-builtins) 1 font-lock-variable-name-face)

    ;; Constants
    (,(jai-keywords-rx jai-constants) 1 font-lock-constant-face)

    ;; Hash directives
    ("#\\_<.+?\\_>" . font-lock-preprocessor-face)

    ;; At directives
    ("@\\_<.+?\\_>" . font-lock-preprocessor-face)

    ;; Strings
    ("\\\".*\\\"" . font-lock-string-face)

    ;; Numbers
    (,(jai-wrap-symbol-rx jai-number-rx) . font-lock-constant-face)

    ;; Types
    (,(jai-keywords-rx jai-typenames) 1 font-lock-type-face)
    (,jai-hat-type-rx 1 font-lock-type-face)
    (,jai-dollar-type-rx 1 font-lock-type-face)

    ("---" . font-lock-constant-face)))

(defconst jai--defun-rx "\(.*\).*\{")

(defmacro jai-paren-level () `(car (syntax-ppss)))

(defun jai-line-is-defun ()
  "return t if current line begins a procedure"
  (interactive)
  (save-excursion
    (beginning-of-line)
    (let (found)
      (while (and (not (eolp)) (not found))
        (if (looking-at jai--defun-rx)
            (setq found t)
          (forward-char 1)))
      found)))

(defun jai-beginning-of-defun ()
  "Go to line on which current function starts."
  (interactive)
  (let ((orig-level (jai-paren-level)))
    (while (and
            (not (jai-line-is-defun))
            (not (bobp))
            (> orig-level 0))
      (setq orig-level (jai-paren-level))
      (while (>= (jai-paren-level) orig-level)
        (skip-chars-backward "^{")
        (backward-char))))
  (when (jai-line-is-defun)
    (beginning-of-line)))

(defun jai-end-of-defun ()
  "Go to line on which current function ends."
  (interactive)
  (let ((orig-level (jai-paren-level)))
    (when (> orig-level 0)
      (jai-beginning-of-defun)
      (end-of-line)
      (setq orig-level (jai-paren-level))
      (skip-chars-forward "^}")
      (while (>= (jai-paren-level) orig-level)
        (skip-chars-forward "^}")
        (forward-char)))))

;; NOTE: taken from the scala-indent package and modified for Jai.
;;   Still uses the js-indent-line as a base, which will have to be
;;   replaced when the language is more mature.
(defun jai--indent-on-parentheses ()
  (when (and (= (char-syntax (char-before)) ?\))
             (= (save-excursion (back-to-indentation) (point)) (1- (point))))
    (js-indent-line)))

(eval-and-compile (c-add-language 'jai-mode 'java-mode))

;;;###autoload
(define-derived-mode jai-mode prog-mode "Jai"
  :syntax-table jai-mode-syntax-table
  :group 'jai
  (c-initialize-cc-mode t)
  (c-init-language-vars jai-mode)
  (c-common-init 'jai-mode)

  (setq-local font-lock-defaults '(jai-font-lock-defaults))
  (setq-local beginning-of-defun-function 'jai-beginning-of-defun)
  (setq-local end-of-defun-function 'jai-end-of-defun)

  (setq-local indent-line-function #'js-indent-line)

  (setq-local open-paren-in-column-0-is-defun-start nil)
  (setq-local parse-sexp-ignore-comments t)

  (setq-local comment-start "// ")
  (setq-local comment-start-skip "\\(?://+\\|/\\*+\\)\\s *")
  (setq-local comment-end "")
  (setq-local c-block-comment-prefix "* ")
  (setq-local c-comment-prefix-regexp "//+\\|\\**")
  (setq-local fill-paragraph-function #'js-fill-paragraph)
  (setq-local normal-auto-fill-function #'js-do-auto-fill)
  (setq-local comment-line-break-function #'c-indent-new-comment-line)
  (setq-local comment-multi-line t)

  (c-foreign-init-lit-pos-cache)
  (add-hook 'before-change-functions #'c-foreign-truncate-lit-pos-cache nil t)
  (c-setup-paragraph-variables)

  (add-hook 'post-self-insert-hook 'jai--indent-on-parentheses)

  (rx-let ((wsym (or word (syntax symbol))))
    (setq-local
     imenu-generic-expression
     `(("cons or val" ,(rx bol (* space) (group-n 1 (+ wsym)) (* space) ":" (+? nonl) ":" (* space) (not "(")) 1)
       ("function"    ,(rx bol (* space) (group-n 1 (+ wsym)) (* space) "::" (* space) "(") 1))))

  (font-lock-ensure))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.jai\\'" . jai-mode))

(provide 'jai-mode)
;;; jai-mode.el ends here
