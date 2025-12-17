;;; simp-python-mode.el --- Minimal Python mode without heavy syntax -*- lexical-binding: t; -*-

;; Author: Cao Tan Duc
;; Version: 0.1
;; Package-Requires: ((emacs "27.1"))
;; Keywords: languages, python

;;; Commentary:
;;
;; simp-python-mode is a deliberately minimal Python major mode.
;;
;; Goals:
;; - NO heavy font-lock / syntax highlighting
;; - Keep indentation, comments, def/class navigation
;; - Avoid background parsing, LSP, tree-sitter, semantic features
;; - Fast and predictable for calculation-heavy or large files
;;
;; This mode intentionally trades features for speed.

;;; Code:

(require 'python)

(defgroup simp-python nil
  "Minimal Python editing mode."
  :group 'languages)

;;;###autoload
(define-derived-mode simp-python-mode prog-mode "SimpPython"
  "A minimal, fast Python mode without heavy syntax processing."

  ;; -----------------
  ;; Basic text rules
  ;; -----------------
  (setq-local comment-start "# ")
  (setq-local comment-end "")
  (setq-local indent-tabs-mode nil)
  (setq-local tab-width 4)

  ;; -----------------
  ;; Indentation
  ;; -----------------
  ;; Reuse Python's indentation logic without enabling full python-mode
  (setq-local indent-line-function #'python-indent-line)
  (setq-local python-indent-guess-indent-offset nil)

  ;; -----------------
  ;; Disable heavy features
  ;; -----------------
  ;; No syntax highlighting
  (setq-local font-lock-defaults nil)
  (font-lock-mode -1)

  ;; No syntax propertization
  (setq-local syntax-propertize-function nil)

  ;; No imenu
  (setq-local imenu-create-index-function nil)

  ;; No native shell completion
  (setq-local python-shell-completion-native-enable nil)

  ;; Ensure LSP / eglot does not auto-start
  ;; (setq-local eglot-managed-mode nil)

  ;; -----------------
  ;; Navigation
  ;; -----------------
  ;; Cheap, indentation-based def/class navigation
  (setq-local beginning-of-defun-function
              #'python-nav-beginning-of-defun)
  (setq-local end-of-defun-function
              #'python-nav-end-of-defun)

  ;; -----------------
  ;; Electric behavior
  ;; -----------------
  (electric-indent-local-mode 1)

  ;; -----------------
  ;; Visual helpers (cheap)
  ;; -----------------
  (show-paren-local-mode 1)

  ;; Final message (optional)
  (setq-local mode-line-format
              (append mode-line-format '((:eval " SimpPy")))))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.py\\'" . simp-python-mode))

(provide 'simp-python-mode)

;;; simp-python-mode.el ends here
