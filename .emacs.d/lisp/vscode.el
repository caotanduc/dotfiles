;;; vscode.el --- VSCode-like UX and keybindings -*- lexical-binding: t; -*-
;; Author: Duc Cao
;; Version: 0.7
;; Keywords: convenience, vscode
;; Package-Requires: ((emacs "29.1") (consult "1.0"))
;;; Commentary:
;; Provides a VSCode-like experience for Emacs:
;;  - Command palette, buffer switching, explorer
;;  - Insert line above/below
;;  - Select word (⌘d) and line (⌘l)
;;  - Optional global minor mode

;;; Code:

(require 'consult)
(require 'project)
(require 'dired)
(require 'dired-x)       ;; for dired-jump
(require 'xref)
(require 'pulse)
(require 'cl-lib)

;; ─────────────────────────────────────────────────────────────
;; Core Helpers
;; ─────────────────────────────────────────────────────────────

(defun vscode-toggle-command ()
  "Toggle the command palette: open M-x if inactive, cancel if active."
  (interactive)
  (if (minibufferp)
      (keyboard-escape-quit)
    (call-interactively #'execute-extended-command)))

(defun vscode-open-explorer ()
  "Open project root in Dired if inside a project, else use `dired-jump'."
  (interactive)
  (if (project-current)
      (project-dired)
    (dired-jump)))

(defun vscode-kill-other-buffers ()
  "Kill all other buffers, keeping only the current one."
  (interactive)
  (mapc #'kill-buffer (delq (current-buffer) (buffer-list)))
  (message "Other buffers killed."))

(defun vscode-switch-buffer ()
  "Switch buffer smartly: use project buffer list if available."
  (interactive)
  (if (project-current)
      (call-interactively #'project-switch-to-buffer)
    (consult-buffer)))

(defun vscode-insert-line-below ()
  "Insert a new line below the current one and keep cursor position."
  (interactive)
  (end-of-line)
  (newline-and-indent))

(defun vscode-insert-line-above ()
  "Insert a new line above the current one and keep cursor position."
  (interactive)
  (beginning-of-line)
  (open-line 1)
  (indent-according-to-mode))

(defun vscode-mark-whole-word ()
  "Select the entire word at point, regardless of cursor position."
  (interactive)
  (let ((bounds (bounds-of-thing-at-point 'word)))
    (if bounds
        (progn
          (set-mark (cdr bounds))
          (goto-char (car bounds))
          (activate-mark))
      (message "No word at point."))))

(defun vscode-mark-whole-line ()
  "Select the entire current line (like VS Code’s ⌘L)."
  (interactive)
  (set-mark (line-end-position))
  (beginning-of-line)
  (activate-mark))

(defun vscode-find-cursor ()
  "Momentarily highlight the current line to find the cursor."
  (interactive)
  (pulse-momentary-highlight-one-line (point)))

;; ─────────────────────────────────────────────────────────────
;; Minor Mode Definition
;; ─────────────────────────────────────────────────────────────

(defvar vscode-mode-map
  (let ((map (make-sparse-keymap)))
    ;; Main bindings
    (define-key map (kbd "M-RET")           #'vscode-toggle-command)
    (define-key map (kbd "s-F")           #'consult-ripgrep)
    (define-key map (kbd "C-c TAB")     #'consult-project-buffer)
    (define-key map (kbd "C-c w")         #'kill-this-buffer)
    (define-key map (kbd "s-s")           #'save-buffer)
    (define-key map (kbd "s-E")           #'vscode-open-explorer)
    (define-key map (kbd "C-o")           #'vscode-insert-line-below)
    (define-key map (kbd "C-c RET")  #'vscode-insert-line-below)
    (define-key map (kbd "C-c d")         #'vscode-mark-whole-word)
    (define-key map (kbd "C-c l")         #'vscode-mark-whole-line)
    (define-key map (kbd "C-c \\")        #'vscode-find-cursor)
    (define-key map (kbd "C-c k w")       #'vscode-kill-other-buffers)
    map))

;;;###autoload
(define-minor-mode vscode-mode
  "VSCode-like UX mode for Emacs."
  :init-value nil
  :lighter " VSCode"
  :keymap vscode-mode-map)

;;;###autoload
(define-globalized-minor-mode global-vscode-mode
  vscode-mode
  (lambda () (vscode-mode 1)))

;; ─────────────────────────────────────────────────────────────
;; Provide
;; ─────────────────────────────────────────────────────────────

(provide 'vscode)
;;; vscode.el ends here
