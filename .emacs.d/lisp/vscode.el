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

;; https://www.emacswiki.org/emacs/MoveText
(defun move-text-internal (arg)
  (cond
   ((and mark-active transient-mark-mode)
    (if (> (point) (mark))
        (exchange-point-and-mark))
    (let ((column (current-column))
          (text (delete-and-extract-region (point) (mark))))
      (forward-line arg)
      (move-to-column column t)
      (set-mark (point))
      (insert text)
      (exchange-point-and-mark)
      (setq deactivate-mark nil)))
   (t
    (let ((column (current-column)))
      (beginning-of-line)
      (when (or (> arg 0) (not (bobp)))
        (forward-line)
        (when (or (< arg 0) (not (eobp)))
          (transpose-lines arg))
        (forward-line -1))
      (move-to-column column t)))))

(defun move-text-down (arg)
  "Move region (transient-mark-mode active) or current line
  arg lines down."
  (interactive "*p")
  (move-text-internal arg))

(defun move-text-up (arg)
  "Move region (transient-mark-mode active) or current line
  arg lines up."
  (interactive "*p")
  (move-text-internal (- arg)))

(defun save-buffer-no-hooks ()
  "Save without running hooks (e.g., formatter)."
  (interactive)
  (let ((inhibit-message t)
        (save-silently t)
        (before-save-hook nil)
        (after-save-hook nil))
    (write-file (buffer-file-name))))

;; ─────────────────────────────────────────────────────────────
;; Minor Mode Definition
;; ─────────────────────────────────────────────────────────────

(defvar vscode-mode-map
  (let ((map (make-sparse-keymap)))
    ;; Main bindings
    (define-key map (kbd "M-RET")           #'vscode-toggle-command)
    (define-key map (kbd "C-c f")           #'consult-ripgrep)
    (define-key map (kbd "C-c TAB")     #'consult-project-buffer)
    (define-key map (kbd "C-c w")         #'kill-this-buffer)
    (define-key map (kbd "s-s")           #'save-buffer)
    (define-key map (kbd "s-E")           #'vscode-open-explorer)
    (define-key map (kbd "C-o")           #'vscode-insert-line-below)
    (define-key map (kbd "C-c o")  #'vscode-insert-line-above)
    (define-key map (kbd "C-c d")         #'vscode-mark-whole-word)
    (define-key map (kbd "C-c l")         #'vscode-mark-whole-line)
    (define-key map (kbd "C-c \\")        #'vscode-find-cursor)
    (define-key map (kbd "C-c /")         #'comment-line)
    (define-key map (kbd "C-c k w")       #'vscode-kill-other-buffers)
    (define-key map (kbd "M-<up>")          #'move-text-up)
    (define-key map (kbd "M-<down>")        #'move-text-down)
    (define-key map (kbd "C-c s")           #'save-buffer-no-hooks)
    (define-key map (kbd "C-c #")         #'consult-imenu)
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
