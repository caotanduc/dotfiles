;;; glow-preview.el --- Live markdown preview with glow -*- lexical-binding: t; -*-

;; Author: Cao Tan Duc
;; Keywords: markdown, preview
;; Package-Requires: ((emacs "27.1"))

;;; Commentary:

;; Preview the current markdown file with `glow -p' in a side window.
;; The preview restarts on every save, so it always reflects the file
;; on disk.
;;
;; Usage:
;;   M-x glow-preview-mode  in a .md buffer to toggle the preview.
;;   M-x glow-preview       to open/refresh it once without the mode.

;;; Code:

(require 'term)

(defgroup glow-preview nil
  "Preview markdown files with glow."
  :group 'tools)

(defcustom glow-preview-command "glow"
  "Path to the glow executable."
  :type 'string)

(defcustom glow-preview-args '("-p")
  "Arguments passed to glow before the file name."
  :type '(repeat string))

(defcustom glow-preview-window-width 0.5
  "Fraction of the frame width used by the preview side window."
  :type 'number)

(defcustom glow-preview-popup-width "80%"
  "Width of the tmux popup used by `glow-preview-popup'."
  :type 'string)

(defcustom glow-preview-popup-height "90%"
  "Height of the tmux popup used by `glow-preview-popup'."
  :type 'string)

(defvar-local glow-preview--buffer nil
  "Preview buffer associated with this markdown buffer.")

(defun glow-preview--buffer-name (file)
  (format "*glow: %s*" (file-name-nondirectory file)))

(defun glow-preview--exec (buffer file)
  "(Re)start glow on FILE inside BUFFER."
  (with-current-buffer buffer
    (let ((proc (get-buffer-process buffer)))
      (when proc
        (set-process-query-on-exit-flag proc nil)
        (delete-process proc)))
    (let ((inhibit-read-only t))
      (erase-buffer))
    (unless (derived-mode-p 'term-mode)
      (term-mode))
    (term-exec buffer "glow-preview" glow-preview-command nil
               (append glow-preview-args (list file)))
    (term-char-mode)
    (when-let ((proc (get-buffer-process buffer)))
      (set-process-query-on-exit-flag proc nil))))

(defun glow-preview--display (buffer)
  (display-buffer
   buffer
   `(display-buffer-in-side-window
     (side . right)
     (window-width . ,glow-preview-window-width))))

;;;###autoload
(defun glow-preview ()
  "Preview the current file with glow in a side window.
Refreshes the preview if it is already open."
  (interactive)
  (let ((file (buffer-file-name)))
    (unless file
      (user-error "Buffer is not visiting a file"))
    (unless (executable-find glow-preview-command)
      (user-error "Cannot find `%s' executable" glow-preview-command))
    (when (buffer-modified-p)
      (save-buffer))
    (let ((buffer (get-buffer-create (glow-preview--buffer-name file))))
      (setq glow-preview--buffer buffer)
      (glow-preview--exec buffer file)
      (glow-preview--display buffer))))

;;;###autoload
(defun glow-preview-popup ()
  "Preview the current file with glow in a tmux popup.
The popup closes when glow exits (press `q')."
  (interactive)
  (let ((file (buffer-file-name)))
    (unless file
      (user-error "Buffer is not visiting a file"))
    (unless (getenv "TMUX")
      (user-error "Not running inside tmux"))
    (unless (executable-find glow-preview-command)
      (user-error "Cannot find `%s' executable" glow-preview-command))
    (when (buffer-modified-p)
      (save-buffer))
    (call-process "tmux" nil 0 nil
                  "display-popup" "-E"
                  "-w" glow-preview-popup-width
                  "-h" glow-preview-popup-height
                  "-T" (format " %s " (file-name-nondirectory file))
                  (format "%s %s %s"
                          glow-preview-command
                          (mapconcat #'shell-quote-argument glow-preview-args " ")
                          (shell-quote-argument file)))))

(defun glow-preview--refresh ()
  "Restart glow after save so the preview matches the file."
  (when (buffer-live-p glow-preview--buffer)
    (glow-preview--exec glow-preview--buffer (buffer-file-name))
    (glow-preview--display glow-preview--buffer)))

(defun glow-preview-stop ()
  "Close the preview for the current buffer."
  (interactive)
  (when (buffer-live-p glow-preview--buffer)
    (when-let ((win (get-buffer-window glow-preview--buffer)))
      (delete-window win))
    (kill-buffer glow-preview--buffer))
  (setq glow-preview--buffer nil))

;;;###autoload
(define-minor-mode glow-preview-mode
  "Live glow preview for the current markdown file.
Opens `glow -p FILE' in a side window and restarts it on every save."
  :lighter " Glow"
  (if glow-preview-mode
      (progn
        (glow-preview)
        (add-hook 'after-save-hook #'glow-preview--refresh nil t)
        (add-hook 'kill-buffer-hook #'glow-preview-stop nil t))
    (remove-hook 'after-save-hook #'glow-preview--refresh t)
    (remove-hook 'kill-buffer-hook #'glow-preview-stop t)
    (glow-preview-stop)))

(provide 'glow-preview)
;;; glow-preview.el ends here
