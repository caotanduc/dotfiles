;;; jumpy.el --- Lightweight cursor jump history -*- lexical-binding: t; -*-
;; Author: Duc Cao
;; Version: 0.2
;; Keywords: convenience, navigation
;; Package-Requires: ((emacs "28.1"))
;; URL: https://github.com/yourname/jumpy
;;; Commentary:
;; Jumpy tracks your cursor positions as you navigate files and buffers,
;; letting you move back and forward through your recent editing locations.
;;
;; Inspired by VS Code’s “Go Back / Go Forward” (Ctrl+- / Ctrl+Shift+-).
;;
;; Usage:
;;   (require 'jumpy)
;;   (jumpy-mode 1)
;;   (global-set-key (kbd "C--")   #'jumpy-back)
;;   (global-set-key (kbd "C-S--") #'jumpy-forward)
;;
;; Configure ignored commands with `jumpy-ignored-commands`.

;;; Code:

(require 'cl-lib)
(require 'pulse)

(defgroup jumpy nil
  "VSCode-like cursor jump navigation."
  :group 'convenience)

(defcustom jumpy-threshold-lines 1
  "Minimum number of lines moved before recording a new jump."
  :type 'integer)

(defcustom jumpy-idle-seconds 0.4
  "Minimum idle time (seconds) between two recorded positions."
  :type 'number)

(defcustom jumpy-max 200
  "Maximum number of cursor positions to keep in history."
  :type 'integer)

(defcustom jumpy-ignored-commands
  '(scroll-up-command scroll-down-command
    pixel-scroll-up pixel-scroll-down
    mwheel-scroll mouse-set-point
    next-line previous-line
    recenter-top-bottom
    move-end-of-line move-beginning-of-line
    windmove-up windmove-down windmove-left windmove-right
    scroll-other-window scroll-other-window-down
    consult-line consult-line-multi
    backward-char forward-char)
  "Commands that Jumpy should ignore when recording cursor positions."
  :type '(repeat symbol)
  :group 'jumpy)

(cl-defstruct (jumpy--pos (:constructor jumpy--pos))
  buffer marker time)

(defvar jumpy--list nil
  "List of cursor positions (oldest ... newest).")

(defvar jumpy--index -1
  "Current index in `jumpy--list'. -1 means newest.")

(defvar-local jumpy--last-line nil)
(defvar jumpy--last-time 0)

(defun jumpy--now () (float-time (current-time)))

(defun jumpy--current-pos ()
  (jumpy--pos :buffer (current-buffer)
              :marker (copy-marker (point-marker) t)
              :time (jumpy--now)))

(defun jumpy--same-pos-p (a b)
  (and a b
       (eq (jumpy--pos-buffer a) (jumpy--pos-buffer b))
       (equal (marker-position (jumpy--pos-marker a))
              (marker-position (jumpy--pos-marker b)))))

(defun jumpy--truncate (n lst)
  (let ((len (length lst)))
    (if (<= len n) lst (cl-subseq lst (- len n)))))

(defun jumpy--push ()
  "Push the current position to the history, truncating forward tail if needed."
  (let* ((cur (jumpy--current-pos))
         (same (and jumpy--list
                    (jumpy--same-pos-p cur (car (last jumpy--list))))))
    (unless same
      (when (>= jumpy--index 0)
        (setf jumpy--list (cl-subseq jumpy--list
                                     0 (- (length jumpy--list)
                                          jumpy--index))
              jumpy--index -1))
      (setq jumpy--list
            (jumpy--truncate jumpy-max
                             (append jumpy--list (list cur)))))))

(defun jumpy--should-record ()
  "Return non-nil if a new position should be recorded."
  (let ((idle (<= jumpy-idle-seconds (- (jumpy--now) jumpy--last-time)))
        (line (line-number-at-pos)))
    (or (not (eq (current-buffer)
                 (and jumpy--list (jumpy--pos-buffer (car (last jumpy--list))))))
        (and jumpy--last-line
             (>= (abs (- line jumpy--last-line)) jumpy-threshold-lines))
        idle)))

(defun jumpy--recorder ()
  "Record meaningful cursor moves after each command, ignoring scroll-only actions."
  (when (and (not (minibufferp))
             (not (memq this-command '(jumpy-back jumpy-forward))))
    (unless (memq this-command jumpy-ignored-commands)
      (when (jumpy--should-record)
        (jumpy--push)
        (setq jumpy--last-line (line-number-at-pos)
              jumpy--last-time (jumpy--now))))))

(defun jumpy--goto (pos)
  "Jump to the given POS."
  (when (and pos (buffer-live-p (jumpy--pos-buffer pos)))
    (let ((buf (jumpy--pos-buffer pos))
          (mk  (jumpy--pos-marker pos)))
      (switch-to-buffer buf)
      (goto-char (marker-position mk))
      (pulse-momentary-highlight-one-line (point)))))

;;;###autoload
(defun jumpy-back ()
  "Go back to previous cursor position (Ctrl+-)."
  (interactive)
  (let ((n (length jumpy--list)))
    (cond
     ((<= n 1) (message "No previous location."))
     (t
      (when (= jumpy--index -1)
        (setq jumpy--index 1))
      (if (>= jumpy--index (1- n))
          (message "At oldest location.")
        (cl-incf jumpy--index)
        (jumpy--goto (nth (- n 1 jumpy--index) jumpy--list)))))))

;;;###autoload
(defun jumpy-forward ()
  "Go forward to next cursor position (Ctrl+Shift+-)."
  (interactive)
  (let ((n (length jumpy--list)))
    (cond
     ((or (<= n 1) (<= jumpy--index 0))
      (setq jumpy--index -1)
      (message "At newest location."))
     (t
      (cl-decf jumpy--index)
      (jumpy--goto (nth (- n 1 jumpy--index) jumpy--list))))))

;;;###autoload
(define-minor-mode jumpy-mode
  "Toggle Jumpy mode.
When enabled, records cursor locations and enables back/forward navigation."
  :init-value t
  :lighter " 🪶"
  (if jumpy-mode
      (add-hook 'post-command-hook #'jumpy--recorder 90)
    (remove-hook 'post-command-hook #'jumpy--recorder)))

(provide 'jumpy)
;;; jumpy.el ends here
