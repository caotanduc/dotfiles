;;; visual-indentation-mode.el --- visualize indentation levels with gradient -*- lexical-binding: t; -*-

;; Maintainer: Duc Cao
;; Version: 2.1
;; License: Unlicense / Public Domain
;; Keywords: faces, convenience, indentation
;; Package-Requires: ((emacs "29.1"))

;;; Commentary:
;; Highlights indentation depth with gradient background colors.
;; Works well for YAML, Python, JSON, etc.

;;; Code:

(defgroup visual-indentation-mode nil
  "Visualize indentation depth with gradient colors."
  :group 'faces
  :prefix "visual-indentation-")

(defcustom visual-indentation-width 2
  "Width of indentation (in spaces)."
  :type 'integer
  :group 'visual-indentation-mode)

;; ─────────────────────────────────────────────────────────────
;; Faces (10-level gradient)
;; ─────────────────────────────────────────────────────────────

;; darker, tty-friendly blue gradient
(defface visual-indentation-face-1  '((t (:background "#010314"))) "Indent level 1."  :group 'visual-indentation-mode)
(defface visual-indentation-face-2  '((t (:background "#021a3b"))) "Indent level 2."  :group 'visual-indentation-mode)
(defface visual-indentation-face-3  '((t (:background "#033a56"))) "Indent level 3."  :group 'visual-indentation-mode)
(defface visual-indentation-face-4  '((t (:background "#04536d"))) "Indent level 4."  :group 'visual-indentation-mode)
(defface visual-indentation-face-5  '((t (:background "#056d84"))) "Indent level 5."  :group 'visual-indentation-mode)
(defface visual-indentation-face-6  '((t (:background "#06799a"))) "Indent level 6."  :group 'visual-indentation-mode)
(defface visual-indentation-face-7  '((t (:background "#0887a8"))) "Indent level 7."  :group 'visual-indentation-mode)
(defface visual-indentation-face-8  '((t (:background "#0997b5"))) "Indent level 8."  :group 'visual-indentation-mode)
(defface visual-indentation-face-9  '((t (:background "#0aa7c3"))) "Indent level 9."  :group 'visual-indentation-mode)
(defface visual-indentation-face-10 '((t (:background "#0cb7cf"))) "Indent level 10." :group 'visual-indentation-mode)

(defconst visual-indentation-max-level 10)

;; ─────────────────────────────────────────────────────────────
;; Helpers
;; ─────────────────────────────────────────────────────────────

(defun visual-indentation--detect-width ()
  "Try to detect indentation width for current mode."
  (or (and (boundp 'python-indent-offset) python-indent-offset)
      (and (boundp 'js-indent-level) js-indent-level)
      (and (boundp 'typescript-indent-level) typescript-indent-level)
      (and (boundp 'c-basic-offset) c-basic-offset)
      (and (boundp 'tab-width) tab-width)
      visual-indentation-width))

(defun visual-indentation--choose-face (col)
  "Return face for given indentation column COL."
  (let* ((level (/ col visual-indentation-width))
         (idx (1+ (mod level visual-indentation-max-level))))
    (intern (format "visual-indentation-face-%d" idx))))

(defun visual-indentation--apply-line ()
  "Color indentation on the current line."
  (save-excursion
    (beginning-of-line)
    (let ((indent (current-indentation)))
      (dotimes (i indent)
        (when (looking-at " ")
          (let ((pos (+ (point) i)))
            (add-text-properties
             pos (1+ pos)
             `(font-lock-face ,(visual-indentation--choose-face i)
                              rear-nonsticky t))))))))

(defun visual-indentation--refresh-buffer ()
  "Repaint indentation for the whole buffer."
  (with-silent-modifications
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
        (visual-indentation--apply-line)
        (forward-line 1)))))

(defun visual-indentation--clear ()
  "Remove all indentation color properties."
  (with-silent-modifications
    (remove-text-properties (point-min) (point-max)
                            '(font-lock-face nil rear-nonsticky nil))))

;; ─────────────────────────────────────────────────────────────
;; Mode definition
;; ─────────────────────────────────────────────────────────────

;;;###autoload
(define-minor-mode visual-indentation-mode
  "Highlight indentation with gradient background colors."
  :lighter " ⸳"
  (if visual-indentation-mode
      (progn
        (setq-local visual-indentation-width (visual-indentation--detect-width))
        (visual-indentation--refresh-buffer)
        (add-hook 'after-change-functions #'visual-indentation--after-change nil t))
    (remove-hook 'after-change-functions #'visual-indentation--after-change t)
    (visual-indentation--clear)))

(defun visual-indentation--after-change (&rest _)
  "Repaint indentation after buffer changes."
  (when visual-indentation-mode
    (visual-indentation--refresh-buffer)))

(provide 'visual-indentation-mode)

;;; visual-indentation-mode.el ends here
