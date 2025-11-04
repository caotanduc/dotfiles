(defun my/welcome-buffer ()
  "Display a centered 'fakeide' logo: 'fake' (yellow on black), 'ide' (black on yellow).
Close the buffer when any key is pressed."
  (interactive)
  (let ((buf (get-buffer-create "*Welcome*"))
        (inhibit-read-only t))
    (with-current-buffer buf
      (erase-buffer)
      ;; No mode line or cursor
      (setq mode-line-format nil
            cursor-type nil)
      ;; Build the two parts
      (let* ((fake (propertize "fake" 'face '(:foreground "yellow" :background "black" :weight bold :height 5.0)))
             (ide  (propertize "ide"  'face '(:foreground "black"  :background "yellow" :weight bold :height 5.0)))
             (text (concat fake ide))
             (win-width (window-body-width))
             (win-height (window-body-height))
             (text-width (string-width "fakeide"))
             (h-padding (/ (- win-width text-width) 2))
             (v-padding (/ win-height 2)))
        ;; Insert padding and text
        (insert (make-string (max 0 v-padding) ?\n))
        (insert (make-string (max 0 h-padding) ?\s))
        (insert text))
      ;; Make buffer unscrollable and read-only
      (setq buffer-read-only t)
      (setq-local window-size-fixed 'both)
      (local-set-key [wheel-up] #'ignore)
      (local-set-key [wheel-down] #'ignore)
      (local-set-key (kbd "<up>") #'ignore)
      (local-set-key (kbd "<down>") #'ignore)

      ;; Define keymap that closes the buffer on any key
      (use-local-map (let ((map (make-sparse-keymap)))
                       (define-key map [t]
                         (lambda ()
                           (interactive)
                           (kill-buffer "*Welcome*")))
                       map)))
    (switch-to-buffer buf)))

(provide 'dashboard)
