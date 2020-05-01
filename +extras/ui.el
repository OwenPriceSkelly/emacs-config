;;; ~/.doom.d/+extras/ui.el -*- lexical-binding: t; -*-

;; TODO doom-modeline config
(setq doom-theme                      'doom-gruvbox

      doom-font                       (font-spec :family "Iosevka Extended" :size 16)
      doom-variable-pitch-font        (font-spec :family "Iosevka Etoile" :size 16)

      display-line-numbers-type       'relative
      which-key-side-window-location  'bottom
      which-key-sort-order            'which-key-key-order-alpha
      which-key-max-description-length nil

      solaire-mode-auto-swap-bg       t
      solaire-mode-remap-line-numbers t
      evil-split-window-below         t
      evil-vsplit-window-right        t
      +latex-viewers                  (if IS-MAC '(pdf-tools))
      +pretty-code-enabled-modes      '(org-mode))

(use-package! rainbow-mode
  :commands (rainbow-mode))
;; persist frame size/fullscreen across sessions
(when-let (dims (doom-cache-get 'last-frame-size))
  (cl-destructuring-bind ((left . top) width height fullscreen) dims
    (setq initial-frame-alist
          (append initial-frame-alist
                  `((left . ,left)
                    (top . ,top)
                    (width . ,width)
                    (height . ,height)
                    (fullscreen . ,fullscreen))))))

(defun save-frame-dimensions ()
  (doom-cache-set 'last-frame-size
                  (list (frame-position)
                        (frame-width)
                        (frame-height)
                        (frame-parameter nil 'fullscreen))))

(add-hook 'kill-emacs-hook  #'save-frame-dimensions)

(defun +my/doom-dashboard-widget-banner ()
  (let ((point (point)))
    (mapc (lambda (line)
            (insert (propertize (+doom-dashboard--center +doom-dashboard--width line)
                                'face 'doom-dashboard-banner) " ")
            (insert "\n"))
          '(""
            ""
            ""
            ""
            ""
            ""
            ""
            ""
            ""
            ""
            ""
            ""
            ""
            ""
            "                                   (E M A C S)                                   "
            ""
            ""
            ""
            ""))
    (when (and (display-graphic-p)
               (stringp fancy-splash-image)
               (file-readable-p fancy-splash-image))
      (let ((image (create-image (fancy-splash-image-file))))
        (add-text-properties
         point (point) `(display ,image rear-nonsticky (display)))
        (save-excursion
          (goto-char point)
          (insert (make-string
                   (truncate
                    (max 0 (+ 1 (/ (- +doom-dashboard--width (car (image-size image nil)))
                                   2)))) 32))))
      (insert (make-string (or (cdr +doom-dashboard-banner-padding) 0) 10)))))
(setq +doom-dashboard-functions
      '(+my/doom-dashboard-widget-banner
        doom-dashboard-widget-shortmenu
        doom-dashboard-widget-loaded
        doom-dashboard-widget-footer))
