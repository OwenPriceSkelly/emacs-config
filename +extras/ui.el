;;; ~/.doom.d/+extras/ui.el -*- lexical-binding: t; -*-

;; TODO doom-modeline config
(setq doom-theme                      'doom-gruvbox-light
      doom-gruvbox-light-variant      'hard
      doom-font                       (font-spec :family "Iosevka Extended" :size 16)
      doom-variable-pitch-font        (font-spec :family "Iosevka Etoile" :size 16)
      ;; doom-unicode-font               (font-spec :family)
      doom-modeline-persp-name t
      doom-modeline-major-mode-icon t

      display-line-numbers-type       'nil
      which-key-side-window-location  'bottom
      which-key-sort-order            'which-key-key-order-alpha
      which-key-max-description-length nil

      treemacs-width 30

      solaire-mode-auto-swap-bg       t
      solaire-mode-remap-line-numbers t
      evil-split-window-below         t
      evil-vsplit-window-right        t
      +latex-viewers                  (if IS-MAC '(pdf-tools))
      +pretty-code-enabled-modes      '(org-mode))

 ;; "       ___           ___           ___           ___           ___      "
 ;; "      /  /\         /__/\         /  /\         /  /\         /  /\     "
 ;; "     /  /:/_       |  |::\       /  /::\       /  /:/        /  /:/_    "
 ;; "    /  /:/ /\      |  |:|:\     /  /:/\:\     /  /:/        /  /:/ /\   "
 ;; "   /  /:/ /:/_   __|__|:|\:\   /  /:/ /::\   /  /:/  ___   /  /:/ /::\  "
 ;; "  /__/:/ /:/ /\ /__/::::| \:\ /__/:/ /:/\:\ /__/:/  /  /\ /__/:/ /:/\:\ "
 ;; "  \  \:\/:/ /:/ \  \:\     \/ \  \:\/:/__\/ \  \:\ /  /:/ \  \:\/:/ /:/ "
 ;; "   \  \::/ /:/   \  \:\        \  \::/       \  \:\  /:/   \  \::/ /:/  "
 ;; "    \  \:\/:/     \  \:\        \  \:\        \  \:\/:/     \__\/ /:/   "
 ;; "     \  \::/       \  \:\        \  \:\        \  \::/        /__/:/    "
 ;; "      \__\/         \__\/         \__\/         \__\/         \__\/     "
 ;;
(defun +my/doom-dashboard-widget-banner ()
  (let ((point (point)))
    (mapc (lambda (line)
            (insert (propertize (+doom-dashboard--center +doom-dashboard--width line)
                                'face 'doom-dashboard-banner) " ")
            (insert "\n"))
          '(""
            "       ___           ___           ___           ___           ___      "
            "      /  /\\         /__/\\         /  /\\         /  /\\         /  /\\     "
            "     /  /:/_       |  |::\\       /  /::\\       /  /:/        /  /:/_    "
            "    /  /:/ /\\      |  |:|:\\     /  /:/\\:\\     /  /:/        /  /:/ /\\   "
            "   /  /:/ /:/_   __|__|:|\\:\\   /  /:/ /::\\   /  /:/  ___   /  /:/ /::\\  "
            "  /__/:/ /:/ /\\ /__/::::| \\:\\ /__/:/ /:/\\:\\ /__/:/  /  /\\ /__/:/ /:/\\:\\ "
            "  \\  \\:\\/:/ /:/ \\  \\:\\     \\/ \\  \\:\\/:/__\\/ \\  \\:\\ /  /:/ \\  \\:\\/:/ /:/ "
            "   \\  \\::/ /:/   \\  \\:\\        \\  \\::/       \\  \\:\\  /:/   \\  \\::/ /:/  "
            "    \\  \\:\\/:/     \\  \\:\\        \\  \\:\\        \\  \\:\\/:/     \\__\\/ /:/   "
            "     \\  \\::/       \\  \\:\\        \\  \\:\\        \\  \\::/        /__/:/    "
            "      \\__\\/         \\__\\/         \\__\\/         \\__\\/         \\__\\/     "
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
