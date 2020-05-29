;;; ~/.doom.d/+extras/ui.el -*- lexical-binding: t; -*-

;; TODO doom-modeline config
(setq +my/themes-list-dark     '(doom-oceanic-next
                                 doom-gruvbox
                                 doom-nord
                                 doom-wilmersdorf
                                 doom-city-lights
                                 doom-moonlight)
      +my/themes-list-light     '(doom-gruvbox-light
                                  doom-nord-light
                                  doom-acario-light
                                  doom-solarized-light)
      doom-theme                (let ((hour (caddr (decode-time (current-time)))))
                                  (if (< 9 hour 15)
                                      (nth (mod hour (length +my/themes-list-light)) +my/themes-list-light)
                                    (nth (mod hour (length +my/themes-list-dark)) +my/themes-list-dark)))
      ;; 'doom-gruvbox-light ;; light theme from 9-5
      ;; 'doom-gruvbox
      solaire-mode-auto-swap-bg       t
      solaire-mode-remap-line-numbers t

      doom-font                       (font-spec :family "Iosevka Extended" :size 16)
      doom-variable-pitch-font        (font-spec :family "Iosevka Etoile" :size 16)
      ;; doom-unicode-font               (font-spec :family)
      doom-modeline-persp-name t
      doom-modeline-major-mode-icon t

 doom-font                       (font-spec :family "Iosevka Extended" :size 16)
 doom-variable-pitch-font        (font-spec :family "Iosevka Etoile" :size 16)
 ;; doom-unicode-font               (font-spec :family)
 doom-modeline-persp-name t
 doom-modeline-major-mode-icon t

      treemacs-width 30

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
  "Modified `doom-dashboard-widget-banner' with ascii art lifted from https://github.com/plexus/chemacs"
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

(setq! +doom-dashboard-menu-sections '(("Reload last session"
                                        :icon (all-the-icons-octicon "history" :face 'doom-dashboard-menu-title)
                                        :when (cond ((require 'persp-mode nil t)
                                                     (file-exists-p (expand-file-name persp-auto-save-fname persp-save-dir)))
                                                    ((require 'desktop nil t)
                                                     (file-exists-p (desktop-full-file-name))))
                                        :face (:inherit (doom-dashboard-menu-title bold))
                                        :action doom/quickload-session)
                                       ("Open today's note"
                                        :icon (all-the-icons-octicon "book" :face 'doom-dashboard-menu-title)
                                        :action org-roam-dailies-today)
                                       ("Recently opened files"
                                        :icon (all-the-icons-octicon "file-text" :face 'doom-dashboard-menu-title)
                                        :action recentf-open-files)
                                       ("Open project"
                                        :icon (all-the-icons-octicon "repo" :face 'doom-dashboard-menu-title)
                                        :action projectile-switch-project)
                                       ;; ("Open org-agenda"
                                       ;;  :icon (all-the-icons-octicon "calendar" :face 'doom-dashboard-menu-title)
                                       ;;  :when (fboundp 'org-agenda)
                                       ;;  :action org-agenda)
                                       ("Jump to bookmark"
                                        :icon (all-the-icons-octicon "bookmark" :face 'doom-dashboard-menu-title)
                                        :action bookmark-jump)
                                       ("Open private configuration"
                                        :icon (all-the-icons-octicon "tools" :face 'doom-dashboard-menu-title)
                                        :when (file-directory-p doom-private-dir)
                                        :action doom/open-private-config))

       +doom-dashboard-functions '(+my/doom-dashboard-widget-banner
                                   doom-dashboard-widget-shortmenu
                                   doom-dashboard-widget-loaded))
