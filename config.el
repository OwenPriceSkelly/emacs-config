;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; refresh' after modifying this file!


;; ----------------------------------------------------------------

;;
;; ----------------------------------------------------------------
;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Owen Price-Skelly"
      user-mail-address "Owen.Price.Skelly@gmail.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
(setq doom-font (font-spec :family "monospace" :size 14))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. These are the defaults.
(setq doom-theme 'doom-horizon)

;; If you intend to use org, it is recommended you change this!
(setq org-directory "~/org/"
      deft-directory org-directory)
;; If you want to change the style of line numbers, change this to `relative' or
;; `nil' to disable it:
(setq display-line-numbers-type 'relative)

;; -----------------------------------------------------------------------------
(map!
 :prefix "gr"
 :nv "d" #'evil-mc-make-and-goto-next-match
 :nv "D" #'evil-mc-make-and-goto-prev-match
 :nv "j" #'evil-mc-make-cursor-move-next-line
 :nv "k" #'evil-mc-make-cursor-move-prev-line
 :nv "m" #'evil-mc-make-all-cursors
 :nv "n" #'evil-mc-make-and-goto-next-cursor
 :nv "N" #'evil-mc-make-and-goto-last-cursor
 :nv "p" #'evil-mc-make-and-goto-prev-cursor
 :nv "P" #'evil-mc-make-and-goto-first-cursor
 :nv "q" #'evil-mc-undo-all-cursors
 :nv "t" #'+multiple-cursors/evil-mc-toggle-cursors
 :nv "u" #'evil-mc-undo-last-added-cursor
 :nv "z" #'+multiple-cursors/evil-mc-make-cursor-here
 :v  "I" #'evil-mc-make-cursor-in-visual-selection-beg
 :v  "A" #'evil-mc-make-cursor-in-visual-selection-end)
;; -----------------------------------------------------------------------------
;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package' `use-package!'for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', where Emacs
;;   looks when you load packages with `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c g k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c g d') to jump to their definition and see how
;; they are implemented.
