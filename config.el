;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

(setq user-full-name "Owen Price-Skelly"
      user-mail-address "Owen.Price.Skelly@gmail.com"

      doom-theme 'doom-horizon
      doom-font (font-spec :family "monospace" :size 14)

      org-directory "~/org/"
      deft-directory org-directory

      display-line-numbers-type 'relative

      doom-localleader-key ",")

;; -----------------------------------------------------------------------------
;; here are some additional functions/macros that could help you configure doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; -----------------------------------------------------------------------------
;; - `use-package' `use-package!'for configuring packages
;; -----------------------------------------------------------------------------
;; - `after!' for running code after a package has loaded
;; -----------------------------------------------------------------------------
;; - `add-load-path!' for adding directories to the `load-path', where Emacs
;;   looks when you load packages with `require' or `use-package'.
;; -----------------------------------------------------------------------------
;; - `map!' for binding new keys
(map! (:when (featurep! :ui tabs))
      :n "SPC ]" #'centaur-tabs-forward-tab
      :n "SPC [" #'centaur-tabs-backward-tab)

;; -----------------------------------------------------------------------------
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c g k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c g d') to jump to their definition and see how
;; they are implemented.
