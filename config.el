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
(map! (:when featurep! :)
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
