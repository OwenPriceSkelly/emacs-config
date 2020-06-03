;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el
(package! hercules
  :pin "557da39")
(package! evil-textobj-line
  :pin "3d401b6")
(package! mathpix.el
  :recipe (:host github :repo "jethrokuan/mathpix.el")
  :pin "52bc9a6")
;; existing packages pinned separately
(package! org-roam
  :recipe (:host github :repo "org-roam/org-roam")
  :pin "cce2db8b5a2c1f1258bcb35f4a3241067e4f950e")
(package! org-roam-server
  :pin nil)
(package! doom-themes
  :recipe (:local-repo  "~/.doom.d/emacs-doom-themes/"))
