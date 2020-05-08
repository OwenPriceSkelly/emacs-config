;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el
(package! hercules
  :pin "557da39")
(package! evil-textobj-line
  :pin "3d401b6")
(package! mathpix.el
  :recipe (:host github :repo "jethrokuan/mathpix.el")
  :pin "52bc9a6")
;; deliberately unpinned
(package! org-roam
  :pin nil)
(package! doom-themes
  :pin "34f181c")
