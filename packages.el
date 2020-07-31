;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el
(package! evil-textobj-line
  :pin "3d401b6")
(package! mathpix.el
  :recipe (:host github :repo "jethrokuan/mathpix.el")
  :pin "52bc9a6")
(package! org-roam-server
  :pin nil)
;; existing packages pinned separately
(unpin! org-roam)
