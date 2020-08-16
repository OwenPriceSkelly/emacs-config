;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el
(package! evil-textobj-line
  :pin "3d401b6831bdbeec967ec8e64177a8950251e812")
(package! mathpix.el
  :recipe (:host github :repo "jethrokuan/mathpix.el")
  :pin "52bc9a6acd34d2282555ac576e905af3c5a0e767")
(package! org-roam-server
  :pin nil)
;; existing packages pinned separately
(unpin! org-roam)
