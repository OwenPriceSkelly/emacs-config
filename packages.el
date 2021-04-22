;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el
(package! evil-textobj-line
  :pin "3d401b6831bdbeec967ec8e64177a8950251e812")
(package! mathpix.el
  :recipe (:host github :repo "jethrokuan/mathpix.el")
  :pin "52bc9a6acd34d2282555ac576e905af3c5a0e767")
(package! solaire-mode :disable t)

;; existing packages that I don't want/need pinned
(if (featurep! :tools lsp +eglot)
    (unpin! eglot)
  (unpin! lsp-mode lsp-ui))


(unpin! org-roam doom-themes)
(package! org-roam-server)
(unless IS-MAC (package! theme-magic))
