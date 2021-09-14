;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el
(package! evil-textobj-line
  :pin "3d401b6831bdbeec967ec8e64177a8950251e812")
(package! mathpix.el
  :recipe (:host github :repo "jethrokuan/mathpix.el")
  :pin "1ce2d4aa7708271cf60ec929688c1ce420c3fc86")

(package! solaire-mode :disable t)

;; existing packages that I don't want/need pinned
(if (featurep! :tools lsp +eglot)
    (unpin! eglot)
  (unpin! lsp-mode lsp-ui))


(unpin! org-roam doom-themes)
