;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el
(package! evil-textobj-line
  :pin "3d401b6831bdbeec967ec8e64177a8950251e812")
(package! mathpix.el
  :recipe (:host github :repo "jethrokuan/mathpix.el")
  :pin "52bc9a6acd34d2282555ac576e905af3c5a0e767")
(package! visual-basic-mode
  :recipe (:host github :repo "emacsmirror/visual-basic-mode") :pin "79689e97d9dc0f90388c4111c5409d544a173631")

(package! solaire-mode :disable t)
;; existing packages pinned separately
(if (featurep! :tools lsp +eglot)
    (unpin! eglot)
  (unpin! lsp-mode lsp-ui))

(package! org-roam-server)
(unpin! org-roam doom-themes)
(package! darktooth-theme)
(package! creamsody-theme)
(package! theme-magic)
