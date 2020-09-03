;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el
(package! evil-textobj-line
  :pin "3d401b6831bdbeec967ec8e64177a8950251e812")
(package! mathpix.el
  :recipe (:host github :repo "jethrokuan/mathpix.el")
  :pin "52bc9a6acd34d2282555ac576e905af3c5a0e767")
(package! zoom :pin "a373e7eed59ad93315e5ae88c816ca70404d2d34")

(package! org-roam-server  :pin "832ba1ab6b3eea89da3355f7676626266a172adc")
;; existing packages pinned separately
(unpin! org-roam)
(package! visual-basic-mode
  :recipe (:host github :repo "emacsmirror/visual-basic-mode") :pin "79689e97d9dc0f90388c4111c5409d544a173631")
