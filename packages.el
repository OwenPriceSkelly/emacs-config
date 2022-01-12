;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el
(package! evil-textobj-line
  :pin "9eaf9a5485c2b5c05e16552b34632ca520cd681d")
(package! mathpix.el
  :recipe (:host github :repo "jethrokuan/mathpix.el")
  :pin "1ce2d4aa7708271cf60ec929688c1ce420c3fc86")

(package! simple-httpd
  :pin "22ce66ea43e0eadb9ec1d691a35d9695fc29cee6")
(package! websocket
  :pin "fda4455333309545c0787a79d73c19ddbeb57980")
(package! org-roam-ui
  :recipe (:host github :repo "org-roam/org-roam-ui" :files ("*.el" "out"))
   :pin "af0a8f2365968bbafc7991304060f381f9e67316")

(package! solaire-mode :disable t)

;; prefer builtin flymake with eglot
(package! flymake :built-in t)

;; existing packages that I don't want/need pinned
(if (featurep! :tools lsp +eglot)
    (unpin! eglot)
  (unpin! lsp-mode lsp-ui))


(unpin! org-roam doom-themes)

