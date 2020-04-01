;;; personal/config/config.el -*- lexical-binding: t; -*-

(when (featurep! +keybindings)
  (load! "+keybindings"))

(when (featurep! +lsp)
  (load! "+lsp"))

(when (featurep! +notes)
  (load! "+notes"))
