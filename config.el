(use-package! solaire-mode
  :init
  (setq solaire-mode-auto-swap-bg t
        solaire-mode-remap-line-numbers t))

;; (use-package! writeroom-mode
;;   :hook (text-mode . writeroom-mode)
;;   :config
;;   (setq writeroom-width 100
;;         writeroom-maximize-window nil
;;         writeroom-mode-line t
;;         writeroom-header-line nil))

(use-package! mixed-pitch
  :hook (org-mode . mixed-pitch-mode)
  :config
  (pushnew! mixed-pitch-fixed-pitch-faces
            'org-date
            'org-special-keyword
            'org-property-value
            'org-ref-cite-face
            'org-tag
            'org-todo-keyword-todo
            'org-todo-keyword-habt
            'org-todo-keyword-done
            'org-todo-keyword-wait
            'org-todo-keyword-kill
            'org-todo-keyword-outd
            'org-todo
            'org-done
            'font-lock-comment-face
            'line-number
            'line-number-current-line))

(setq which-key-side-window-location 'bottom
      which-key-sort-order 'which-key-key-order-alpha
      which-key-max-description-length nil

      display-line-numbers-type nil

      evil-split-window-below t
      evil-vsplit-window-right t

      doom-modeline-persp-name t
      doom-modeline-major-mode-icon t)

(+global-word-wrap-mode)
(remove-hook! text-mode hl-line-mode)

(if IS-MAC (set-frame-parameter nil 'internal-border-width 4))
(toggle-frame-fullscreen)
(setq frame-title-format '("%b – Emacs")
      icon-title-format frame-title-format)

(setq +my/themes-list-dark      '(doom-gruvbox
                                  doom-moonlight
                                  doom-henna
                                  doom-oceanic-next)
       +my/themes-list-light     '(doom-gruvbox-light
                                   doom-nord-light)
       doom-gruvbox-dark-variant 'hard
       doom-gruvbox-light-variant 'soft
       +override-theme           'doom-oceanic-next ; nil ;'doom-gruvbox-light ;oceanic-next
       doom-theme                (or +override-theme
                                     (let ((hour (caddr (decode-time nil)))
                                           (sec (car (decode-time nil))))
                                       (let ((theme-choices
                                              (if (<= 9 hour 15)
                                                  +my/themes-list-light
                                                +my/themes-list-dark)))
                                         (nth (mod sec (length theme-choices))
                                              theme-choices)))))

(doom-themes-set-faces nil
  '(org-block-begin-line :background nil)
  '(org-block-end-line :background nil)
  ;; '(org-block :background nil)
  )

(setq doom-font                       (font-spec
                                       :family "Iosevka Extended"
                                       :size 14)
      doom-variable-pitch-font        (font-spec
                                       :family "Iosevka Sparkle"
                                       :size 14)

      +zen-text-scale                 0
      ;+latex-viewers                  (if IS-MAC '(pdf-tools))
      +pretty-code-enabled-modes      '(org-mode))

(defun +my/doom-dashboard-widget-banner ()
  (let ((point (point)))
    (mapc (lambda (line)
            (insert (propertize (+doom-dashboard--center +doom-dashboard--width line)
                                'face 'doom-dashboard-banner) " ")
            (insert "\n"))
          '(""
            "       ___           ___           ___           ___           ___      "
            "      /  /\\         /__/\\         /  /\\         /  /\\         /  /\\     "
            "     /  /:/_       |  |::\\       /  /::\\       /  /:/        /  /:/_    "
            "    /  /:/ /\\      |  |:|:\\     /  /:/\\:\\     /  /:/        /  /:/ /\\   "
            "   /  /:/ /:/_   __|__|:|\\:\\   /  /:/ /::\\   /  /:/  ___   /  /:/ /::\\  "
            "  /__/:/ /:/ /\\ /__/::::| \\:\\ /__/:/ /:/\\:\\ /__/:/  /  /\\ /__/:/ /:/\\:\\ "
            "  \\  \\:\\/:/ /:/ \\  \\:\\     \\/ \\  \\:\\/:/__\\/ \\  \\:\\ /  /:/ \\  \\:\\/:/ /:/ "
            "   \\  \\::/ /:/   \\  \\:\\        \\  \\::/       \\  \\:\\  /:/   \\  \\::/ /:/  "
            "    \\  \\:\\/:/     \\  \\:\\        \\  \\:\\        \\  \\:\\/:/     \\__\\/ /:/   "
            "     \\  \\::/       \\  \\:\\        \\  \\:\\        \\  \\::/        /__/:/    "
            "      \\__\\/         \\__\\/         \\__\\/         \\__\\/         \\__\\/     "
            ""
            ""
            ""
            ""))
    (when (and (display-graphic-p)
               (stringp fancy-splash-image)
               (file-readable-p fancy-splash-image))
      (let ((image (create-image (fancy-splash-image-file))))
        (add-text-properties
         point (point) `(display ,image rear-nonsticky (display)))
        (save-excursion
          (goto-char point)
          (insert (make-string
                   (truncate
                    (max 0 (+ 1 (/ (- +doom-dashboard--width (car (image-size image nil)))
                                   2)))) 32))))
      (insert (make-string (or (cdr +doom-dashboard-banner-padding) 0) 10)))))

(add-hook! +doom-dashboard-mode (hl-line-mode -1))
(setq! +doom-dashboard-menu-sections
       '(("Reload last session"
          :icon (all-the-icons-octicon "history" :face 'doom-dashboard-menu-title)
          :when (cond ((require 'persp-mode nil t)
                       (file-exists-p (expand-file-name persp-auto-save-fname persp-save-dir)))
                      ((require 'desktop nil t)
                       (file-exists-p (desktop-full-file-name))))
          :face (:inherit (doom-dashboard-menu-title bold))
          :action doom/quickload-session)
         ("Open today's note"
          :icon (all-the-icons-octicon "book" :face 'doom-dashboard-menu-title)
          :action org-roam-dailies-today)
         ("Recently opened files"
          :icon (all-the-icons-octicon "file-text" :face 'doom-dashboard-menu-title)
          :action recentf-open-files)
         ("Open project"
          :icon (all-the-icons-octicon "repo" :face 'doom-dashboard-menu-title)
          :action projectile-switch-project)
         ("Jump to bookmark"
          :icon (all-the-icons-octicon "bookmark" :face 'doom-dashboard-menu-title)
          :action bookmark-jump)
         ("Open private configuration"
          :icon (all-the-icons-octicon "tools" :face 'doom-dashboard-menu-title)
          :when (file-directory-p doom-private-dir)
          :action doom/open-private-config))

       +doom-dashboard-functions '(+my/doom-dashboard-widget-banner
                                   doom-dashboard-widget-shortmenu
                                   doom-dashboard-widget-loaded))

(setq! user-full-name "Owen Price-Skelly"
       user-mail-address "Owen.Price.Skelly@gmail.com"
       ;; +mu4e-backend 'offlineimap TODO
       iedit-occurrence-context-lines 1
       fill-column 88
       company-idle-delay 0.1
       +workspaces-on-switch-project-behavior t)

(use-package! evil-textobj-line
  :demand t)

(when (featurep! :tools lsp)
  (if (featurep! :tools lsp +eglot)
      (use-package! eglot
        :commands eglot eglot-ensure
        :config
        (setq eglot-send-changes-idle-time 0.05)
        (set-lookup-handlers! 'eglot--managed-mode ;:async t
          :implementations #'eglot-find-implementation
          :type-definition #'eglot-find-typeDefinition
          :documentation #'+eglot/documentation-lookup-handler
          ;; :definition
          ;; :references
          )
        (add-to-list 'eglot-ignored-server-capabilites :documentHighlightProvider))
    ;; (use-package! lsp-mode
    ;;   :after lsp-mode
    ;;   ;; TODO
    ;;   )
    ))

(defun +my/org-basic-settings ()
  (setq  org-src-window-setup             'plain
         org-export-with-toc               nil
         org-imenu-depth                   9
         org-directory                     (if IS-MAC "~/.org" "~/.org.d")
         org-preview-latex-default-process 'dvisvgm
         ;; org-preview-latex-default-process 'imagemagick
         ;; org-preview-latex-default-process 'dvipng
         org-startup-folded                'content
         org-startup-with-latex-preview    nil
         org-highlight-latex-and-related   nil))
(defun +my/org-variables-config ()
  (setq! org-ellipsis                      " ▾ "
         org-superstar-headline-bullets-list '("#") ;; '("☰" "☱" "☳" "☷" "☶" "☴")
         org-entities-user
         ;; org |   LaTeX | mathp | html  |ascii|latin1|utf-8
         '(("Z"   "\\mathbb{Z}" t "&#x2124;"  "Z" "Z"  "ℤ")
           ("C"   "\\mathbb{C}" t "&#x2102;"  "C" "C"  "ℂ")
           ("H"   "\\mathbb{H}" t "&#x210D;"  "H" "H"  "ℍ")
           ("N"   "\\mathbb{N}" t "&#x2115;"  "N" "N"  "ℕ")
           ("P"   "\\mathbb{P}" t "&#x2119;"  "P" "P"  "ℙ")
           ("Q"   "\\mathbb{Q}" t "&#x211A;"  "Q" "Q"  "ℚ")
           ("R"   "\\mathbb{R}" t "&#x211D;"  "R" "R"  "ℝ"))
         org-format-latex-options          '(:foreground default
                                             :background default
                                             :scale 1.0
                                             :html-scale 1.0
                                             :html-foreground "Black"
                                             :html-background "Transparent"
                                             :matchers ("begin" "$1" "$" "$$" "\\(" "\\["))
         org-todo-keywords                 '((sequence "[ ](t)" "[~](p)" "[*](w)" "[!](r)" "|"
                                                       "[X](d)" "[-](k)")
                                             (sequence "TODO(T)" "PROG(P)" "WAIT(W)" "WARN(R)" "|"
                                                       "DONE(D)" "DROP(K)"))
         org-todo-keyword-faces            '(("[~]"   . +org-todo-active)
                                             ("[*]"   . +org-todo-onhold)
                                             ("[!]"   . compilation-error)
                                             ("WARN"   . compilation-error)
                                             ("PROG"  . +org-todo-active)
                                             ("WAIT"  . +org-todo-onhold)))
  (sp-local-pair '(org-mode) "$" "$") ;; For inline latex stuff
  ;; (set-popup-rule! "^\\*Org Src" :ignore t)
  )

(use-package! org
  ;; :after org
  :defer t
  :hook (org-mode . toc-org-mode)
  :hook (org-mode . +org-pretty-mode)
;;  :hook (org-mode . writeroom-mode)
  :config
  (+my/org-basic-settings)
  (+my/org-variables-config))

(defun +my/org-roam-templates-config ()
    (setq org-roam-capture-ref-templates
        (list (list "r" "ref" 'plain (list 'function #'org-roam-capture--get-point)
                    "%?"
                    :file-name "${slug}"
                    :head (concat "#+TITLE: ${title}\n"
                                  "#+ROAM_KEY: ${ref}\n"
                                  "#+ROAM_TAGS:\n"
                                  "* Description: \n"
                                  "* Related: \n")
                    :unnarrowed t))
        org-roam-capture-templates
        (list (list "d" "default" 'plain (list 'function #'org-roam-capture--get-point)
                    "%?"
                    :file-name "%<%Y-%m-%d>-${slug}"
                    :head (concat "#+TITLE: ${title}\n"
                                  "#+ROAM_TAGS:\n"
                                  "* Description: \n"
                                  "* Related: \n" )
                    :unnarrowed t))
        org-roam-dailies-capture-templates
        (list (list "d" "daily" 'plain (list 'function #'org-roam-capture--get-point)
                    ""
                    :immediate-finish t
                    :file-name "%<%Y-%m-%d-%A>"
                    :head (concat "#+TITLE: %<%A, %B %d, %Y>\n"
                                  "#+ROAM_TAGS: journal\n"
                                  "* Tasks: \n" )))
        ;; '(("d" "daily" plain (function org-roam-capture--get-point)
        ;;    ""
        ;;    :immediate-finish t
        ;;    :file-name "%<%Y-%m-%d-%A>"
        ;;    :head "#+TITLE: %<%A, %B %d, %Y>"))
        ))

(defun +my/org-roam-vars-config ()
    (setq! org-roam-directory               org-directory
           org-roam-index-file              "./index.org"
           org-roam-tag-sort                t
           org-roam-tag-sources             '(prop)
           org-roam-tag-separator           ", "
           org-roam-verbose                 t
           org-roam-buffer-position         'right
           org-roam-buffer-width            0.26
           org-roam-graph-max-title-length  40
           org-roam-graph-shorten-titles    'truncate
           org-roam-graph-exclude-matcher   '("old/" "Sunday" "Monday" "Tuesday" "Wednesday" "Thursday" "Friday" "Saturday" "journal")
           org-roam-graph-viewer            (executable-find
                                             (if IS-MAC "open" "firefox"))
           org-roam-graph-executable        "dot"
           org-roam-graph-node-extra-config '(("shape" . "underline")
                                              ("style" . "rounded,filled")
                                              ("fillcolor" . "#EEEEEE")
                                              ("color" . "#C9C9C9")
                                              ("fontcolor" . "#111111"))))

(use-package! org-roam
  :after org
  :config
  (+my/org-roam-templates-config)
  (+my/org-roam-vars-config)
  (remove-hook 'org-roam-buffer-prepare-hook 'org-roam-buffer--insert-citelinks)
  (add-hook! 'org-roam-buffer-prepare-hook
             :append
             org-set-startup-visibility ;; (λ!! (org-global-cycle '(4)))
             ))

(use-package! org-roam-server
  :commands (org-roam-server-mode))

(use-package! mathpix
  :commands (mathpix-screenshot)
  :config
  (setq mathpix-app-id            "owenpriceskelly_gmail_com_2bbd51"
        mathpix-app-key           "0b3d8ae26f3762b4d5b8"
        mathpix-screenshot-method "screencapture -i %s"))

(setq +markdown-compile-functions '(+markdown-compile-pandoc
                                    +markdown-compile-marked
                                    +markdown-compile-markdown
                                    +markdown-compile-multimarkdown))

(use-package! python
  :after python
  :config
  (sp-local-pair '(python-mode) "f\"" "\"" :post-handlers '(:add sp-python-fix-tripple-quotes)))

(setq  doom-leader-key "SPC"
       doom-leader-alt-key (if IS-LINUX "C-SPC"
                             "M-SPC" )
       doom-localleader-key ","
       doom-localleader-alt-key "C-,"
       )

(use-package! expand-region
  :config
  (setq expand-region-contract-fast-key "V"))

(use-package! evil-snipe
  :init
  (setq evil-snipe-scope                     'whole-visible
        evil-snipe-spillover-scope           'whole-buffer
        evil-snipe-repeat-scope              'buffer
        evil-snipe-tab-increment             t
        evil-snipe-repeat-keys               t
        evil-snipe-override-evil-repeat-keys t)
  :config
  ;; when f/t/s searching, interpret open/close square brackets to be any
  ;; open/close delimiters, respectively
  (push '(?\[ "[[{(]") evil-snipe-aliases)
  (push '(?\] "[]})]") evil-snipe-aliases)
  ;; "C-;" pre-fills avy-goto-char-2 with most recent snipe
  (map! :map (evil-snipe-parent-transient-map evil-snipe-local-mode-map)
        "C-;" (cmd! (if evil-snipe--last
                        (let ((most-recent-chars (nth 1 evil-snipe--last)))
                          (if (eq 2 (length most-recent-chars))
                              (apply #'avy-goto-char-2 most-recent-chars)
                            (call-interactively #'avy-goto-char-2))))))
  (setq! avy-all-windows t)
  (evil-snipe-override-mode +1))

(map! :nv [tab]  #'evil-jump-item
        (:when (featurep! :ui workspaces)
         :g [C-tab] #'+workspace/switch-right)

        (:when (featurep! :completion company)
         :i "C-i" #'+company/complete
         :i [C-i] #'+company/complete)

        ;;lispy
        (:after lispy
         (:map (lispy-mode-map lispy-mode-map-evilcp lispy-mode-map-lispy)
          "[" nil
          "]" nil)
         (:map lispyville-mode-map
           "M-[" #'lispy-backward
           "M-]" #'lispy-forward)))

;; multiedit
(map! :nv "R"     #'evil-multiedit-match-all
      :n "s-d"    #'evil-multiedit-match-symbol-and-next
      :n "s-D"  #'evil-multiedit-match-symbol-and-prev
      :v "s-d"    #'evil-multiedit-match-and-next
      :v "s-D"  #'evil-multiedit-match-and-prev
      (:after evil-multiedit
       (:map evil-multiedit-state-map
        "n"       #'evil-multiedit-next
        "N"       #'evil-multiedit-prev
        "s-d"     #'evil-multiedit-match-and-next
        "s-D"   #'evil-multiedit-match-and-prev
        "V"       #'iedit-show/hide-unmatched-lines)))

(map! :leader
      :desc "Search project" "/" #'+default/search-project
      :desc "Visual expand" "v" #'er/expand-region

      (:prefix ("w" . "window")
       :desc "Switch to last window" "w"    #'evil-window-mru)

      (:prefix ("b" . "buffer")
       :desc "Fallback buffer"        "h"   #'+doom-dashboard/open
       :desc "Messages buffer"        "m"   #'view-echo-area-messages
       :desc "ibuffer (other window)" "I"   #'ibuffer-other-window)

      (:prefix ("f" . "file")
       :desc "find file new window"   "F"   #'find-file-other-window)

      (:prefix ("t"  "toggle")
       :desc "toggle fullscreen" "F" #'toggle-frame-fullscreen
       :desc "toggle hl-line mode" "h" (cmd! (hl-line-mode (if hl-line-mode -1 +1)))
       :desc "toggle decorated"  "d" (cmd! (set-frame-parameter nil 'undecorated (not (frame-parameter nil 'undecorated)))))

      (:when (featurep! :emacs undo +tree)
       :desc "Undo tree"              "U"   #'undo-tree-visualize)

      (:when (featurep! :ui treemacs)
       :desc "Project sidebar"        "0"   #'+treemacs/toggle)

      (:when (featurep! :ui workspaces)
       (:prefix "TAB"
        :desc "Main workspace"       "`"    #'+workspace/switch-to-0
        :desc "Previous workspace"   "TAB"  #'+workspace/other
        :desc "Forward frame"        "f"    #'+evil/next-frame
        :desc "Backward frame"       "F"    #'+evil/previous-frame))

      (:when (featurep! :completion ivy)
       :desc "Ivy M-x"                "SPC" #'counsel-M-x))

(map! :map org-mode-map
      :localleader
      :desc "Sort"                     "S" #'org-sort
      :desc "preview LaTeX fragments"  "L" #'org-latex-preview
      :desc "toggle pretty entities"   "p" #'+org-pretty-mode)

(map! :leader
      (:prefix ("n" . "notes")
       :desc "roam buffer"        "r"  #'org-roam
       :desc "find"               "f"  #'org-roam-find-file
       :desc "find"               "n"  #'org-roam-find-file
       :desc "jump to index"      "x"  #'org-roam-jump-to-index
       :desc "insert"             "i"  #'org-roam-insert
       :desc "today's file"       "t"  #'org-roam-dailies-today
       :desc "tomorrow's file"    "T"  #'org-roam-dailies-tomorrow
       :desc "yesterday's file"   "y"  #'org-roam-dailies-yesterday
       :desc "<date>'s file"      "d"  #'org-roam-dailies-date
       :desc "mathpix.el"         "m"  #'mathpix-screenshot
       (:prefix ( "g" . "graph")
        :desc "toggle server"     "s"  #'org-roam-server-mode
        :desc "graph all notes"   "g"  #'org-roam-graph
        :desc "graph neighbors"   "n"  (λ! (org-roam-graph 1))
        :desc "graph connected"   "c"  (λ!! #'org-roam-graph '(4)))))
