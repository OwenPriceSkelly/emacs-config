(use-package! mixed-pitch
  :hook (text-mode . mixed-pitch-mode)
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

(use-package! solaire-mode
  :init
  (setq solaire-mode-auto-swap-bg t
        solaire-mode-remap-line-numbers t))

(setq which-key-side-window-location 'bottom
      which-key-sort-order 'which-key-key-order-alpha
      which-key-max-description-length nil

      display-line-numbers-type nil
      evil-split-window-below t
      evil-vsplit-window-right t

      doom-modeline-persp-name t
      doom-modeline-major-mode-icon t)

(setq! +popup-defaults
       (list :side   'bottom
             :height .20 ;; 0.16
             :width  40
             :quit   t
             :select #'ignore
             :ttl    5))

(use-package! zoom
  :commands (zoom-mode))


;; (+global-word-wrap-mode)

;; (remove-hook! text-mode hl-line-mode)
;; (add-hook! prog-mode hl-line-mode)

(setq frame-title-format '("%b – Emacs")
      icon-title-format frame-title-format)
(modify-frame-parameters nil '((fullscreen . maximized)
                               (undecorated . t)))
;; (setq initial-frame-alist
;;        '((top . 1) (left . 1) (width . 140) (height . 58)))
;; (pushnew! default-frame-alist
;;           ;; '(width . 90)
;;           ;; '(height . 58)
;;           ;; '(left . 878)
;;           '(width . 238)
;;           '(height . 58)
;;           '(fullscreen . maximized))

(let* ((+override-theme 'doom-old-hope ;; 'doom-oceanic-next
        ) ;; 'doom-gruvbox-light

       (+my/themes-list-dark '(doom-gruvbox doom-horizon doom-oceanic-next))
       (+my/themes-list-light (append +my/themes-list-dark '(doom-gruvbox-light doom-nord-light)))
       (hour (caddr (decode-time nil)))
       (sec (car (decode-time nil))))
  (setq! doom-gruvbox-dark-variant 'soft
        doom-gruvbox-light-variant 'soft

        doom-theme                (or +override-theme
                                      (let ((theme-choices
                                             (if (<= 9 hour 15)
                                                 +my/themes-list-light
                                               +my/themes-list-dark)))
                                        (nth (mod sec (length theme-choices))
                                             theme-choices))))
  (intern (concat (symbol-name doom-theme) "-brighter-comments"))  t)

(doom-themes-set-faces nil
  '(org-block-begin-line :background nil)
  '(org-block :background nil)
  '(org-block-end-line :background nil))

(setq doom-font                       (font-spec
                                       :family "Iosevka Extended"
                                       :size 12)
      doom-variable-pitch-font        (font-spec
                                       :family "Iosevka Etoile"
                                       :size 12)

      +zen-text-scale                 0
      +latex-viewers                  (if IS-MAC '(pdf-tools))
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
(setq! +doom-dashboard-name "*dashboard*" )

(defun +my/doom-dashboard-widget-loaded ()
  (insert
   "\n\n"
   (propertize
    (+doom-dashboard--center
     +doom-dashboard--width
     (+my/doom-display-benchmark-h 'return))
    'face 'doom-dashboard-loaded)
   "\n"))
(defun +my/doom-display-benchmark-h (&optional return-p)
  (funcall (if return-p #'format #'message)
           "Loaded %d packages across %d modules in %.03fs"
           (- (length load-path) (length doom--initial-load-path))
           (if doom-modules (hash-table-count doom-modules) 0)
           (or doom-init-time
               (setq doom-init-time
                     (float-time (time-subtract (current-time) before-init-time))))))
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
         ;; ("Recently opened files"
         ;;  :icon (all-the-icons-octicon "file-text" :face 'doom-dashboard-menu-title)
         ;;  :action recentf-open-files)
         ("Open project"
          :icon (all-the-icons-octicon "repo" :face 'doom-dashboard-menu-title)
          :action projectile-switch-project)
         ;; ("Jump to bookmark"
         ;;  :icon (all-the-icons-octicon "bookmark" :face 'doom-dashboard-menu-title)
         ;;  :action bookmark-jump)
         ("Open private configuration"
          :icon (all-the-icons-octicon "tools" :face 'doom-dashboard-menu-title)
          :when (file-directory-p doom-private-dir)
          :action doom/open-private-config))

       +doom-dashboard-functions '(+my/doom-dashboard-widget-banner
                                   doom-dashboard-widget-shortmenu
                                   +my/doom-dashboard-widget-loaded))

(setq user-full-name "Owen Price Skelly"
      user-mail-address "Owen.Price.Skelly@gmail.com"
      ;; +mu4e-backend 'offlineimap TODO
      iedit-occurrence-context-lines 1
      fill-column 88
      company-idle-delay nil
      +workspaces-on-switch-project-behavior t)

(use-package! evil-textobj-line
  :commands (evil-inner-line evil-a-line))

(use-package! org
  :defer t
  :hook (org-mode . toc-org-mode)
  :hook (org-mode . +org-pretty-mode)
  :config
  ;; basic settings
  (setq org-directory            "~/Notes" ;; now symlinked to icloud documents for app
        org-agenda-files         (list org-directory)
        org-src-window-setup     'plain
        org-export-with-toc      nil
        org-export-with-section-numbers nil
        org-use-sub-superscripts t
        org-export-with-entities t
        org-imenu-depth          9
        org-startup-folded       'content)  ;; showeverything ;; t ;; nil

  ;; fontifying, keywords
  (setq org-ellipsis                      " ▾ "
        org-todo-keywords                 '((sequence "[ ](t)" "[~](p)" "[*](w)" "[!](r)" "|"
                                                      "[X](d)" "[-](k)")
                                            (sequence "TODO(T)" "PROG(P)" "WAIT(W)" "WARN(R)" "|"
                                                      "DONE(D)" "DROP(K)"))
        org-todo-keyword-faces            '(("[~]"   . +org-todo-active)
                                            ("[*]"   . +org-todo-onhold)
                                            ("[!]"   . compilation-error)
                                            ("WARN"  . compilation-error)
                                            ("PROG"  . +org-todo-active)
                                            ("WAIT"  . +org-todo-onhold)))
  ;; inline LaTeX/math-related
  (sp-local-pair '(org-mode) "$" "$")
  (setq org-preview-latex-default-process 'dvisvgm ;'imagemagick ;'dvipng
        org-startup-with-latex-preview nil
        org-highlight-latex-and-related nil
        org-entities-user
        ;;org  | LaTeX str |math?| html     |ascii|latin1|utf-8
        '(("Z" "\\mathbb{Z}" t    "&#x2124;"  "Z"   "Z"    "ℤ")
          ("C" "\\mathbb{C}" t    "&#x2102;"  "C"   "C"    "ℂ")
          ("H" "\\mathbb{H}" t    "&#x210D;"  "H"   "H"    "ℍ")
          ("N" "\\mathbb{N}" t    "&#x2115;"  "N"   "N"    "ℕ")
          ("P" "\\mathbb{P}" t    "&#x2119;"  "P"   "P"    "ℙ")
          ("Q" "\\mathbb{Q}" t    "&#x211A;"  "Q"   "Q"    "ℚ")
          ("R" "\\mathbb{R}" t    "&#x211D;"  "R"   "R"    "ℝ"))
        org-format-latex-options '(:foreground default
                                   :background default
                                   :scale 1.0
                                   :html-scale 1.0
                                   :html-foreground "Black"
                                   :html-background "Transparent"
                                   :matchers ("begin" "$1" "$" "$$" "\\(" "\\["))))

(use-package! ox-hugo
  :after org
  :config
  (setq org-hugo-preserve-filling nil
        org-hugo-section "notes"))

(use-package! org-roam
  :after org
  :commands (org-roam-buffer-toggle-display
             org-roam-find-file
             org-roam-dailies-date
             org-roam-dailies-today
             org-roam-dailies-tomorrow
             org-roam-dailies-yesterday)
  :init
  (setq! org-roam-directory               org-directory
         org-roam-tag-sort                t
         org-roam-tag-sources             '(prop)
         org-roam-tag-separator           ", "
         org-roam-verbose                 t
         org-roam-buffer-width            0.25
         org-roam-graph-max-title-length  40
         org-roam-graph-shorten-titles    'truncate
         org-roam-graph-exclude-matcher   '("old/" "Sunday" "Monday" "Tuesday" "Wednesday" "Thursday" "Friday" "Saturday" "journal")
         org-roam-graph-viewer            (executable-find "open"))
  (+my/org-roam-templates)

  (remove-hook 'org-roam-buffer-prepare-hook 'org-roam-buffer--insert-ref-links)
  (add-hook! org-roam-mode (org-hugo-auto-export-mode) :local))

(defun +my/org-roam-templates ()
  (setq org-roam-capture-ref-templates (list (list "r" "ref" 'plain (list 'function #'org-roam-capture--get-point)
                                                   "%?"
                                                   :file-name "${slug}"
                                                   :head (concat "#+title: ${title}\n"
                                                                 "#+roam_key: ${ref}\n"
                                                                 "#+roam_tags: article\n"
                                                                 "#+setupfile: ./setup.org\n"
                                                                 "* Related: \n"
                                                                 "  - [[${ref}][url]]\n")
                                                   :unnarrowed t))
        org-roam-capture-templates (list (list "d" "default" 'plain (list 'function #'org-roam-capture--get-point)
                                               "%?"
                                               :file-name "%<%Y-%m-%d>-${slug}"
                                               :head (concat "#+title: ${title}\n"
                                                             "#+roam_tags:\n"
                                                             "#+setupfile: ./setup.org\n"
                                                             "* Description: \n"
                                                             "* Related: \n" )
                                               :unnarrowed t))
        org-roam-capture-immediate-template `("d" "default" plain #'org-roam-capture--get-point
                                             "%?"
                                             :file-name "%<%Y-%m-%d>-${slug}"
                                             :head ,(concat "#+title: ${title}\n"
                                                            "#+roam_tags:\n"
                                                            "#+setupfile: ./setup.org\n"
                                                            "* Description: \n"
                                                            "* Related: \n")
                                             :unnarrowed t
                                             :immediate-finish t)
        org-roam-dailies-capture-templates (list (list "d" "daily" 'plain (list 'function #'org-roam-capture--get-point)
                                                       ""
                                                       :immediate-finish t
                                                       :file-name "%<%Y-%m-%d-%A>"
                                                       :head (concat "#+title: %<%a, %b %d, %y>\n"
                                                                     "#+roam_tags: journal\n"
                                                                     "* Tasks: \n" )))))

(use-package! org-roam-server
  :commands (org-roam-server-mode))

(use-package! org-superstar ; "prettier" bullets
  :hook (org-mode . org-superstar-mode)
  :config
  (setq org-superstar-headline-bullets-list '("☰" "☱" "☳" "☷" "☶" "☴")  ;; '("#")
        org-superstar-prettify-item-bullets t
        org-superstar-item-bullet-alist
        '((?* . ?»)
          (?+ . ?»)
          (?- . ?›))
        org-superstar-special-todo-items nil))

(use-package! mathpix
  :commands (mathpix-screenshot)
  :config
  (setq mathpix-app-id            (password-store-get "mathpix.com/app-id")
        mathpix-app-key           (password-store-get "mathpix.com/app-key")
        mathpix-screenshot-method "screencapture -i %s"))

(setq +markdown-compile-functions '(+markdown-compile-pandoc
                                    +markdown-compile-marked
                                    +markdown-compile-markdown
                                    +markdown-compile-multimarkdown))

(use-package! python
  :after python
  :config
  (sp-local-pair '(python-mode) "f\"" "\"" :post-handlers '(:add sp-python-fix-tripple-quotes)))

(when (featurep! :tools lsp +eglot)
  (use-package! eglot
    :commands (eglot eglot-ensure)
    :config
    (setq eglot-send-changes-idle-time 0.05)
    (set-lookup-handlers! 'eglot--managed-mode ;:async t
      :implementations #'eglot-find-implementation
      :type-definition #'eglot-find-typeDefinition
      :documentation #'+eglot/documentation-lookup-handler
      ;; :definition
      ;; :references
      )
    (add-to-list 'eglot-ignored-server-capabilites :documentHighlightProvider)))
;; (when (featurep! :tools lsp +peek)
;;   (use-package! lsp-ui
;;     :defer t
;;     :config
;;     (setq lsp-ui-doc-max-height 10
;;           lsp-ui-doc-max-width 88
;;           lsp-ui-sideline-diagnostic-max-line-length 35
;;           lsp-ui-sideline-ignore-duplicate t
;;           lsp-ui-doc-enable nil
;;           ;; Don't show symbol definitions in the sideline. They are pretty noisy,
;;           ;; and there is a bug preventing Flycheck errors from being shown (the
;;           ;; errors flash briefly and then disappear).
;;           lsp-ui-sideline-show-hover nil)))

(setq  doom-leader-key "SPC"
       doom-leader-alt-key "C-SPC"
       doom-localleader-key ","
       doom-localleader-alt-key "C-,")

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
(map! :nv "R"  #'evil-multiedit-match-all
      :n "s-d" #'evil-multiedit-match-symbol-and-next
      :n "s-D" #'evil-multiedit-match-symbol-and-prev
      :v "s-d" #'evil-multiedit-match-and-next
      :v "s-D" #'evil-multiedit-match-and-prev
      (:after evil-multiedit
       (:map evil-multiedit-state-map
        "n"    #'evil-multiedit-next
        "N"    #'evil-multiedit-prev
        "s-d"  #'evil-multiedit-match-and-next
        "s-D"  #'evil-multiedit-match-and-prev
        "V"    #'iedit-show/hide-unmatched-lines)))

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
       :desc "toggle maximized" "M" #'toggle-frame-maximized
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
      :prefix ("n" . "notes")
      :desc "roam buffer"        "r"  #'org-roam
      :desc "random note"        "R"  #'org-roam-random-note
      :desc "find"               "n"  #'org-roam-find-file
      ;; :desc "jump to index"      "x"  #'org-roam-jump-to-index
      :desc "insert"             "i"  #'org-roam-insert
      :desc "insert immediate"   "I"  #'org-roam-insert-immediate
      :desc "today's file"       "t"  #'org-roam-dailies-today
      :desc "tomorrow's file"    "T"  #'org-roam-dailies-tomorrow
      :desc "yesterday's file"   "y"  #'org-roam-dailies-yesterday
      :desc "<date>'s file"      "d"  #'org-roam-dailies-date
      ;; :desc "daily entries map" "d"  org-roam-dailies-map
      :desc "mathpix screenshot" "m"  #'mathpix-screenshot
      (:prefix ( "g" . "graph")
       :desc "server view"       "s"  (cmd! (unless org-roam-server-mode
                                              (org-roam-server-mode))
                                            (browse-url
                                             (url-recreate-url
                                              (url-generic-parse-url
                                               (concat "http://" org-roam-server-host ":" (int-to-string org-roam-server-port))))))
       :desc "graph all"   "g"  #'org-roam-graph
       :desc "graph connected" "c" (cmd!! #'org-roam-graph '(4))))
