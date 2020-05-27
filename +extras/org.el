;;; $DOOMDIR/+extra/org.el -*- lexical-binding: t; -*-
;;; Configures org, org-roam for notes

(use-package! org
  :init
  (setq org-directory                   (if IS-MAC "~/.org/" "~/.org.d/"))
  (sp-local-pair '(org-mode) "$" "$") ;; For inline latex stuff
  (add-hook! (org-mode) #'(+org-pretty-mode  variable-pitch-mode)) ;;enable variable pitch font and ligatures etc

  :config
  (setq org-entities-user
        ;; org |latex |mathp|html         |ascii|latin1|utf-8
        '(("Z"   "\\mathbb{Z}" t "&#x2124;"  "Z" "Z"  "ℤ")
          ("C"   "\\mathbb{C}" t "&#x2102;"  "C" "C"  "ℂ")
          ;; ("H"   "\\mathbb{H}" t "&#x210D;"  "H" "H"  "ℍ")
          ("N"   "\\mathbb{N}" t "&#x2115;"  "N" "N"  "ℕ")
          ("P"   "\\mathbb{P}" t "&#x2119;"  "P" "P"  "ℙ")
          ("Q"   "\\mathbb{Q}" t "&#x211A;"  "Q" "Q"  "ℚ")
          ("R"   "\\mathbb{R}" t "&#x211D;"  "R" "R"  "ℝ")))
  (setq org-startup-folded              'content
        org-startup-with-latex-preview nil
        org-highlight-latex-and-related nil
        org-preview-latex-default-process 'dvisvgm
        org-format-latex-options '(:foreground default :background default :scale 1.0
                                   :html-foreground "Black" :html-background "Transparent"
                                   :html-scale 1.0 :matchers ("begin" "$1" "$" "$$" "\\(" "\\["))
        org-ellipsis " ▾ "
        org-superstar-headline-bullets-list '("☰" "☱" "☳" "☷" "☶" "☴" ;; "☵" "☲"
                                              )
        org-todo-keywords '((sequence "[ ](t)" "[~](p)" "[*](w)" "|"
                                      "[X](d)" "[-](k)")
                            (sequence "TODO(T)" "PROG(P)" "WAIT(W)" "|"
                                      "DONE(D)" "DROP(K)"))
        org-todo-keyword-faces '(("[~]"   . +org-todo-active)
                                 ("[*]"   . +org-todo-onhold)
                                 ("PROG"  . +org-todo-active)
                                 ("WAIT"  . +org-todo-onhold))))

(defun +my/org-roam-templates ()
  (setq org-roam-capture-ref-templates (list (list "r" "ref" 'plain (list 'function #'org-roam-capture--get-point)
                                               "%?"
                                               :file-name "${slug}"
                                               :head (concat "#+TITLE: ${title}\n" "#+ROAM_TAGS:\n" "#+ROAM_KEY: ${ref}\n" "* Tags:\n" "- Tag:  \n" "- Tag:  \n" "- Tag:  \n" "* Description: ")
                                               :unnarrowed t))
        org-roam-capture-templates (list (list "d" "default" 'plain (list 'function #'org-roam-capture--get-point)
                                                   "%?"
                                                   :file-name "%<%Y-%m-%d>-${slug}"
                                                   :head (concat "#+TITLE: ${title}\n" "#+ROAM_TAGS:\n" "* Tags:\n" "- Tag:  \n" "- Tag:  \n" "- Tag:  \n" "* Description: \n" "* Related: \n")
                                                   :unnarrowed t)
                                             (list "m" "math" 'plain (list 'function #'org-roam-capture--get-point)
                                                   "%?"
                                                   :file-name "%<%Y-%m-%d>-${slug}"
                                                   :head (concat "#+TITLE: ${title}\n" "#+ROAM_TAGS: math, \n" "* Tags:\n" "- Tag: [[file:2020-04-02-math.org][::math]]\n" "- Tag:  \n" "- Tag:  \n" "* Description: ")
                                                   :unnarrowed t)
                                             (list "w" "work" 'plain (list 'function #'org-roam-capture--get-point)
                                                   "%?"
                                                   :file-name "%<%Y-%m-%d>-${slug}"
                                                   :head (concat "#+TITLE: ${title}\n" "#+ROAM_TAGS:\n" "* Tags:\n" "- Sprint: \n" "- Category: \n" "- Project: \n" "* Description: ")
                                                   :unnarrowed t))
        org-roam-dailies-capture-templates      '(("d" "daily" plain (function org-roam-capture--get-point)
                                                       ""
                                                       :immediate-finish t
                                                       :file-name "%<%Y-%m-%d-%A>"
                                                       :head "#+TITLE: %<%A, %B %d, %Y>"))))
(use-package! org-roam
  :custom
  (org-roam-buffer-prepare-hook            '(hide-mode-line-mode
                                             org-roam-buffer--insert-title
                                             org-roam-buffer--insert-backlinks)) ;;org-roam-buffer--insert-citelinks
  :config
  (+my/org-roam-templates)
  (setq org-roam-directory                      org-directory
        org-roam-index-file                     "./index.org"
        org-roam-tag-sort                       t
        org-roam-verbose                        t
        org-roam-buffer-position                'right
        org-roam-buffer-width                   0.27
        org-roam-graph-max-title-length         40
        org-roam-graph-shorten-titles          'truncate
        org-roam-graph-exclude-matcher          '("old/" "Sunday" "Monday" "Tuesday" "Wednesday" "Thursday" "Friday" "Saturday" "journal")
        org-roam-graph-viewer                   (executable-find (if IS-MAC "open" "firefox"))
        org-roam-graph-executable               "dot"
        org-roam-graph-node-extra-config        '(("shape"      . "underline")
                                                  ("style"      . "rounded,filled")
                                                  ("fillcolor"  . "#EEEEEE")
                                                  ("color"      . "#C9C9C9")
                                                  ("fontcolor"  . "#111111")))

  (remove-hook 'org-roam-buffer-prepare-hook 'org-roam-buffer--insert-citelinks)
  ;; have org-roam-buffer use same display defaults as other org-files
  (add-hook! 'org-roam-buffer-prepare-hook :append (λ!! (org-global-cycle '(4)))))

(use-package! mathpix
  :commands (mathpix-screenshot)
  :config
  (setq mathpix-app-id            "owenpriceskelly_gmail_com_2bbd51"
        mathpix-app-key           "0b3d8ae26f3762b4d5b8"
        mathpix-screenshot-method "screencapture -i %s"))
