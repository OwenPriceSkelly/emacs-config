;;; personal/config/+notes.el -*- lexical-binding: t; -*-
;;; Configure org and org-roam for notes

(use-package! org
  :init
  (setq org-directory                   (if IS-MAC "~/.org/" "~/.org.d/"))
  (add-hook! (org-mode) #'(+org-pretty-mode  variable-pitch-mode))
  (add-hook! org-mode :local (setq display-line-numbers 0))
  :config
  (sp-local-pair '(org-mode) "$" "$") ; For inline latex stuff
  (setq org-startup-folded              'content
        ;; org-preview-latex-default-process 'dvipng
        org-startup-with-latex-preview t

        org-preview-latex-default-process 'dvisvgm
        ;; org-preview-latex-default-process 'imagemagick
        org-format-latex-options '(:foreground default :background default :scale 1.0
                                               :html-foreground "Black" :html-background "Transparent"
                                               :html-scale 1.0 :matchers ("begin" "$1" "$" "$$" "\\(" "\\["))
        org-agenda-files (list org-directory)
        org-bullets-bullet-list '( "▶" "◉" "▸" "○" "✸" "•" "★")
        org-startup-folded 'content
        org-todo-keywords '((sequence "[ ](t/!)"     ; A subtask
                                      "[~](p)"     ; Subtask currently in-progress
                                      "[*](w@)"    ; Subtask is being held up or paused
                                      "|"
                                      "[X](d@)"    ; Subtask was completed ; ; ;
                                      "[-](k@)")   ; Subtask was dropped
                            (sequence "TODO(T/!)"    ; A task that needs doing & is ready to do
                                      "PROG(P)"    ; Mark a task as in-progress
                                      "WAIT(W@)"   ; Something is holding up this task or it is paused
                                      "|"
                                      "DONE(D@)"   ; Task successfully completed
                                      "DROP(K@)")) ; Task was cancelled or is no longer applicable
        org-todo-keyword-faces '(("[~]"   . +org-todo-active)
                                 ("[*]"   . +org-todo-onhold)
                                 ("PROG"  . +org-todo-active)
                                 ("WAIT"  . +org-todo-onhold))))

(defun +toggle-exclude-journals ()
  (setq org-roam-graph-exclude-matcher
        (if (eq org-roam-graph-exclude-matcher "old/")
            (list "old/" "Sunday" "Monday" "Tuesday" "Wednesday" "Thursday" "Friday" "Saturday" "journal")
          "old/")))

(setq +my/default-roam-header (concat "#+TITLE: ${title}\n"
                                      "* Tags:\n"
                                      "** Tag:  \n"
                                      "** Tag:  \n"
                                      "** Tag:  \n"
                                      "* Description: \n"
                                      "* Related: \n")
      +my/math-roam-header (concat "#+TITLE: ${title}\n"
                                   "* Tags:\n"
                                   "** Tag: [[file:2020-04-02-math.org][::math]]\n"
                                   "** Tag:  \n"
                                   "** Tag:  \n"
                                   "* Description: \n"
                                   "* Related: \n")
      +my/org-roam-capture-templates (if IS-MAC (list (list "d" "default" 'entry (list 'function #'org-roam-capture--get-point)
                                                            "%?"
                                                            :file-name "%<%Y-%m-%d>-${slug}"
                                                            :head +my/default-roam-header
                                                            :unnarrowed t)
                                                      (list "m" "math" 'entry (list 'function #'org-roam-capture--get-point)
                                                            "%?"
                                                            :file-name "%<%Y-%m-%d>-${slug}"
                                                            :head +my/math-roam-header
                                                            :unnarrowed t))
                                       (list (list "d" "default" 'entry (list 'function #'org-roam-capture--get-point)
                                                   "%?"
                                                   :file-name "%<%Y-%m-%d>-${slug}"
                                                   :head +my/default-roam-header
                                                   :unnarrowed t))))


(use-package! org-roam
  :commands
  (org-roam org-roam-find-file
            org-roam-buffer
            org-roam-insert
            org-roam-graph-show
            org-roam-today
            org-roam-tomorrow
            org-roam-yesterday
            org-roam-date)
  :config
  (setq org-roam-directory              org-directory
        org-roam-db-location            (concat org-roam-directory "org-roam.db")
        org-roam-buffer-width           0.3
        org-roam-completion-system      'ivy
        org-roam-completion-fuzzy-match t
        org-roam-date-filename-format   "%Y-%m-%d-%A"   ; YYYY-mm-dd-Weekday
        org-roam-date-title-format      "%A, %B %d, %Y" ; Weekday, Month dd, YYYY
        org-roam-graph-max-title-length 40
        org-roam-buffer-position        'right
        org-roam-graph-viewer           (if IS-MAC "open" "firefox")
        org-roam-graph-exclude-matcher  (list "old/" "Sunday" "Monday" "Tuesday" "Wednesday" "Thursday" "Friday" "Saturday" "journal")
        org-roam-capture-templates      +my/org-roam-capture-templates

        org-capture-templates           org-roam-capture-templates)
  (org-roam-mode +1))

(use-package! org-roam-protocol
  :after org-protocol)

(use-package! company-org-roam
  :when (featurep! :completion company)
  :after org-roam
  :config
  (set-company-backend! 'org-mode '(company-org-roam company-yasnippet company-dabbrev)))

(use-package! mathpix
  :commands (mathpix-screenshot)
  :config
  (setq mathpix-app-id            "owenpriceskelly_gmail_com_2bbd51"
        mathpix-app-key           "0b3d8ae26f3762b4d5b8"
        mathpix-screenshot-method "screencapture -i %s"))
