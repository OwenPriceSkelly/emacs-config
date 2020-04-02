;;; personal/config/+notes.el -*- lexical-binding: t; -*-
;;; Configure org and org-roam for notes

(use-package! org
  :init
  (setq org-directory                   (if IS-MAC "~/.org/" "~/.org.d/"))
  :config
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

;;TODO
(defun toggle-exclude-journals ())

(use-package! org-roam
  :defer-incrementally org
  :commands
  (org-roam org-roam-find-file
            org-roam-insert
            org-roam-graph-show
            org-roam-today
            org-roam-tomorrow
            org-roam-yesterday
            org-roam-date)
  :config
  (setq org-roam-directory              org-directory
        org-roam-db-location            (concat org-roam-directory "org-roam.db")
        org-roam-buffer-width           0.30
        org-roam-completion-fuzzy-match t
        org-roam-date-filename-format   "%Y-%m-%d-%A"   ; YYYY-mm-dd-Weekday
        org-roam-date-title-format      "%A, %B %d, %Y" ; Weekday, Month dd, YYYY
        org-roam-graph-max-title-length 40
        org-roam-buffer-position        'right
        org-roam-graph-viewer           (if IS-MAC "open" "firefox")
        org-roam-graph-exclude-matcher  (list "old/" "Sunday" "Monday" "Tuesday" "Wednesday" "Thursday" "Friday" "Saturday" "journal")
        org-roam-capture-templates      (list (list "d" "default" 'entry (list 'function #'org-roam-capture--get-point)
                                                    "%?"
                                                    :file-name "%<%Y-%m-%d>-${slug}"
                                                    :head "#+TITLE: ${title}\n* Tags:\n** From: [[file:%F][${captured-from}]]\n** Category: \n* Description: "
                                                    :unnarrowed t))
        org-capture-templates           org-roam-capture-templates)
  (org-roam-mode +1))

(use-package! org-roam-protocol
  :after org-protocol)

(use-package! company-org-roam
  :when (featurep! :completion company)
  :after org-roam
  :config
  (set-company-backend! 'org-mode '(company-org-roam company-yasnippet company-dabbrev)))
