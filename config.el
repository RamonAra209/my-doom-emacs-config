(setq doom-theme 'doom-old-hope)

(custom-theme-set-faces! 'doom-old-hope
    (set-face-foreground 'line-number "#708090")
    (set-face-foreground 'line-number-current-line "#ef7c2b")) ;; orange

(beacon-mode 1) ;; never lose that cursor ever again

(require 'evil-snipe)

(use-package! evil-goggles
        :config
        (setq evil-goggles-duration 0.150)
        (setq evil-goggles-enable-change t)
        (setq evil-goggles-enable-delete t)
        (evil-goggles-use-diff-faces)
)

(setq lsp-ui-sideline-enable nil)
(setq lsp-ui-sideline-show-hover t)
(setq lsp-ui-doc-show-with-mouse t)
(setq lsp-ui-doc-enable t)

(setq org-directory '("~/Notes/")) ;; main org directory

(after! org ;; opening segment of org mode section

(map! :map org-mode-map ;; Moving indent blocks with vim keybindings
        :n "M-j" #'org-metadown
        :n "M-k" #'org-metaup)

(setq org-agenda-files (apply 'append
        (mapcar
            (lambda (directory)
                (directory-files-recursively
                directory org-agenda-file-regexp))
        '("~/Notes/" "~/Developer/" "~/Library/Mobile Documents/com~apple~CloudDocs/Documents/org"))))

(require 'org-bullets)
(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))
(use-package! org-fancy-priorities
                :hook (org-mode . org-fancy-priorities-mode)
                :config
                (setq org-fancy-priorities-list '("HIGH" "MEDIUM" "LOW" "☕"))
                    org-todo-keywords '((sequence "HW")))

(setq org-agenda-skip-scheduled-if-done t ;; for setting todo priority colors
        org-priority-faces '((65 :foreground "#FF0000")
                        (66 :foreground "#0098dd")
                        (67 :foreground "#da8548")))

(setq org-capture-templates
      '(
        ("t" "General Todo" entry (file+headline "~/Library/Mobile Documents/com~apple~CloudDocs/Documents/org/gtd.org" "Tasks")
         "* TODO %?\n  %i\n  %a")
        ("j" "Journal" entry (file+datetree "~/Library/Mobile Documents/com~apple~CloudDocs/Documents/org/journal.org")
         "* %?\nEntered on %U\n  %i\n  %a")
        ("w" "Work Todo Entries")
            ("we" "No Time" entry (file "~/Library/Mobile Documents/com~apple~CloudDocs/Documents/org/work.org")
             "** %^{Type|HW|READ|TODO|PROJ} %^{Todo title} %?" :prepend t :empty-lines-before 0
             :refile-targets (("~/Library/Mobile Documents/com~apple~CloudDocs/Documents/org/work.org" :maxlevel . 2)))

            ("ws" "Scheduled" entry (file "~/Library/Mobile Documents/com~apple~CloudDocs/Documents/org/work.org")
             "** %^{Type|HW|READ|TODO|PROJ} %^{Todo title}\nSCHEDULED: %^t%?" :prepend t :empty-lines-before 0
             :refile-targets (("~/Library/Mobile Documents/com~apple~CloudDocs/Documents/org/work.org" :maxlevel . 2)))

            ("wd" "Deadline" entry (file "~/Library/Mobile Documents/com~apple~CloudDocs/Documents/org/work.org")
             "** %^{Type|HW|READ|TODO|PROJ} %^{Todo title}\nDEADLINE: %^t%?" :prepend t :empty-lines-before 0
             :refile-targets (("~/Library/Mobile Documents/com~apple~CloudDocs/Documents/org/work.org" :maxlevel . 2)))

            ("ww" "Scheduled & deadline" entry (file "~/Library/Mobile Documents/com~apple~CloudDocs/Documents/org/work.org")
             "** %^{Type|HW|READ|TODO|PROJ} %^{Todo title}\nSCHEDULED: %^t DEADLINE: %^t %?" :prepend t :empty-lines-before 0
             :refile-targets (("~/Library/Mobile Documents/com~apple~CloudDocs/Documents/org/work.org" :maxlevel . 2)))
            )
)

;; (setq ((org-super-agenda-groups
;;         '(
;;             (:name "Today" :time-grid t :scheduled today)
;;             (:name "Due today" :deadline today)
;;             (:name "Important" :priority "A")
;;             (:name "Overdue" :deadline past)
;;             (:name "Due soon" :deadline future)
;;             (:name "All other priorites" :priority<= "B" :order 1)
;;           )
;;         ))
;;     (org-agenda nil "a")
;; )

(require 'latex-preview-pane)
(latex-preview-pane-enable)

(load-library "ox-reveal")
(setq org-reveal-root "/Users/tahpramen/reveal.js-master")

(use-package! org-auto-tangle
    :defer t
    :hook (org-mode . org-auto-tangle-mode)
    :config
    (setq org-auto-tangle-default t))

(require 'org-tempo)
(add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
(add-to-list 'org-structure-template-alist '("py" . "src python :results output"))

) ;; closing parentheses for org mode section

(use-package! pyvenv
  :diminish
  :config
  (setq pyvenv-mode-line-indicator
        '(pyvenv-virtual-env-name ("[venv:" pyvenv-virtual-env-name "] ")))
  (pyvenv-mode +1))

(use-package! numpydoc
  :ensure t
  :bind (:map python-mode-map
              ("C-c C-n" . numpydoc-generate)))

(yas-global-mode 1)
(add-hook 'yas-minor-mode-hook (lambda () (yas-activate-extra-mode 'fundamental-mode)))

(setq projectile-project-search-path '("~/Developer/" "~/Developer/Personal-Projects/")) ;; add downloads here

;; (add-to-list 'eglot-server-programs '(python-mode . ("pyright")))

(map! :after vterm ;; allows ctrl-c to kill process in vterm
      :map vterm-mode-map
      :ni "C-c" #'vterm-send-C-c)

(map! :leader ;; sets default behavior so that spc-w-v follows new window'
      :desc "Split current window vertically, then focus on new window"
      "w v" #'+evil/window-vsplit-and-follow)

(evilem-default-keybindings "SPC")
(map! :leader :desc "evilmotion find" "f j" #'avy-goto-char-timer)

(after! company ;; enabling tab complete how it should be
  (dolist (key '("<return>" "RET"))
    (define-key company-active-map (kbd key)
      `(menu-item nil company-complete
                  :filter ,(lambda (cmd)
                             (when (company-explicit-action-p)
                              cmd)))))
  ;; (define-key company-active-map (kbd "TAB") #'company-complete-selection)
  (map! :map company-active-map "TAB" #'company-complete-selection)
  (map! :map company-active-map "<tab>" #'company-complete-selection)
  (define-key company-active-map (kbd "SPC") nil)

  (setq company-auto-commit-chars nil)
  )

(setq company-idle-delay 0.05)
