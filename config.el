(setq doom-theme 'doom-old-hope)

(custom-theme-set-faces! 'doom-old-hope
    (set-face-foreground 'line-number "#708090")
    (set-face-foreground 'line-number-current-line "#ef7c2b"))

(require 'evil-snipe)

(use-package evil-goggles
        :config
        (setq evil-goggles-duration 0.150)
        (setq evil-goggles-enable-change t)
        (setq evil-goggles-enable-delete t)
        (evil-goggles-use-diff-faces)
)

(setq lsp-ui-sideline-show-hover t)
(setq lsp-ui-doc-show-with-mouse t)

(setq org-directory '("~/Notes/")) ;; main org directory

(after! org ;; opening segment of org mode section

(map! :map org-mode-map ;; Moving indent blocks with vim keybindings
        :n "M-j" #'org-metadown
        :n "M-k" #'org-metaup)

(setq org-agenda-files (directory-files-recursively "~/Notes/" "\\.org$")) ;; dirs to search for TODOs

(require 'org-bullets)
(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))

(setq org-agenda-skip-scheduled-if-done t ;; for setting todo priority colors
        org-priority-faces '((65 :foreground "#FF0000")
                        (66 :foreground "#0098dd")
                        (67 :foreground "#da8548")))

(require 'latex-preview-pane)
(latex-preview-pane-enable)

(use-package! org-auto-tangle
    :defer t
    :hook (org-mode . org-auto-tangle-mode)
    :config
    (setq org-auto-tangle-default t))

(require 'org-tempo)
(add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
(add-to-list 'org-structure-template-alist '("py" . "src python :results output"))

(require 'org-download)
(add-hook 'dired-mode-hook 'org-download-enable)
(use-package org-download
  :ensure t
  :defer t
  :init
  ;; Add handlers for drag-and-drop when Org is loaded.
    (with-eval-after-load 'org
      (org-download-enable)))

) ;; closing parentheses for org mode section

(use-package pyvenv
  :diminish
  :config
  (setq pyvenv-mode-line-indicator
        '(pyvenv-virtual-env-name ("[venv:" pyvenv-virtual-env-name "] ")))
  (pyvenv-mode +1))

(yas-global-mode 1)
(add-hook 'yas-minor-mode-hook (lambda () (yas-activate-extra-mode 'fundamental-mode)))

(setq projectile-project-search-path '("~/Developer/" "~/Developer/Personal-Projects/")) ;; add downloads here

(after! tramp ;; never using vim on my Pi ever again
    (setq tramp-inline-compress-start-size 1000)
    (setq tramp-copy-size-limit 10000)
    (setq vc-handled-backends '(Git))
    (setq tramp-verbose 10)
    (setq tramp-default-method "scp")
    (setq tramp-use-ssh-controlmaster-options nil)
    (setq projectile--mode-line "Projectile")
    (setq tramp-verbose 10))

(map! :after vterm ;; allows ctrl-c to kill process in vterm
      :map vterm-mode-map
      :ni "C-c" #'vterm-send-C-c)

(map! :leader ;; sets default behavior so that spc-w-v follows new window'
      :desc "Split current window vertically, then focus on new window"
      "w v" #'+evil/window-vsplit-and-follow)

(setq company-idle-delay 0.2)
