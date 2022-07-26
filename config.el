(setq doom-theme 'doom-old-hope)

(custom-theme-set-faces! 'doom-old-hope
    (set-face-foreground 'line-number "#708090")
    (set-face-foreground 'line-number-current-line "#ef7c2b")) ;; orange

(beacon-mode 1) ;; never lose that cursor ever again

;; ;; This is *NECESSARY* for Doom users who enabled `dired' module
;; (map! :map dired-mode-map :ng "q" #'quit-window)

;; (use-package dirvish
;;   :init
;;   (dirvish-override-dired-mode)
;;   :custom
;;   ;; Go back home? Just press `bh'
;;   (dirvish-bookmark-entries
;;    '(("h" "~/"                          "Home")
;;      ("d" "~/Downloads/"                "Downloads")
;;      ("m" "/mnt/"                       "Drives")
;;      ("t" "~/.local/share/Trash/files/" "TrashCan")))
;;   ;; (dirvish-header-line-format '(:left (path) :right (free-space)))
;;   (dirvish-mode-line-format ; it's ok to place string inside
;;    '(:left (sort file-time " " file-size symlink) :right (omit yank index)))
;;   ;; Don't worry, Dirvish is still performant even you enable all these attributes
;;   (dirvish-attributes '(all-the-icons file-size collapse subtree-state vc-state git-msg))
;;   ;; Maybe the icons are too big to your eyes
;;   ;; (dirvish-all-the-icons-height 0.8)
;;   ;; In case you want the details at startup like `dired'
;;   (dirvish-hide-details nil)
;;   :config
;;   (dirvish-peek-mode) ;; Opens a minibuffer on the right side that lets you see contents of file
;;   ;; Dired options are respected except a few exceptions, see *In relation to Dired* section above
;;   (setq dired-dwim-target t)
;;   (setq delete-by-moving-to-trash t)
;;   ;; Enable mouse drag-and-drop files to other applications
;;   (setq dired-mouse-drag-files t)                   ; added in Emacs 29
;;   (setq mouse-drag-and-drop-region-cross-program t) ; added in Emacs 29
;;   ;; Make sure to use the long name of flags when exists
;;   ;; eg. use "--almost-all" instead of "-A"
;;   ;; Otherwise some commands won't work properly
;;   (setq dired-listing-switches
;;         "-l --almost-all --human-readable --time-style=long-iso --group-directories-first --no-group")
;;   :bind
;;   ;; Bind `dirvish|dirvish-side|dirvish-dwim' as you see fit
;;   (
;;    ("C-c f" . dirvish-dwim)
;;    :map dired-mode-map ; Dirvish respects all the keybindings in this map
;;    ("h" . dired-up-directory)
;;    ;; ("j" . dired-next-line)
;;    ;; ("k" . dired-previous-line)
;;    ("l" . dired-find-file)
;;    ;; ("i" . wdired-change-to-wdired-mode)
;;    ;; ("." . dired-omit-mode)
;;    ("b"   . dirvish-dwim-bookmark-jump)
;;    ("f"   . dirvish-dwim-file-info-menu)
;;    ("y"   . dirvish-dwim-yank-menu)
;;    ("N"   . dirvish-dwim-narrow)
;;    ("^"   . dirvish-dwim-history-last)
;;    ("s"   . dirvish-dwim-quicksort) ; remapped `dired-sort-toggle-or-edit'
;;    ("?"   . dirvish-dwim-dispatch)  ; remapped `dired-summary'
;;    ("TAB" . dirvish-dwim-subtree-toggle)
;;    ("SPC" . dirvish-dwim-history-jump)
;;    ("M-n" . dirvish-dwim-history-go-forward)
;;    ("M-p" . dirvish-dwim-history-go-backward)
;;    ("M-l" . dirvish-dwim-ls-switches-menu)
;;    ("M-m" . dirvish-dwim-mark-menu)
;;    ("M-f" . dirvish-dwim-toggle-fullscreen)
;;    ("M-s" . dirvish-dwim-setup-menu)
;;    ("M-e" . dirvish-dwim-emerge-menu)
;;    ("M-j" . dirvish-dwim-fd-jump)))

(require 'evil-snipe)

(use-package evil-goggles
        :config
        (setq evil-goggles-duration 0.150)
        (setq evil-goggles-enable-change t)
        (setq evil-goggles-enable-delete t)
        (evil-goggles-use-diff-faces)
)

(setq lsp-ui-sideline-enable t)
(setq lsp-ui-sideline-show-hover t)
(setq lsp-ui-doc-show-with-mouse t)
(setq lsp-ui-doc-enable t)

(setq org-directory '("~/Notes/")) ;; main org directory

(after! org ;; opening segment of org mode section

(map! :map org-mode-map ;; Moving indent blocks with vim keybindings
        :n "M-j" #'org-metadown
        :n "M-k" #'org-metaup)

(setq org-agenda-files (directory-files-recursively "~/Notes/" "\\.org$")) ;; dirs to search for TODOs

(require 'org-bullets)
(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))
(use-package! org-fancy-priorities
                :hook (org-mode . org-fancy-priorities-mode)
                :config
                (setq org-fancy-priorities-list '("HIGH" "MEDIUM" "LOW" "☕")))

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

) ;; closing parentheses for org mode section

(use-package pyvenv
  :diminish
  :config
  (setq pyvenv-mode-line-indicator
        '(pyvenv-virtual-env-name ("[venv:" pyvenv-virtual-env-name "] ")))
  (pyvenv-mode +1))

(use-package numpydoc
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

(setq company-idle-delay 0.05)
