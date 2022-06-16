;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets. It is optional.
(setq user-full-name "Ramon Arambula"
      user-mail-address "r_arambula@u.pacific.edu")

;; Doom exposes five (optional) variables for controlling fonts in Doom:
;;
;; - `doom-font' -- the primary font to use
;; - `doom-variable-pitch-font' -- a non-monospace font (where applicable)
;; - `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;; - `doom-unicode-font' -- for unicode glyphs
;; - `doom-serif-font' -- for the `fixed-pitch-serif' face
;;
;; See 'C-h v doom-font' for documentation and more examples of what they
;; accept. For example:
;;
;;(setq doom-font (font-spec :family "Fira Code" :size 12 :weight 'semi-light)
;;      doom-variable-pitch-font (font-spec :family "Fira Sans" :size 13))
;;
;; If you or Emacs can't find your font, use 'M-x describe-font' to look them
;; up, `M-x eval-region' to execute elisp code, and 'M-x doom/reload-font' to
;; refresh your font settings. If Emacs still can't find your font, it likely
;; wasn't installed correctly. Font issues are rarely Doom issues!

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-old-hope)

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")


;; Whenever you reconfigure a package, make sure to wrap your config in an
;; `after!' block, otherwise Doom's defaults may override your settings. E.g.
;;
;;   (after! PACKAGE
;;     (setq x y))
;;
;; The exceptions to this rule:
;;
;;   - Setting file/directory variables (like `org-directory')
;;   - Setting variables which explicitly tell you to set them before their
;;     package is loaded (see 'C-h v VARIABLE' to look up their documentation).
;;   - Setting doom variables (which start with 'doom-' or '+').
;;
;; Here are some additional functions/macros that will help you configure Doom.
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;; Alternatively, use `C-h o' to look up a symbol (functions, variables, faces,
;; etc).
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.

;; EVIL
(require 'evil-snipe)
(setq evil-snipe-scope 'visible)
(use-package evil-goggles
        :config
        (setq evil-goggles-duration 0.150)
        (setq evil-goggles-enable-change t)
        (setq evil-goggles-enable-delete t)
        (evil-goggles-use-diff-faces)
)

(setq projectile-project-search-path '("~/Developer/" "~/Developer/Personal-Projects/"))

(after! tramp
    (setq tramp-inline-compress-start-size 1000)
    (setq tramp-copy-size-limit 10000)
    (setq vc-handled-backends '(Git))
    (setq tramp-verbose 10)
    (setq tramp-default-method "scp")
    (setq tramp-use-ssh-controlmaster-options nil)
    (setq projectile--mode-line "Projectile")
    (setq tramp-verbose 10))

;; LSP-UI
(use-package lsp-ui
  :config
  (setq lsp-ui-sideline-show-hover t)
  (setq lsp-ui-doc-show-with-mouse t))

;; (setq doom-modeline-github t ;; Github notifications
;;       ;; need to set interval here: https://github.com/magit/ghub
;;       ;; requires ghub package
;;       )

(after! org
        (map! :map org-mode-map
                :n "M-j" #'org-metadown
                :n "M-k" #'org-metaup)

        ;; (setq org-startup-with-latex-preview t)
        (setq org-agenda-skip-scheduled-if-done t
                org-priority-faces '((65 :foreground "#FF0000")
                                (66 :foreground "#0098dd")
                                (67 :foreground "#da8548"))
        )

        (setq org-hide-emphasis-markers t)
        (setq org-directory '("~/Notes/"))
        (setq org-agenda-files (directory-files-recursively "~/Notes/" "\\.org$"))

        (require 'org-bullets)
        (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))
        (use-package! org-super-agenda
                :after org-agenda
                :init
                (setq org-super-agenda-groups '((:name "Today" :time-grid t :scheduled today)
                                                (:name "Due today" :deadline today)
                                                (:name "Important" :priority "A")
                                                (:name "Overdue" :deadline past)
                                                (:name "Due soon" :deadline future)
                                                (:name "Big Outcomes" :tag "bo")))
                :config
                (org-super-agenda-mode)
        )

        (use-package! org-fancy-priorities
                :hook (org-mode . org-fancy-priorities-mode)
                :config
                (setq org-fancy-priorities-list '("HIGH" "MEDIUM" "LOW" "☕")))

        ;; latex
        (require 'latex-preview-pane)
        (latex-preview-pane-enable)
 )

(map! :after vterm
      :map vterm-mode-map
      :ni "C-c" #'vterm-send-C-c)
