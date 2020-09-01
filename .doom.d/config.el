;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!

;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "John Doe"
      user-mail-address "john@doe.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
;; (setq doom-font (font-spec :family "monospace" :size 12 :weight 'semi-light)
;;       doom-variable-pitch-font (font-spec :family "sans" :size 13))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
;; (setq doom-theme 'doom-one)
;; (setq doom-theme 'doom-vibrant)
(setq doom-theme 'humanoid-dark)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)

(setq confirm-kill-emacs nil)

;; Here are some additional functions/macros that could help you configure Doom:
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
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.

;;  (use-package! xah-fly-keys
;;    :config
;;    (xah-fly-keys-set-layout "qwerty")
;;    (after! counsel
;;      (define-key xah-fly-command-map (kbd "n") 'swiper))
;;    (after! avy
;;      (define-key xah-fly-t-keymap (kbd "i") 'avy-goto-char))
;;    (setq xah-fly-use-meta-key nil)
;;    (xah-fly-keys 1)
;; )

;; (after! projectile
;; (setq doom-leader-alt-key "M-n"))
(after! ivy
  (define-key ivy-minibuffer-map (kbd "TAB") 'ivy-done))
(map! :map emmet-mode-keymap
        "M-j" #'emmet-expand-line)
(map! :map evil-window-map "," #'other-window )
(map! :map evil-window-map "3" #'delete-other-windows)
(map! :map evil-window-map "4" #'split-window-right)
(map! :leader
      (:prefix-map ("o" . "open")
       :desc "Dired"              "d"  #'dired-jump))
(map! :map evil-normal-state-map "u" 'undo-fu-only-undo)
(map! :map evil-normal-state-map "C-r" 'undo-fu-only-redo)
;; (defun xah-unsplit-window-or-next-frame ()
;;   "Unsplit window. If current frame has only one window, switch to next frame.
;; Version 2017-01-29"
;;   (interactive)
;;   (if (one-window-p)
;;       (other-frame 1)
;;     (delete-other-windows)))
;;(use-package! humanoid-themes
;;  :config
;;  (load-theme 'humanoid-dark t))
