(setq gc-cons-threshold 100000000)
(add-hook 'after-init-hook (lambda () (setq gc-cons-threshold 800000)))
;;(load "~/.emacs.d/lisp/my-abbrev.el")

(require 'package)
(setq package-enable-at-startup nil)

;; PACKAGES 
(setq package-list '(ivy swiper flycheck-irony flycheck flycheck-pos-tip dumb-jump company company-irony irony powerline yasnippet yasnippet-snippets flycheck-inline web-mode xah-fly-keys tide emmet-mode smart-mode-line smart-mode-line-powerline-theme js2-mode rjsx-mode use-package xah-find color-theme tangotango-theme org-bullets lsp-mode company-lsp alect-themes hemisu-theme ample-theme spacemacs-theme smart-mode-line))
;; MELPA
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))
  ;;   (when no-ssl
  ;;     (warn "\
  ;; Your version of Emacs does not support SSL connections,
  ;; which is unsafe because it allows man-in-the-middle attacks.
  ;; There are two things you can do about this warning:
  ;; 1. Install an Emacs version that does support SSL and be safe.
  ;; 2. Remove this warning from your init file so you won't see it again."))
  ;; Comment/uncomment these two lines to enable/disable MELPA and MELPA Stable as desired
  (add-to-list 'package-archives (cons "melpa" (concat proto "://melpa.org/packages/")) t)
  ;;(add-to-list 'package-archives (cons "melpa-stable" (concat proto "://stable.melpa.org/packages/")) t)
  (when (< emacs-major-version 24)
    ;; For important compatibility libraries like cl-lib
    (add-to-list 'package-archives (cons "gnu" (concat proto "://elpa.gnu.org/packages/")))))

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(unless package-archive-contents
  (package-refresh-contents))
;; install the missing packages
(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))

(eval-when-compile
  (require 'use-package))

;; Determine OS and setup
(setq p/linux (featurep 'x))
(setq p/win32 (not p/linux))

;; Setup
(defun petri-general-settings ()
  (setq column-number-mode t)
  (setq mode-line-position '((line-number-mode ("%l " (column-number-mode ": %c")))))
  (setq-default frame-title-format "%f")
  (when (version<= "26.0.50" emacs-version )
    (global-display-line-numbers-mode))
  ;; Bigger undo
  (setq undo-limit 20000000)
  (setq undo-strong-limit 40000000)
  ;;disable backup
  (setq backup-inhibited t)
  ;;disable auto save
  (setq auto-save-default nil)
  ;; disable lockfiles
  (setq create-lockfiles nil)
  ;; Disable mousewheel
  (mouse-wheel-mode -1)
  (global-set-key [wheel-up] 'ignore)
  (global-set-key [wheel-down] 'ignore)
  (global-set-key [double-wheel-up] 'ignore)
  (global-set-key [double-wheel-down] 'ignore)
  (global-set-key [triple-wheel-up] 'ignore)
  (global-set-key [triple-wheel-down] 'ignore)
  (set-face-attribute 'default nil :font "Dejavu Sans Mono 11" )
  (set-frame-font "Dejavu Sans Mono 11" nil t)
  (menu-bar-mode -1)
  (tool-bar-mode -1)
  (show-paren-mode 1)
  ;; (set-foreground-color "white")
  ;; (set-background-color "#1E1E1E")
  ;; (setq split-window-preferred-function nil)
  (global-hl-line-mode 1)
  ;; (set-cursor-color "red")
  ;; (set-face-background 'hl-line "midnight blue")
  (setq ring-bell-function 'ignore)
  (blink-cursor-mode 0)
  ;; (require 'vc)
  (eval-after-load "vc" '(remove-hook 'find-file-hook 'vc-find-file-hook))
  (setq vc-handled-backends nil)
  ;; Bookmarks
  (setq bookmark-save-flag 1) ; everytime bookmark is changed, automatically save it
  (setq bookmark-save-flag t) ; save bookmark when emacs quits
  ;; Startup
  (setq inhibit-splash-screen t)
  ;; (require 'bookmark)
  ;; (switch-to-buffer "*scratch*")
  ;; (split-window-right)
  (bookmark-bmenu-list)
  (switch-to-buffer "*Bookmark List*")
  ;; (toggle-frame-maximized)
  ;; Defalias
  (defalias 'list-buffers 'ibuffer)
  ;; (defalias 'isearch-forward 'swiper)
  ;; (defalias 'query-replace 'replace-string)
  ;; (defalias 'xah-insert-date 'petri-insert-date)
  ;; Dired
  ;; Auto refresh buffers
  (global-auto-revert-mode 1)
  ;; Also auto refresh dired, but be quiet about it
  (setq global-auto-revert-non-file-buffers t)
  (setq auto-revert-verbose nil)
)

(use-package color-theme
  :ensure t
  :config
  (setq color-theme-is-global t)
  (color-theme-initialize)
  ;; (load-theme 'tangotango t)
  ;; (load-theme 'light-blue t)
  ;; (load-theme 'hemisu-dark t)
  (load-theme 'spacemacs-dark t)
  ;; (load-theme 'ample-light t)
  ;; (load-theme 'alect-light t)
)

(use-package org-bullets
  :ensure t
  :init
  (setq org-bullets-bullet-list
	'("◉" "◎" "<img draggable="false" class="emoji" alt="⚫" src="https://s0.wp.com/wp-content/mu-plugins/wpcom-smileys/twemoji/2/svg/26ab.svg">" "○" "►" "◇"))
  :config
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

(use-package xah-fly-keys
  :ensure t
  :config
  (xah-fly-keys-set-layout "qwerty") ; required
  ;; possible layout values:
  ;; "azerty"
  ;; "azerty-be"
  ;; "colemak"
  ;; "colemak-mod-dh"
  ;; "dvorak"
  ;; "programer-dvorak"
  ;; "qwerty"
  ;; "qwerty-abnt"
  ;; "qwertz"
  ;; "workman"
  (xah-fly-keys 1)
)

;; Custom keys
(defun petri-keybind-hook ()
  ;;(define-key global-map "\C-r" 'kill-region)
  (define-key global-map [C-tab] 'indent-region)
  (global-set-key (kbd "M-s") 'save-buffer)
  (global-set-key (kbd "M-q") 'kill-this-buffer)
  (global-set-key (kbd "C-q") 'kill-region)
  (global-set-key (kbd "M-r") 'query-replace)
  (global-set-key (kbd "C-x C-r") 'eval-region)
  (global-set-key (kbd "M-;") 'comment-eclipse)
  (global-set-key (kbd "C-;") 'toggle-comment-on-line)

  (global-set-key (kbd "M-u") 'mark-word)

  (global-set-key (kbd "C-+") 'text-scale-increase)
  (global-set-key (kbd "C--") 'text-scale-decrease)

  ;; easy keys for split windows
  (global-set-key (kbd "M-3") 'delete-other-windows)
  (global-set-key (kbd "M-4") 'split-window-below)
  (global-set-key (kbd "M-5") 'split-window-right)
  (global-set-key (kbd "M-RET") 'other-window)
  (global-set-key (kbd "M-0") 'delete-window)

  (global-set-key (kbd "C-x C-y") 'yas-insert-snippet)

  ;; Find files/Switch buffer
  (define-key global-map "\ef" 'find-file)
  (define-key global-map "\eF" 'petri-find-file-other-window)

  (define-key global-map "\eb" 'ivy-switch-buffer)
  (define-key global-map "\eB" 'petri-switch-buffer-other-window)

  (global-set-key (kbd "<C-up>") 'xah-backward-block)
  (global-set-key (kbd "<C-down>") 'xah-forward-block)

  (global-set-key (kbd "C-a") 'xah-beginning-of-line-or-block)
  (global-set-key (kbd "C-e") 'xah-end-of-line-or-block)
)

(defun petri-conf-mode-hook ()
  (setq c-basic-offset 2)
  ;;(setq-default tab-width 2)
)

;; Init hook
;; (add-hook 'after-init-hook #'my-package-init)
(add-hook 'after-init-hook #'petri-general-settings)
(add-hook 'after-init-hook #'petri-keybind-hook)

(add-hook 'conf-mode-hook #'petri-conf-mode-hook)

;; Custom functions

(defun petri-insert-date ()
  "Version of xah-insert-date function that alwys uses format 4
   2019-03-22 16:22:32+02:00"
  (interactive)
  (when (use-region-p) (delete-region (region-beginning) (region-end)))
  (insert (concat
           (format-time-string "%Y-%m-%d %T")
           (funcall (lambda ($x) (format "%s:%s" (substring $x 0 3) (substring $x 3 5))) (format-time-string "%z"))))
)

(defun what-face (pos)
    (interactive "d")
        (let ((face (or (get-char-property (point) 'read-face-name)
            (get-char-property (point) 'face))))
	  (if face (message "Face: %s" face) (message "No face at %d" pos))))

(defun petri-find-file-other-window ()
  "Find the file in other window and switch back"
  (interactive)
  (let (filename)
    (setq filename (read-file-name "Enter filename: "))
    (find-file-other-window filename)
    (other-window -1))
  )

(defun petri-switch-buffer-other-window ()
  "Find the file in other window and switch back"
  (interactive)
  (ivy-switch-buffer-other-window)
  (other-window -1)
  )

(defun place-brace ()
  (if (eq current-coding-style 'default) " {" "\n{"))

(defun comment-eclipse ()
      (interactive)
      (let ((start (line-beginning-position))
            (end (line-end-position)))
        (when (or (not transient-mark-mode) (region-active-p))
          (setq start (save-excursion
                        (goto-char (region-beginning))
                        (beginning-of-line)
                        (point))
                end (save-excursion
                      (goto-char (region-end))
                      (end-of-line)
                      (point))))
        (comment-or-uncomment-region start end)))

(defun toggle-comment-on-line ()
  "comment or uncomment current line"
  (interactive)
  (comment-or-uncomment-region (line-beginning-position) (line-end-position)))

;; Flycheck

;; use local eslint from node_modules before global
;; http://emacs.stackexchange.com/questions/21205/flycheck-with-file-relative-eslint-executable
(defun my/use-eslint-from-node-modules ()
  (let* ((root (locate-dominating-file
                (or (buffer-file-name) default-directory)
                "node_modules"))
         (eslint (and root
                      (expand-file-name "node_modules/eslint/bin/eslint.js"
                                        root))))
    (when (and eslint (file-executable-p eslint))
      (setq-local flycheck-javascript-eslint-executable eslint))))

(use-package flycheck
  :ensure t
  :config
  (setq flycheck-indication-mode nil)
  ;; (setq flycheck--idle-trigger-timer 0.9)
  (setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc emacs-lisp))
  (setq-default flycheck-disabled-checkers
		(append flycheck-disabled-checkers
			'(javascript-jshint)))
  ;; disable jshint since we prefer eslint checking
  
  ;; use eslint with web-mode for jsx files
  ;; (flycheck-add-mode 'javascript-eslint 'web-mode)
  ;; customize flycheck temp file prefix
  (setq-default flycheck-temp-prefix ".flycheck")
  ;; disable json-jsonlist checking for json files
  (add-hook 'after-init-hook #'global-flycheck-mode)
  (set-face-attribute 'flycheck-error nil :background "dark red" :foreground "white" :underline nil :weight 'bold)
  (set-face-attribute 'flycheck-info nil :background "forest green" :foreground "burlywood3" :underline nil :weight 'bold)
  (set-face-attribute 'flycheck-warning nil :background "gold" :foreground "black" :underline nil :weight 'bold)
)

(use-package flycheck-pos-tip
  :ensure t
  :config
  (flycheck-pos-tip-mode 1)
  (setq pos-tip-background-color "red")
  (setq pos-tip-foreground-color "white") 
)

(use-package flycheck-irony
  :ensure t
)

;; IVY
(use-package ivy
  :ensure t
  :init
  (setq ivy-use-virtual-buffers t)
  (setq ivy-count-format "(%d/%d) ")
  :config
  (ivy-mode 1)
)


;; DUMB-JUMP
(use-package dumb-jump
  :ensure t
  :config
  (dumb-jump-mode 1)
)
;;_Smart-Mode-Line
;; (use-package smart-mode-line
  ;; :ensure t
  ;; :config
  ;; (setq sml/no-confirm-load-theme t)
  ;; (setq sml/theme 'dark)
  ;; (sml/setup)
;; )

;; POWERLINE
;; (use-package powerline
 ;; :ensure t
 ;; :init
 ;; (powerline-default-theme)
 ;; (setq powerline-default-separator 'curve)
 ;; :config
 ;; (set-face-attribute 'mode-line-buffer-id nil :underline nil :overline nil)
 ;; (set-face-attribute 'powerline-active0 nil :background "#FF8C00" :foreground "#383838")
 ;; (set-face-attribute 'powerline-active1 nil :background "#383838" :foreground "#FFFFFF")
 ;; (set-face-attribute 'powerline-active2 nil :background "#666666" :foreground "#FFFFFF")
;; )

;; These two lines are just examples
;; (setq powerline-arrow-shape 'curve)
;; (setq powerline-default-separator-dir '(right . left))
;; (setq sml/shorten-directory t)
;; (setq sml/shorten-modes t)
;; (setq sml/name-width t)
;; (setq sml/mode-width t)
;; These two lines you really need.
;; (setq sml/theme 'powerline)
;; (setq sml/no-confirm-load-theme t)
;; (sml/setup)


;; Yasnippet
(use-package yasnippet
  :ensure t
  :config
  (setq yas-wrap-around-region t)
  (yas-global-mode 1)
)

;; Default for coding style
(setq current-coding-style 'default)

; C++ indentation style
(defconst my-c-indentation
  '((c-electric-pound-behavior   . nil)
    ;; (c-tab-always-indent         . t)
    (c-comment-only-line-offset  . 0)
    (c-hanging-braces-alist      . ((class-open)
                                    (class-close)
                                    (defun-open)
                                    (defun-close)
                                    (inline-open)
                                    (inline-close)
				    (brace-entry-open)
                                    (brace-list-open)
                                    (brace-list-close)
                                    (brace-list-intro)
                                    (brace-list-entry)
                                    (block-open)
                                    (block-close)
                                    (substatement-open)
                                    (statement-case-open)
		                    (class-open)))
    (c-hanging-colons-alist      . ((inher-intro)
                                    (case-label)
                                    (label)
                                    (access-label)
                                    (access-key)
                                    (member-init-intro)))
    (c-cleanup-list              . (scope-operator
                                    list-close-comma
                                    defun-close-semi))
    (c-offsets-alist             . ((arglist-close         .  0);c-lineup-arglist)
				    (statement-cont        .  0)
                                    (label                 . -4)
                                    (access-label          . -4)
                                    (substatement-open     .  0)
                                    (statement-case-intro  .  4)
                                    (case-label            .  4)
                                    (block-open            .  0)
                                    (inline-open           .  0)
                                    (topmost-intro-cont    .  0)
                                    (knr-argdecl-intro     . -4)
                                    (brace-list-open       .  0)
                                    (brace-list-intro      .  4)))
    (c-echo-syntactic-information-p . t))
  "asd")


;; CC++ mode handling
(defun petri-c-hook ()
  ;; Set my style for the current buffer
  (c-add-style "Petri" my-c-indentation t)
  ;; brace indent style for yasnippets
  (setq current-coding-style 'c-style)
  
  ;; 4-space tabs
  (setq tab-width 4)
  (setq indent-tabs-mode nil)

  ;; Additional style stuff
  (c-set-offset 'member-init-intro '++)

  ;; No hungry backspace
  (c-toggle-auto-hungry-state nil)

  ;; Newline indents, semi-colon doesn't
  (define-key c++-mode-map "\C-m" 'newline-and-indent)
  (setq c-hanging-semi&comma-criteria '((lambda () 'stop)))
  
  ;; (add-to-list 'company-backends 'company-irony)
  (add-to-list (make-local-variable 'company-backends) 'company-irony)

  ;; Handle super-tabbify (TAB completes, shift-TAB actually tabs)
  (setq dabbrev-case-replace t)
  (setq dabbrev-case-fold-search t)
  (setq dabbrev-upcase-means-case-search t)

  ;; Abbrevation expansion
  ;; (abbrev-mode 1)
)

(add-hook 'c-mode-hook 'petri-c-hook)

;; LSP
(use-package lsp-mode
  :ensure t
  :commands lsp
  :config
  (setq lsp-prefer-flymake nil)
  ;; (add-hook 'js-mode-hook #'lsp)
)

;; (use-package lsp-ui
  ;; :ensure t
  ;; :commands lsp-ui-mode
;; )

(use-package company-lsp
  :ensure t
  :commands company-lsp
  :config
  ;; (push 'company-lsp company-backends)
)

;; Company-mode
(use-package company
  :ensure t
  :init
  (setq company-dabbrev-downcase 0)
  (setq company-idle-delay 0.1)
  (setq company-minimum-prefix-length 2)
  (setq company-dabbrev-downcase nil)
  (setq company-dabbrev-other-buffers t)
  (setq company-auto-complete nil)
  (setq company-dabbrev-code-other-buffers 'all)
  (setq company-dabbrev-code-everywhere t)
  (setq company-dabbrev-code-ignore-case t)
  (setq company-tooltip-align-annotations t)
  :config
  (add-hook 'after-init-hook 'global-company-mode)
  (set-face-attribute 'company-tooltip nil :background "#4d4d4d" :foreground "white")
  (set-face-attribute 'company-scrollbar-bg nil :background "#4d4d4d")
  (set-face-attribute 'company-scrollbar-fg nil :background "#737373")
  (set-face-attribute 'company-tooltip-annotation nil :foreground "black" :slant 'italic' :height 1.1)
  (set-face-attribute 'company-tooltip-common nil :foreground "gold" :weight 'bold' :height 1.2)
  (set-face-attribute 'company-tooltip-selection nil :background "#8080ff")
) 

;; IRONY
(use-package irony
  :ensure t
  :config
    ;; (unless (irony--find-server-executable)
    ;; (with-minibuffer-input
  	;; (call-interactively #'irony-install-server) ""))
  (add-hook 'c++-mode-hook 'irony-mode)
  (add-hook 'c-mode-hook 'irony-mode)
  (add-hook 'objc-mode-hook 'irony-mode)
  (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)
  )

;; WEB-MODE
(use-package web-mode
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("\\.erb\\'"    . web-mode))       ;; ERB
  (add-to-list 'auto-mode-alist '("\\.json\\'"   . web-mode))       ;; JSON
  (add-to-list 'auto-mode-alist '("\\.html?\\'"  . web-mode))       ;; Plain HTML
  ;; (add-to-list 'auto-mode-alist '("\\.js[x]?\\'" . web-mode))       ;; JS + JSX
  (add-to-list 'auto-mode-alist '("\\.es6\\'"    . web-mode))       ;; ES6
  (add-to-list 'auto-mode-alist '("\\.php\\'"   . web-mode))        ;; PHP
  (add-to-list 'auto-mode-alist '("\\.blade\\.php\\'" . web-mode))  ;; Blade template
  (add-hook 'web-mode-hook 'my-web-mode-hook)
  ;; (add-hook 'web-mode-hook #'setup-tide-mode)
)

(use-package rjsx-mode
  :ensure t
  :config
  (with-eval-after-load 'rjsx-mode
    (define-key rjsx-mode-map "<" nil)
    (define-key rjsx-mode-map (kbd "C-d") nil)
    (define-key rjsx-mode-map ">" nil))
  (add-to-list 'auto-mode-alist '("\\.js[x]?\\'" . rjsx-mode))
  (add-to-list 'auto-mode-alist '("\\.jsx\\'" . rjsx-mode))
  (add-hook 'js-mode-hook 'my-js-mode-hook)
)

;; (use-package js2-mode
  ;; :ensure t
  ;; :config
  ;; (add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
  ;; (add-to-list 'auto-mode-alist '("\\.jsx\\'" . js2-jsx-mode))
  ;; (add-hook 'js-mode-hook 'my-js-mode-hook) 
;; )

;; (add-hook 'js-mode-hook 'js2-minor-mode)

(defun my-js-mode-hook ()
  (interactive)
  ;; (setq js2-strict-missing-semi-warning nil)
  (setq js2-mode-show-parse-errors nil)
  (setq js2-mode-show-strict-warnings nil)
  ;; (set-face-attribute 'js2-function-call nil :foreground "#DCDBAC")
  ;; (set-face-attribute 'js2-external-variable nil :foreground "#92DBFC")
  ;; (set-face-attribute 'js2-function-param nil :foreground "#92DBFC")
  ;; (set-face-attribute 'rjsx-tag nil :foreground "#32C1A9")
  ;; (set-face-attribute 'rjsx-tag-bracket-face nil :foreground "#7D7D7D")
  ;; (set-face-attribute 'font-lock-variable-name-face nil :foreground "#92DBFC")
  ;; (set-face-attribute 'default nil :foreground "#92DBFC")
  ;; (set-face-attribute 'default nil :foreground "#FFF")
  ;; (set-face-attribute 'font-lock-keyword-face nil :foreground "#C080B5")

  ;; disable jshint since we prefer eslint checking
  (setq-default flycheck-disabled-checkers
		(append flycheck-disabled-checkers
			'(javascript-jshint)))
  ;; use eslint with web-mode for jsx files
  (flycheck-add-mode 'javascript-eslint 'rjsx-mode)

  ;; customize flycheck temp file prefix
  (setq-default flycheck-temp-prefix ".flycheck")

  ;; disable json-jsonlist checking for json files
  (setq-default flycheck-disabled-checkers
		(append flycheck-disabled-checkers
			'(json-jsonlist)))

  (setup-tide-mode)
  (emmet-mode)
  (setq emmet-expand-jsx-className? t) ;; default nil
  (add-hook 'flycheck-mode-hook #'my/use-eslint-from-node-modules)
  (lsp)
  (setq lsp-clients-typescript-server "typescript-language-server"
       lsp-clients-typescript-server-args '("--stdio"))
  (setq js-indent-level 2)
  (setq sgml-basic-offset 2)
  (setq js-switch-indent-offset 2)
  (setq js2-strict-missing-semi-warning nil)
  ;; (add-hook 'emmet-mode-hook (lambda () (setq emmet-indent-after-insert nil)))
)

;; (add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
;; (add-hook 'js2-mode-hook 'my-js-mode-hook)

;; (add-to-list 'auto-mode-alist '("\\.jsx?\\'" . js2-jsx-mode))

;; (setq web-mode-content-types-alist
      ;; '(("jsx" . "\\.js[x]?\\'")
        ;; ("javascript" . "\\.es6?\\'")))

;; (defadvice web-mode-highlight-part (around tweak-jsx activate)
  ;; (if (equal web-mode-content-type "jsx")
    ;; (let ((web-mode-enable-part-face nil))
      ;; ad-do-it)
    ;; ad-do-it))

;; use web-mode for .jsx files
;; (add-to-list 'auto-mode-alist '("\\.jsx$" . web-mode))

;; Emmet
(use-package emmet-mode
  :ensure t
  :config
  (setq emmet-move-cursor-between-quotes t) ;; default nil
  ;; (add-hook 'emmet-mode-hook (lambda () (setq emmet-indent-after-insert nil)))
  (add-hook 'emmet-mode-hook (lambda () (setq emmet-indentation 2))) ;; indent 2 spaces.
)

(defun setup-tide-mode ()
  (interactive)
  (tide-setup)
  ;; (flycheck-mode +1)
  (add-hook 'before-save-hook 'tide-format-before-save)
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (eldoc-mode +1)
  (tide-hl-identifier-mode +1)
  ;; configure javascript-tide checker to run after your default javascript checker
  (flycheck-add-next-checker 'javascript-eslint 'javascript-tide 'append)
  (setq-default flycheck-disabled-checkers (append flycheck-disabled-checkers '(tsx-tide)))
  ;; (add-to-list 'company-backends 'company-tide)
    ;; company is an optional dependency. You have to
  ;; install it separately via package-install
  ;; `M-x package-install [ret] company`
  ;; (company-mode +1)
  )

;; (require 'web-mode)
;; (add-to-list 'auto-mode-alist '("\\.jsx\\'" . web-mode))
;; (add-hook 'web-mode-hook
          ;; (lambda ()
            ;; (when (string-equal "jsx" (file-name-extension buffer-file-name))
              ;; (setup-tide-mode))))
;; configure jsx-tide checker to run after your default jsx checker
;; (flycheck-add-mode 'javascript-eslint 'web-mode)
;; (flycheck-add-next-checker 'javascript-eslint 'jsx-tide 'append)

;; (add-hook 'typescript-mode-hook #'setup-tide-mode)

(defun petri-web-save-and-format ()
  "Runs formatting based on file type (html or js)"
  (interactive)
  (if (string-equal (or "js" "jsx" "ts") (file-name-extension buffer-file-name))
      ;; (message "Doing tide formatting")
      (tide-format)
    (if (string-equal (or  "html" "xhtml" "php") (file-name-extension buffer-file-name))
        ;; (message "Doing html web formatting")
	(web-mode-buffer-indent))))

(defun petriweb-format-before-save ()
  "Formats web-mode buffer. 
   Similiar to tide-format-before-save hook."
  (interactive)
  (if (derived-mode-p 'web-mode)
      (web-mode-buffer-indent)
    (message "not web mode!")))

;; TSX
;; (require 'web-mode)
;; (add-to-list 'auto-mode-alist '("\\.tsx\\'" . web-mode))
;; (add-hook 'web-mode-hook
          ;; (lambda ()
            ;; (when (string-equal "tsx" (file-name-extension buffer-file-name))
              ;; (setup-tide-mode))))
;; enable typescript-tslint checker
;; (flycheck-add-mode 'typescript-tslint 'web-mode)



(defun my-web-mode-hook ()
  "Hooks for Web mode."
  (interactive)
  (company-mode)
  (add-hook 'before-save-hook #'petriweb-format-before-save)
  ;; (add-hook 'flycheck-mode-hook #'my/use-eslint-from-node-modules)
  ;; (setq web-mode-script-padding 2)
  ;; (setq web-mode-style-padding 2)
  ;; (setq web-mode-block-padding 2)
  (setq web-mode-markup-indent-offset 2)
  ;; (setq current-coding-style 'default)
  (setq web-mode-enable-auto-pairing t)
  (setq web-mode-enable-css-colorization t)
  (setq web-mode-enable-current-element-highlight t)
  (setq web-mode-enable-auto-closing t)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2)
  (setq web-mode-attr-indent-offset 2)
  (setq web-mode-comment-style 2)
  ;; (setq web-mode-indent-style 2)
  (setq web-mode-tag-auto-close-style t)
  (setq web-mode-enable-auto-indentation t)
  (setq web-mode-enable-auto-opening t)
  ;; (setq-default indent-tabs-mode nil)
  ;; (setq web-mode-enable-auto-quoting t)
  ;; (setq web-mode-enable-auto-quoting nil)
  ;; (set-face-attribute 'web-mode-html-tag-face 'nil :foreground "#569CD6")
  ;; (set-face-attribute 'web-mode-current-element-highlight-face 'nil :foreground "set")
  (electric-pair-mode t)
  (emmet-mode)
  (setq emmet-expand-jsx-className? nil) ;; default nil
)

(defun my-css-hook ()
  (setq css-indent-offset 2)
  (emmet-mode)
)

;; (add-hook 'js2-mode-hook 'setup-tide-mode)
(add-hook 'css-mode-hook 'my-css-hook)


;;-Windows-performance-tweaks
(when p/win32
  (when (boundp 'w32-pipe-read-delay)
    (setq w32-pipe-read-delay 0))
  ;; Set the buffer size to 64K on Windows (from the original 4K)
  (when (boundp 'w32-pipe-buffer-size)
    (setq irony-server-w32-pipe-buffer-size (* 64 1024)))
)

;; Colors
;; font-lock-variable-name-face
;; (set-face-attribute 'default nil :foreground "#FFFFFF")
;; (set-face-attribute 'font-lock-builtin-face nil :foreground "#DAB98F")
;; (set-face-attribute 'font-lock-comment-face nil :foreground "sea green")
;; (set-face-attribute 'font-lock-constant-face nil :foreground "#D6D6A1")
;; (set-face-attribute 'font-lock-function-name-face nil :foreground "#D6D6A1")
;; (set-face-attribute 'font-lock-keyword-face nil :foreground "#569CD6")
;; (set-face-attribute 'font-lock-string-face nil :foreground "#C88D75")
;; (set-face-attribute 'font-lock-type-face nil :foreground "#4EC3A6")
;; (set-face-attribute 'font-lock-variable-name-face nil :foreground "white")
;; (set-face-attribute 'font-lock-variable-name-face nil :foreground "#92DBFC")
;; (set-face-attribute 'font-lock-preprocessor-face nil :foreground "#C586C0")
;; (set-face-attribute 'region nil :background "#766EC8")

;;Webmode


;; Flycheck


;;Company
;; (eval-after-load 'company
  ;; '(progn
     ;; (set-face-attribute 'company-tooltip nil :background "#4d4d4d" :foreground "white")
     ;; (set-face-attribute 'company-scrollbar-bg nil :background "#4d4d4d")
     ;; (set-face-attribute 'company-scrollbar-fg nil :background "#737373")
     ;; (set-face-attribute 'company-tooltip-annotation nil :foreground "black" :slant 'italic' :height 1.1)
     ;; (set-face-attribute 'company-tooltip-common nil :foreground "gold" :weight 'bold' :height 1.2)
     ;; (set-face-attribute 'company-tooltip-selection nil :background "#8080ff"))
  ;; )

;; Dired
;; (eval-after-load 'dired
  ;; '(progn    
     ;; (set-face-attribute 'dired-directory nil :foreground "#66ccff" :height 1.2 :weight 'bold)))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (alect-themes spacemacs-theme lsp-mode color-identifiers-mode yasnippet-snippets xah-fly-keys web-mode tide swiper smart-mode-line-powerline-theme lsp-javascript-typescript js2-mode flycheck-pos-tip flycheck-irony flycheck-inline emmet-mode eglot dumb-jump company-lsp company-irony))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
