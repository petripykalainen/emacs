;; PACKAGES 
(setq package-list '(ivy swiper flycheck flycheck-pos-tip dumb-jump company company-irony irony smart-mode-line yasnippet yasnippet-snippets))

;; MELPA
(require 'package)
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

;; Abbrevs
;; (load "~/.emacs.d/lisp/my-abbrev.el")

; fetch the list of packages available
(unless package-archive-contents
  (package-refresh-contents))

; install the missing packages
(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))

;; Setup
(add-to-list 'default-frame-alist '(font . "Dejavu Sans Mono 13"))
(set-face-attribute 'default t :font "Dejavu Sans Mono 13")
(menu-bar-mode -1)
(tool-bar-mode -1)
(set-foreground-color "white")
(set-background-color "#1E1E1E")
(set-cursor-color "#40FF40")
(setq split-window-preferred-function nil)
(global-hl-line-mode 1)
(set-face-background 'hl-line "midnight blue")

;; Bookmarks
(setq bookmark-save-flag 1) ; everytime bookmark is changed, automatically save it
(setq bookmark-save-flag t) ; save bookmark when emacs quits

;; Startup
(setq inhibit-splash-screen t)
(require 'bookmark)
;; (switch-to-buffer "*scratch*")
;; (split-window-right)
(bookmark-bmenu-list)
(switch-to-buffer "*Bookmark List*")

;; Custom keys

;;(define-key global-map "\C-r" 'kill-region)
(define-key global-map [C-tab] 'indent-region)
(global-set-key (kbd "C-q") 'delete-region)
(global-set-key (kbd "M-r") 'query-replace)
(global-set-key (kbd "C-x C-r") 'eval-region)
(global-set-key (kbd "M-;") 'comment-eclipse)
(global-set-key (kbd "C-;") 'toggle-comment-on-line)

;; easy keys for split windows
(global-set-key (kbd "M-3") 'delete-other-windows)
(global-set-key (kbd "M-4") 'split-window-below)
(global-set-key (kbd "M-5") 'split-window-right)
(global-set-key (kbd "M-RET") 'other-window)
(global-set-key (kbd "M-0") 'delete-window)

(global-set-key (kbd "C-x C-y") 'yas-insert-snippet)

(define-key global-map "\ef" 'find-file)
(define-key global-map "\eF" 'find-file-other-window)

(define-key global-map "\eb" 'ivy-switch-buffer)
(define-key global-map "\eB" 'petri-switch-buffer-other-window)


;; Defalias
(defalias 'list-buffers 'ibuffer)
(defalias 'isearch-forward 'swiper)
(defalias 'query-replace 'replace-string)

;; Custom functions

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

(defun my-paska ()
  "Needlessly complicatedly mark already marked region and expand it with yasnippets"
  (interactive)
  (let (b1 b2)
    (exchange-point-and-mark)
    (next-line)
    (beginning-of-line)
    (setq b1 (point))
    (exchange-point-and-mark)
    (previous-line)
    (end-of-line)
    (setq b2 (point))
    (next-line)
    (end-of-line)
    (copy-region-as-kill b1 b2)
    (setq yas-selected-text (car kill-ring))
    (yas-expand))
  )


(defun another-paskla ()
  "Just add region to yas-selected-text"
  (interactive)
  (let (b1 b2)
    (setq b1 (region-beginning))
    (setq b2 (region-end))
    (copy-region-as-kill b1 b2)
    (setq yas-selected-text (car kill-ring)))
  )

;; Disable mousewheel
(mouse-wheel-mode -1)
(global-set-key [wheel-up] 'ignore)
(global-set-key [wheel-down] 'ignore)
(global-set-key [double-wheel-up] 'ignore)
(global-set-key [double-wheel-down] 'ignore)
(global-set-key [triple-wheel-up] 'ignore)
(global-set-key [triple-wheel-down] 'ignore)

;; ;; transparent mark
;; (defadvice set-mark-command (after no-bloody-t-m-m activate)
;;   "Prevent consecutive marks activating bloody `transient-mark-mode'."
;;   (if transient-mark-mode (setq transient-mark-mode nil)))

;; Bigger undo
(setq undo-limit 20000000)
(setq undo-strong-limit 40000000)

;disable backup
(setq backup-inhibited t)
;disable auto save
(setq auto-save-default nil)
;; disable lockfiles
(setq create-lockfiles nil)

;; tabwidth
;;(setq-default c-basic-offset 4)
;;(setq c-default-style "linux"
;;      c-basic-offset 4)

;; Flycheck
(global-flycheck-mode 1)
(flycheck-pos-tip-mode 1)
(setq flycheck-indication-mode nil)
(setq flycheck--idle-trigger-timer 0.3)
(setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc emacs-lisp))

;; IVY
(setq ivy-use-virtual-buffers t)
(setq ivy-count-format "(%d/%d) ")
(ivy-mode 1)

;; DUMB-JUMP
(dumb-jump-mode 1)

;; smart mode line
(setq sml/theme 'dark)
(setq sml/no-confirm-load-theme t)
(sml/setup)

;; Yasnippet
(setq yas-wrap-around-region t)
(yas-global-mode 1)

;; Show matchin parenthesis
(show-paren-mode 1)

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
  (setq tab-width 8)
  (setq indent-tabs-mode nil)
  ;; Additional style stuff
  (c-set-offset 'member-init-intro '++)


  ;; No hungry backspace
  (c-toggle-auto-hungry-state nil)

  ;; Newline indents, semi-colon doesn't
  (define-key c++-mode-map "\C-m" 'newline-and-indent)
  (setq c-hanging-semi&comma-criteria '((lambda () 'stop)))

  ;; Handle super-tabbify (TAB completes, shift-TAB actually tabs)
  (setq dabbrev-case-replace t)
  (setq dabbrev-case-fold-search t)
  (setq dabbrev-upcase-means-case-search t)

  ;; Abbrevation expansion
  ;; (abbrev-mode 1)
)

(add-hook 'c-mode-common-hook 'petri-c-hook)

;; Company-mode
(setq company-dabbrev-downcase 0)
(setq company-idle-delay 0)
(add-hook 'after-init-hook 'global-company-mode)
(eval-after-load 'company
  '(add-to-list 'company-backends 'company-irony))

;; IRONY
(add-hook 'c++-mode-hook 'irony-mode)
(add-hook 'c-mode-hook 'irony-mode)
(add-hook 'objc-mode-hook 'irony-mode)
(add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)

;; Windows performance tweaks
(when (boundp 'w32-pipe-read-delay)
  (setq w32-pipe-read-delay 0))
;; Set the buffer size to 64K on Windows (from the original 4K)
(when (boundp 'w32-pipe-buffer-size)
  (setq irony-server-w32-pipe-buffer-size (* 64 1024)))

;; Colors
(set-face-attribute 'font-lock-builtin-face nil :foreground "#DAB98F")
(set-face-attribute 'font-lock-comment-face nil :foreground "sea green")
(set-face-attribute 'font-lock-constant-face nil :foreground "#D6D6A1")
(set-face-attribute 'font-lock-function-name-face nil :foreground "#D6D6A1")
(set-face-attribute 'font-lock-keyword-face nil :foreground "#569CD6")
(set-face-attribute 'font-lock-string-face nil :foreground "#C88D75")
(set-face-attribute 'font-lock-type-face nil :foreground "#4EC3A6")
(set-face-attribute 'font-lock-variable-name-face nil :foreground "white")
(set-face-attribute 'font-lock-preprocessor-face nil :foreground "#C586C0")
(set-face-attribute 'region nil :background 'nil)
(set-face-attribute 'flycheck-error nil :background "red1" :foreground "gray")
(set-face-attribute 'flycheck-info nil :background "forest green" :foreground "burlywood3")
(set-face-attribute 'flycheck-warning nil :background "gold" :foreground "black")


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa" "a27c00821ccfd5a78b01e4f35dc056706dd9ede09a8b90c6955ae6a390eb1c1e" default)))
 '(irony-extra-cmake-args nil)
 '(package-selected-packages
   (quote
    (org-edit-latex yasnippet yasnippet-snippets counsel swiper flycheck dumb-jump))))
;; (custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 ;; '(flycheck-error ((t (:background "red1" :foreground "gray21"))))
 ;; '(flycheck-info ((t (:background "forest green" :foreground "burlywood3"))))
 ;; '(flycheck-warning ((t (:background "gold" :foreground "gray21" :underline (:color "DarkOrange" :style wave))))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(require 'package)
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))
  ;; Comment/uncomment these two lines to enable/disable MELPA and MELPA Stable as desired
  (add-to-list 'package-archives (cons "melpa" (concat proto "://melpa.org/packages/")) t)
  ;;(add-to-list 'package-archives (cons "melpa-stable" (concat proto "://stable.melpa.org/packages/")) t)
  (when (< emacs-major-version 24)
    ;; For important compatibility libraries like cl-lib
    (add-to-list 'package-archives '("gnu" . (concat proto "://elpa.gnu.org/packages/")))))
(package-initialize)

(menu-bar-mode -1)
(maximize-frame)
(set-foreground-color "burlywood3")
(set-background-color "#161616")
(set-cursor-color "#40FF40")
