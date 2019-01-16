;; PACKAGES 
(setq package-list '(ivy swiper flycheck flycheck-pos-tip dumb-jump company company-irony irony smart-mode-line yasnippet yasnippet-snippets flycheck-inline))

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

; fetch the list of packages available
(unless package-archive-contents
  (package-refresh-contents))

; install the missing packages
(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))

;; Determine OS and setup
(setq p/linux (featurep 'x))
(setq p/win32 (not p/linux))

(when p/win32
  (setq makescript "makecommand.bat")
)

(when p/linux
  (setq makescript "./makecommand.sh")
)

;; Setup
(add-to-list 'default-frame-alist '(font . "Dejavu Sans Mono 10"))
(set-face-attribute 'default t :font "Dejavu Sans Mono 10")
(menu-bar-mode -1)
(tool-bar-mode -1)
(set-foreground-color "white")
(set-background-color "#1E1E1E")
(set-cursor-color "#40FF40")
(setq split-window-preferred-function nil)
(global-hl-line-mode 1)
(set-face-background 'hl-line "midnight blue")
(setq ring-bell-function 'ignore)

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

(global-set-key (kbd "C-+") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)

;; easy keys for split windows
(global-set-key (kbd "M-3") 'delete-other-windows)
(global-set-key (kbd "M-4") 'split-window-below)
(global-set-key (kbd "M-5") 'split-window-right)
(global-set-key (kbd "M-RET") 'other-window)
(global-set-key (kbd "M-0") 'delete-window)

(global-set-key (kbd "C-x C-y") 'yas-insert-snippet)

(define-key global-map "\ef" 'find-file)
(define-key global-map "\eF" 'petri-find-file-other-window)

(define-key global-map "\eb" 'ivy-switch-buffer)
(define-key global-map "\eB" 'petri-switch-buffer-other-window)

(global-set-key (kbd "M-m") 'make-without-asking)

;; Defalias
(defalias 'list-buffers 'ibuffer)
(defalias 'isearch-forward 'swiper)
(defalias 'query-replace 'replace-string)

;; Custom functions

(defun make-without-asking ()
  "Make the current build."
  (interactive)
  (compile makescript)
  (other-window 1))

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

;; Flycheck
(global-flycheck-mode 1)
(flycheck-pos-tip-mode 1)
(setq flycheck-indication-mode nil)
(setq flycheck--idle-trigger-timer 0.9)
(setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc emacs-lisp))
(flycheck-irony-setup)

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
(when p/win32
  (when (boundp 'w32-pipe-read-delay)
    (setq w32-pipe-read-delay 0))
  ;; Set the buffer size to 64K on Windows (from the original 4K)
  (when (boundp 'w32-pipe-buffer-size)
    (setq irony-server-w32-pipe-buffer-size (* 64 1024)))
)

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

(set-face-attribute 'flycheck-error nil :background "dark red" :foreground "white" :underline nil :weight 'bold)
(set-face-attribute 'flycheck-info nil :background "forest green" :foreground "burlywood3" :underline nil :weight 'bold)
(set-face-attribute 'flycheck-warning nil :background "gold" :foreground "black" :underline nil :weight 'bold)
(setq pos-tip-background-color "red")
(setq pos-tip-foreground-color "white")


(eval-after-load 'company
  '(progn
     (set-face-attribute 'company-tooltip nil :background "#4d4d4d" :foreground "white")
     (set-face-attribute 'company-scrollbar-bg nil :background "#4d4d4d")
     (set-face-attribute 'company-scrollbar-fg nil :background "#737373")
     (set-face-attribute 'company-tooltip-annotation nil :foreground "black" :slant 'italic' :height 1.1)
     (set-face-attribute 'company-tooltip-common nil :foreground "gold" :weight 'bold' :height 1.2)
     (set-face-attribute 'company-tooltip-selection nil :background "#8080ff"))
  )

(eval-after-load 'dired
  '(progn    
     (set-face-attribute 'dired-directory nil :foreground "#66ccff" :height 1.2 :weight 'bold))
  )
