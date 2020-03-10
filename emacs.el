(eval-and-compile
  (defvar petri-emacs-dir (expand-file-name user-emacs-directory)
    "The path to the emacs.d directory.")

  (defvar petri-local-dir (concat petri-emacs-dir ".local/")
    "Root directory for local Emacs files. Use this as permanent storage for files
  that are safe to share across systems (if this config is symlinked across
  several computers).")

  (defvar petri-snippet-dir (concat petri-local-dir "snippets")
  "Snippet location")

  ;; (defvar bmacs-host-dir (concat bmacs-local-dir "@" (system-name))
  ;;   "Directory for hostname-specific file storage. Used by `bmacs-etc-dir' and
  ;; `bmacs-cache-dir'.")

  ;; (defvar bmacs-etc-dir (concat bmacs-host-dir "/etc/")
  ;;   "Host-namespaced directory for non-volatile storage. These are not deleted or
  ;; tampored with by BMACS functions. Use this for dependencies like servers or
  ;; config files that are stable (i.e. it should be unlikely that you need to delete
  ;; them if something goes wrong).")

  ;; (defvar bmacs-cache-dir (concat bmacs-host-dir "/cache/")
  ;;   "Host-namespaced directory for volatile storage. Deleted when `bmacs/reset' is
  ;; called. Use this for transient files that are generated on the fly like caches
  ;; and temporary files. Anything that may need to be cleared if there are
  ;; problems.")

  ;; (defvar bmacs-packages-dir (concat bmacs-local-dir "packages/")
  ;;   "Where package.el and quelpa plugins (and their caches) are stored.")

  ;; (defvar bmacs-custom-dir (concat bmacs-emacs-dir "custom/")
  ;;   "Where custom lisp files are stored")

  (dolist (dir (list petri-local-dir petri-snippet-dir (expand-file-name "elpa" petri-local-dir)))
    (unless (file-directory-p dir)
      (make-directory dir t))))

(eval-and-compile
  (setq gc-cons-threshold 402653184
        gc-cons-percentage 0.6))

(add-hook 'after-init-hook #'(lambda () (setq gc-cons-threshold 16777216
                                                   gc-cons-percentage 0.1)))

(setq max-lisp-eval-depth 50000)
(setq max-specpdl-size 10000)

(add-to-list 'default-frame-alist '(inhibit-double-buffering . t))

(setq byte-compile-warnings '(not free-vars unresolved noruntime lexical make-local))

(setq use-package-always-defer t
      use-package-verbose t)

(eval-and-compile
  (setq package-user-dir (expand-file-name "elpa" petri-local-dir)))

(setq load-prefer-newer noninteractive
      package--init-file-ensured t
      package-enable-at-startup nil)

(eval-and-compile
  (setq load-path (append load-path (directory-files package-user-dir t "^[^.]" t))))

(eval-when-compile
  (require 'package)

  (unless (assoc-default "melpa" package-archives)
    (add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t))
  (unless (assoc-default "melpa-stable" package-archives)
    (add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t))
  (unless (assoc-default "org" package-archives)
    (add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t))

  (setq package-pinned-packages
    '((evil . "melpa-stable")))

  (package-initialize)
  (unless (package-installed-p 'use-package)
    (package-refresh-contents)
    (package-install 'use-package))
  (require 'use-package)
  (require 'bind-key)
  (setq use-package-always-ensure t))

(use-package async :demand t)
(use-package f :demand t)
(use-package subr-x :demand t :ensure nil)
(eval-when-compile (require 'cl-lib))

(when (fboundp 'set-charset-priority)
  (set-charset-priority 'unicode))
(prefer-coding-system        'utf-8)
(set-terminal-coding-system  'utf-8)
(set-keyboard-coding-system  'utf-8)
(set-selection-coding-system 'utf-8)
(setq locale-coding-system   'utf-8)
(setq-default buffer-file-coding-system 'utf-8)

(setq-default auto-save-default nil
              create-lockfiles nil
              make-backup-files nil)

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

(defun petri-flycheck-colors ()
  "sets better visual for error, warning and info"
  (interactive)
     (set-face-attribute 'flycheck-error nil :background "dark red" :foreground "white" :underline nil :weight 'bold)
     (set-face-attribute 'flycheck-info nil :background "forest green" :foreground "black" :underline nil :weight 'bold)
     (set-face-attribute 'flycheck-warning nil :background "gold" :foreground "black" :underline nil :weight 'bold)
)

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

(tooltip-mode -1)
(menu-bar-mode -1)
(if (fboundp 'tool-bar-mode)   (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

(defun bmacs|enable-line-numbers ()
  "Enables the display of line numbers, using `display-line-numbers' (in Emacs
26+) or `nlinum-mode'."
  (if (boundp 'display-line-numbers)
      (setq display-line-numbers t)
      (global-display-line-numbers-mode)))

(defun bmacs|disable-line-numbers ()
  "Disable the display of line numbers."
  (if (boundp 'display-line-numbers)
      (setq display-line-numbers nil)
      (global-display-line-numbers-mode -1)))

(add-hook 'prog-mode-hook #'bmacs|enable-line-numbers)
(add-hook 'text-mode-hook #'bmacs|enable-line-numbers)
(add-hook 'conf-mode-hook #'bmacs|enable-line-numbers)

(setq-default
 indent-tabs-mode nil         ; don't insert tabs by default
 require-final-newline t      ; ensure newline exists at end of file
 tab-always-indent t          ; always indent line when pressing TAB (don't add tab character)
 tab-width 2                  ; default tab width of 2 characters
 tabify-regexp "^\t* [ \t]+") ; only tabify initial whitespace when converting to tabifying

;; whitespace-mode
;; (setq-default
;;  whitespace-line-column fill-column
;;  whitespace-style
;;  '(face indentation tabs tab-mark spaces space-mark newline newline-mark trailing lines-tail)
;;  whitespace-display-mappings
;;  '((tab-mark ?\t [?› ?\t])
;;    (newline-mark ?\n [?¬ ?\n])
;;    (space-mark ?\  [?·] [?.])))

(setq column-number-mode t)
(setq mode-line-position '((line-number-mode ("%l " (column-number-mode ": %c"))))) 
(setq-default frame-title-format "%f")

;; Bigger undo
(setq undo-limit 20000000)
(setq undo-strong-limit 40000000)
(global-hl-line-mode 1)
(setq ring-bell-function 'ignore)
(blink-cursor-mode 0)
;; (require 'vc)
(eval-after-load "vc" '(remove-hook 'find-file-hook 'vc-find-file-hook))
(setq vc-handled-backends nil)

;; Show paren
(show-paren-mode 1)

;; Bookmarks
(setq bookmark-save-flag 1) ; everytime bookmark is changed, automatically save it
(setq bookmark-save-flag t) ; save bookmark when emacs quits

;; Startup
(setq inhibit-splash-screen t)
(bookmark-bmenu-list)
(switch-to-buffer "*Bookmark List*")
;; (toggle-frame-maximized)

;; Defalias

(fset 'list-buffers 'ibuffer)
(fset 'isearch-forward 'swiper)
;;(defalias 'query-replace 'replace-string)
;; (defalias 'xah-insert-date 'petri-insert-date)

;; Dired
;; Auto refresh buffers
(add-hook 'dired-mode-hook 'auto-revert-mode)
(global-auto-revert-mode 1)
;; Also auto refresh dired, but be quiet about it
(setq global-auto-revert-non-file-buffers t)
(setq auto-revert-verbose nil)

(eldoc-mode 1)

(setq-default
 bookmark-save-flag 1)  ; automatically save bookmarks after every change

(setq-default
 fill-column 100                  ; set line-wrapping column to 100
 word-wrap t                     ; enable word wrap so lines are wrapped at nearest space
)

(setq-default
 scroll-conservatively 1001             ; always scroll to the point no matter how far away (don't recenter)
 scroll-margin 0                        ; don't automatically scroll to retain a margin
 scroll-preserve-screen-position t)     ; preserve point location on screen when scrolling

(setq-default
 indent-tabs-mode nil         ; don't insert tabs by default
 require-final-newline t      ; ensure newline exists at end of file
 tab-always-indent t          ; always indent line when pressing TAB (don't add tab character)
 tab-width 2                  ; default tab width of 2 characters
 tabify-regexp "^\t* [ \t]+") ; only tabify initial whitespace when converting to tabifying

 ;; whitespace-mode
(setq-default
 whitespace-line-column fill-column
 whitespace-style
 '(face indentation tabs tab-mark spaces space-mark newline newline-mark trailing lines-tail)
 whitespace-display-mappings
 '((tab-mark ?\t [?› ?\t])
   (newline-mark ?\n [?¬ ?\n])
   (space-mark ?\  [?·] [?.])))

(set-face-attribute 'default nil :font "Dejavu Sans Mono 12" )

;; https://with-emacs.com/posts/tips/quit-current-context/
(defun keyboard-quit-context+ ()
  "Quit current context.

    This function is a combination of `keyboard-quit' and
    `keyboard-escape-quit' with some parts omitted and some custom
    behavior added."
  (interactive)
  (cond ((region-active-p)
         ;; Avoid adding the region to the window selection.
         (setq saved-region-selection nil)
         (let (select-active-regions)
           (deactivate-mark)))
        ((eq last-command 'mode-exited) nil)
        (current-prefix-arg
         nil)
        (defining-kbd-macro
          (message
           (substitute-command-keys
            "Quit is ignored during macro defintion, use \\[kmacro-end-macro] if you want to stop macro definition"))
          (cancel-kbd-macro-events))
        ((active-minibuffer-window)
         (when (get-buffer-window "*Completions*")
           ;; hide completions first so point stays in active window when
           ;; outside the minibuffer
           (minibuffer-hide-completions))
         (abort-recursive-edit))
        ;; Also quits help buffers (Ones with 'press q to quit' 
        (t
         (unless (let ((found nil))
                   (dolist (w (window-list))
                     (when (with-current-buffer (window-buffer w)
                             (derived-mode-p 'help-mode))
                       (save-selected-window
                         (setq found t)
                         (quit-window nil w))))
                   found)
           (keyboard-quit)))))

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

;; https://github.com/abo-abo/swiper/issues/1638#issuecomment-399224033
(defun petri-counsel-ag ()
  (interactive)
  (counsel-ag nil default-directory))

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
      (set-face-attribute 'mode-line nil :background "orangered" :foreground "white")
      (setq-local flycheck-javascript-eslint-executable eslint))))

(defun setup-tide-mode ()
  (interactive)
  (tide-setup)
  (add-hook 'before-save-hook 'tide-format-before-save)
  (tide-hl-identifier-mode +1)
  (flycheck-mode +1)
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (eldoc-mode +1)
  (company-mode +1))

(global-set-key [remap keyboard-quit] #'keyboard-quit-context+)

(defun my-text-mode-hook ()
  (interactive)
  (setq indent-tabs-mode nil)
  (setq tab-width 2)
  (setq-default indent-line-function 'indent-to-left-margin)
  (setq-default tab-always-indent nil))

(add-hook 'text-mode-hook 'my-text-mode-hook)

(defun my-js-mode-hook ()
  (interactive)
  ;; (setq js2-strict-missing-semi-warning nil)
  ;; (setq js2-mode-show-parse-errors nil)
  ;; (setq js2-mode-show-strict-warnings nil)

  ;; (set-face-attribute 'js2-function-call nil :foreground "#DCDBAC")
  ;; (set-face-attribute 'js2-external-variable nil :foreground "#92DBFC")
  ;; (set-face-attribute 'js2-function-param nil :foreground "#92DBFC")
  ;; (set-face-attribute 'rjsx-tag nil :foreground "#32C1A9")
  ;; (set-face-attribute 'rjsx-tag-bracket-face nil :foreground "#7D7D7D")
  ;; (set-face-attribute 'font-lock-variable-name-face nil :foreground "#92DBFC")
  ;; (set-face-attribute 'default nil :foreground "#92DBFC")
  ;; (set-face-attribute 'default nil :foreground "#FFF")
  ;; (set-face-attribute 'font-lock-keyword-face nil :foreground "#C080B5")

  ;; use eslint with rjsx-mode for jsx files
  ;; (add-hook 'js-mode-hook #'my/use-eslint-from-node-modules)
  ;; (flycheck-add-mode 'javascript-eslint 'rjsx-mode)

  (emmet-mode)
  (setq emmet-expand-jsx-className? t) ;; default nil
  (setq js-indent-level 2)
  ;; (setq sgml-basic-offset 2)
  (setq js-switch-indent-offset 2)
  (setq js2-strict-missing-semi-warning nil)
  (setq tab-width 2)
  (electric-pair-mode t)
  ;; (add-hook 'emmet-mode-hook (lambda () (setq emmet-indent-after-insert nil)))
)

(defun my-web-mode-hook ()
  "Hooks for Web mode."
  (interactive)
  (company-mode)
  (add-hook 'before-save-hook #'petriweb-format-before-save)
  ;; (setq web-mode-script-padding 2)
  ;; (setq web-mode-style-padding 2)
  ;; (setq web-mode-block-padding 2)
  ;; (setq web-mode-markup-indent-offset 2)
  ;; (setq current-coding-style 'default)
  (setq web-mode-enable-auto-pairing t)
  (setq web-mode-enable-css-colorization t)
  (setq web-mode-enable-current-element-highlight t)
  (setq web-mode-enable-auto-closing t)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2)
  (setq web-mode-attr-indent-offset 2)
  (setq web-mode-comment-style 2)
  (setq web-mode-indent-style 2)
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
  (add-to-list 'company-backends '(company-css company-abbrev company-dabbrev))
  (emmet-mode))

(add-hook 'css-mode-hook 'my-css-hook)

(use-package bind-key)

(defun my-xfk-command-color () 
(set-face-attribute 'mode-line nil :background "DarkGoldenrod2" :foreground "black"))

(defun my-xfk-insert-color ()
(set-face-attribute 'mode-line nil :background "chartreuse3" :foreground "black"))

(add-hook 'xah-fly-command-mode-activate-hook 'my-xfk-command-color)
(add-hook 'xah-fly-insert-mode-activate-hook  'my-xfk-insert-color)

(use-package xah-fly-keys
  :diminish xah-fly-keys
  :config
  (xah-fly-keys-set-layout "qwerty") 
  (setq xah-fly-use-meta-key nil)
  (xah-fly-keys 1)
  (define-key xah-fly-key-map (kbd "M-SPC") 'xah-fly-command-mode-activate)
  :hook 
  ('xah-fly-command-mode-activate-hook 'my-xfk-command-color)
  ('xah-fly-insert-mode-activate-hook  'my-xfk-insert-color))

(use-package spacemacs-common
    :ensure spacemacs-theme
    :config (load-theme 'spacemacs-dark t)
    (set-face-attribute 'mode-line-buffer-id nil :foreground "black")
    (set-face-attribute 'mode-line nil :background "DarkGoldenrod2" :foreground "black"))

(use-package hl-todo
  :init
  (add-hook 'after-init-hook (lambda () (setq hl-todo-keyword-faces
   '(("TODO"   . "#ff0000")
     ("FIXME"  . "#ff0000")
     ("DEBUG"  . "#a020f0")
     ("GOTCHA" . "#ff4500")
     ("STUB"   . "#1e90ff")))))
  ;; (setq hl-todo-keyword-faces
  ;;       '(("HOLD" . "#d0bf8f")
  ;;        ("TODO" . "#cc9393")
  ;;        ("NEXT" . "#dca3a3")
  ;;        ("THEM" . "#dc8cc3")
  ;;        ("PROG" . "#7cb8bb")
  ;;        ("OKAY" . "#7cb8bb")
  ;;        ("DONT" . "#5f7f5f")
  ;;        ("FAIL" . "#8c5353")
  ;;        ("DONE" . "#afd8af")
  ;;        ("NOTE" . "#d0bf8f")
  ;;        ("KLUDGE" . "#d0bf8f")
  ;;        ("HACK" . "#d0bf8f")
  ;;        ("TEMP" . "#d0bf8f")
  ;;        ("FIXME" . "#cc9393")
  ;;        ("XXX+" . "#cc9393")))
  :config
  (global-hl-todo-mode 1)
)

(use-package fzf)

(use-package typescript-mode
  :config
  (setq typescript-indent-level 2)
  (add-to-list 'auto-mode-alist '("\\.ts[x]?\\'" . typescript-mode))
)

(use-package tide
  :after (typescript-mode company flycheck)
  :hook ((typescript-mode . tide-setup)
         (typescript-mode . tide-hl-identifier-mode)
         (before-save . tide-format-before-save)))

(use-package multiple-cursors
  :config
  (global-set-key (kbd "C->") 'mc/mark-next-like-this)
  (global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
  (global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this))

(use-package dotenv-mode
  :config
  ;; for optionally supporting additional file extensions such as `.env.test' with this major mode
  (add-to-list 'auto-mode-alist '("\\.env\\..*\\'" . dotenv-mode)) 
)

(use-package yaml-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode))
  :after lsp
  :config
  (add-hook 'yaml-mode-hook #'lsp))

(use-package dockerfile-mode
  :config
  (add-to-list 'auto-mode-alist '("Dockerfile\\'" . dockerfile-mode)))

(use-package wgrep
:config
(set-face-attribute 'wgrep-face nil :background "#449" :foreground "white"))

(use-package which-key
  :diminish which-key-mode
  :config
  (which-key-mode)
  (which-key-setup-minibuffer))

(use-package diminish
  :diminish eldoc-mode)

(use-package add-node-modules-path
:ensure t
:config
;; automatically run the function when web-mode starts
;;(eval-after-load 'rjsx-mode
(add-hook 'js-mode-hook 'add-node-modules-path)
(add-hook 'typescript-mode-hook 'add-node-modules-path))

(autoload 'pkg-info-version-info "pkg-info")

(use-package flycheck
  :commands (flycheck-mode flycheck-list-errors flycheck-buffer)
  :init (add-hook 'prog-mode-hook 'flycheck-mode)
  :config
  (petri-flycheck-colors)
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc emacs-lisp javascript-jshint json-jsonlint))
  (flycheck-add-mode 'javascript-eslint 'rjsx-mode)
  (flycheck-add-mode 'javascript-eslint 'typescript-mode)
)

(use-package flycheck-pos-tip
  :demand t
  :after flycheck
  :config
  (setq flycheck-pos-tip-timeout 10
        flycheck-display-errors-delay 0.5)
  (flycheck-pos-tip-mode +1))

  (defun petri-flycheck-colors ()
    "sets better visual for error, warning and info"
    (interactive)
       (set-face-attribute 'flycheck-error nil :background "dark red" :foreground "white" :underline nil :weight 'bold)
       (set-face-attribute 'flycheck-info nil :background "forest green" :foreground "black" :underline nil :weight 'bold)
       (set-face-attribute 'flycheck-warning nil :background "gold" :foreground "black" :underline nil :weight 'bold))

  ;; (use-package flycheck
  ;;   :commands (global-flycheck-mode)
  ;;   :init
  ;;   (global-flycheck-mode)
  ;;   :config
  ;;   (petri-flycheck-colors)
  ;;   (setq flycheck-indication-mode nil)
  ;;   (setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc emacs-lisp javascript-jshint json-jsonlint))
  ;;   (setq flycheck-check-syntax-automatically '(save mode-enabled))
  ;;   (flycheck-add-mode 'javascript-eslint 'rjsx-mode))

  ;; (add-hook 'flycheck-mode-hook #'my/use-eslint-from-node-modules)

  ;; (use-package flycheck-pos-tip
  ;;   :config
  ;;   (flycheck-pos-tip-mode 1)
  ;;   (setq pos-tip-background-color "red")
  ;;   (setq pos-tip-foreground-color "white") 
  ;; )

(use-package ivy
  :init
  (setq ivy-use-virtual-buffers t)
  (setq ivy-count-format "(%d/%d) ")
  :config
  (ivy-mode 1))

(use-package swiper
  :after ivy
  :config
  (fset 'isearch-forward 'swiper))

(use-package counsel
  :after ivy
  ;; :config 
  ;; (counsel-mode)
)

(use-package dumb-jump
  :commands (dumb-jump-go dumb-jump-go-other-window dumb-jump-go-prompti dumb-jump-go-prefer-external dumb-jump-go-prefer-external-other-window )
  :bind (("M-g o" . dumb-jump-go-other-window)
         ("M-g j" . dumb-jump-go)
         ("M-g i" . dumb-jump-go-prompt)
         ("M-g x" . dumb-jump-go-prefer-external)
         ("M-g z" . dumb-jump-go-prefer-external-other-window))
  :config
  (setq dumb-jump-prefer-searcher 'ag)
  (setq dumb-jump-selector 'ivy) ;; (setq dumb-jump-selector 'helm)
)

(use-package yasnippet
  :diminish yas-global-mode
  :config
  ;;(setq yas-snippet-dirs (concat petri-local-dir "snippets"))
  (use-package yasnippet-snippets
    :ensure t)
  (setq yas-wrap-around-region t)
  (yas-global-mode 1)
)

(use-package lsp-mode
  :commands lsp
  :init
  (add-hook 'prog-mode-hook #'lsp)
  :config
  (setq lsp-diagnostic-package :none)
)

(use-package company
  :config
  (setq company-dabbrev-downcase 0)
  (setq company-idle-delay 0.4)
  (setq company-minimum-prefix-length 4)
  (setq company-dabbrev-downcase nil)
  (setq company-dabbrev-other-buffers t)
  (setq company-auto-complete nil)
  (setq company-dabbrev-code-other-buffers 'all)
  (setq company-dabbrev-code-everywhere t)
  (setq company-dabbrev-code-ignore-case t)
  (setq company-tooltip-align-annotations t)
  (add-hook 'after-init-hook 'global-company-mode)
  ;; (set-face-attribute 'company-tooltip nil :background "#4d4d4d" :foreground "white")
  ;; (set-face-attribute 'company-scrollbar-bg nil :background "#4d4d4d")
  ;; (set-face-attribute 'company-scrollbar-fg nil :background "#737373")
  ;; (set-face-attribute 'company-tooltip-annotation nil :foreground "black" :slant 'italic' :height 1.1)
  ;; (set-face-attribute 'company-tooltip-common nil :foreground "gold" :weight 'bold' :height 1.2)
  ;; (set-face-attribute 'company-tooltip-selection nil :background "#8080ff")
) 

(use-package company-lsp
  :commands company-lsp
  :config
  (setq company-lsp-cache-candidates t)
  (push 'company-lsp company-backends))

(use-package web-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.erb\\'"    . web-mode))       ;; ERB
  ;; (add-to-list 'auto-mode-alist '("\\.json\\'"   . web-mode))       ;; JSON
  (add-to-list 'auto-mode-alist '("\\.html?\\'"  . web-mode))       ;; Plain HTML
  (add-to-list 'auto-mode-alist '("\\.es6\\'"    . web-mode))       ;; ES6
  (add-to-list 'auto-mode-alist '("\\.php\\'"   . web-mode))        ;; PHP
  (add-to-list 'auto-mode-alist '("\\.blade\\.php\\'" . web-mode))  ;; Blade template
  ;; (add-to-list 'auto-mode-alist '("\\.ts[x]?\\'" . web-mode))
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-sql-indent-offset 2)
  (add-hook 'web-mode-hook 'my-web-mode-hook))

(use-package json-mode
:mode ("\\.json\\'" . json-mode))

(use-package rjsx-mode
  :config
  (with-eval-after-load 'rjsx-mode
    (define-key rjsx-mode-map (kbd "M-,") nil)
    (define-key rjsx-mode-map (kbd "M-.") nil)
    (define-key rjsx-mode-map (kbd "M-/") nil)
    (define-key rjsx-mode-map "<" nil)
    (define-key rjsx-mode-map (kbd "C-d") nil)
    (define-key rjsx-mode-map ">" nil))
  (add-to-list 'auto-mode-alist '("\\.js[x]?\\'" . rjsx-mode)))
  (add-hook 'js-mode-hook 'my-js-mode-hook)

(use-package emmet-mode
  :config
  (setq emmet-move-cursor-between-quotes t) ;; default nil
  ;; (add-hook 'emmet-mode-hook (lambda () (setq emmet-indent-after-insert nil)))
  (add-hook 'emmet-mode-hook (lambda () (setq emmet-indentation 2))) ;; indent 2 spaces.
)

(setq-default tab-width 2
              c-basic-offset 4
              coffee-tab-width 2
              javascript-2-level 2
              js-2-level 2
              js2-basic-offset 2
              web-mode-markup-2-offset 2
              web-mode-css-2-offset 2
              web-mode-code-2-offset 2
              css-2-offset 2
              rust-indent-offset 4)

(add-hook 'js2-mode-hook (lambda ()
                           (setq js2-basic-offset 2)))
