;; PACKAGES 
(setq package-list '(ivy swiper flycheck-irony flycheck flycheck-pos-tip dumb-jump company company-irony irony smart-mode-line yasnippet yasnippet-snippets flycheck-inline web-mode rjsx-mode js2-mode xah-fly-keys company-tern tide emmet-mode))

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
(set-face-attribute 'default nil :font "Dejavu Sans Mono 11" )
(set-frame-font "Dejavu Sans Mono 11" nil t)
(menu-bar-mode -1)
(tool-bar-mode -1)
(set-foreground-color "white")
(set-background-color "#1E1E1E")
(setq split-window-preferred-function nil)
(global-hl-line-mode 1)
;; (set-cursor-color "red")
;; (set-face-background 'hl-line "midnight blue")
(setq ring-bell-function 'ignore)
(blink-cursor-mode 0)

(defun pjr-font-scale-on-frame-width ()
  (if (< (frame-width) 76)
      (text-scale-set -1.1)
    (text-scale-set 0))
  )

(add-hook 'window-configuration-change-hook 'pjr-font-scale-on-frame-width)


(require 'xah-fly-keys)

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
(toggle-frame-maximized)

;; Custom keys

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


;;(global-set-key (kbd "M-m") 'make-without-asking)

;; movement
;; http://ergoemacs.org/emacs/emacs_useful_user_keybinding.html
;; (global-set-key (kbd "M-i") 'previous-line)
;; (global-set-key (kbd "M-j") 'backward-char)
;; (global-set-key (kbd "M-k") 'next-line)
;; (global-set-key (kbd "M-l") 'forward-char)

;; (global-set-key (kbd "M-u") 'backward-word)
;; (global-set-key (kbd "M-o") 'forward-word)

(global-set-key (kbd "<C-up>") 'xah-backward-block)
(global-set-key (kbd "<C-down>") 'xah-forward-block)

(global-set-key (kbd "C-a") 'xah-beginning-of-line-or-block)
(global-set-key (kbd "C-e") 'xah-end-of-line-or-block)
;; (define-key xah-fly-key-map (kbd "M-h") 'xah-end-of-line-or-block)

;; (global-set-key (kbd "M-6") 'xah-select-block)
;; (global-set-key (kbd "M-7") 'xah-select-line)
;; (global-set-key (kbd "M-8") 'xah-extend-selection)


;; Defalias
(defalias 'list-buffers 'ibuffer)
(defalias 'isearch-forward 'swiper)
(defalias 'query-replace 'replace-string)


;; Custom functions

(defun xah-select-block ()
  "Select the current/next block of text between blank lines.
If region is active, extend selection downward by block.

URL `http://ergoemacs.org/emacs/modernization_mark-word.html'
Version 2017-11-01"
  (interactive)
  (if (region-active-p)
      (re-search-forward "\n[ \t]*\n" nil "move")
    (progn
      (skip-chars-forward " \n\t")
      (when (re-search-backward "\n[ \t]*\n" nil "move")
        (re-search-forward "\n[ \t]*\n"))
      (push-mark (point) t t)
      (re-search-forward "\n[ \t]*\n" nil "move"))))


(defun xah-select-line ()
  "Select current line. If region is active, extend selection downward by line.
URL `http://ergoemacs.org/emacs/modernization_mark-word.html'
Version 2017-11-01"
  (interactive)
  (if (region-active-p)
      (progn
        (forward-line 1)
        (end-of-line))
    (progn
      (end-of-line)
      (set-mark (line-beginning-position)))))


(defun xah-extend-selection ()
  "Select the current word, bracket/quote expression, or expand selection.
Subsequent calls expands the selection.

when no selection,
• if cursor is on a bracket, select whole bracketed thing including bracket
• if cursor is on a quote, select whole quoted thing including quoted
• if cursor is on the beginning of line, select the line.
• else, select current word.

when there's a selection, the selection extension behavior is still experimental.
Roughly:
• if 1 line is selected, extend to next line.
• if multiple lines is selected, extend to next line.
• if a bracketed text is selected, extend to include the outer bracket. If there's no outer, select current line.

 to line, or bracket/quoted text,
or text block, whichever is the smallest.

URL `http://ergoemacs.org/emacs/modernization_mark-word.html'
Version 2017-01-15"
  (interactive)
  (if (region-active-p)
      (progn
        (let (($rb (region-beginning)) ($re (region-end)))
          (goto-char $rb)
          (cond
           ((looking-at "\\s(")
            (if (eq (nth 0 (syntax-ppss)) 0)
                (progn
                  (message "left bracket, depth 0.")
                  (end-of-line) ; select current line
                  (set-mark (line-beginning-position)))
              (progn
                (message "left bracket, depth not 0")
                (up-list -1 t t)
                (mark-sexp))))
           ((eq $rb (line-beginning-position))
            (progn
              (goto-char $rb)
              (let (($firstLineEndPos (line-end-position)))
                (cond
                 ((eq $re $firstLineEndPos)
                  (progn
                    (message "exactly 1 line. extend to next whole line." )
                    (forward-line 1)
                    (end-of-line)))
                 ((< $re $firstLineEndPos)
                  (progn
                    (message "less than 1 line. complete the line." )
                    (end-of-line)))
                 ((> $re $firstLineEndPos)
                  (progn
                    (message "beginning of line, but end is greater than 1st end of line" )
                    (goto-char $re)
                    (if (eq (point) (line-end-position))
                        (progn
                          (message "exactly multiple lines" )
                          (forward-line 1)
                          (end-of-line))
                      (progn
                        (message "multiple lines but end is not eol. make it so" )
                        (goto-char $re)
                        (end-of-line)))))
                 (t (error "logic error 42946" ))))))
           ((and (> (point) (line-beginning-position)) (<= (point) (line-end-position)))
            (progn
              (message "less than 1 line" )
              (end-of-line) ; select current line
              (set-mark (line-beginning-position))))
           (t (message "last resort" ) nil))))
    (progn
      (cond
       ((looking-at "\\s(")
        (message "left bracket")
        (mark-sexp)) ; left bracket
       ((looking-at "\\s)")
        (message "right bracket")
        (backward-up-list) (mark-sexp))
       ((looking-at "\\s\"")
        (message "string quote")
        (mark-sexp)) ; string quote
       ((and (eq (point) (line-beginning-position)) (not (looking-at "\n")))
        (message "beginning of line and not empty")
        (end-of-line)
        (set-mark (line-beginning-position)))
       ((or (looking-back "\\s_" 1) (looking-back "\\sw" 1))
        (message "left is word or symbol")
        (skip-syntax-backward "_w" )
        ;; (re-search-backward "^\\(\\sw\\|\\s_\\)" nil t)
        (mark-sexp))
       ((and (looking-at "\\s ") (looking-back "\\s " 1))
        (message "left and right both space" )
        (skip-chars-backward "\\s " ) (set-mark (point))
        (skip-chars-forward "\\s "))
       ((and (looking-at "\n") (looking-back "\n" 1))
        (message "left and right both newline")
        (skip-chars-forward "\n")
        (set-mark (point))
        (re-search-forward "\n[ \t]*\n")) ; between blank lines, select next text block
       (t (message "just mark sexp" )
          (mark-sexp))
       ;;
       ))))



(defvar xah-brackets nil "string of left/right brackets pairs.")
(setq xah-brackets "()[]{}<>（）［］｛｝⦅⦆〚〛⦃⦄“”‘’‹›«»「」〈〉《》【】〔〕⦗⦘『』〖〗〘〙｢｣⟦⟧⟨⟩⟪⟫⟮⟯⟬⟭⌈⌉⌊⌋⦇⦈⦉⦊❛❜❝❞❨❩❪❫❴❵❬❭❮❯❰❱❲❳〈〉⦑⦒⧼⧽﹙﹚﹛﹜﹝﹞⁽⁾₍₎⦋⦌⦍⦎⦏⦐⁅⁆⸢⸣⸤⸥⟅⟆⦓⦔⦕⦖⸦⸧⸨⸩｟｠⧘⧙⧚⧛⸜⸝⸌⸍⸂⸃⸄⸅⸉⸊᚛᚜༺༻༼༽⏜⏝⎴⎵⏞⏟⏠⏡﹁﹂﹃﹄︹︺︻︼︗︘︿﹀︽︾﹇﹈︷︸")

(defvar xah-left-brackets '("(" "{" "[" "<" "〔" "【" "〖" "〈" "《" "「" "『" "“" "‘" "‹" "«" )
  "List of left bracket chars.")
(progn
;; make xah-left-brackets based on xah-brackets
  (setq xah-left-brackets '())
  (dotimes ($x (- (length xah-brackets) 1))
    (when (= (% $x 2) 0)
      (push (char-to-string (elt xah-brackets $x))
            xah-left-brackets)))
  (setq xah-left-brackets (reverse xah-left-brackets)))

(defvar xah-right-brackets '(")" "]" "}" ">" "〕" "】" "〗" "〉" "》" "」" "』" "”" "’" "›" "»")
  "list of right bracket chars.")
(progn
  (setq xah-right-brackets '())
  (dotimes ($x (- (length xah-brackets) 1))
    (when (= (% $x 2) 1)
      (push (char-to-string (elt xah-brackets $x))
            xah-right-brackets)))
  (setq xah-right-brackets (reverse xah-right-brackets)))

(defun xah-backward-left-bracket ()
  "Move cursor to the previous occurrence of left bracket.
The list of brackets to jump to is defined by `xah-left-brackets'.
URL `http://ergoemacs.org/emacs/emacs_navigating_keys_for_brackets.html'
Version 2015-10-01"
  (interactive)
  (re-search-backward (regexp-opt xah-left-brackets) nil t))

(defun xah-forward-right-bracket ()
  "Move cursor to the next occurrence of right bracket.
The list of brackets to jump to is defined by `xah-right-brackets'.
URL `http://ergoemacs.org/emacs/emacs_navigating_keys_for_brackets.html'
Version 2015-10-01"
  (interactive)
  (re-search-forward (regexp-opt xah-right-brackets) nil t))

(defun xah-goto-matching-bracket ()
  "Move cursor to the matching bracket.
If cursor is not on a bracket, call `backward-up-list'.
The list of brackets to jump to is defined by `xah-left-brackets' and `xah-right-brackets'.
URL `http://ergoemacs.org/emacs/emacs_navigating_keys_for_brackets.html'
Version 2016-11-22"
  (interactive)
  (if (nth 3 (syntax-ppss))
      (backward-up-list 1 'ESCAPE-STRINGS 'NO-SYNTAX-CROSSING)
    (cond
     ((eq (char-after) ?\") (forward-sexp))
     ((eq (char-before) ?\") (backward-sexp ))
     ((looking-at (regexp-opt xah-left-brackets))
      (forward-sexp))
     ((looking-back (regexp-opt xah-right-brackets) (max (- (point) 1) 1))
      (backward-sexp))
     (t (backward-up-list 1 'ESCAPE-STRINGS 'NO-SYNTAX-CROSSING)))))

(defun xah-forward-block (&optional n)
  "Move cursor beginning of next text block.
A text block is separated by blank lines.
This command similar to `forward-paragraph', but this command's behavior is the same regardless of syntax table.
URL `http://ergoemacs.org/emacs/emacs_move_by_paragraph.html'
Version 2016-06-15"
  (interactive "p")
  (let ((n (if (null n) 1 n)))
    (re-search-forward "\n[\t\n ]*\n+" nil "NOERROR" n)))

(defun xah-backward-block (&optional n)
  "Move cursor to previous text block.
See: `xah-forward-block'
URL `http://ergoemacs.org/emacs/emacs_move_by_paragraph.html'
Version 2016-06-15"
  (interactive "p")
  (let ((n (if (null n) 1 n))
        ($i 1))
    (while (<= $i n)
      (if (re-search-backward "\n[\t\n ]*\n+" nil "NOERROR")
          (progn (skip-chars-backward "\n\t "))
        (progn (goto-char (point-min))
               (setq $i n)))
      (setq $i (1+ $i)))))
(defun xah-beginning-of-line-or-block ()
  "Move cursor to beginning of line or previous paragraph.

• When called first time, move cursor to beginning of char in current line. (if already, move to beginning of line.)
• When called again, move cursor backward by jumping over any sequence of whitespaces containing 2 blank lines.

URL `http://ergoemacs.org/emacs/emacs_keybinding_design_beginning-of-line-or-block.html'
Version 2017-05-13"
  (interactive)
  (let (($p (point)))
    (if (or (equal (point) (line-beginning-position))
            (equal last-command this-command ))
        (if (re-search-backward "\n[\t\n ]*\n+" nil "NOERROR")
            (progn
              (skip-chars-backward "\n\t ")
              (forward-char ))
          (goto-char (point-min)))
      (progn
        (back-to-indentation)
        (when (eq $p (point))
          (beginning-of-line))))))

(defun xah-end-of-line-or-block ()
  "Move cursor to end of line or next paragraph.

• When called first time, move cursor to end of line.
• When called again, move cursor forward by jumping over any sequence of whitespaces containing 2 blank lines.

URL `http://ergoemacs.org/emacs/emacs_keybinding_design_beginning-of-line-or-block.html'
Version 2017-05-30"
  (interactive)
  (if (or (equal (point) (line-end-position))
          (equal last-command this-command ))
      (progn
        (re-search-forward "\n[\t\n ]*\n+" nil "NOERROR" ))
    (end-of-line)))

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
(require 'company)
;; (require 'company-tern)
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

;; WEB-MODE
(require 'web-mode)

(add-to-list 'auto-mode-alist '("\\.erb\\'"    . web-mode))       ;; ERB
(add-to-list 'auto-mode-alist '("\\.json\\'"   . web-mode))       ;; JSON
(add-to-list 'auto-mode-alist '("\\.html?\\'"  . web-mode))       ;; Plain HTML
(add-to-list 'auto-mode-alist '("\\.js[x]?\\'" . web-mode))       ;; JS + JSX
(add-to-list 'auto-mode-alist '("\\.es6\\'"    . web-mode))       ;; ES6
(add-to-list 'auto-mode-alist '("\\.css\\'"    . web-mode))       ;; CSS
(add-to-list 'auto-mode-alist '("\\.scss\\'"   . web-mode))       ;; SCSS
(add-to-list 'auto-mode-alist '("\\.php\\'"   . web-mode))        ;; PHP
(add-to-list 'auto-mode-alist '("\\.blade\\.php\\'" . web-mode))  ;; Blade template

(setq web-mode-content-types-alist
      '(("jsx" . "\\.js[x]?\\'")
        ("javascript" . "\\.es6?\\'")))

(defadvice web-mode-highlight-part (around tweak-jsx activate)
  (if (equal web-mode-content-type "jsx")
    (let ((web-mode-enable-part-face nil))
      ad-do-it)
    ad-do-it))

;; use web-mode for .jsx files
(add-to-list 'auto-mode-alist '("\\.jsx$" . web-mode))

;; http://www.flycheck.org/manual/latest/index.html
(require 'flycheck)

;; turn on flychecking globally
(add-hook 'after-init-hook #'global-flycheck-mode)

;; disable jshint since we prefer eslint checking
(setq-default flycheck-disabled-checkers
  (append flycheck-disabled-checkers
    '(javascript-jshint)))

;; use eslint with web-mode for jsx files
(flycheck-add-mode 'javascript-eslint 'web-mode)

;; customize flycheck temp file prefix
(setq-default flycheck-temp-prefix ".flycheck")

;; disable json-jsonlist checking for json files
(setq-default flycheck-disabled-checkers
  (append flycheck-disabled-checkers
    '(json-jsonlist)))

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
(add-hook 'flycheck-mode-hook #'my/use-eslint-from-node-modules)


;; (require 'rjsx-mode)
;; (add-to-list 'auto-mode-alist '("components\\/.*\\.js\\'" . rjsx-mode))

;; Emmet
(setq emmet-expand-jsx-className? t) ;; default nil

;; TIDE
(defun setup-tide-mode ()
  (interactive)
  (tide-setup)
  (flycheck-mode +1)
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (eldoc-mode +1)
  (tide-hl-identifier-mode +1)
  ;; company is an optional dependency. You have to
  ;; install it separately via package-install
  ;; `M-x package-install [ret] company`
  (company-mode +1))

;; aligns annotation to the right hand side
;; (setq company-tooltip-align-annotations t)

;; formats the buffer before saving
(add-hook 'before-save-hook 'tide-format-before-save)

(add-hook 'typescript-mode-hook #'setup-tide-mode)

(require 'web-mode)
;; (add-to-list 'auto-mode-alist '("\\.jsx\\'" . web-mode))
(add-hook 'web-mode-hook
          (lambda ()
            (when (string-equal "jsx" (file-name-extension buffer-file-name))
              (setup-tide-mode))))
;; configure jsx-tide checker to run after your default jsx checker
(flycheck-add-mode 'javascript-eslint 'web-mode)
;; (flycheck-add-next-checker 'javascript-eslint 'jsx-tide 'append)
 ;
;; TSX
(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.tsx\\'" . web-mode))
(add-hook 'web-mode-hook
          (lambda ()
            (when (string-equal "tsx" (file-name-extension buffer-file-name))
              (setup-tide-mode))))
;; enable typescript-tslint checker
(flycheck-add-mode 'typescript-tslint 'web-mode)

(add-hook 'web-mode-hook #'setup-tide-mode)
;; configure javascript-tide checker to run after your default javascript checker
;; (flycheck-add-next-checker 'javascript-eslint 'javascript-tide 'append)
(setq-default flycheck-disabled-checkers (append flycheck-disabled-checkers '(tsx-tide)))

(defun my-web-mode-hook ()
  "Hooks for Web mode."
  (setq web-mode-script-padding 2)
  (setq web-mode-style-padding 2)
  (setq web-mode-block-padding 2)
  (setq web-mode-markup-indent-offset 2)
  ;; (setq current-coding-style 'default)
  (setq web-mode-enable-auto-pairing t)
  (setq web-mode-enable-css-colorization t)
  (setq web-mode-enable-current-element-highlight t)
  (setq web-mode-enable-auto-closing t)
  (setq web-mode-css-indent-offset 2)
  (set-face-attribute 'web-mode-css-property-name-face nil :foreground "pink2")
  ;; (set (make-local-variable 'company-backends)
       ;; '((company-css company-files)))
  (setq web-mode-code-indent-offset 2)
  (setq web-mode-attr-indent-offset 2)
  (setq web-mode-indent-style 2)
  (setq web-mode-tag-auto-close-style t)
  (setq web-mode-enable-auto-indentation t)
  (setq web-mode-enable-auto-opening t)
  (setq-default indent-tabs-mode nil)
  ;; (setq web-mode-enable-auto-quoting t)
  (setq web-mode-enable-auto-quoting nil)
  (electric-pair-mode t)
  )

;; (add-hook 'js2-mode-hook 'my-web-mode-hook)
(add-hook 'web-mode-hook 'my-web-mode-hook)
;; (add-hook 'web-mode-hook 'tern-mode t)
(add-hook 'web-mode-hook 'emmet-mode)


;; (add-hook 'web-mode-hook
;;           (lambda()
;;             (global-unset-key (kbd "M-j"))))

;; (add-hook 'web-mode-hook
;; 	  (lambda ()
;; 	    (if (equal web-mode-content-type "javascript")
;; 		(web-mode-set-content-type "jsx")
;; 	      (message "now set to: %s" web-mode-content-type))))


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
(set-face-attribute 'region nil :background "#766EC8")

;;Webmode
(set-face-attribute 'web-mode-html-tag-face 'nil :foreground "#569CD6")
(set-face-attribute 'web-mode-current-element-highlight-face 'nil :foreground "set")

;; Flycheck
(set-face-attribute 'flycheck-error nil :background "dark red" :foreground "white" :underline nil :weight 'bold)
(set-face-attribute 'flycheck-info nil :background "forest green" :foreground "burlywood3" :underline nil :weight 'bold)
(set-face-attribute 'flycheck-warning nil :background "gold" :foreground "black" :underline nil :weight 'bold)
(setq pos-tip-background-color "red")
(setq pos-tip-foreground-color "white")

;;Company
(eval-after-load 'company
  '(progn
     (set-face-attribute 'company-tooltip nil :background "#4d4d4d" :foreground "white")
     (set-face-attribute 'company-scrollbar-bg nil :background "#4d4d4d")
     (set-face-attribute 'company-scrollbar-fg nil :background "#737373")
     (set-face-attribute 'company-tooltip-annotation nil :foreground "black" :slant 'italic' :height 1.1)
     (set-face-attribute 'company-tooltip-common nil :foreground "gold" :weight 'bold' :height 1.2)
     (set-face-attribute 'company-tooltip-selection nil :background "#8080ff"))
  )

;; Dired
(eval-after-load 'dired
  '(progn    
     (set-face-attribute 'dired-directory nil :foreground "#66ccff" :height 1.2 :weight 'bold))
  )
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (xah-fly-keys yasnippet-snippets swiper smart-mode-line flycheck-pos-tip flycheck-inline dumb-jump company-irony))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
