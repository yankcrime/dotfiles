;; Startup speed tweaks / cheats
;; https://github.com/hlissner/doom-emacs/wiki/FAQ#how-is-dooms-startup-so-fast
(setq gc-cons-threshold 402653184
      gc-cons-percentage 0.6)

;;========================================
;; start the emacsserver that listens to emacsclient
(server-start)

;; User-interface stuff

(add-to-list 'default-frame-alist '(width . 110))
(scroll-bar-mode 0)
(tool-bar-mode -1)
(column-number-mode 1)
(set-face-attribute 'default nil
                    :family "Triplicate T4c"
                    :height 140
                    :width 'normal)

(setq initial-scratch-message "")
(fset 'yes-or-no-p 'y-or-n-p)

;; Custom themes outside of ELPA etc.
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")

;; Tooltips etc.
(set-face-attribute 'variable-pitch nil
                    :family "Helvetica Neue"
                    :height 140
                    :weight 'regular)

;; Kill the welcome buffer
(setq inhibit-startup-message t)

;; Highlight matching parens
(show-paren-mode)

;; Do something sensible with long lines
(set-default 'truncate-lines t)

;; Show trailing whitespace
(setq-default show-trailing-whitespace t)

;; Window title
(setq frame-title-format '(buffer-file-name "%f" ("%b")))

;; Disable auto-save and auto-backup
(setq auto-save-default nil)
(setq make-backup-files nil)

;; Use y / n instead of yes / no
(fset 'yes-or-no-p 'y-or-n-p)

;; Global keybindings, some mirroring macOS behaviour
(global-set-key (kbd "C--") 'split-window-vertically)
(global-set-key (kbd "C-\\") 'split-window-horizontally)
(global-set-key (kbd "M-w") 'kill-this-buffer)
(global-set-key (kbd "M-s") 'evil-write)
;; (global-set-key (kbd "M-v") 'evil-visual-paste)
(global-set-key (kbd "M-f") 'evil-search-forward)
(global-set-key (kbd "M-F") 'query-replace)
(global-set-key (kbd "M-\=") 'text-scale-increase)
(global-set-key (kbd "M--") 'text-scale-decrease)
(global-set-key (kbd "M-o") 'counsel-find-file)
(global-set-key (kbd "C-s") 'counsel-projectile-ag)
(global-set-key (kbd "C-b") 'ivy-switch-buffer)
(global-set-key (kbd "M-v") 'yank)
(global-set-key (kbd "M-c") 'kill-ring-save)
(global-set-key (kbd "M-a") 'mark-whole-buffer)
(global-set-key [(kbd "M-w")]
                (lambda () (interactive) (delete-window)))
(global-set-key (kbd "M-z") 'undo)

;; Package management
(require 'package)
(setq package-enable-at-startup nil)

(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/"))
(add-to-list 'package-archives '("marmalade" . "https://marmalade-repo.org/packages/"))

(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))


(setq-default tab-width 4 indent-tabs-mode nil)
(define-key global-map (kbd "RET") 'newline-and-indent)

(use-package ivy
  :ensure t
  :config
  (ivy-mode 1)
  (setq ivy-height 20)
  (define-key ivy-minibuffer-map (kbd "C-j") 'ivy-next-line)
  (define-key ivy-minibuffer-map (kbd "C-k") 'ivy-previous-line)
  (define-key ivy-minibuffer-map (kbd "<escape>") 'minibuffer-keyboard-quit)
  (define-key ivy-minibuffer-map (kbd "C-.")
  (lambda () (interactive) (insert (format "%s" (with-ivy-window (thing-at-point 'symbol))))))
  (define-key ivy-minibuffer-map (kbd "M-.")
  (lambda () (interactive) (insert (format "%s" (with-ivy-window (thing-at-point 'word))))))
  (add-hook 'minibuffer-setup-hook
            (lambda ()
              (setq show-trailing-whitespace nil)))
  (setq ivy-use-virtual-buffers t)
  (setq enable-recursive-minibuffers t))

(use-package counsel
  :ensure t)

(use-package counsel-projectile
  :ensure t)

(use-package ivy-rich
  :ensure t
  :config
  (setq ivy-virtual-abbreviate 'full
        ivy-rich-switch-buffer-align-virtual-buffer t)
  (setq ivy-rich-abbreviate-paths t)
  (ivy-set-display-transformer 'ivy-switch-buffer 'ivy-rich-switch-buffer-transformer))

(use-package org-download
  :ensure t
  :config
  (setq org-download-method 'attach))


(use-package git-gutter-fringe
  :ensure t
  :config
  (set-face-foreground 'git-gutter-fr:added "darkgreen")
  (set-face-background 'git-gutter-fr:added "#e2e2e2")
  (set-face-foreground 'git-gutter-fr:modified "blue")
  (set-face-background 'git-gutter-fr:modified "#e2e2e2")
  (require 'git-gutter)
  (require 'git-gutter-fringe))

(use-package leuven-theme
    :ensure t
    :init
    (setq leuven-scale-outline-headlines nil)
    (setq leuven-scale-org-agenda-structure nil))

(load-theme 'whiteboard)

(use-package solaire-mode
  :ensure t
  :config
  (require 'solaire-mode)
  (add-hook 'after-change-major-mode-hook #'turn-on-solaire-mode)
  (add-hook 'minibuffer-setup-hook #'solaire-mode-in-minibuffer)
  (solaire-mode-swap-bg))

;(use-package doom-themes
;  :ensure t
;  :init
;  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
;      doom-themes-enable-italic t)
;  :config
;  (doom-themes-visual-bell-config)
;  (doom-themes-org-config)
;  (load-theme 'doom-one t))

; Evil mode and related
(use-package evil
  :init
  (use-package evil-leader
    :init (global-evil-leader-mode)
    :ensure t
    :config
    (setq evil-leader/in-all-states t)
    (evil-leader/set-leader "<SPC>")
    (evil-leader/set-key
      "<SPC>" 'evil-buffer
      "b" 'ivy-switch-buffer
      "pp" 'projectile-switch-project
      "pf" 'projectile-find-file
      "m" 'magit-file-popup
      "d" 'deft
      "gg" 'magit-status
      "ga" 'magit-stage-file
      "gc" 'magit-commit
      "tl" 'org-todo-list
      "ta" 'org-agenda
      "tc" 'org-task-capture
      "o" 'delete-other-windows
      "q" 'evil-quit
      "x" 'evil-save-and-close
      "ws" 'evil-window-split
      "f" 'fzf)

  :ensure t
  :config
  (evil-mode 1)
  (define-key evil-normal-state-map (kbd "C-u") 'evil-scroll-up)
  (define-key evil-normal-state-map (kbd "C-h") 'evil-window-left)
  (define-key evil-normal-state-map (kbd "C-j") 'evil-window-down)
  (define-key evil-normal-state-map (kbd "C-k") 'evil-window-up)
  (define-key evil-normal-state-map (kbd "C-l") 'evil-window-right)
  (define-key evil-normal-state-map (kbd "Q") 'fill-paragraph)
  (define-key evil-motion-state-map ";" 'evil-ex)
  (global-set-key (kbd "M-s") 'evil-write)
  (global-set-key (kbd "M-f") 'evil-search-forward)
  (setq evil-mode-line-format '(before . mode-line-front-space))
  (setq evil-want-C-u-scroll t)
  (setq evil-symbol-word-search t)
  (setq evil-normal-state-tag " N ")
  (setq evil-insert-state-tag " I ")
  (setq evil-visual-state-tag " V ")

  (use-package evil-magit
    :ensure t)

  (use-package evil-escape
    :ensure t
    :diminish evil-escape-mode
    :init
    (evil-escape-mode))

  (use-package evil-visual-mark-mode
    :ensure t)))

;; org-mode
(use-package org
  :ensure t
  :config
  (setq org-directory "~/Dropbox/org")
  (setq org-agenda-files '("~/Dropbox/org/"))
  (defun pop-to-org-agenda (split)
    (interactive "P")
    (org-agenda-list)
    (when (not split)
      (delete-other-windows)))
  (setq org-default-notes-file (concat org-directory "/notes.org"))
  (define-key global-map (kbd "C-c c") 'org-capture)
  (define-key global-map (kbd "C-c t a") 'pop-to-org-agenda)
  (define-key global-map (kbd "C-c t l") 'org-todo-list)
  (setq org-log-done 'time)
  (setq org-adapt-indentation nil)
  (setq org-startup-indented 'true)
  (setq org-src-tab-acts-natively t)
  (setq org-refile-targets '((org-agenda-files :maxlevel . 2)))
  (setq org-capture-templates
        '(("a" "New TODO:" entry
           (file "todo.org")
           "* TODO%?
SCHEDULED: %t
:PROPERTIES:
:CREATED: %U\n
:END:")))

  (defun org-task-capture ()
    (interactive)
    (org-capture nil "a"))

  (setq org-ellipsis "•••")

  (use-package org-bullets
    :ensure t
    :config
    (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))))

(use-package ob-http
  :ensure t)

(use-package ob-ipython
  :ensure t)

(org-babel-do-load-languages
 'org-babel-load-languages
 '((shell . t)
   (http . t)
   (ipython . t)
   (ruby . t)))

(eval-after-load 'org-agenda
 '(progn
    (evil-set-initial-state 'org-agenda-mode 'normal)
    (evil-define-key 'normal org-agenda-mode-map
      (kbd "<DEL>") 'org-agenda-show-scroll-down
      (kbd "<RET>") 'org-agenda-switch-to
      (kbd "\t") 'org-agenda-goto
      "\C-n" 'org-agenda-next-line
      "\C-p" 'org-agenda-previous-line
      "\C-r" 'org-agenda-redo
      "a" 'org-agenda-archive-default-with-confirmation
      ;b
      "c" 'org-agenda-goto-calendar
      "d" 'org-agenda-day-view
      "e" 'org-agenda-set-effort
      ;f
      "g " 'org-agenda-show-and-scroll-up
      "gG" 'org-agenda-toggle-time-grid
      "gh" 'org-agenda-holidays
      "gj" 'org-agenda-goto-date
      "gJ" 'org-agenda-clock-goto
      "gk" 'org-agenda-action
      "gm" 'org-agenda-bulk-mark
      "go" 'org-agenda-open-link
      "gO" 'delete-other-windows
      "gr" 'org-agenda-redo
      "gv" 'org-agenda-view-mode-dispatch
      "gw" 'org-agenda-week-view
      "g/" 'org-agenda-filter-by-tag
      "h"  'org-agenda-earlier
      "i"  'org-agenda-diary-entry
      "j"  'org-agenda-next-line
      "k"  'org-agenda-previous-line
      "l"  'org-agenda-later
      "m" 'org-agenda-bulk-mark
      "n" nil                           ; evil-search-next
      "o" 'delete-other-windows
      ;p
      "q" 'org-agenda-quit
      "r" 'org-agenda-redo
      "s" 'org-save-all-org-buffers
      "t" 'org-agenda-todo
      "u" 'org-agenda-bulk-unmark
      ;v
      "x" 'org-agenda-exit
      "y" 'org-agenda-year-view
      "z" 'org-agenda-add-note
      "{" 'org-agenda-manipulate-query-add-re
      "}" 'org-agenda-manipulate-query-subtract-re
      "$" 'org-agenda-archive
      "%" 'org-agenda-bulk-mark-regexp
      "+" 'org-agenda-priority-up
      "," 'org-agenda-priority
      "-" 'org-agenda-priority-down
      "." 'org-agenda-goto-today
      "0" 'evil-digit-argument-or-evil-beginning-of-line
      ":" 'org-agenda-set-tags
      ";" 'org-timer-set-timer
      "<" 'org-agenda-filter-by-category
      ">" 'org-agenda-date-prompt
      "?" 'org-agenda-show-the-flagging-note
      "A" 'org-agenda-append-agenda
      "B" 'org-agenda-bulk-action
      "C" 'org-agenda-convert-date
      "D" 'org-agenda-toggle-diary
      "E" 'org-agenda-entry-text-mode
      "F" 'org-agenda-follow-mode
      ;G
      "H" 'org-agenda-holidays
      "I" 'org-agenda-clock-in
      "J" 'org-agenda-next-date-line
      "K" 'org-agenda-previous-date-line
      "L" 'org-agenda-recenter
      "M" 'org-agenda-phases-of-moon
      ;N
      "O" 'org-agenda-clock-out
      "P" 'org-agenda-show-priority
      ;Q
      "R" 'org-agenda-clockreport-mode
      "S" 'org-agenda-sunrise-sunset
      "T" 'org-agenda-show-tags
      ;U
      ;V
      ;W
      "X" 'org-agenda-clock-cancel
      ;Y
      ;Z
      "[" 'org-agenda-manipulate-query-add
      "g\\" 'org-agenda-filter-by-tag-refine
      "]" 'org-agenda-manipulate-query-subtract)))

;; mmm-mode
(use-package mmm-mode
  :ensure t)

;; mmm-jinja2
(use-package mmm-jinja2
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("\\.j2\\'" . ansible))
  (mmm-add-mode-ext-class 'jinja2-mode "\\.j2\\'" 'jinja2))

;; MultiTerm
(use-package multi-term
  :ensure t
  :config
  (setq multi-term-program "/usr/local/bin/zsh")
  (set-face-attribute 'term nil :background 'unspecified)
  (add-hook 'term-mode-hook
            (lambda ()
              (setq show-trailing-whitespace nil))))

;; Handy function to rename MultiTerm buffers
(defun rename-term (name)
  (interactive "s")
  (rename-buffer (concat "*term* " name)))

;; Nyan
(use-package nyan-mode
  :ensure t
  :config
  (setq nyan-bar-length 20)
(setq-default nyan-wavy-trail t))

;; Magit
(use-package magit
  :ensure t
  :config
  (setq magit-completing-read-function 'ivy-completing-read)
  (global-set-key (kbd "<f10>") 'magit-status))

;; Projectile
(use-package projectile
  :ensure t
  :config
  (projectile-global-mode +1)
  (setq projectile-completion-system 'ivy)
  (setq projectile-enable-caching t)
  (global-set-key (kbd "<f1>") 'projectile-switch-project)
  (global-set-key (kbd "<f2>") 'projectile-find-file))

;; Flycheck
(use-package flycheck
  :ensure t
  :config
  ;; (add-hook 'after-init-hook #'global-flycheck-mode)
  )

;; Tame window arrangement for consistency's sake
(use-package shackle
  :ensure t
  :config
  (shackle-mode 1)
  (setq shackle-rules
        `(;; Util
          ("*esup*"            :align below :size 0.4 :noselect t)
          ("*minor-modes*"     :align below :size 0.5 :noselect t)
          ("*eval*"            :align below :size 16  :noselect t)
          ;; Emacs
          ("*Pp Eval Output*"  :align below :size 0.3)
          ("*Apropos*"         :align below :size 0.3)
          ("*Backtrace*"       :align below :size 25  :noselect t)
          ("*Help*"            :align below :size 16  :select t)
          ("*Messages*"        :align below :size 15  :select t)
          ("*Warnings*"        :align below :size 10  :noselect t)
          ("*compilation*"     :align below :size 15  :noselect t)
          ("*Flycheck errors*" :align below :size 15  :noselect t)
          (compilation-mode    :align below :size 15  :noselect t)
          (eww-mode            :align below :size 30  :select t)
          ("*command-log*"     :align right :size 28  :noselect t)
          ;; vcs
          ("*magit" :align below :size 50 :select t)
          ("*vc-diff*"         :align below :size 15  :noselect t)
          ("*vc-change-log*"   :align below :size 15  :select t)
          (vc-annotate-mode    :same t))))

;; Which modes are active?
(defun which-active-modes ()
  "Give a message of which minor modes are enabled in the current buffer."
  (interactive)
  (let ((active-modes))
    (mapc (lambda (mode) (condition-case nil
                             (if (and (symbolp mode) (symbol-value mode))
                                 (add-to-list 'active-modes mode))
                           (error nil) ))
          minor-mode-list)
    (message "Active modes are %s" active-modes)))

;; Open files in dired mode using 'open'
(eval-after-load "dired"
  '(progn
     (define-key dired-mode-map (kbd "z")
       (lambda () (interactive)
         (let ((fn (dired-get-file-for-visit)))
           (start-process "default-app" nil "open" fn))))))

;; Deft
(use-package deft
  :ensure t
  :config
  (global-set-key [f3] 'deft)
  (setq deft-extensions '("org" "txt"))
  (setq deft-directory "~/Dropbox/org")
  (defun deft-enter-insert-mode ()
    ;; delay seems necessary
    (run-at-time "0.1 sec" nil 'evil-insert-state))
  (add-hook 'deft-mode-hook 'deft-enter-insert-mode)
  (setq deft-use-filename-as-title t))

(use-package exec-path-from-shell
  :ensure t)

;; Which-key - command previews
(use-package which-key
  :ensure t
  :config
  (which-key-mode))

(use-package pyenv-mode-auto
  :ensure t
  :config
  (pyenv-mode))

;; Modes

;; Golang
(use-package go-mode
  :ensure t
  :init
  (add-hook 'before-save-hook 'gofmt-before-save)
  (set (make-local-variable 'compile-command)
       "go build -v && go test -v && go vet"))

(use-package go-projectile
  :ensure t)

(use-package puppet-mode
  :ensure t
  :config
  (add-hook 'puppet-mode-hook 'flycheck-mode))

(use-package python-mode
  :ensure t
  :config
  (add-hook 'python-mode-hook 'flycheck-mode))

(use-package yaml-mode
  :ensure t
  :config
  (add-hook 'yaml-mode-hook 'flycheck-mode))

(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))

(use-package dockerfile-mode
  :ensure t)

(use-package ruby-mode
  :ensure t)

(use-package toml-mode
  :ensure t)

(use-package ansible
  :ensure t
  :config
  (add-hook 'yaml-mode-hook '(lambda () (ansible 1))))

(use-package json-mode
  :ensure t)

;; Fantastical
(defun applescript-quote-string (argument)
  "Quote a string for passing as a string to AppleScript."
  (if (or (not argument) (string-equal argument ""))
      "\"\""
    ;; Quote using double quotes, but escape any existing quotes or
    ;; backslashes in the argument with backslashes.
    (let ((result "")
          (start 0)
          end)
      (save-match-data
        (if (or (null (string-match "[^\"\\]" argument))
                (< (match-end 0) (length argument)))
            (while (string-match "[\"\\]" argument start)
              (setq end (match-beginning 0)
                    result (concat result (substring argument start end)
                                   "\\" (substring argument end (1+ end)))
                    start (1+ end))))
        (concat "\"" result (substring argument start) "\"")))))

(defun send-region-to-fantastical (beg end)
  "Send the selected region to Fantastical.
Parse the first line to create the event and use the second
and subsequent lines as the event note."
  (interactive "r")
  (let* ((region (buffer-substring-no-properties beg end))
         (match (string-match "^\\(.*\\)$" region))
         (event (substring region (match-beginning 1) (match-end 1)))
         (notes (if (< (match-end 0) (length region))
                   (concat (substring region (+ (match-end 0) 1) nil) "\n\n")
                 "")))
    (do-applescript
     (format "set theDate to current date
              set eventText to %s
              set eventNotes to %s
              set eventNotes to (eventNotes) & \"Added from Emacs on \" & (theDate as string)
              tell application \"Fantastical\"
                parse sentence (eventText) notes (eventNotes)
              end tell"
             (applescript-quote-string event)
             (applescript-quote-string notes)))))

(autoload 'send-region-to-fantastical "fantastical-capture" "Send region to Fantastical" t)
(global-set-key (kbd "C-c C-f") 'send-region-to-fantastical)

;; Rename current buffer and file
(defun rename-current-buffer-file ()
  "Renames current buffer and file it is visiting."
  (interactive)
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
        (error "Buffer '%s' is not visiting a file!" name)
      (let ((new-name (read-file-name "New name: " filename)))
        (if (get-buffer new-name)
            (error "A buffer named '%s' already exists!" new-name)
          (rename-file filename new-name 1)
          (rename-buffer new-name)
          (set-visited-file-name new-name)
          (set-buffer-modified-p nil)
          (message "File '%s' successfully renamed to '%s'"
                   name (file-name-nondirectory new-name)))))))

(global-set-key (kbd "C-x C-r") 'rename-current-buffer-file)

;; use Marked.app to preview Markdown
(setq markdown-open-command "~/bin/mark")

;; Open mail messages
(org-add-link-type "message" 'org-email-open)
  (defun org-email-open (record-location)
    (shell-command (concat "open \"message:" record-location "\"")))

;; Hide some menu junk
(define-key global-map [menu-bar tools gnus] nil)
(define-key global-map [menu-bar tools rmail] nil)
(define-key global-map [menu-bar tools compose-mail] nil)
(define-key global-map [menu-bar tools games] nil)

; Shorten major and minor mode names
(use-package dim
  :ensure t
  :config
  (dim-major-name 'emacs-lisp-mode "EL")
  (dim-major-name 'lisp-mode "L")
  (dim-major-name 'buffer "b")
  (dim-major-name 'inferior "i")
  (dim-major-name 'interaction "i")
  (dim-major-name 'interactive "i")
  (dim-major-name 'mode "mode")
  (dim-major-name 'diff "diff")
  (dim-major-name 'fundamental "fund")
  (dim-minor-name 'undo-tree-mode "")
  (dim-minor-name 'which-key-mode "")
  (dim-minor-name 'projectile-mode "")
  (dim-minor-name 'git-gutter-mode "")
  (dim-minor-name 'ivy-mode "")
  (dim-minor-name 'evil-escape-mode "")
  (dim-minor-name 'auto-revert-mode ""))

; Always wrap text in compilation windows
(add-hook 'compilation-mode-hook
          (lambda () (visual-line-mode 1)))

(add-hook 'compilation-minor-mode-hook
          (lambda () (visual-line-mode 1)))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(ansi-color-names-vector
   ["black" "red3" "ForestGreen" "yellow3" "blue" "magenta3" "DeepSkyBlue" "gray50"])
 '(column-number-mode t)
 '(custom-safe-themes
   (quote
    ("dcb9fd142d390bb289fee1d1bb49cb67ab7422cd46baddf11f5c9b7ff756f64c" "5adc266aa04b9419a6ce88b3ec9993d03e1f96d8365b2864158204fdffb36474" "42b8102c1234a9f680722953161c1127cc59ec68ad8d5c710af60d68c3b6e6ef" "a94f1a015878c5f00afab321e4fef124b2fc3b823c8ddd89d360d710fc2bddfc" "53d1bb57dadafbdebb5fbd1a57c2d53d2b4db617f3e0e05849e78a4f78df3a1b" "b5ecb5523d1a1e119dfed036e7921b4ba00ef95ac408b51d0cd1ca74870aeb14" "bffa9739ce0752a37d9b1eee78fc00ba159748f50dc328af4be661484848e476" "06f0b439b62164c6f8f84fdda32b62fb50b6d00e8b01c2208e55543a6337433a" "4bfced46dcfc40c45b076a1758ca106a947b1b6a6ff79a3281f3accacfb3243c" "5e402ccb94e32d7d09e300fb07a62dc0094bb2f16cd2ab8847b94b01b9d5e866" "9a155066ec746201156bb39f7518c1828a73d67742e11271e4f24b7b178c4710" "b8c5adfc0230bd8e8d73450c2cd4044ad7ba1d24458e37b6dec65607fc392980" "41c926d688a69c7d3c7d2eeb54b2ea3c32c49c058004483f646c1d7d1f7bf6ac" "bb749a38c5cb7d13b60fa7fc40db7eced3d00aa93654d150b9627cabd2d9b361" "44c566df0e1dfddc60621711155b1be4665dd3520b290cb354f8270ca57f8788" "43c1a8090ed19ab3c0b1490ce412f78f157d69a29828aa977dae941b994b4147" "d5f17ae86464ef63c46ed4cb322703d91e8ed5e718bf5a7beb69dd63352b26b2" "ad9747dc51ca23d1c1382fa9bd5d76e958a5bfe179784989a6a666fe801aadf2" "98cc377af705c0f2133bb6d340bf0becd08944a588804ee655809da5d8140de6" "5dc0ae2d193460de979a463b907b4b2c6d2c9c4657b2e9e66b8898d2592e3de5" "fed140fbad5134f2ca780b4507d79060cd4fcd59e6f647bbc24a9b4face10420" "b9e9ba5aeedcc5ba8be99f1cc9301f6679912910ff92fdf7980929c2fc83ab4d" "a27c00821ccfd5a78b01e4f35dc056706dd9ede09a8b90c6955ae6a390eb1c1e" "3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa" "c74e83f8aa4c78a121b52146eadb792c9facc5b1f02c917e3dbb454fca931223" "9aace541a72eb1e70a84aa08e5dd4d05678d321509b8d7bff25aa61f59e84d7d" "8ea17fc2a0a0641aa444372e328610b26d0cd6ced5dea3732f2ce94f601b4433" default)))
 '(evil-escape-mode t)
 '(fci-rule-color "#222222")
 '(hl-sexp-background-color "#efebe9")
 '(ivy-mode t)
 '(nyan-mode nil)
 '(package-selected-packages
   (quote
    (all-the-icons jbeans-theme spacemacs-theme color-theme-sanityinc-tomorrow pyenv-mode-auto jinja2-mode mmm-mode color-theme-modern company-emoji org-download ansible mmm-jinja2 counsel-projectile ivy-rich counsel ivy github-modern-theme go-projectile json-mode evil-surround yaoddmuse evil-mu4e evil-escape worf material-theme git-gutter-fringe git-gutter telephone-line which-key fzf toml-mode dockerfile-mode flymake-yaml yaml-mode markdown-mode python-mode puppet-mode go-mode exec-path-from-shell deft shackle dim projectile nyan-mode multi-term org-bullets evil-org evil-visual-mark-mode evil-magit evil-leader evil leuven-theme use-package)))
 '(pdf-view-midnight-colors (quote ("#ffffff" . "#222222")))
 '(pyenv-mode t)
 '(tramp-syntax (quote default) nil (tramp))
 '(vc-annotate-background "#222222"))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-document-title ((t (:weight bold :height 1.0 :family "Triplicate T4c")))))

(defun my/org-mode-hook ()
  "Stop the org-level headers from increasing in height relative to the other text."
  (dolist (face '(org-level-1
                  org-level-2
                  org-level-3
                  org-level-4
                  org-level-5))
    (set-face-attribute face nil :height 1.0)))

(add-hook 'org-mode-hook 'my/org-mode-hook)


;; Modeline
(use-package all-the-icons
  :ensure t
  :init
  (progn (defun -custom-modeline-github-vc ()
           (let ((branch (mapconcat 'concat (cdr (split-string vc-mode "[:-]")) "-")))
             (concat
              (propertize (format " %s" (all-the-icons-octicon "git-branch"))
                          'face `(:height 1 :family ,(all-the-icons-octicon-family))
                          'display '(raise 0))
              (propertize (format " %s" branch)))))

         (defun -custom-modeline-svn-vc ()
           (let ((revision (cadr (split-string vc-mode "-"))))
             (concat
              (propertize (format " %s" (all-the-icons-faicon "cloud"))
                          'face `(:height 1)
                          'display '(raise 0))
              (propertize (format " %s" revision) 'face `(:height 0.9)))))

         (defun simple-mode-line-render (left right)
           "Return a string of `window-width' length containing LEFT, and RIGHT aligned respectively."
           (let* ((available-width (- (window-width) (length left) 0)))
             (format (format " %%s %%%ds " available-width) left right)))

         (defvar mode-line-my-vc
           '(:propertize
             (:eval (when vc-mode
             (cond
              ((string-match "Git[:-]" vc-mode) (-custom-modeline-github-vc))
              ((string-match "SVN-" vc-mode) (-custom-modeline-svn-vc))
              (t (format "%s" vc-mode)))))
             face mode-line-directory)
           "Formats the current directory.")

         ;; (setcar mode-line-position "")
         )
  :config
  (progn (setq-default mode-line-format
                       '((:eval (simple-mode-line-render
                                 ;; left
                                 (format-mode-line (list
                                                    mode-line-mule-info
                                                    " "
                                                    mode-line-modified
                                                    "  "
                                                    mode-line-buffer-identification
                                                    mode-line-my-vc))
                                 ;; right
                                 (format-mode-line (list
                                                    mode-line-modes
                                                    " "
                                                    "ℓ %l:%c %p%%"))))))))

(set-face-attribute 'mode-line nil
                    :background "#f1f1f1"
                    :foreground "#332233"
                    :box '(:line-width 1 :color "#cccccc")
                    :overline nil
                    :underline nil)

(set-face-attribute 'mode-line-inactive nil
                    :background "#fdfdfd"
                    :foreground "#aaaaaa"
                    :box '(:line-width 1 :color "#cccccc")
                    :overline nil
                    :underline nil)

;; Startup speed tweaks / cheats
(setq gc-cons-threshold 16777216
      gc-cons-percentage 0.1)
