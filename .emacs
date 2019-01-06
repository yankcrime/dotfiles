;; Startup speed tweaks
(setq gc-cons-threshold (* 5 1000 1000))

(setq user-mail-address "nick@dischord.org")

;; User-interface stuff
(add-to-list 'default-frame-alist '(width . 110))
(scroll-bar-mode 0)
(tool-bar-mode -1)
(column-number-mode 1)
(set-face-attribute 'default nil
                    :family "IBM Plex Mono"
                    :height 130
                    :width 'normal)

;; Hide some menu junk
(define-key global-map [menu-bar tools gnus] nil)
(define-key global-map [menu-bar tools rmail] nil)
(define-key global-map [menu-bar tools compose-mail] nil)
(define-key global-map [menu-bar tools games] nil)

(setq initial-scratch-message "")
(fset 'yes-or-no-p 'y-or-n-p)

;; Tooltips etc.
(set-face-attribute 'variable-pitch nil
                    :family "Helvetica Neue"
                    :height 140
                    :weight 'regular)

;; Minimal startup
(setq inhibit-startup-message t)
(setq inhibit-splash-screen t)
(setq initial-scratch-message nil)

;; Shut up
(setq ring-bell-function 'ignore)

;; Buffer switching
(winner-mode t)

;; Better scrolling
(pixel-scroll-mode)

;; Hack
(setq max-lisp-eval-depth 10000)
(setq max-specpdl-size 1000)

;; Titlebar on macOS
;;(add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
;;(add-to-list 'default-frame-alist '(ns-appearance . light))

;; Highlight matching parens
(show-paren-mode)

;; Do something sensible with long lines
(set-default 'truncate-lines t)

;; Show trailing whitespace
;; (setq-default show-trailing-whitespace t)

;; Window title
(setq frame-title-format '(buffer-file-name "%f" ("%b")))

;; Disable auto-save and auto-backup
(setq auto-save-default nil
  make-backup-files nil)

;; Use y / n instead of yes / no
(fset 'yes-or-no-p 'y-or-n-p)

(setq-default tab-width 4 indent-tabs-mode nil)
(define-key global-map (kbd "RET") 'newline-and-indent)

;; Package management
(require 'package)
(setq package-enable-at-startup nil)
(package-initialize)

(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/"))

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

;; Always ensure packages are installed
(customize-set-variable 'use-package-always-ensure t)

;; Mode-line
(setq mode-line-percent-position '(-3 "%o"))

(defun simple-mode-line-render (left right)
  "Return a string of `window-width' length containing LEFT, and RIGHT aligned respectively."
  (let* ((available-width (- (window-width) (length left) -0)))
    (format (format " %%s %%%ds " available-width) left right)))

(setq-default mode-line-buffer-identification
              (list (propertize "%12b" 'face
                                (list :weight 'bold))))

(setq-default mode-line-format
                     '(:eval (simple-mode-line-render
                              ;; left
                              (format-mode-line (list
                                                 mode-line-modified
                                                 " "
                                                 "%10b"
                                                 " "
                                                 mode-line-modes
                                                 vc-mode))
                              ;; right
                              (format-mode-line (list
                                                 "ℓ %l:%c %p%%")))))


(use-package minions
  :init (minions-mode)
  :config (setq minions-direct '(cider-mode
                                 overwrite-mode)))

;; Dim inactive buffers
(use-package auto-dim-other-buffers
  :config
  (add-hook 'after-init-hook (lambda ()
                               (when (fboundp 'auto-dim-other-buffers-mode)
                                 (auto-dim-other-buffers-mode t)))))

;; Completion framework
(use-package counsel
  :after ivy
  :demand t)

(use-package counsel-projectile
  :defer t)

;; Completion frontend
(use-package ivy
  :demand t
  :config
  (ivy-mode 1)
  (setq ivy-height 20
        ivy-use-virtual-buffers t
        enable-recursive-minibuffers t)
  (define-key ivy-minibuffer-map (kbd "C-j") 'ivy-next-line)
  (define-key ivy-minibuffer-map (kbd "C-k") 'ivy-previous-line)
  (define-key ivy-minibuffer-map (kbd "<escape>") 'minibuffer-keyboard-quit)
  (with-eval-after-load 'ivy
    (define-key ivy-minibuffer-map (kbd "M-v") 'yank))
  (add-hook 'minibuffer-setup-hook
            (lambda ()
              (setq show-trailing-whitespace nil))))

(use-package eyebrowse
  :config
  (setq eyebrowse-mode 1))

(use-package org-download
  :defer t
  :config
  (setq org-download-method 'attach))

(use-package tramp
  :defer t
  :init
  (use-package tramp-theme
    :defer t)
  (use-package vagrant-tramp
    :defer t)
  :config
  (setq tramp-default-method "ssh"))

(use-package ein
  :defer t
  :commands (ein:notebooklist-open))

(use-package elpy
  :defer t
  :disabled
  :init
  (with-eval-after-load 'python
    (elpy-enable)
    (elpy-use-ipython)
    (delete 'elpy-module-highlight-indentation elpy-modules)))

(use-package ranger
  :defer t
  :config
  (define-key ranger-mode-map (kbd "-") 'ranger-up-directory)
  (ranger-override-dired-mode t))

(use-package git-gutter-fringe
  :defer t
  :config
  (set-face-foreground 'git-gutter-fr:added "darkgreen")
  (set-face-background 'git-gutter-fr:added "#e2e2e2")
  (set-face-foreground 'git-gutter-fr:modified "blue")
  (set-face-background 'git-gutter-fr:modified "#e2e2e2")
  (require 'git-gutter)
  (require 'git-gutter-fringe))

(use-package doom-themes
  :init
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t)
  :config
  (doom-themes-org-config)
  (load-theme 'doom-one-light t))

;; Override and set a light background
(set-background-color "#F4F4F4")

; Evil mode and related
(use-package evil
  :init
  (use-package evil-leader
    :init (global-evil-leader-mode)
    :config
    (setq evil-leader/in-all-states t)
    (evil-leader/set-leader "<SPC>")
    (evil-leader/set-key
      "<SPC>" 'evil-buffer
      "b" 'ivy-switch-buffer
      "pp" 'counsel-projectile-switch-project
      "pf" 'counsel-projectile-find-file
      "ps" 'counsel-projectile-ag
      "m" 'magit-file-popup
      "d" 'deft
      "gs" 'magit-status
      "ga" 'magit-stage-file
      "gc" 'magit-commit
      "gp" 'magit-push
      "ol" 'org-todo-list
      "oa" 'org-agenda
      "oc" 'org-task-capture
      "oj" 'org-journal-new-entry
      "ts" 'flyspell-mode
      "wo" 'delete-other-windows
      "q" 'evil-quit
      "x" 'evil-save-and-close
      "ws" 'evil-window-split
      "f" 'counsel-fzf)
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
  (setq evil-want-C-u-scroll t)
  (setq evil-symbol-word-search t)

  (use-package evil-magit)

  (use-package evil-escape
    :commands
    (evil-escape-pre-command-hook)
    :hook (pre-command . evil-escape-pre-command-hook))

  (add-hook 'evil-command-window-hook
            (lambda ()
              (setq show-trailing-whitespace nil)))

  (use-package evil-visual-mark-mode)))

(use-package yasnippet
  :defer t
  :config
  (unless yas-global-mode (yas-global-mode 1))
  (yas-minor-mode 1)
  (use-package yasnippet-snippets
  :defer t))

(use-package fzf
  :defer t)

;; org-mode
(use-package org
  :defer t
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
  (define-key global-map (kbd "C-c l") 'org-store-link)
  (define-key global-map (kbd "C-c t a") 'pop-to-org-agenda)
  (define-key global-map (kbd "C-c t l") 'org-todo-list)
  (setq org-log-done 'time)
  (setq org-adapt-indentation nil)
  (setq org-startup-indented 'true)
  (setq org-src-tab-acts-natively t)
  (setq org-startup-with-inline-images t)
  (setq org-image-actual-width nil)
  (add-hook
   'org-babel-after-execute-hook
   (lambda ()
     (when org-inline-image-overlays
       (org-redisplay-inline-images))))
  (setq org-refile-targets '((org-agenda-files :maxlevel . 2)))
  (setq org-capture-templates
        '(("a" "New TODO:" entry
           (file "todo.org")
           "* TODO%?
SCHEDULED: %t
:PROPERTIES:
:CREATED: %U\n
:END:")))

  (use-package org-journal
    :after org
    :config
    (setq org-journal-dir "~/Dropbox/org/journal/"))

  (use-package ob-async
    :defer t)

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
         "c" 'org-agenda-goto-calendar
         "d" 'org-agenda-day-view
         "e" 'org-agenda-set-effort
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
         "n" nil
         "o" 'delete-other-windows
         "q" 'org-agenda-quit
         "r" 'org-agenda-redo
         "s" 'org-save-all-org-buffers
         "t" 'org-agenda-todo
         "u" 'org-agenda-bulk-unmark
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
         "H" 'org-agenda-holidays
         "I" 'org-agenda-clock-in
         "J" 'org-agenda-next-date-line
         "K" 'org-agenda-previous-date-line
         "L" 'org-agenda-recenter
         "M" 'org-agenda-phases-of-moon
         "O" 'org-agenda-clock-out
         "P" 'org-agenda-show-priority
         "R" 'org-agenda-clockreport-mode
         "S" 'org-agenda-sunrise-sunset
         "T" 'org-agenda-show-tags
         "X" 'org-agenda-clock-cancel
         "[" 'org-agenda-manipulate-query-add
         "g\\" 'org-agenda-filter-by-tag-refine
         "]" 'org-agenda-manipulate-query-subtract)))

  (defun org-task-capture ()
    (interactive)
    (org-capture nil "a"))

  ;; Open mail messages
  (org-add-link-type "message" 'org-email-open)

  (defun turn-on-visual-line-mode () (visual-line-mode 1))
  (add-hook 'org-mode-hook 'turn-on-visual-line-mode)

  (defun org-email-open (record-location)
    (shell-command (concat "open \"message:" record-location "\"")))

  (add-hook 'org-mode-hook 'turn-on-flyspell)

  (use-package org-download
    :defer t
    :config
    (setq-default org-download-method 'attach)
    (setq-default org-download-image-dir "~/Dropbox/org/files")
    (add-hook 'dired-mode-hook 'org-download-enable))

  (org-babel-do-load-languages
   'org-babel-load-languages
   '((shell . t)
     (python . t)
     (emacs-lisp . t)
     ))

  ;; Appearance stuff
  (setq org-ellipsis "•••")

  (use-package org-bullets
    :defer t
    :hook (org-mode . org-bullets-mode)))

(use-package polymode)

(use-package poly-markdown
  :config
  (add-to-list 'auto-mode-alist '("\\.md" . poly-markdown-mode)))

(use-package poly-ansible
  :config
  (add-to-list 'auto-mode-alist '("\\.j2" . poly-ansible-mode)))

;; Magit
(use-package magit
  :defer t
  :init
  (setq magit-completing-read-function 'ivy-completing-read)
  (setq magit-revision-show-gravatars '("^Author:     " . "^Commit:     "))
  :config
  (add-hook 'magit-popup-mode-hook
            (lambda ()
              (setq show-trailing-whitespace nil)))
  (global-set-key (kbd "<f10>") 'magit-status))

(add-hook 'calendar-initial-window-hook
(lambda ()
  (setq show-trailing-whitespace nil)))

;; Projectile
(use-package projectile
  :defer t
  :config
  (projectile-global-mode +1)
  (setq projectile-completion-system 'ivy)
  (setq projectile-enable-caching t)
  (setq projectile-mode-line
      '(:eval
        (if (file-remote-p default-directory)
            " Projectile[*remote*]"
          (format " Projectile[%s]" (projectile-project-name)))))
  (global-set-key (kbd "<f1>") 'projectile-switch-project)
  (global-set-key (kbd "<f2>") 'projectile-find-file))

;; Attempt to tame layout consistency of various windows which popup
(use-package shackle
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
          (compilation-mode    :align below :size 15  :noselect t)
          (eww-mode            :align below :size 30  :select t)
          ("*command-log*"     :align right :size 28  :noselect t)
          ("*magit*"            :align below :size 50 :select t)
          ("*evil*"            :align below :size 50 :select t)
          ("*vc-diff*"         :align below :size 15  :noselect t)
          ("*vc-change-log*"   :align below :size 15  :select t)
          (vc-annotate-mode    :same t))))

;; Do something about popups as well
(use-package popwin
  :config
  (popwin-mode 1))

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
     ;; Make Dired a bit more like vim's dirvish
     (define-key dired-mode-map "-"
         (lambda ()
           (interactive)
           (find-alternate-file "..")))
     (define-key dired-mode-map (kbd "z")
       (lambda () (interactive)
         (let ((fn (dired-get-file-for-visit)))
           (start-process "default-app" nil "open" fn))))))

;; Deft
(use-package deft
  :defer t
  :config
  (setq deft-extensions '("org" "txt"))
  (setq deft-directory "~/Dropbox/org")
  (setq deft-text-mode 'org-mode)
  (setq deft-use-filename-as-title t)
  (setq deft-use-filter-string-for-filename t)
  (setq deft-default-extension "org")
  (defun deft-enter-insert-mode ()
    ;; delay seems necessary
    (run-at-time "0.1 sec" nil 'evil-insert-state))
  (add-hook 'deft-mode-hook 'deft-enter-insert-mode))

(use-package exec-path-from-shell
  :config
  (when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize)))

;; Which-key - command previews
(use-package which-key
  :config
  (which-key-mode))

;; Modes

;; Golang
(use-package go-mode
  :defer t
  :init
  (set (make-local-variable 'compile-command)
       "go build -v && go test -v && go vet")
  :config
  ;; Define function to call when go-mode loads
  (defun my-go-mode-hook ()
  (add-hook 'before-save-hook 'gofmt-before-save) ; gofmt before every save
  (setq gofmt-command "goimports")                ; gofmt uses invokes goimports
  (if (not (string-match "go" compile-command))   ; set compile command default
      (set (make-local-variable 'compile-command)
           "go build -v && go test -v && go vet"))

  ;; guru settings
  (go-guru-hl-identifier-mode)                    ; highlight identifiers

  ;; Key bindings specific to go-mode
  (local-set-key (kbd "M-.") 'godef-jump)         ; Go to definition
  (local-set-key (kbd "M-*") 'pop-tag-mark)       ; Return from whence you came
  (local-set-key (kbd "M-p") 'compile)            ; Invoke compiler
  (local-set-key (kbd "M-P") 'recompile)          ; Redo most recent compile cmd
  (local-set-key (kbd "M-]") 'next-error)         ; Go to next error (or msg)
  (local-set-key (kbd "M-[") 'previous-error)     ; Go to previous error or msg

  ;; Ensure the go specific autocomplete is active in go-mode.
  (with-eval-after-load 'go-mode
    (require 'go-autocomplete)))

  (use-package go-autocomplete
    :defer t)

  (use-package go-projectile
    :defer t)

  (use-package go-guru
    :defer t))

(use-package slime
  :defer t
  :config
  (setq inferior-lisp-program "/usr/local/bin/sbcl")
  (setq slime-contribs '(slime-fancy)))

(use-package yaml-mode
  :defer t)

(use-package markdown-mode
  :defer t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))

(use-package dockerfile-mode
  :defer t)

(use-package ruby-mode
  :defer t)

(use-package toml-mode
  :defer t)

(use-package ansible
  :defer t
  :config
  (add-hook 'yaml-mode-hook '(lambda () (ansible 1))))

(use-package json-mode
  :defer t)

(use-package terraform-mode
  :defer t)

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

; Always wrap text in compilation windows
(add-hook 'compilation-mode-hook
          (lambda () (visual-line-mode 1)))

(add-hook 'compilation-minor-mode-hook
          (lambda () (visual-line-mode 1)))

;; Appeareance-related overrides
(defun my/org-mode-hook ()
  "Stop the org-level headers from increasing in height relative to the other text."
  (dolist (face '(org-level-1
                  org-level-2
                  org-level-3
                  org-level-4
                  org-level-5))
    (set-face-attribute face nil :height 1.0)))

(add-hook 'org-mode-hook 'my/org-mode-hook)

;; Mode-line colouring
(defvar active-modeline-bg "#e9e9e9")
(defvar active-modeline-fg "#332233")
(defvar inactive-modeline-fg "#777777")
(defvar inactive-modeline-bg "#c6c6c6")

(set-face-attribute 'mode-line nil
                    :background active-modeline-bg
                    :foreground active-modeline-fg
                    :overline "#cccccc")

(set-face-attribute 'mode-line-inactive nil
                    :background inactive-modeline-bg
                    :foreground inactive-modeline-fg)

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(auto-dim-other-buffers-face ((t (:background "#ececec"))))
 '(magit-mode-line-process ((t (:foreground "MediumBlue"))))
 '(org-document-title ((t (:weight bold :height 1.0 :family "IBM Plex Mono")))))

;; Global keybinding overrides, some mirroring macOS behaviour
(global-set-key (kbd "C--") 'split-window-vertically)
(global-set-key (kbd "C-\\") 'split-window-horizontally)
(global-set-key (kbd "M-w") 'kill-this-buffer)
(global-set-key (kbd "M-s") 'evil-write)
(global-set-key (kbd "M-f") 'evil-search-forward)
(global-set-key (kbd "M-F") 'query-replace)
(global-set-key (kbd "M-\=") 'text-scale-increase)
(global-set-key (kbd "M--") 'text-scale-decrease)
(global-set-key (kbd "M-o") 'counsel-find-file)
(global-set-key (kbd "C-s") 'counsel-projectile-ag)
(global-set-key (kbd "C-,") 'counsel-imenu)
(global-set-key (kbd "M-a") 'mark-whole-buffer)
(global-set-key [(kbd "M-w")]
                (lambda () (interactive) (delete-window)))
(global-set-key (kbd "M-z") 'undo)
(global-set-key (kbd "M-N") 'make-frame-command)
(global-set-key (kbd "M-W") 'delete-frame)
(global-set-key (kbd "M-v") 'yank)
(global-set-key (kbd "M-c") 'kill-ring-save)
(global-set-key (kbd "M-h") 'ns-do-hide-emacs)
(global-set-key (kbd "M-`") 'ns-next-frame)

;; Ensure we're using ⌘ as Meta
(setq mac-option-modifier nil
      mac-command-modifier 'meta)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(max-specpdl-size 10000)
 '(package-selected-packages
   (quote
    (htmlize ob-async ob-shell org-journal org-download go-guru flycheck go-autocomplete go-mode nyan-mode mac-key-mode poly-ansible git-gutter-fringe counsel-projectile projectile yaml-mode org-bullets counsel which-key exec-path-from-shell popwin shackle poly-markdown fzf evil-visual-mark-mode evil-escape evil-magit evil-leader doom-themes ranger ivy minions use-package))))

(server-start)

;; Lower GC values post-initialisation
(setq gc-cons-threshold (* 5 1000 1000))
