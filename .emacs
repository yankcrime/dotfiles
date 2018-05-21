;; Startup speed tweaks / cheats
;; https://github.com/hlissner/doom-emacs/wiki/FAQ#how-is-dooms-startup-so-fast
(setq gc-cons-threshold 402653184
      gc-cons-percentage 0.6)

(setq user-mail-address "nick@dischord.org")

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
(global-set-key (kbd "M-f") 'evil-search-forward)
(global-set-key (kbd "M-F") 'query-replace)
(global-set-key (kbd "M-\=") 'text-scale-increase)
(global-set-key (kbd "M--") 'text-scale-decrease)
(global-set-key (kbd "M-o") 'counsel-find-file)
(global-set-key (kbd "C-s") 'counsel-projectile-ag)
(global-set-key (kbd "M-v") 'yank)
(global-set-key (kbd "M-c") 'kill-ring-save)
(global-set-key (kbd "M-a") 'mark-whole-buffer)
(global-set-key [(kbd "M-w")]
                (lambda () (interactive) (delete-window)))
(global-set-key (kbd "M-z") 'undo)

(setq-default tab-width 4 indent-tabs-mode nil)
(define-key global-map (kbd "RET") 'newline-and-indent)


;; Package management
(require 'package)
(setq package-enable-at-startup nil)
(package-initialize)

(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/"))
(add-to-list 'package-archives '("marmalade" . "https://marmalade-repo.org/packages/"))

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

;; Modeline

(defun simple-mode-line-render (left right)
  "Return a string of `window-width' length containing LEFT, and RIGHT aligned respectively."
  (let* ((available-width (- (window-width) (length left) 0)))
    (format (format " %%s %%%ds " available-width) left right)))

(progn (setq-default mode-line-format
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
                                                 "ℓ %l:%c %p%%"))))))

(use-package ivy
  :ensure t
  :config
  (ivy-mode 1)
  (setq ivy-height 20)
  (define-key ivy-minibuffer-map (kbd "C-j") 'ivy-next-line)
  (define-key ivy-minibuffer-map (kbd "C-k") 'ivy-previous-line)
  (define-key ivy-minibuffer-map (kbd "<escape>") 'minibuffer-keyboard-quit)
  (define-key ivy-minibuffer-map (kbd "C-.")
    (lambda ()
      (interactive)
      (insert (format "%s" (with-ivy-window (thing-at-point 'symbol))))))
  (define-key ivy-minibuffer-map (kbd "M-.")
    (lambda ()
      (interactive)
      (insert (format "%s" (with-ivy-window (thing-at-point 'word))))))
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

(use-package company
  :ensure t)

(use-package org-download
  :ensure t
  :config
  (setq org-download-method 'attach))

(use-package tramp
  :ensure nil
  :init
  (use-package tramp-theme
    :ensure t)
  (use-package vagrant-tramp
    :ensure t)
  :config
  (add-to-list 'tramp-default-proxies-alist
               '("stack@dev-director" nil "/ssh:ilab-gate:"))
  (setq tramp-default-method "ssh"))

(use-package ein
  :ensure t
  :commands (ein:notebooklist-open))

(use-package elpy
  :ensure t
  :disabled
  :init
  (with-eval-after-load 'python
    (flycheck-mode)
    (elpy-enable)
    (elpy-use-ipython)
    (delete 'elpy-module-highlight-indentation elpy-modules)))

(use-package ranger
  :ensure t
  :config
  (ranger-override-dired-mode t))

(use-package git-gutter-fringe
  :ensure t
  :config
  (set-face-foreground 'git-gutter-fr:added "darkgreen")
  (set-face-background 'git-gutter-fr:added "#e2e2e2")
  (set-face-foreground 'git-gutter-fr:modified "blue")
  (set-face-background 'git-gutter-fr:modified "#e2e2e2")
  (require 'git-gutter)
  (require 'git-gutter-fringe))

(use-package doom-themes
  :ensure t
  :init
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t)
  :config
  (doom-themes-org-config)
  (load-theme 'doom-one-light t))

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
      "pp" 'counsel-projectile-switch-project
      "pf" 'counsel-projectile-find-file
      "ps" 'counsel-projectile-ag
      "m" 'magit-file-popup
      "d" 'deft
      "gs" 'magit-status
      "ga" 'magit-stage-file
      "gc" 'magit-commit
      "gp" 'magit-push
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
  (setq evil-want-C-u-scroll t)
  (setq evil-symbol-word-search t)
  (setq evil-normal-state-tag " N ")
  (setq evil-insert-state-tag " I ")
  (setq evil-visual-state-tag " V ")

  (use-package evil-magit
    :ensure t)

  (use-package evil-escape
    :ensure t
    :commands
    (evil-escape-pre-command-hook)
    :init
    (add-hook 'pre-command-hook 'evil-escape-pre-command-hook))

  (use-package evil-visual-mark-mode
    :ensure t)))

(use-package yasnippet
  :ensure t
  :config
  (unless yas-global-mode (yas-global-mode 1))
  (yas-minor-mode 1)
  (use-package yasnippet-snippets
  :ensure t))

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
  (define-key global-map (kbd "C-c l") 'org-store-link)
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

  (use-package ob-async
    :defer t)

  (use-package ob-python
    :defer t
    :ensure org-plus-contrib
    :commands (org-babel-execute:python))

  (use-package ob-shell
    :defer t
    :ensure org-plus-contrib
    :commands
    (org-babel-execute:sh
     org-babel-expand-body:sh

     org-babel-execute:bash
     org-babel-expand-body:bash))

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

  ;; Appearance stuff
  (setq org-ellipsis "•••")

  (use-package org-bullets
    :ensure t
    :config
    (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))))

;; mmm-mode
(use-package mmm-mode
  :ensure t
  :config
  (setq mmm-global-mode 'maybe))

;; mmm-jinja2
(use-package mmm-jinja2
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("Dockerfile.j2" . dockerfile-mode))
  (mmm-add-mode-ext-class 'dockerfile-mode "Dockerfile.j2" 'jinja2))

;; MultiTerm
(use-package multi-term
  :ensure t
  :config
  (setq multi-term-program "/bin/bash")
  (set-face-attribute 'term nil :background 'unspecified)
  (add-hook 'term-mode-hook
            (lambda ()
              (setq show-trailing-whitespace nil))))

;; Handy function to rename MultiTerm buffers
(defun rename-term (name)
  (interactive "s")
  (rename-buffer (concat "*term* " name)))

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
  (setq projectile-mode-line
      '(:eval
        (if (file-remote-p default-directory)
            " Projectile[*remote*]"
          (format " Projectile[%s]" (projectile-project-name)))))
  (global-set-key (kbd "<f1>") 'projectile-switch-project)
  (global-set-key (kbd "<f2>") 'projectile-find-file))

;; Flycheck
(use-package flycheck
  :ensure t
  :config
  ;; (add-hook 'after-init-hook #'global-flycheck-mode)
  (add-hook 'python-mode-hook 'flycheck-mode)
  (setq-default flycheck-flake8-maximum-line-length 110))

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
          ("*magit*"            :align below :size 50 :select t)
          ("*vc-diff*"         :align below :size 15  :noselect t)
          ("*vc-change-log*"   :align below :size 15  :select t)
          (vc-annotate-mode    :same t))))

;; Do something about popups as well
(use-package popwin
  :ensure t
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
  :ensure t
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
  :ensure t)

;; Which-key - command previews
(use-package which-key
  :ensure t
  :config
  (which-key-mode))

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

(use-package slime
  :ensure t
  :config
  (setq inferior-lisp-program "/usr/local/bin/sbcl")
  (setq slime-contribs '(slime-fancy)))

(use-package puppet-mode
  :ensure t
  :config
  (add-hook 'puppet-mode-hook 'flycheck-mode))

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

(use-package jinja2-mode
  :ensure t
  :config
  (add-hook 'jinja2-mode-hook 'jinja2-mode))

(use-package json-mode
  :ensure t
  :config
  (add-hook 'json-mode-hook 'flycheck-mode))

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
  (dim-major-name 'lisp-mode "")
  (dim-major-name 'buffer "b")
  (dim-major-name 'inferior "i")
  (dim-major-name 'interaction "i")
  (dim-major-name 'interactive "i")
  (dim-major-name 'mode "mode")
  (dim-major-name 'diff "diff")
  (dim-major-name 'fundamental "fund")
  (dim-major-name 'json-mode "")
  (dim-major-name 'python-mode "")
  (dim-major-name 'ruby-mode "")
  (dim-major-name 'gfm-mode "")
  (dim-minor-name 'eldoc-mode "")
  (dim-minor-name 'global-eldoc-mode "")
  (dim-minor-name 'yas-global-mode "")
  (dim-minor-name 'yas-minor-mode "")
  (dim-minor-name 'company-mode "")
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
    ("c74e83f8aa4c78a121b52146eadb792c9facc5b1f02c917e3dbb454fca931223" default)))
 '(fci-rule-color "#222222")
 '(hl-sexp-background-color "#efebe9")
 '(ivy-mode t)
 '(package-selected-packages
   (quote
    (slime ranger all-the-icons-ivy vagrant-tramp company yasnippet-snippets yasnippet tramp-theme doom-themes ein popwin spaceline jinja2-mode mmm-mode color-theme-modern company-emoji org-download ansible mmm-jinja2 counsel-projectile ivy-rich counsel ivy github-modern-theme go-projectile json-mode evil-surround yaoddmuse evil-mu4e evil-escape worf material-theme git-gutter-fringe git-gutter telephone-line which-key fzf toml-mode dockerfile-mode flymake-yaml yaml-mode markdown-mode puppet-mode go-mode exec-path-from-shell deft shackle dim projectile multi-term org-bullets evil-org evil-visual-mark-mode evil-magit evil-leader evil leuven-theme use-package)))
 '(pdf-view-midnight-colors (quote ("#ffffff" . "#222222")))
 '(pyenv-mode t)
 '(tramp-default-method "ssh" nil (tramp))
 '(vc-annotate-background "#222222"))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(magit-mode-line-process ((t (:foreground "spring green"))))
 '(org-document-title ((t (:weight bold :height 1.0 :family "Triplicate T4c")))))

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

(set-face-attribute 'mode-line nil
                    :background "#e9e9e9"
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
