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

;; Activate line numbers on programming modes
(add-hook 'prog-mode-hook
          'display-line-numbers-mode)

(setq-default mode-line-format
              '("%e"
                " "
                mode-line-mule-info
                mode-line-client
                mode-line-modified
                mode-line-remote
                mode-line-frame-identification
                mode-line-buffer-identification " " mode-line-position
                (vc-mode vc-mode)
                " " mode-line-modes
                mode-line-end-spaces))

;; Better scrolling
(pixel-scroll-mode)

;; Highlight matching parens
(show-paren-mode)

;; Do something sensible with long lines
(set-default 'truncate-lines t)

(setq mode-require-final-newline t)

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

(setq use-package-compute-statistics t)

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
    (define-key ivy-minibuffer-map (kbd "M-v") 'yank)))

(use-package eyebrowse
  :config
  (setq eyebrowse-mode 1))

(use-package org-download
  :ensure t
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
  (load-theme 'doom-one-light t)
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
                      :foreground inactive-modeline-fg))

(use-package minions
  :init (minions-mode)
  :config (setq minions-direct '(cider-mode
                                 flycheck-mode
                                 overwrite-mode)))

;; Override theme background
;; Light
;;(set-background-color "#F4F4F4")

;; Dark
;; (set-background-color "#0C0C0C")

; Evil mode and related
(use-package evil
  :ensure t
  :defer .1 ;; don't block emacs when starting, load evil immediately after startup
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-search-module 'evil-search)
  (setq evil-ex-complete-emacs-commands nil)
  (setq evil-vsplit-window-right t)
  (setq evil-split-window-below t)
  (setq evil-shift-round nil)
  (setq evil-want-C-u-scroll t)
  (setq evil-mode-line-format '(before . mode-line-mule-info))
  (setq evil-normal-state-tag "N ")
  (setq evil-insert-state-tag "I ")
  (setq evil-visual-state-tag "V ")
  (setq evil-motion-state-tag "M ")
  (setq evil-operator-state-tag "O ")

  :config
  (evil-mode)
  (kill-buffer "*Messages*")

  (use-package evil-leader
    :init
    (global-evil-leader-mode)
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
      "oe" 'org-export-dispatch
      "ofa" 'org-attach
      "off" 'org-attach-attach
      "ofo" 'org-attach-open
      "oj" 'org-journal-new-entry
      "ts" 'flyspell-mode
      "wo" 'delete-other-windows
      "q" 'evil-quit
      "x" 'evil-save-and-close
      "ws" 'evil-window-split
      "wg" 'golden-ratio
      "f" 'counsel-fzf))

  ;; vim-like keybindings everywhere in emacs
  (use-package evil-collection
    :after evil
    :ensure t
    :config
    (evil-collection-init))

  ;; and in magit
  (use-package evil-magit)

  ;; gl and gL operators, like vim-lion
  (use-package evil-lion
    :ensure t
    :bind (:map evil-normal-state-map
                ("g l " . evil-lion-left)
                ("g L " . evil-lion-right)
                :map evil-visual-state-map
                ("g l " . evil-lion-left)
                ("g L " . evil-lion-right)))

  ;; gc operator, like vim-commentary
  (use-package evil-commentary
    :ensure t
    :bind (:map evil-normal-state-map
                ("gc" . evil-commentary)))

  ;; gr operator, like vim's ReplaceWithRegister
  (use-package evil-replace-with-register
    :ensure t
    :bind (:map evil-normal-state-map
                ("gr" . evil-replace-with-register)
                :map evil-visual-state-map
                ("gr" . evil-replace-with-register)))

  (use-package evil-visualstar
    :ensure t
    :bind (:map evil-visual-state-map
                ("*" . evil-visualstar/begin-search-forward)
                ("#" . evil-visualstar/begin-search-backward)))

  (use-package evil-expat
    :ensure t
    :defer t)

  ;; visual hints while editing
  (use-package evil-goggles
    :ensure t
    :config
    (evil-goggles-use-diff-faces)
    (evil-goggles-mode))

  ;; like vim-surround
  (use-package evil-surround
    :ensure t
    :commands
    (evil-surround-edit
     evil-Surround-edit
     evil-surround-region
     evil-Surround-region)
    :init
    (evil-define-key 'operator global-map "s" 'evil-surround-edit)
    (evil-define-key 'operator global-map "S" 'evil-Surround-edit)
    (evil-define-key 'visual global-map "S" 'evil-surround-region)
    (evil-define-key 'visual global-map "gS" 'evil-Surround-region)))

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
  (setq org-src-window-setup 'other-window)
  (setq org-startup-with-inline-images t)
  (setq org-image-actual-width nil)
  (setq org-export-backends (quote(
                                   md)))
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

  (eval-after-load 'org-agenda
    '(progn
       (evil-set-initial-state 'org-agenda-mode 'normal)
       (evil-define-key 'normal org-agenda-mode-map
         (kbd "<DEL>") 'org-agenda-show-scroll-down
         (kbd "<RET>") 'org-agenda-switch-to
         (kbd "\t") 'org-agenda-goto
         "\C-n" 'org-agenda-next-line
         "\C-p" 'org-agenda-previous-line
         "\C-r" 'org-agenda-redo)))

  (use-package org-journal
    :disabled
    :after org
    :config
    (setq org-journal-dir "~/Dropbox/org/journal/"))

  (use-package ob-async
    :defer t)

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
  (global-set-key (kbd "<f10>") 'magit-status))

;; Projectile
(use-package projectile
  :defer t
  :config
  (projectile-global-mode +1)
  (setq projectile-completion-system 'ivy)
  (setq projectile-enable-caching t)
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
          ("*magit*"           :align below :size 50 :select t)
          ("*evil*"            :align below :size 50 :select t)
          ("*vc-diff*"         :align below :size 15  :noselect t)
          ("*vc-change-log*"   :align below :size 15  :select t)
          ("^\\*Org Agenda.*"  :align right :regexp t :select t)
          (vc-annotate-mode    :same t))))

;; Do something about popups as well
(use-package popwin
  :config
  (popwin-mode 1))

;; Easily swap around buffers
(use-package transpose-frame)

;; Resize active frame according to 'golden ratio' principles
(use-package golden-ratio
  :config
  (golden-ratio-mode t)
  (setq golden-ratio-extra-commands
        (append golden-ratio-extra-commands
                '(evil-window-left
                  evil-window-right
                  evil-window-up
                  evil-window-down
                  select-window-1
                  select-window-2
                  select-window-3
                  select-window-4
                  select-window-5))))

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
  :ensure t
  :defer 2
  :config
  (dolist (var '("GOPATH"  "NVM_BIN"))
    (add-to-list 'exec-path-from-shell-variables var))
  (exec-path-from-shell-initialize))

;; Which-key - command previews
(use-package which-key
  :config
  (which-key-mode))

;; Flycheck
(use-package flycheck
  :defer t
  :init
  (add-hook 'json-mode-hook 'flycheck-mode)
  (add-hook 'yaml-mode-hook 'flycheck-mode))

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

(defun markdown-preview-file ()
  "run Marked on the current file and revert the buffer"
  (interactive)
  (shell-command
   (format "open -a /Applications/Marked\\ 2.app %s"
       (shell-quote-argument (buffer-file-name)))))

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

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(auto-dim-other-buffers-face ((t (:background "#ececec"))))
 '(evil-goggles-change-face ((t (:inherit diff-removed))))
 '(evil-goggles-delete-face ((t (:inherit diff-removed))))
 '(evil-goggles-paste-face ((t (:inherit diff-added))))
 '(evil-goggles-undo-redo-add-face ((t (:inherit diff-added))))
 '(evil-goggles-undo-redo-change-face ((t (:inherit diff-changed))))
 '(evil-goggles-undo-redo-remove-face ((t (:inherit diff-removed))))
 '(evil-goggles-yank-face ((t (:inherit diff-changed))))
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
(global-set-key (kbd "M-n") 'evil-buffer-new)
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
    (nyan-mode go-projectile golden-ratio transpose-frame evil-surround evil-goggles evil-expat evil-visualstar evil-replace-with-register evil-exchange evil-commentary evil-lion evil-collection htmlize ob-async ob-shell org-journal org-download go-guru flycheck go-autocomplete go-mode mac-key-mode poly-ansible git-gutter-fringe counsel-projectile projectile yaml-mode org-bullets counsel which-key exec-path-from-shell popwin shackle poly-markdown fzf evil-visual-mark-mode evil-escape evil-magit evil-leader doom-themes ranger ivy minions use-package)))
 '(use-package-always-ensure t))

(server-start)

;; Lower GC values post-initialisation
(setq gc-cons-threshold (* 5 1000 1000))
