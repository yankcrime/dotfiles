;;========================================
;; start the emacsserver that listens to emacsclient
(server-start)

;; User-interface stuff
(add-to-list 'default-frame-alist '(width . 110))
(scroll-bar-mode 0)
(tool-bar-mode -1)
(set-face-attribute 'default nil
                    :family "Triplicate T4c"
                    :height 140)

;; Ligatures
;; (mac-auto-operator-composition-mode)

;; Tooltips etc.
(set-face-attribute 'variable-pitch nil
                    :family "Helvetica Neue"
                    :height 120
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
(global-set-key (kbd "M-o") 'helm-find-files)

;; Package management
(require 'package)
(setq package-enable-at-startup nil)

(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/"))
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))

(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(use-package leuven-theme
  :ensure t
  :config
  (load-theme 'leuven t))

;; I prefer whiteboard, but Leuven comes with a load of extra for org-mode.
;; Solution - load leuven first, then whiteboard! \o/
;; (load-theme 'whiteboard)

(setq-default tab-width 4 indent-tabs-mode nil)
(define-key global-map (kbd "RET") 'newline-and-indent)

; Evil mode and related
(use-package evil
  :ensure t
  :config
  (evil-mode 1)
  (define-key evil-normal-state-map (kbd "C-u") 'evil-scroll-up)
  (define-key evil-normal-state-map (kbd "C-h") 'evil-window-left)
  (define-key evil-normal-state-map (kbd "C-j") 'evil-window-down)
  (define-key evil-normal-state-map (kbd "C-k") 'evil-window-up)
  (define-key evil-normal-state-map (kbd "C-l") 'evil-window-right)
  (global-set-key (kbd "M-s") 'evil-write)
  (global-set-key (kbd "M-f") 'evil-search-forward)
  (setq evil-mode-line-format '(before . mode-line-front-space))
  (setq evil-want-C-u-scroll t)

  (use-package evil-leader
    :ensure t
    :config
    (global-evil-leader-mode)
    (evil-leader/set-leader "<SPC>")
    (evil-leader/set-key
      "<SPC>" 'evil-buffer
      "b" 'helm-buffers-list
      "pp" 'projectile-switch-project
      "pf" 'projectile-find-file
      "d" 'deft
      "gg" 'magit-status
      "ga" 'magit-stage-file
      "gc" 'magit-commit
      "o" 'delete-other-windows
      "q" 'evil-quit
      "w" 'evil-write
      "x" 'evil-save-and-close
      "f" 'fzf))

  (use-package evil-magit
    :ensure t)

  (use-package evil-visual-mark-mode
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
  (define-key global-map (kbd "C-c t a") 'pop-to-org-agenda)
  (define-key global-map (kbd "C-c t l") 'org-todo-list)
  (setq org-log-done 'time)
  (setq org-adapt-indentation nil)

  (setq org-ellipsis "•••")

  (use-package org-bullets
    :ensure t
    :config
    (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))


;; MultiTerm
(use-package multi-term
  :ensure t
  :config
  (setq multi-term-program "/usr/local/bin/zsh")
  (add-hook 'term-mode-hook
            (lambda ()
              (setq show-trailing-whitespace nil))))

;; Nyan
(use-package nyan-mode
  :ensure t
  :config
(setq-default nyan-wavy-trail t))

;; Magit
(use-package magit
  :ensure t
  :config
  (global-set-key (kbd "<f10>") 'magit-status))

;; Helm
(use-package helm
  :bind (:map helm-map
              ("C-j" . helm-next-line)
              ("C-k" . helm-previous-line)
              ("C-h" . helm-next-source)
              ("C-S-h" . describe-key)
              ("C-l" . "RET")
              ([escape] . helm-keyboard-quit))
  :ensure t
  :init
  (helm-mode 1)
  :config
  (global-set-key (kbd "M-o") 'helm-find-files)
  (global-set-key (kbd "M-x") 'helm-M-x)
  (defun spacemacs//hide-cursor-in-helm-buffer ()
    "Hide the cursor in helm buffers."
    (with-helm-buffer
      (setq cursor-in-non-selected-windows nil)))
  (add-hook 'helm-after-initialize-hook
            'spacemacs//hide-cursor-in-helm-buffer)

  (dolist (keymap (list helm-find-files-map helm-read-file-map))
    (define-key keymap (kbd "C-l") 'helm-execute-persistent-action)
    (define-key keymap (kbd "C-h") 'helm-find-files-up-one-level)
    (define-key keymap (kbd "C-S-h") 'describe-key))
  (lambda ()
    (set-face-attribute 'helm-source-header nil
                        :family "Triplicate T4c"
                        :slant 'italic
                        :height 140))
  (use-package helm-ag
    :ensure t))

;; Projectile
(use-package projectile
  :ensure t
  :config
  (projectile-global-mode)
  (setq projectile-enable-caching t)
  (setq projectile-switch-project-action 'helm-projectile-find-file)
  (global-set-key (kbd "<f1>") 'projectile-switch-project)
  (global-set-key (kbd "<f2>") 'projectile-find-file)

  (use-package helm-projectile
    :ensure t
    :init
    (helm-projectile-on)
    :config
    (global-set-key (kbd "C-x s") 'helm-projectile-ag)
    (progn
      (setq projectile-switch-project-action 'projectile-dired)
      (setq projectile-completion-system 'helm))))


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
  (dim-minor-name 'helm-mode "")
  (dim-minor-name 'auto-revert-mode ""))

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
          (compilation-mode    :align below :size 15  :noselect t)
          (eww-mode            :align below :size 30  :select t)
          ("*command-log*"     :align right :size 28  :noselect t)
          ;; vcs
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
  (setq deft-extensions '("txt" "tex" "org"))
  (setq deft-directory "~/Dropbox/org")
    ;; delay seems necessary
    (run-at-time "0.1 sec" nil 'evil-insert-state))
  (add-hook 'deft-mode-hook 'deft-enter-insert-mode)
  (setq deft-use-filename-as-title t))

(use-package exec-path-from-shell
  :ensure t)

;; Modes
(use-package go-mode
  :ensure t
  :config
  (add-hook 'go-mode-hook 'flycheck-mode))

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

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (toml-mode dockerfile-mode flymake-yaml yaml-mode markdown-mode python-mode puppet-mode go-mode exec-path-from-shell deft shackle dim helm-projectile projectile helm-ag helm nyan-mode multi-term org-bullets evil-org evil-visual-mark-mode evil-magit evil-leader evil leuven-theme use-package))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-document-title ((t (:foreground "black" :weight bold :height 1.2 :family "Triplicate T4c")))))
