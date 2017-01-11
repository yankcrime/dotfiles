;;========================================
;; start the emacsserver that listens to emacsclient
(server-start)

;; User-interface stuff
(add-to-list 'default-frame-alist '(width . 110))
(scroll-bar-mode 0)
(tool-bar-mode -1)
(set-face-attribute 'default nil
                    :family "Triplicate T4s"
                    :height 140
                    :weight 'normal)

;; Tooltips etc.
(set-face-attribute 'variable-pitch nil
                    :family "Helvetica Neue"
                    :height 120
                    :weight 'regular)


;; Kill the welcome buffer
(setq inhibit-startup-message t)

;; packages
(require 'package)

(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/"))
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))

(setq package-enable-at-startup nil)

;; disable auto-save and auto-backup
(setq auto-save-default nil)
(setq make-backup-files nil)

(defun ensure-package-installed (&rest packages)
  "Assure every package is installed, ask for installation if it’s not.

Return a list of installed packages or nil for every skipped package."
  (mapcar
   (lambda (package)
     (if (package-installed-p package)
         nil
       (if (y-or-n-p (format "Package %s is missing. Install it? " package))
           (package-install package)
         package)))
   packages))

;; Make sure to have downloaded archive description.
(or (file-exists-p package-user-dir)
    (package-refresh-contents))

;; Activate installed packages
(package-initialize)

(ensure-package-installed 'evil 'magit 'evil-magit 'evil-leader
                          'evil-visual-mark-mode 'helm' helm-ag 'projectile 'helm-projectile
                          'puppet-mode 'go-mode 'dim 'yaml-mode 'markdown-mode
                          'whitespace 'multi-term 'project-explorer
                          'flycheck 'highlight-indentation 'indent-guide 'shackle
                          'exec-path-from-shell 'deft)

(load-theme 'whiteboard)

(setq-default tab-width 4 indent-tabs-mode nil)
(define-key global-map (kbd "RET") 'newline-and-indent)

; evil mode and related
(setq evil-mode-line-format '(before . mode-line-front-space))
(setq evil-want-C-u-scroll t)

(require 'evil-leader)
(global-evil-leader-mode)
(evil-leader/set-leader "<SPC>")
(evil-leader/set-key "<SPC>" 'evil-buffer)
(evil-leader/set-key "b" 'helm-buffers-list)
(evil-leader/set-key "o" 'delete-other-windows)
(evil-leader/set-key "q" 'evil-quit)
(evil-leader/set-key "w" 'evil-write)
(evil-leader/set-key "x" 'evil-save-and-close)
(evil-leader/set-key "f" 'fzf)

(require 'evil)
(require 'evil-magit)
(evil-mode t)

(define-key evil-normal-state-map (kbd "C-h") 'evil-window-left)
(define-key evil-normal-state-map (kbd "C-j") 'evil-window-down)
(define-key evil-normal-state-map (kbd "C-k") 'evil-window-up)
(define-key evil-normal-state-map (kbd "C-l") 'evil-window-right)

;; org-mode
(require 'org)
(setq org-agenda-files '("~/Dropbox/org/"))
(defun pop-to-org-agenda (split)
  "Visit the org agenda, in the current window or a SPLIT."
  (interactive "P")
  (org-agenda-list)
  (when (not split)
    (delete-other-windows)))

(define-key global-map (kbd "C-c t a") 'pop-to-org-agenda)
(define-key global-map (kbd "C-c t t") 'org-todo-list)

(setq org-log-done 'time)

;; Disable startup screen
(setq inhibit-splash-screen t)

;; Show column numbers
(column-number-mode)

;; MultiTerm
(require 'multi-term)
(setq multi-term-program "/usr/local/bin/zsh")
(add-hook 'term-mode-hook
          (lambda ()
            (setq show-trailing-whitespace nil)))

;; Highlight matching parens
(show-paren-mode)

;; Do something sensible with long lines
(set-default 'truncate-lines t)

;; Show trailing whitespace
(setq-default show-trailing-whitespace t)

(setq frame-title-format '(buffer-file-name "%f" ("%b")))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("5a7830712d709a4fc128a7998b7fa963f37e960fd2e8aa75c76f692b36e6cf3c" "1263771faf6967879c3ab8b577c6c31020222ac6d3bac31f331a74275385a452" "3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa" "3d47d88c86c30150c9a993cc14c808c769dad2d4e9d0388a24fee1fbf61f0971" "3380a2766cf0590d50d6366c5a91e976bdc3c413df963a0ab9952314b4577299" "0c3b1358ea01895e56d1c0193f72559449462e5952bded28c81a8e09b53f103f" "78c1c89192e172436dbf892bd90562bc89e2cc3811b5f9506226e735a953a9c6" "bffa9739ce0752a37d9b1eee78fc00ba159748f50dc328af4be661484848e476" "c74e83f8aa4c78a121b52146eadb792c9facc5b1f02c917e3dbb454fca931223" "a27c00821ccfd5a78b01e4f35dc056706dd9ede09a8b90c6955ae6a390eb1c1e" default)))
 '(package-selected-packages
   (quote
    (deft magit-gh-pulls web-mode smart-mode-line zenburn-theme exec-path-from-shell shackle indent-guide highlight-indentation project-explorer multi-term fzf dockerfile-mode ag spacemacs-theme markdown-mode dim diminish helm-ag yaml-mode helm-projectile ## projectile puppet-mode helm flycheck evil-visual-mark-mode evil-magit)))
 '(projectile-enable-caching t)
 '(projectile-mode t nil (projectile))
 '(projectile-mode-line
   (quote
    (:eval
     (if
         (file-remote-p default-directory)
         " "
       (format " [%s]"
               (projectile-project-name))))))
 '(projectile-switch-project-action (quote helm-projectile-find-file)))

;; Projectile
;; turns on projectile mode by default for all file types
(projectile-global-mode)

;; enable caching
(setq projectile-enable-caching t)

;; asks for file to open when project is switched
(setq projectile-switch-project-action 'helm-projectile-find-file)

;; Helm
;; turns on helm bindings for projectile
(require 'helm-config)
(helm-mode 1)
(helm-projectile-on)

(defun spacemacs//hide-cursor-in-helm-buffer ()
  "Hide the cursor in helm buffers."
  (with-helm-buffer
    (setq cursor-in-non-selected-windows nil)))
(add-hook 'helm-after-initialize-hook
          'spacemacs//hide-cursor-in-helm-buffer)

;; vim-like keybindings in Helm
(define-key helm-map (kbd "C-j") 'helm-next-line)
(define-key helm-map (kbd "C-k") 'helm-previous-line)
(define-key helm-map (kbd "C-h") 'helm-next-source)
(define-key helm-map (kbd "C-S-h") 'describe-key)
(define-key helm-map (kbd "C-l") (kbd "RET"))
(define-key helm-map [escape] 'helm-keyboard-quit)
(dolist (keymap (list helm-find-files-map helm-read-file-map))
  (define-key keymap (kbd "C-l") 'helm-execute-persistent-action)
  (define-key keymap (kbd "C-h") 'helm-find-files-up-one-level)
  (define-key keymap (kbd "C-S-h") 'describe-key))

;; use Marked.app to preview Markdown
(setq markdown-open-command "~/bin/mark")

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(eval-after-load 'helm
  (lambda ()
    (set-face-attribute 'helm-source-header nil
                        :family "Triplicate T4s"
                        :slant 'italic
                        :height 160)))

;; Keybindings, some mirroring macOS behaviour
(global-set-key (kbd "<f1>") 'projectile-switch-project)
(global-set-key (kbd "<f2>") 'projectile-find-file)
(global-set-key (kbd "<f10>") 'magit-status)
(global-set-key (kbd "C--") 'split-window-vertically)
(global-set-key (kbd "C-\\") 'split-window-horizontally)
(global-set-key (kbd "M-o") 'helm-find-files)
(global-set-key (kbd "C-x s") 'helm-projectile-ag)
(global-set-key (kbd "M-s") 'evil-write)
(global-set-key (kbd "M-f") 'evil-search-forward)

; Quickly flip between buffers
; (global-set-key (kbd "M-o")  'mode-line-other-buffer)

; Kill current buffer
(global-set-key  (kbd "M-w") 'kill-this-buffer)

; Shorten major and minor mode names
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
(dim-minor-name 'auto-revert-mode "")

;; Flycheck
;; (add-hook 'after-init-hook #'global-flycheck-mode)

;; use y / n instead of yes / no
(fset 'yes-or-no-p 'y-or-n-p)

;; Tame window arrangement for consistency's sake
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
        (vc-annotate-mode    :same t)))

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
(require 'deft)
(setq deft-extensions '("txt" "tex" "org"))
(setq deft-directory "~/Dropbox/org")
(global-set-key [f3] 'deft)
(defun deft-enter-insert-mode ()
    ;; delay seems necessary
    (run-at-time "0.1 sec" nil 'evil-insert-state))
(add-hook 'deft-mode-hook 'deft-enter-insert-mode)
(setq deft-use-filename-as-title t)

;; Generic hooks
(add-hook 'puppet-mode-hook 'flycheck-mode)
(add-hook 'python-mode-hook 'flycheck-mode)
(add-hook 'go-mode-hook 'flycheck-mode)
