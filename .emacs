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

(load-theme 'whiteboard)

;; packages
(require 'package)

(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/"))
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))

(setq package-enable-at-startup nil)

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

(ensure-package-installed 'evil 'magit 'helm 'evil-magit 'projectile
			  'evil-leader 'evil-visual-mark-mode 'helm-ag 'helm-projectile
			  'puppet-mode 'flycheck 'go-mode 'dim 'yaml-mode
			  'markdown-mode 'whitespace)

(setq-default tab-width 4 indent-tabs-mode nil)
(define-key global-map (kbd "RET") 'newline-and-indent)

; evil mode and related
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

(require 'evil)
(require 'evil-magit)
(evil-mode t)

(define-key evil-normal-state-map (kbd "C-h") 'evil-window-left)
(define-key evil-normal-state-map (kbd "C-j") 'evil-window-down)
(define-key evil-normal-state-map (kbd "C-k") 'evil-window-up)
(define-key evil-normal-state-map (kbd "C-l") 'evil-window-right)

;; org-mode
(require 'org)

;; Disable startup screen
(setq inhibit-splash-screen t)

;; Show column numbers
(column-number-mode)

;; Highlight matching parens
(show-paren-mode)

;; Do something sensible with long lines
(set-default 'truncate-lines t)

;; Show trailing whitespace
(setq-default show-trailing-whitespace t)

(setq frame-title-format "%b")

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("78c1c89192e172436dbf892bd90562bc89e2cc3811b5f9506226e735a953a9c6" "bffa9739ce0752a37d9b1eee78fc00ba159748f50dc328af4be661484848e476" "c74e83f8aa4c78a121b52146eadb792c9facc5b1f02c917e3dbb454fca931223" "a27c00821ccfd5a78b01e4f35dc056706dd9ede09a8b90c6955ae6a390eb1c1e" default)))
 '(package-selected-packages
   (quote
    (ag spacemacs-theme markdown-mode dim diminish helm-ag yaml-mode helm-projectile ## projectile puppet-mode helm flycheck evil-visual-mark-mode evil-magit)))
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

;(require 'whitespace)
; (global-whitespace-mode)
;(setq whitespace-style (quote (tabs newline tab-mark newline-mark)))
;(setq whitespace-display-mappings
;      '((newline-mark 10 [182 10])
;       (tab-mark 9 [8677 9] [92 9])))

;; Keybindings
(global-set-key (kbd "<f1>") 'projectile-switch-project)
(global-set-key (kbd "<f2>") 'projectile-find-file)
(global-set-key (kbd "<f10>") 'magit-status)
(global-set-key (kbd "C--") 'split-window-vertically)
(global-set-key (kbd "C-\\") 'split-window-horizontally)
(global-set-key (kbd "C-x f") 'helm-find-files)
; Quickly flip between buffers
(global-set-key (kbd "M-o")  'mode-line-other-buffer)

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
