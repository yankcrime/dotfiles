;; Startup speed tweaks
(defvar file-name-handler-alist-old file-name-handler-alist)

;; Only run this code once, even when reloading the file.
(defvar pre-init-p t)
(when pre-init-p
  (setq file-name-handler-alist nil
        pre-init-p nil
        gc-cons-threshold 402653184
        gc-cons-percentage 0.6
        load-prefer-newer t
        package-enable-at-startup nil
        package--init-file-ensured t))

(add-hook 'after-init-hook
          `(lambda ()
             (setq file-name-handler-alist file-name-handler-alist-old
                   gc-cons-threshold 800000
                   gc-cons-percentage 0.1)
             (garbage-collect))
          t)

(setq user-mail-address "nick@dischord.org")
(setq create-lockfiles nil)

(setq x-super-keysym 'meta)

;; User-interface stuff
(add-to-list 'default-frame-alist '(width . 110))
(scroll-bar-mode 0)
(tool-bar-mode -1)
(column-number-mode 1)
(menu-bar-mode -1)

;; Hide some menu junk
(define-key global-map [menu-bar tools gnus] nil)
(define-key global-map [menu-bar tools rmail] nil)
(define-key global-map [menu-bar tools compose-mail] nil)
(define-key global-map [menu-bar tools games] nil)

(setq initial-scratch-message nil
      initial-major-mode 'fundamental-mode)
(fset 'yes-or-no-p 'y-or-n-p)

;; Be able to mash Esc instead of Ctrl-G to get out of trouble
(define-key key-translation-map (kbd "ESC") (kbd "C-g"))

;; Minimal startup
(setq inhibit-startup-message t
      inhibit-splash-screen t
      initial-scratch-message nil
      frame-inhibit-implied-resize t
      initial-major-mode 'fundamental-mode)
(setq initial-frame-alist
      (append initial-frame-alist
              '((ns-appearance . light)
                (ns-transparent-titlebar . t)
                )))
(defun get-frame-name (&optional frame)
  "Return the string that names FRAME (a frame).  Default is selected frame."
  (unless frame (setq frame  (selected-frame)))
  (if (framep frame)
      (cdr (assq 'name (frame-parameters frame)))
    (error "Function `get-frame-name': Argument not a frame: `%s'" frame)))

(defun set-selected-frame-dark ()
  (interactive)
  (let ((frame-name (get-frame-name (selected-frame))))
    (call-process-shell-command (concat "xprop -f _GTK_THEME_VARIANT 8u -set _GTK_THEME_VARIANT \"dark\" -name \""
                                        frame-name
                                        "\""))))

(if (window-system)
    (set-selected-frame-dark))
;; Shut up
(setq ring-bell-function 'ignore)

;; Buffer switching
(winner-mode t)

(setq ispell-dictionary "english")

;; Fix window splitting now we all have widescreen monitors
;; See https://lists.gnu.org/archive/html/help-gnu-emacs/2015-08/msg00339.html
(with-eval-after-load "window"
  (defcustom split-window-below nil
    "If non-nil, vertical splits produce new windows below."
    :group 'windows
    :type 'boolean)

  (defcustom split-window-right nil
    "If non-nil, horizontal splits produce new windows to the right."
    :group 'windows
    :type 'boolean)

  (fmakunbound #'split-window-sensibly)

  (defun split-window-sensibly
      (&optional window)
    (setq window (or window (selected-window)))
    (or (and (window-splittable-p window t)
             ;; Split window horizontally.
             (split-window window nil (if split-window-right 'left  'right)))
        (and (window-splittable-p window)
             ;; Split window vertically.
             (split-window window nil (if split-window-below 'above 'below)))
        (and (eq window (frame-root-window (window-frame window)))
             (not (window-minibuffer-p window))
             ;; If WINDOW is the only window on its frame and is not the
             ;; minibuffer window, try to split it horizontally disregarding the
             ;; value of `split-width-threshold'.
             (let ((split-width-threshold 0))
               (when (window-splittable-p window t)
                 (split-window window nil (if split-window-right
                                              'left
                                            'right))))))))

(setq-default split-height-threshold  4
              split-width-threshold   160) ; the reasonable limit for horizontal splits

(defun toggle-window-split ()
  (interactive)
  (if (= (count-windows) 2)
      (let* ((this-win-buffer (window-buffer))
         (next-win-buffer (window-buffer (next-window)))
         (this-win-edges (window-edges (selected-window)))
         (next-win-edges (window-edges (next-window)))
         (this-win-2nd (not (and (<= (car this-win-edges)
                     (car next-win-edges))
                     (<= (cadr this-win-edges)
                     (cadr next-win-edges)))))
         (splitter
          (if (= (car this-win-edges)
             (car (window-edges (next-window))))
          'split-window-horizontally
        'split-window-vertically)))
    (delete-other-windows)
    (let ((first-win (selected-window)))
      (funcall splitter)
      (if this-win-2nd (other-window 1))
      (set-window-buffer (selected-window) this-win-buffer)
      (set-window-buffer (next-window) next-win-buffer)
      (select-window first-win)
      (if this-win-2nd (other-window 1))))))

(global-set-key (kbd "C-x |") 'toggle-window-split)
(global-set-key (kbd "C-SPC") nil)
(global-unset-key (kbd "C-SPC"))

;; Activate line numbers on programming modes
(add-hook 'prog-mode-hook
          'display-line-numbers-mode)

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
(setq frame-title-format '(buffer-file-name "%f" ("%b - GNU Emacs"))
      icon-title-format frame-title-format)

;; Disable auto-save and auto-backup
(setq auto-save-default nil
  make-backup-files nil)

;; Use y / n instead of yes / no
(fset 'yes-or-no-p 'y-or-n-p)

(setq-default tab-width 4 indent-tabs-mode nil)
(define-key global-map (kbd "RET") 'newline-and-indent)

;; Package management
(require 'package)
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

(use-package highlight-indent-guides
  :ensure t
  :init
  (setq highlight-indent-guides-method 'character
        ;; default is \x2502 but it is very slow on Mac
        highlight-indent-guides-character ?\xFFE8
        highlight-indent-guides-responsive 'top))

(use-package ns-auto-titlebar
  :ensure t
  :config
  (ns-auto-titlebar-mode))

(setq default-frame-alist
      '((vertical-scroll-bars . nil)
        (internal-border-width . 0)
        (height . 60)
        (width . 150)
        (font . "SF Mono 11")))

(use-package doom-themes
  :load-path "~/src/emacs-doom-themes"
  :init
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t
        doom-themes-visual-bell-config t
        doom-themes-org-config t)
  :config
  (load-theme 'doom-one-light t))

(use-package mini-frame
  :ensure t
  :custom
  (mini-frame-show-parameters
   '((top . 10)
     (width . 0.7)
     (left . 0.5)
     (background-color . "#f2f2f2")
     (border-color . "#cccccc")
     (internal-border-width . 1)))
  :config
  (mini-frame-mode))

(use-package minions
  :disabled
  :init (minions-mode)
  :config
  (setq minions-mode-line-lighter "#"
        minions-direct '(cider-mode
                         projectile-mode
                         visual-line-mode
                         flyspell-mode
                         flycheck-mode
                         company-mode
                         overwrite-mode
                         lsp-mode)))

(use-package diminish
  :config
  (diminish '(auto-revert-mode
              org-indent-mode)))

(eval-after-load "eldoc" '(diminish 'eldoc-mode))
(eval-after-load 'org-indent '(diminish 'org-indent-mode))
(eval-after-load 'magit '(diminish 'auto-revert-mode))

(use-package smart-mode-line
  :config
  (setq sml/mode-width 'right
        sml/pre-modes-separator "  "
        sml/theme nil)
  ;; Override this function to get better spacing once we rearrange.
  (defun sml/fill-for-buffer-identification () "  ")
  (column-number-mode) ;; Show column number next to the line number.
  (sml/setup)
  ;; Rearrange mode-line to put position and line number on the right.
  (setq-default
   mode-line-format
   '("%e"
     mode-line-client
     mode-line-modified
     mode-line-remote
     "  "
     mode-line-frame-identification
     mode-line-buffer-identification
     sml/pos-id-separator
     (vc-mode vc-mode)
     sml/pre-modes-separator
     mode-line-modes
     mode-line-misc-info
     mode-line-front-space
     mode-line-position
     mode-line-end-spaces
     " ")))

(use-package all-the-icons
  :custom
  (all-the-icons-scale-factor 1.0))

;; Zoom (increase or decrease) default font size for all buffers / frames
;; with C-M-= and C-M--
(use-package default-text-scale
  :ensure t
  :config
  (default-text-scale-mode))

(use-package undo-tree
  :diminish
  :config
  (global-undo-tree-mode))

;; Which-key - command previews
(use-package which-key
  :diminish
  :custom-face
  (which-key-posframe ((t (:background "#f2f2f2"))))
  (which-key-posframe-border ((t (:background "#cccccc"))))
  :custom
  (which-key-posframe-border-width 1)
  :config
  (which-key-mode))

;;(use-package which-key-posframe
;;  :config
;;  (setq which-key-posframe-poshandler 'posframe-poshandler-frame-top-center)
;;  (which-key-posframe-mode))

(use-package general
  :config
  (setq general-override-states '(normal visual motion))
  (general-override-mode)
  (declare-function 'general-define-key "general")
  (defmacro my-leader (&rest args)
    "Bind ARGS as leader bindings."
    (declare (indent 0))
    `(progn
       (require 'general)
       ,@(cl-loop for (key func doc) in args
                  collect
                  `(progn
                     (when ,doc
                       (which-key-add-key-based-replacements ,(concat "SPC " key) ,doc))
                     (general-define-key :prefix "SPC" :states '(normal motion) :keymaps 'override ,key ,func))))))

(my-leader
 ("bb" 'ivy-switch-buffer "Switch buffer")
 ("bd" 'kill-this-buffer "Kill this buffer")
 ("pp" 'projectile-switch-project "Switch project")
 ("pf" 'counsel-projectile-find-file "Find file")
 ("ps" 'counsel-projectile-rg "Search in files")
 ("gs" 'magit-status "Status")
 ("ga" 'magit-stage-file "stAge file")
 ("gb" 'magit-blame "Blame")
 ("gc" 'magit-commit "Commit")
 ("gp" 'magit-push "Push")
 ("aol" 'org-todo-list "Todo list")
 ("aoa" 'org-agenda "Agenda")
 ("aoc" 'org-task-capture "Capture task")
 ("afl" 'list-flycheck-errors "List errors")
 ("asc" 'flyspell-correct-word-before-point "Correct word")
 ("ts" 'flyspell-mode "Flyspell")
 ("tc" 'company-mode "Company")
 ("tf" 'flycheck-mode "Flycheck")
 ("ldt" 'lsp-describe-thing-at-point "LSP Describe Thing at point")
 ("lgd" 'lsp-goto-type-definition "LSP Goto type Definition")
 ("lgi" 'lsp-goto-implementation "LSP Goto Implentation")
 ("lfr" 'lsp-find-references "LSP Find References")
 ("lfd" 'lsp-find-definition "LSP Find Definition")
 ("lfE" 'lsp-find-declaration "LSP Find dEclaration")
 ("tv" 'visual-line-mode "Visual line mode")
 ("tw" 'whitespace-mode "Whitespace mode")
 ("w1" 'winum-select-window-1 "Select 1")
 ("w2" 'winum-select-window-2 "Select 2")
 ("w3" 'winum-select-window-3 "Select 3")
 ("w4" 'winum-select-window-4 "Select 4")
 ("w5" 'winum-select-window-5 "Select 5")
 ("w6" 'winum-select-window-6 "Select 6")
 ("w7" 'winum-select-window-7 "Select 7")
 ("w8" 'winum-select-window-8 "Select 8")
 ("w9" 'winum-select-window-9 "Select 9")
 ("ww" 'winum-select-window-by-number "Select by number")
 ("w|" 'evil-window-vsplit "Vertical split")
 ("w-" 'evil-window-split "Horizontal split")
 ("wd" 'evil-window-delete "Delete")
 ("wo" 'delete-other-windows "delete Other windows")
 ("wf" 'toggle-frame-fullscreen "make Frame Fullscreen"))

(which-key-add-key-based-replacements
  "SPC a" "Apps"
  "SPC ao" "Orgmode"
  "SPC af" "Flycheck"
  "SPC as" "Flyspell"
  "SPC b" "Buffers"
  "SPC c" "Compile"
  "SPC e" "Errors"
  "SPC f" "Files"
  "SPC g" "Git"
  "SPC h" "Help"
  "SPC hd" "Describe"
  "SPC j" "Jump"
  "SPC l" "LSP"
  "SPC ld" "LSP Describe"
  "SPC lf" "LSP Find"
  "SPC lg" "LSP Goto"
  "SPC p" "Project"
  "SPC q" "Quit"
  "SPC s" "Search"
  "SPC t" "Toggle"
  "SPC w" "Windows"
  "SPC v" "Venvs")


;; Tame windows

(use-package shackle
  :config
  (progn
    (setq shackle-lighter "")
    (setq shackle-select-reused-windows nil) ; default nil
    (setq shackle-default-alignment 'below) ; default below
    (setq shackle-default-size 0.4) ; default 0.5

    (setq shackle-rules
          '((compilation-mode :select nil)
            ("*undo-tree*" :size 0.25 :align right)
            ("*Help*" :select t   :inhibit-window-quit t :other t)
            ("*Completions*" :size 0.3 :align t)
            ("*Messages*" :select nil :inhibit-window-quit t :other t)
            ("*Calendar*" :select t :size 0.3 :align below)
            ("*info*" :select t :inhibit-window-quit t :same t)))

    (shackle-mode 1)))

;; Completion frontend
(use-package ivy
  :diminish
  :demand t
  :config
  (ivy-mode 1)
  (setq ivy-height 20
        ivy-use-virtual-buffers t
        enable-recursive-minibuffers t)
  (define-key ivy-switch-buffer-map (kbd "C-j") 'ivy-next-line)
  (define-key ivy-switch-buffer-map (kbd "C-k") 'ivy-previous-line)
  (define-key ivy-minibuffer-map (kbd "C-j") 'ivy-next-line)
  (define-key ivy-minibuffer-map (kbd "C-k") 'ivy-previous-line)
  (define-key ivy-switch-buffer-map (kbd "<escape>") 'minibuffer-keyboard-quit)
  (with-eval-after-load 'ivy
    (define-key ivy-minibuffer-map (kbd "M-v") 'yank)))

(use-package ivy-rich
 :defer t
 :after counsel
 :init
   (setq ivy-rich-display-transformers-list
       '(ivy-switch-buffer
         (:columns
          ((ivy-rich-candidate (:width 35))
           (ivy-rich-switch-buffer-project (:width 25 :face success))
           (ivy-rich-switch-buffer-major-mode (:width 20 :face warning)))
          :predicate
          (lambda (cand) (get-buffer cand)))
         counsel-M-x
         (:columns
          ((counsel-M-x-transformer (:width 35))
           (ivy-rich-counsel-function-docstring
            (:width 60 :face font-lock-doc-face))))
         counsel-describe-function
         (:columns
          ((counsel-describe-function-transformer (:width 35))
           (ivy-rich-counsel-function-docstring
            (:width 34 :face font-lock-doc-face))))
         counsel-describe-variable
         (:columns
          ((counsel-describe-variable-transformer (:width 35))
           (ivy-rich-counsel-variable-docstring
            (:width 34 :face font-lock-doc-face))))
         package-install
         (:columns
          ((ivy-rich-candidate (:width 35))
           (ivy-rich-package-version (:width 22 :face font-lock-comment-face))
           (ivy-rich-package-archive-summary
            (:width 7 :face font-lock-builtin-face))
           (ivy-rich-package-install-summary
            (:width 33 :face font-lock-doc-face))))))
 :config
 (ivy-rich-mode +1)
 (setcdr (assq t ivy-format-functions-alist) #'ivy-format-function-line))

(use-package posframe
  :custom-face
  (internal-border ((t (:background "#cccccc")))))
  
;;(use-package ivy-posframe
;;  :custom-face
;;  (internal-border ((t (:background "#cccccc"))))
;;  :after (ivy)
;;  :config
;;  (setq ivy-posframe-display-functions-alist
;;        '((t . ivy-posframe-display-at-frame-top-center))
;;        ivy-posframe-height-alist '((t . 20))
;;        ivy-posframe-parameters '((internal-border-width . 10))
;;        ivy-posframe-parameters
;;        '((left-fringe . 8)
;;          (top-fringe . 10)
;;          (right-fringe . 8))
;;        ivy-posframe-border-width 1
;;        ivy-posframe-hide-minibuffer t
;;        ivy-posframe-width 110)
;;  (ivy-posframe-mode 1))

(use-package ranger
  :ensure t
  :config
  (ranger-override-dired-mode t))

;; Completion framework
(use-package counsel
  :after ivy
  :defer t
  :bind (("M-x" . counsel-M-x))
  :config
  (setq ivy-height 20)
  (add-to-list 'ivy-height-alist '(counsel-evil-registers . 10)))

(use-package counsel-projectile
  :defer t)

(use-package ivy-prescient
  :after ivy
  :config
  (ivy-prescient-mode)
  (prescient-persist-mode))

(use-package lsp-mode
  :init
  (setq lsp-keymap-prefix "C-SPC")
  :commands (lsp lsp-deferred)
  :custom
  (lsp-modeline-code-actions-enable nil)
  (lsp-prefer-flymake nil)
  (lsp-completion-provider :capf)
  (read-process-output-max (* 1024 1024))
  (lsp-file-watch-threshold 2000)
  :config
  (with-eval-after-load 'lsp-mode
    (setq lsp-modeline-diagnostics-enable nil))
  (lsp-enable-which-key-integration)
  (lsp-register-client
   (make-lsp-client :new-connection (lsp-stdio-connection '("/usr/local/bin/terraform-ls" "serve"))
                    :major-modes '(terraform-mode)
                    :server-id 'terraform-ls))
  (add-hook 'terraform-mode-hook #'lsp))

(use-package lsp-ui
  :defer t
  :config
  (define-key lsp-ui-mode-map
    [remap xref-find-definitions] #'lsp-ui-peek-find-definitions)
  (define-key lsp-ui-mode-map
    [remap xref-find-references] #'lsp-ui-peek-find-references)
  (setq lsp-ui-sideline-enable nil
        lsp-ui-doc-enable nil
        lsp-signature-auto-activate nil
        lsp-ui-flycheck-enable t
        lsp-ui-imenu-enable t
        lsp-ui-sideline-ignore-duplicate t))

(use-package company
  :diminish
  :defer t
  :bind (("C-<tab>" . company-complete))
  :init
  (add-hook 'terraform-mode-hook 'company-mode)
  (add-hook 'terraform-mode-hook 'company-terraform-init)
  (add-hook 'go-mode-hook 'company-mode)
  :config
  (setq company-idle-delay 0.2))

(use-package company-prescient
  :after company)

(use-package company-terraform
  :defer t)

(use-package company-go
  :defer t)

(use-package company-box
  :diminish
  :hook (company-mode . company-box-mode)
  :init
  (setq company-box-doc-delay 0)
  (setq company-box-max-candidates 1000)
  (setq company-box-show-single-candidate t)
  (setq company-tooltip-align-annotations t)
  (setq company-box-doc-frame-parameters
        `((internal-border-width . 1))))

(use-package avy
  :config
  :bind
  ("M-c" . avy-goto-char)
  ("M-C" . avy-goto-char-2))

(use-package eyebrowse
  :config
  (setq eyebrowse-mode 1))

(use-package org-download
  :after org
  :config
  (setq org-download-method 'attach)
  (org-download-enable))

(use-package tramp
  :defer t
  :init
  (use-package tramp-theme
    :defer t)
  (use-package vagrant-tramp
    :defer t)
  :config
  (setq tramp-default-method "ssh"))

(use-package winum
  :config
  (setq winum-mode-line-position 6
        winum-format "%s "
        winum-auto-setup-mode-line t)
  (winum-mode))

(use-package elpy
  :defer t
  :disabled
  :init
  (with-eval-after-load 'python
    (elpy-enable)
    (elpy-use-ipython)
    (delete 'elpy-module-highlight-indentation elpy-modules)))

(use-package python-black
  :demand t
  :after python)

(use-package git-gutter-fringe
  :defer t
  :config
  (set-face-foreground 'git-gutter-fr:added "darkgreen")
  (set-face-background 'git-gutter-fr:added "#e2e2e2")
  (set-face-foreground 'git-gutter-fr:modified "blue")
  (set-face-background 'git-gutter-fr:modified "#e2e2e2")
  (require 'git-gutter)
  (require 'git-gutter-fringe))

; Evil mode and related
(use-package evil
  :defer .1 ;; don't block emacs when starting, load evil immediately after startup
  :init
  (setq evil-normal-state-cursor '(box "#4078f2")
      evil-emacs-state-cursor  '(box "#7F5AB6")
      evil-insert-state-cursor '("pink" (bar . 2))
      evil-want-integration t
      evil-want-keybinding nil
      evil-search-module 'evil-search
      evil-ex-complete-emacs-commands nil
      evil-vsplit-window-right t
      evil-split-window-below t
      evil-shift-round nil
      evil-echo-state nil
      evil-want-C-u-scroll t
      evil-mode-line-format '(before . mode-line-client)
      evil-normal-state-tag (propertize " N ")
      evil-insert-state-tag (propertize " I ")
      evil-visual-state-tag " V "
      evil-motion-state-tag " M "
      evil-operator-state-tag " O "
      evil-emacs-state-tag " E ")
  :bind (:map evil-normal-state-map
              ("-" . deer)
              :map ranger-mode-map ("-" . ranger-up-directory))
  :config
  (evil-mode)
  (evil-set-undo-system 'undo-tree)

  (defun my-exit-evil-command-window ()
    "Exit evil command window."
    (interactive)
    (other-window -1)
    (other-window 1)
    (kill-this-buffer)
    (evil-window-delete))
  (evil-define-key 'normal evil-command-window-mode-map [escape] 'my-exit-evil-command-window)

  ;; vim-like keybindings everywhere in emacs
  (use-package evil-collection
    :after evil
    :config
    (setq evil-collection-mode-list
          '(dired
            ediff
            elisp-mode
            flycheck
            magit
            magit-todos
            company))
    (evil-collection-init))

  ;; gl and gL operators, like vim-lion (alignment operators)
  (use-package evil-lion
    :bind (:map evil-normal-state-map
                ("g l " . evil-lion-left)
                ("g L " . evil-lion-right)
                :map evil-visual-state-map
                ("g l " . evil-lion-left)
                ("g L " . evil-lion-right)))

  ;; gc operator, like vim-commentary
  (use-package evil-commentary
    :bind (:map evil-normal-state-map
                ("gc" . evil-commentary)))

  ;; gr operator, like vim's ReplaceWithRegister
  (use-package evil-replace-with-register
    :bind (:map evil-normal-state-map
                ("gr" . evil-replace-with-register)
                :map evil-visual-state-map
                ("gr" . evil-replace-with-register)))

  (use-package evil-visualstar
    :bind (:map evil-visual-state-map
                ("*" . evil-visualstar/begin-search-forward)
                ("#" . evil-visualstar/begin-search-backward)))

  (use-package evil-expat
    :defer t)

  ;; visual hints while editing
  (use-package evil-goggles
    :diminish
    :config
    (evil-goggles-use-diff-faces)
    (evil-goggles-mode))

  ;; like vim-surround
  (use-package evil-surround
    :commands
    (evil-surround-edit
     evil-Surround-edit
     evil-surround-region
     evil-Surround-region)
    :init
    (evil-define-key 'operator global-map "s" 'evil-surround-edit)
    (evil-define-key 'operator global-map "S" 'evil-Surround-edit)
    (evil-define-key 'visual global-map "S" 'evil-surround-region)
    (evil-define-key 'visual global-map "gS" 'evil-Surround-region))

  (use-package evil-owl
    :diminish
    :config
    (define-key evil-owl-popup-map (kbd "C-k") #'evil-owl-scroll-popup-up)
    (define-key evil-owl-popup-map (kbd "C-j") #'evil-owl-scroll-popup-down)
    (setq evil-owl-register-char-limit 100
          evil-owl-display-method 'posframe
          evil-owl-extra-posframe-args '(:internal-border-width 1
                                         :internal-border-color "#cccccc"))
    (evil-owl-mode)))

(use-package yasnippet
  :diminish (yas-minor-mode)
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
  (setq org-directory "~/Sync/org"
        org-agenda-files (directory-files-recursively "~/Sync/org/" "\.org$")
        org-default-notes-file (concat org-directory "/notes.org"))
  (define-key global-map (kbd "C-c c") 'org-capture)
  (define-key global-map (kbd "C-c l") 'org-store-link)
  (define-key global-map (kbd "C-c t a") 'pop-to-org-agenda)
  (define-key global-map (kbd "C-c t l") 'org-todo-list)
  (require 'org-tempo)
  (setq org-log-done 'time
        org-adapt-indentation nil
        org-startup-indented 'true
        org-src-tab-acts-natively t
        org-src-window-setup 'other-window
        org-startup-with-inline-images t
        org-image-actual-width (/ (display-pixel-width) 10))

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
    :after org
    :custom
    (org-journal-file-format "%Y-%m-%d.org")
    (org-journal-date-format "%A, %d %B %Y")
    (org-journal-dir "~/Sync/org/journal/"))

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

  (org-babel-do-load-languages
   'org-babel-load-languages
   '((shell . t)
     (python . t)
     (emacs-lisp . t)
     ))

  (setq org-ellipsis "•••")

  (use-package org-bullets
    :defer t
    :hook (org-mode . org-bullets-mode)))

;; Magit
(use-package magit
  :diminish
  :defer t
  :init
  (setq magit-completing-read-function 'ivy-completing-read)
  (setq magit-revision-show-gravatars '("^Author:     " . "^Commit:     "))
  :config
  (global-set-key (kbd "<f10>") 'magit-status))

;; Projectile
(use-package projectile
  :diminish
  :defer t
  :config
  (defun projectile-short-mode-line ()
    (format " [%s]" (projectile-project-name)))
  (setq projectile-mode-line-function 'projectile-short-mode-line)
  (setq projectile-completion-system 'ivy)
  (setq projectile-enable-caching nil)
  (setq projectile-switch-project-action #'projectile-dired)
  (projectile-global-mode +1))

;; Do something about popups as well
(use-package popwin
  :config
  (popwin-mode 1))

;; Easily swap around buffers
(use-package transpose-frame)

;; Resize active frame according to 'golden ratio' principles
(use-package zoom
  :diminish
  :config
  (setq zoom-size '(0.618 . 0.618))
  (zoom-mode t))

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

(use-package exec-path-from-shell
  :defer 2
  :config
  (dolist (var '("GOPATH"  "NVM_BIN"))
    (add-to-list 'exec-path-from-shell-variables var))
  (exec-path-from-shell-initialize))

;; Flycheck
(use-package flycheck
  :defer t
  :init
  (add-hook 'json-mode-hook 'flycheck-mode)
  (add-hook 'yaml-mode-hook 'flycheck-mode))

(use-package flycheck-posframe
  :after (flycheck posframe company)
  :config
  (setq flycheck-posframe-border-width 1)

  (add-to-list 'flycheck-posframe-inhibit-functions
               '(lambda (&rest _)
                  (bound-and-true-p company-backend)))

  (flycheck-posframe-configure-pretty-defaults)
  (add-hook 'flycheck-mode-hook #'flycheck-posframe-mode)

  (set-face-attribute 'flycheck-posframe-background-face nil :inherit 'ivy-posframe)
  (set-face-attribute 'flycheck-posframe-border-face nil :inherit 'ivy-posframe-border)
  (set-face-attribute 'flycheck-posframe-warning-face nil :inherit 'flycheck-warning-list-warning)
  (set-face-attribute 'flycheck-posframe-error-face nil :inherit 'flycheck-error-list-error)
  (set-face-attribute 'flycheck-posframe-info-face nil :inherit 'flycheck-error-list-info))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package rainbow-mode)

;; Modes

(use-package polymode
  :defer t)

(use-package poly-markdown
  :after polymode
  :ensure t
  :config
  ;; Wrap lines at column limit, but don't put hard returns in
  (add-hook 'markdown-mode-hook (lambda () (visual-line-mode 1)))
  ;; Flyspell on
  (add-hook 'markdown-mode-hook (lambda () (flyspell-mode 1))))

(use-package poly-ansible
  :after polymode
  :ensure t
  :mode
  ("playbook\\.ya?ml\\'" . poly-ansible-mode)
  ("/ansible/.*\\.ya?ml\\'" . poly-ansible-mode)
  ("/\\(?:group\\|host\\)_vars/" . poly-ansible-mode)

  :init
  (with-eval-after-load 'fill-column-indicator
    (add-hook 'ansible-hook 'fci-mode))

  :config
  (setq pm-inner/jinja2
        (pm-inner-chunkmode :mode #'jinja2-mode
                            :head-matcher "{[%{#][+-]?"
                            :tail-matcher "[+-]?[%}#]}"
                            :head-mode 'body
                            :tail-mode 'body
                            :head-adjust-face nil
                            :tail-adjust-face nil)))

;; Golang
;; Need to install some Go tools seperately
;; go get -u github.com/mdempsky/gocode
;; go get -u github.com/rogpeppe/godef
;; go get -u golang.org/x/tools/cmd/goimports
;; go get -u github.com/jstemmer/gotags
(use-package go-autocomplete
  :defer t)
(use-package go-projectile
  :defer t)

(use-package go-guru
  :defer t
  :config
  (go-guru-hl-identifier-mode))

(use-package go-mode
  :defer t
  :init
  (set (make-local-variable 'compile-command)
       "go build -v && go test -v && go vet")
  :bind
  ("M-]" . lsp-find-definition)         ; Go to definition
  ("M-}" . pop-tag-mark)       ; Return from whence you came
  ("M-p" . compile)            ; Invoke compiler
  ("M-P" . recompile)          ; Redo most recent compile cmd
  ("M->" . next-error)         ; Go to next error (or msg)
  ("M-<" . previous-error)     ; Go to previous error or msg
  :config
  ;; Define function to call when go-mode loads
  (defun my-go-mode-hook ()
    (flycheck-mode)
    (lsp-deferred)
    (add-hook 'before-save-hook 'gofmt-before-save) ; gofmt before every save
    (setq gofmt-command "goimports")                ; gofmt uses invokes goimports
    (if (not (string-match "go" compile-command))   ; set compile command default
        (set (make-local-variable 'compile-command)
             "go build -v && go test -v && go vet")))

  (add-hook 'go-mode-hook 'my-go-mode-hook)

  ;; Ensure the go specific autocomplete is active in go-mode.
  (with-eval-after-load 'go-mode
    (require 'go-autocomplete)))

(use-package slime
  :defer t
  :config
  (setq inferior-lisp-program "/usr/local/bin/sbcl")
  (setq slime-contribs '(slime-fancy)))

(use-package yaml-mode
  :defer t)

(use-package jinja2-mode
  :defer t)

(use-package markdown-mode
  :defer t
  :commands (markdown-mode gfm-mode)
  :mode (("\\.md\\'" . gfm-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown")
  :config
  (setq markdown-spaces-after-code-fence 0))

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

(use-package hcl-mode
  :defer t)

(use-package terraform-mode
  :defer t
  :config
  (add-hook 'terraform-mode-hook 'terraform-format-on-save-mode))

(use-package highlight-indent-guides
  :diminish
  :config
  (setq highlight-indent-guides-method 'character)
  ;; Indent character samples: | ┆ ┊
  (setq highlight-indent-guides-character ?\┆)
  (add-hook 'prog-mode-hook 'highlight-indent-guides-mode)
  (add-hook 'yaml-mode-hook 'highlight-indent-guides-mode))

(use-package vterm
  :load-path "~/src/emacs-libvterm"
  :commands (vterm)
  :config
  (add-hook 'vterm-mode-hook
            (lambda ()
              (evil-insert-state)))
  (define-key vterm-mode-map [return]                      #'vterm-send-return)
  (setq vterm-keymap-exceptions nil)
  (evil-define-key 'insert vterm-mode-map (kbd "C-e")      #'vterm--self-insert)
  (evil-define-key 'insert vterm-mode-map (kbd "C-f")      #'vterm--self-insert)
  (evil-define-key 'insert vterm-mode-map (kbd "C-a")      #'vterm--self-insert)
  (evil-define-key 'insert vterm-mode-map (kbd "C-v")      #'vterm--self-insert)
  (evil-define-key 'insert vterm-mode-map (kbd "C-b")      #'vterm--self-insert)
  (evil-define-key 'insert vterm-mode-map (kbd "C-w")      #'vterm--self-insert)
  (evil-define-key 'insert vterm-mode-map (kbd "C-u")      #'vterm--self-insert)
  (evil-define-key 'insert vterm-mode-map (kbd "C-d")      #'vterm--self-insert)
  (evil-define-key 'insert vterm-mode-map (kbd "C-n")      #'vterm--self-insert)
  (evil-define-key 'insert vterm-mode-map (kbd "C-m")      #'vterm--self-insert)
  (evil-define-key 'insert vterm-mode-map (kbd "C-p")      #'vterm--self-insert)
  (evil-define-key 'insert vterm-mode-map (kbd "C-j")      #'vterm--self-insert)
  (evil-define-key 'insert vterm-mode-map (kbd "C-k")      #'vterm--self-insert)
  (evil-define-key 'insert vterm-mode-map (kbd "C-r")      #'vterm--self-insert)
  (evil-define-key 'insert vterm-mode-map (kbd "C-t")      #'vterm--self-insert)
  (evil-define-key 'insert vterm-mode-map (kbd "C-g")      #'vterm--self-insert)
  (evil-define-key 'insert vterm-mode-map (kbd "C-c")      #'vterm--self-insert)
  (evil-define-key 'insert vterm-mode-map (kbd "C-SPC")    #'vterm--self-insert)
  (evil-define-key 'normal vterm-mode-map (kbd "C-d")      #'vterm--self-insert)
  (evil-define-key 'normal vterm-mode-map (kbd "i")        #'evil-insert-resume)
  (evil-define-key 'normal vterm-mode-map (kbd "o")        #'evil-insert-resume)
  (evil-define-key 'normal vterm-mode-map (kbd "<return>") #'evil-insert-resume))

(use-package vterm-toggle
  :after evil
  :custom
  (vterm-toggle-fullscreen-p nil)
  :defer t
  :bind
  ("C-`" . vterm-toggle)
  :config
  (add-to-list 'display-buffer-alist
               '("^v?term.*"
                 (display-buffer-reuse-window display-buffer-in-direction)
                 (direction . top)
                 (reusable-frames . visible)
                 (window-height . 0.5))))

(use-package writeroom-mode
  :defer t
  :commands (writeroom-mode)
  :config
  (add-to-list 'writeroom-global-effects 'visual-line-mode)
  (add-to-list 'writeroom-global-effects 'text-scale-increase)
  (delq 'writeroom-set-fullscreen writeroom-global-effects)
  (setq writeroom-restore-window-config t
        writeroom-width 100))

(add-hook 'python-mode-hook
          (lambda ()
            (flycheck-mode)
            (company-mode)
            (setq flycheck-python-pylint-executable "/Users/nick/.local/bin/pylint")
            (setq flycheck-pylintrc "/Users/nick/.pylintrc")))

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

(defun toggle-fold ()
  "Toggle fold all lines larger than indentation on current line"
  (interactive)
  (let ((col 1))
    (save-excursion
      (back-to-indentation)
      (setq col (+ 1 (current-column)))
      (set-selective-display
       (if selective-display nil (or col 1))))))

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

;; Global keybinding overrides, some mirroring macOS behaviour
(global-set-key (kbd "C--") 'split-window-vertically)
(global-set-key (kbd "C-\\") 'split-window-horizontally)
(global-set-key (kbd "M-w") 'kill-this-buffer)
(global-set-key (kbd "M-s") 'evil-write)
(global-set-key (kbd "M-f") 'swiper)
(global-set-key (kbd "M-F") 'query-replace)
(global-set-key (kbd "M-\=") 'text-scale-increase)
(global-set-key (kbd "M--") 'text-scale-decrease)
(global-set-key (kbd "M-o") 'counsel-find-file)
(global-set-key (kbd "C-s") 'counsel-rg)
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
(global-set-key (kbd "M-1") 'winum-select-window-1)
(global-set-key (kbd "M-2") 'winum-select-window-2)
(global-set-key (kbd "M-3") 'winum-select-window-3)
(global-set-key (kbd "M-4") 'winum-select-window-4)
(global-set-key (kbd "M-5") 'winum-select-window-5)
(global-set-key (kbd "M-6") 'winum-select-window-6)
(global-set-key (kbd "M-7") 'winum-select-window-7)
(global-set-key (kbd "M-8") 'winum-select-window-8)
(global-set-key (kbd "M-9") 'winum-select-window-9)
(global-set-key (kbd "M-`") 'ns-next-frame)

;; Ensure we're using ⌘ as Meta
(setq mac-option-modifier nil
      mac-command-modifier 'meta)

;; Keep custom junk that Emacs generates in a seperate file
(setq custom-file "~/.emacs.d/custom.el")
(unless (file-exists-p custom-file)
  (write-region "" nil custom-file))
(load custom-file)

(server-start)
