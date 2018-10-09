;; -*- mode: emacs-lisp -*-
;; This file is loaded by Spacemacs at startup.
;; It must be stored in your home directory.

(defun dotspacemacs/layers ()
  (setq-default
   dotspacemacs-distribution 'spacemacs
   dotspacemacs-enable-lazy-installation 'unused
   dotspacemacs-ask-for-lazy-installation t
   dotspacemacs-configuration-layer-path '()
   dotspacemacs-configuration-layers
   '(
     yaml
     ansible
     python
     go
     puppet
     terraform
     ivy
     better-defaults
     emacs-lisp
     git
     markdown
     org
     osx
     (shell :variables
            shell-default-height 30
            shell-default-position 'bottom)
     (spell-checking :variables
                     spell-checking-enable-by-default nil
                     )
     syntax-checking
     ranger
     vagrant
     )
   dotspacemacs-additional-packages '(
                                      doom-themes
                                      org-bullets)
   dotspacemacs-frozen-packages '()
   dotspacemacs-excluded-packages '(
                                    lorem-ipsum
                                    gnuplot
                                    fancy-battery
                                    evil-ediff
                                    winner
				                            persp-mode
                                    neotree)
   dotspacemacs-install-packages 'used-only))

(defun dotspacemacs/init ()
  (setq-default
   dotspacemacs-elpa-https t
   dotspacemacs-elpa-timeout 5
   dotspacemacs-check-for-update nil
   dotspacemacs-elpa-subdirectory nil
   dotspacemacs-editing-style 'vim
   dotspacemacs-verbose-loading nil
   dotspacemacs-startup-banner 'nil
   dotspacemacs-startup-lists '((recents . 5)
                                (projects . 7))
   dotspacemacs-startup-buffer-responsive t
   dotspacemacs-scratch-mode 'text-mode
   dotspacemacs-themes '(doom-one-light
                         doom-one
                         leuven
                         spacemacs-dark
                         spacemacs-light)
   dotspacemacs-colorize-cursor-according-to-state t
   dotspacemacs-default-font '("Triplicate T4c"
                               :size 14
                               :powerline-scale 1.0)
   dotspacemacs-leader-key "SPC"
   dotspacemacs-emacs-command-key "SPC"
   dotspacemacs-ex-command-key ":"
   dotspacemacs-emacs-leader-key "M-m"
   dotspacemacs-major-mode-leader-key ","
   dotspacemacs-major-mode-emacs-leader-key "C-M-m"
   dotspacemacs-distinguish-gui-tab nil
   dotspacemacs-remap-Y-to-y$ nil
   dotspacemacs-retain-visual-state-on-shift t
   dotspacemacs-visual-line-move-text nil
   dotspacemacs-ex-substitute-global nil
   dotspacemacs-default-layout-name "Default"
   dotspacemacs-display-default-layout nil
   dotspacemacs-auto-resume-layouts nil
   dotspacemacs-large-file-size 1
   dotspacemacs-auto-save-file-location 'cache
   dotspacemacs-max-rollback-slots 5
   dotspacemacs-helm-resize nil
   dotspacemacs-helm-no-header nil
   dotspacemacs-helm-position 'bottom
   dotspacemacs-helm-use-fuzzy 'always
   dotspacemacs-enable-paste-transient-state nil
   dotspacemacs-which-key-delay 0.4
   dotspacemacs-which-key-position 'bottom
   dotspacemacs-loading-progress-bar t
   dotspacemacs-fullscreen-at-startup nil
   dotspacemacs-fullscreen-use-non-native nil
   dotspacemacs-maximized-at-startup nil
   dotspacemacs-active-transparency 90
   dotspacemacs-inactive-transparency 90
   dotspacemacs-show-transient-state-title t
   dotspacemacs-show-transient-state-color-guide t
   dotspacemacs-mode-line-unicode-symbols nil
   dotspacemacs-smooth-scrolling t
   dotspacemacs-line-numbers nil
   dotspacemacs-folding-method 'evil
   dotspacemacs-smartparens-strict-mode nil
   dotspacemacs-smart-closing-parenthesis nil
   dotspacemacs-highlight-delimiters 'all
   dotspacemacs-persistent-server nil
   dotspacemacs-search-tools '("ag" "pt" "ack" "grep")
   dotspacemacs-default-package-repository nil
   dotspacemacs-whitespace-cleanup nil
   dotspacemacs-mode-line-theme 'spacemacs
   ))

(defun dotspacemacs/user-config ()

  ;; Dark
  ;; (set-background-color "#0D0C0C")

  ;; Light
  (set-background-color "#F4F4F4")

  (spacemacs/toggle-vi-tilde-fringe-off)

  (spaceline-toggle-column-off)
  (spaceline-toggle-hud-off)
  (spaceline-toggle-line-column-on)
  (spaceline-toggle-buffer-encoding-off)
  (spaceline-toggle-buffer-encoding-abbrev-off)
  (spaceline-toggle-input-method-off)
  (spaceline-toggle-buffer-size-off)
  (spaceline-toggle-version-control-on)
  (spaceline-toggle-python-pyenv-on)
  (spaceline-toggle-python-pyvenv-off)
  (spaceline-toggle-projectile-root-on)
  (spaceline--column-number-at-pos -1)
  (spaceline-toggle-flycheck-info-on)
  (spaceline-toggle-flycheck-warning-on)
  (spaceline-toggle-flycheck-error-on)
  (spaceline-toggle-minor-modes-off)
  (spaceline-toggle-point-position-off)
  (spaceline-toggle-purpose-off)

  (setq powerline-default-separator 'nil)

  ;; Mirror some standard macOS keybindings
  (global-set-key (kbd "M-s") 'evil-write)
  (global-set-key (kbd "M-v") 'yank)
  (global-set-key (kbd "M-c") 'kill-ring-save)
  (global-set-key (kbd "M-a") 'mark-whole-buffer)

  ;; Display Visited File's Path in the Frame Title
  (setq frame-title-format
        '((:eval (if (buffer-file-name)
                     (abbreviate-file-name (buffer-file-name))
                   "%b"))))

  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t)
  (doom-themes-org-config)

  (setq org-directory "~/Dropbox/org")
  (setq org-agenda-files '("~/Dropbox/org/"))

  (setq org-capture-templates '(
                                ("a" "New TODO:" entry
                                 (file "todo.org")
                                 "* TODO%?
  SCHEDULED: %t
  :PROPERTIES:
  :CREATED: %U\n
  :END:")
                                ("j" "Journal Entry"
                                 entry (file+datetree "journal.org")
                                 "* Event: %?\n\n  %i\n\n  From: %a"
                                 :empty-lines 1)
                                ))

  (setq org-ellipsis "•••")

  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))

  (defun my/org-mode-hook ()
    "Stop the org-level headers from increasing in height relative to the other text."
    (dolist (face '(org-level-1
                    org-level-2
                    org-level-3
                    org-level-4
                    org-level-5))
      (set-face-attribute face nil :height 1.0)))

  (add-hook 'org-mode-hook 'my/org-mode-hook)

  ;; Only syntax check on save
  (setq flycheck-check-syntax-automatically '(mode-enabled save))

  (setq org-startup-truncated nil)
  (setq org-adapt-indentation nil)
  (setq org-startup-indented 'true)

  (setq spaceline-highlight-face-func 'spaceline-highlight-face-evil-state)

  (set-face-attribute 'spaceline-evil-normal nil
                      :background "#e9e9e9")

  (set-face-attribute 'mode-line nil
                      :background "#e9e9e9"
                      :foreground "#332233"
                      :box '(:line-width 1 :color "#ebebeb")
                      :overline nil
                      :underline nil)

  (set-face-attribute 'powerline-active1 nil
                      :background "#e9e9e9"
                      :foreground "#332233"
                      :box '(:line-width 1 :color "#ebebeb")
                      :overline nil
                      :underline nil)

  (set-face-attribute 'powerline-active2 nil
                      :background "#e9e9e9"
                      :foreground "#332233"
                      :box '(:line-width 1 :color "#ebebeb")
                      :overline nil
                      :underline nil)

  (set-face-attribute 'powerline-inactive1 nil
                      :background "#f9f9f9"
                      :foreground "#110011"
                      :box '(:line-width 1 :color "#ebebeb")
                      :overline nil
                      :underline nil)

  (set-face-attribute 'powerline-inactive2 nil
                      :background "#f9f9f9"
                      :foreground "#110011"
                      :box '(:line-width 1 :color "#ebebeb")
                      :overline nil
                      :underline nil)

  (set-face-attribute 'mode-line-inactive nil
                      :background "#f9f9f9"
                      :foreground "#110011"
                      :box '(:line-width 1 :color "#ebebeb")
                      :overline nil
                      :underline nil)
)
;; Do not write anything past this comment. This is where Emacs will
;; auto-generate custom variable definitions.
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#f0f0f0" "#e45649" "#50a14f" "#986801" "#4078f2" "#a626a4" "#0184bc" "#1b2229"])
 '(custom-safe-themes
   (quote
    ("b01b91ba9276deb39aa892c105a8644ef281b4d1804ab7c48de96e9c5d2aaa48" default)))
 '(evil-want-Y-yank-to-eol nil)
 '(fci-rule-color "#383a42" t)
 '(jdee-db-active-breakpoint-face-colors (cons "#f0f0f0" "#4078f2"))
 '(jdee-db-requested-breakpoint-face-colors (cons "#f0f0f0" "#50a14f"))
 '(jdee-db-spec-breakpoint-face-colors (cons "#f0f0f0" "#9ca0a4"))
 '(package-selected-packages
   (quote
    (treepy graphql terraform-mode hcl-mode dim unfill reveal-in-osx-finder pbcopy osx-trash osx-dictionary mwim launchctl wgrep smex ivy-hydra flyspell-correct-ivy counsel-projectile counsel swiper ivy helm-company helm-c-yasnippet fuzzy company-statistics company-go company-ansible company-anaconda company auto-yasnippet yasnippet ac-ispell auto-complete rvm ruby-tools ruby-test-mode rubocop rspec-mode robe rbenv rake puppet-mode minitest chruby bundler inf-ruby yapfify pyvenv pytest pyenv-mode py-isort pip-requirements live-py-mode hy-mode dash-functional helm-pydoc go-guru go-eldoc go-mode cython-mode anaconda-mode pythonic jinja2-mode ansible-doc ansible ranger yaml-mode doom-themes org-projectile org-plus-contrib xterm-color smeargle shell-pop orgit org-category-capture org-present org-pomodoro alert log4e gntp org-mime org-download multi-term mmm-mode markdown-toc markdown-mode magit-gitflow htmlize helm-gitignore gnuplot gitignore-mode gitconfig-mode gitattributes-mode git-timemachine git-messenger git-link gh-md flyspell-correct-helm flyspell-correct flycheck-pos-tip pos-tip flycheck evil-magit magit magit-popup git-commit ghub with-editor eshell-z eshell-prompt-extras esh-help auto-dictionary ws-butler winum which-key volatile-highlights vi-tilde-fringe uuidgen use-package toc-org spaceline powerline restart-emacs request rainbow-delimiters popwin persp-mode pcre2el paradox spinner org-bullets open-junk-file neotree move-text macrostep lorem-ipsum linum-relative link-hint indent-guide hydra hungry-delete hl-todo highlight-parentheses highlight-numbers parent-mode highlight-indentation helm-themes helm-swoop helm-projectile helm-mode-manager helm-make projectile pkg-info epl helm-flx helm-descbinds helm-ag google-translate golden-ratio flx-ido flx fill-column-indicator fancy-battery eyebrowse expand-region exec-path-from-shell evil-visualstar evil-visual-mark-mode evil-unimpaired evil-tutor evil-surround evil-search-highlight-persist evil-numbers evil-nerd-commenter evil-mc evil-matchit evil-lisp-state smartparens evil-indent-plus evil-iedit-state iedit evil-exchange evil-escape evil-ediff evil-args evil-anzu anzu evil goto-chg undo-tree eval-sexp-fu highlight elisp-slime-nav dumb-jump f dash s diminish define-word column-enforce-mode clean-aindent-mode bind-map bind-key auto-highlight-symbol auto-compile packed aggressive-indent adaptive-wrap ace-window ace-link ace-jump-helm-line helm avy helm-core popup async)))
 '(vc-annotate-background "#fafafa")
 '(vc-annotate-color-map
   (list
    (cons 20 "#50a14f")
    (cons 40 "#688e35")
    (cons 60 "#807b1b")
    (cons 80 "#986801")
    (cons 100 "#ae7118")
    (cons 120 "#c37b30")
    (cons 140 "#da8548")
    (cons 160 "#c86566")
    (cons 180 "#b74585")
    (cons 200 "#a626a4")
    (cons 220 "#ba3685")
    (cons 240 "#cf4667")
    (cons 260 "#e45649")
    (cons 280 "#d2685f")
    (cons 300 "#c07b76")
    (cons 320 "#ae8d8d")
    (cons 340 "#383a42")
    (cons 360 "#383a42")))
 '(vc-annotate-very-old-color nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(spacemacs-normal-face ((t (:background "#4078f2" :foreground "#e7e7e7" :inherit (quote mode-line))))))
(defun dotspacemacs/emacs-custom-settings ()
  "Emacs custom settings.
This is an auto-generated function, do not modify its content directly, use
Emacs customize menu instead.
This function is called at the very end of Spacemacs initialization."
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#f0f0f0" "#e45649" "#50a14f" "#986801" "#4078f2" "#a626a4" "#0184bc" "#1b2229"])
 '(custom-safe-themes
   (quote
    ("b01b91ba9276deb39aa892c105a8644ef281b4d1804ab7c48de96e9c5d2aaa48" default)))
 '(evil-want-Y-yank-to-eol nil)
 '(fci-rule-color "#383a42" t)
 '(jdee-db-active-breakpoint-face-colors (cons "#f0f0f0" "#4078f2"))
 '(jdee-db-requested-breakpoint-face-colors (cons "#f0f0f0" "#50a14f"))
 '(jdee-db-spec-breakpoint-face-colors (cons "#f0f0f0" "#9ca0a4"))
 '(package-selected-packages
   (quote
    (vagrant-tramp vagrant treepy graphql terraform-mode hcl-mode dim unfill reveal-in-osx-finder pbcopy osx-trash osx-dictionary mwim launchctl wgrep smex ivy-hydra flyspell-correct-ivy counsel-projectile counsel swiper ivy helm-company helm-c-yasnippet fuzzy company-statistics company-go company-ansible company-anaconda company auto-yasnippet yasnippet ac-ispell auto-complete rvm ruby-tools ruby-test-mode rubocop rspec-mode robe rbenv rake puppet-mode minitest chruby bundler inf-ruby yapfify pyvenv pytest pyenv-mode py-isort pip-requirements live-py-mode hy-mode dash-functional helm-pydoc go-guru go-eldoc go-mode cython-mode anaconda-mode pythonic jinja2-mode ansible-doc ansible ranger yaml-mode doom-themes org-projectile org-plus-contrib xterm-color smeargle shell-pop orgit org-category-capture org-present org-pomodoro alert log4e gntp org-mime org-download multi-term mmm-mode markdown-toc markdown-mode magit-gitflow htmlize helm-gitignore gnuplot gitignore-mode gitconfig-mode gitattributes-mode git-timemachine git-messenger git-link gh-md flyspell-correct-helm flyspell-correct flycheck-pos-tip pos-tip flycheck evil-magit magit magit-popup git-commit ghub with-editor eshell-z eshell-prompt-extras esh-help auto-dictionary ws-butler winum which-key volatile-highlights vi-tilde-fringe uuidgen use-package toc-org spaceline powerline restart-emacs request rainbow-delimiters popwin pcre2el paradox spinner org-bullets open-junk-file neotree move-text macrostep lorem-ipsum linum-relative link-hint indent-guide hydra hungry-delete hl-todo highlight-parentheses highlight-numbers parent-mode highlight-indentation helm-themes helm-swoop helm-projectile helm-mode-manager helm-make projectile pkg-info epl helm-flx helm-descbinds helm-ag google-translate golden-ratio flx-ido flx fill-column-indicator fancy-battery eyebrowse expand-region exec-path-from-shell evil-visualstar evil-visual-mark-mode evil-unimpaired evil-tutor evil-surround evil-search-highlight-persist evil-numbers evil-nerd-commenter evil-mc evil-matchit evil-lisp-state smartparens evil-indent-plus evil-iedit-state iedit evil-exchange evil-escape evil-ediff evil-args evil-anzu anzu evil goto-chg undo-tree eval-sexp-fu highlight elisp-slime-nav dumb-jump f dash s diminish define-word column-enforce-mode clean-aindent-mode bind-map bind-key auto-highlight-symbol auto-compile packed aggressive-indent adaptive-wrap ace-window ace-link ace-jump-helm-line helm avy helm-core popup async)))
 '(vc-annotate-background "#fafafa")
 '(vc-annotate-color-map
   (list
    (cons 20 "#50a14f")
    (cons 40 "#688e35")
    (cons 60 "#807b1b")
    (cons 80 "#986801")
    (cons 100 "#ae7118")
    (cons 120 "#c37b30")
    (cons 140 "#da8548")
    (cons 160 "#c86566")
    (cons 180 "#b74585")
    (cons 200 "#a626a4")
    (cons 220 "#ba3685")
    (cons 240 "#cf4667")
    (cons 260 "#e45649")
    (cons 280 "#d2685f")
    (cons 300 "#c07b76")
    (cons 320 "#ae8d8d")
    (cons 340 "#383a42")
    (cons 360 "#383a42")))
 '(vc-annotate-very-old-color nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(spacemacs-normal-face ((t (:background "#4078f2" :foreground "#e7e7e7" :inherit (quote mode-line))))))
)
