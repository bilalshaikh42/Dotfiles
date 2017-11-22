;;; code:

;; General Settings


;;save between sessions
(desktop-save-mode 1)

;; Dont show the start up screen/other messages  when emacs opens
(setq inhibit-startup-message t
      inhibit-default-init t
      inhibit-startup-echo-area-message "Bilal"
      initial-scratch-message "")

;;show line numbers on left margin
(global-linum-mode t)

;;highlight matching parens
(global-linum-mode t)


;; automatically refresh files if there are outside changes. Do this for remote files too

(setq global-auto-revert-mode t)
'( auto-revert-remote-files t)

(fset 'yes-or-no-p 'y-or-n-p) ;; accept y/n for yes/no prompts


;;Backup and auto-save settings
(setq backup-by-copying t  ;;dont overwrite symlinks
      backup-directory-alist '(("." . "~/.emacs.d/auto-saves/")) ;; dont litter FS with save files
      delete-old-versions t ;;dont keep too many old files
      kept-new-versions 10
      kept-old-versions 5
      version-control t) ;;use versioning for backups

(setq auto-save-file-name-transforms
      '((".*" "~/.emacs.d/auto-saves/" t)))

;; tramp auto save seetings
'( tramp-auto-save-directory "~/.emacs.d/auto-saves/")


;;use the ibuffer buffer management

(global-set-key (kbd "C-x C-b") 'ibuffer)

;; C mode settings
;; Allow for tabs to be used within lines
'(c-tab-always-indent nil)




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                                                       ;
;                        Packages                                       ;
;                                                                       ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;load the package system. Add the melpa, marmalade, gnu archives
(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives '("marmalade" . "https://marmalade-repo.org/packages/"))
(add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/"))
(package-initialize)

(package-refresh-contents)

;;get the use-package system
(unless (package-installed-p 'use-package)
        (package-refresh-contents)
        (package-install 'use-package))


;;Install try, which lets you run a package without installing it
(use-package try
	     :ensure t)

;; Install, which key, which brings up help on key combinations
(use-package which-key
	     :ensure t
	     :config
	     (which-key-mode))

;;Install Magit, a git porcelain. Set key for common command.
(use-package magit
  :ensure t
  :defer t
  :bind
  ("C-x g" . magit-status))


;;install fly-check and fly-check-tip, which do syntax checking

(use-package flycheck
  :ensure t
  :config
  (global-flycheck-mode))

(use-package flycheck-pos-tip
  :ensure t
  :config
  '(flycheck-pos-tip-mode))


;;smart parens, which provides IDE like paren management
(use-package smartparens
  :ensure t
  :config
  (require 'smartparens-config)
  '(sp-base-key-binding 'paraedit)
  (setq sp-autoskip-closing-pair 'always)
  (setq sp-hybrid-kill-entire-symbol nil)
  (sp-use-paredit-bindings))



;;Theme Settings

(use-package solarized-theme
             :ensure t)
(load-theme 'solarized-dark t)

;;Font Settings
'(default ((t (:family "DejaVu Sans Mono" :foundry "PfEd" :slant normal :weight normal :height 116 :width normal))))

(provide '.emacs)
;;; .emacs ends here
