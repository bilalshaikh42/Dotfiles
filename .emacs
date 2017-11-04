

;; General Settings
;; Dont show the start up screen/other messages  when emacs opens
(setq inhibit-startup-message t
      inhibit-default-init t
      inhibit-startup-echo-area-message "Bilal"
      initial-scratch-message "")

;; automatically refresh files if there are outside changes. Do this for remote files too

(setq global-auto-revert-mode t
      auto-revert-remote-files t)

(fset 'yes-or-no-p 'y-or-n-p) ;; accept y/n for yes/no prompts


;;Backup and auto-save settings
(setq backup-by-copying t  ;;dont overwrite symlinks
      backup-directory-alist '(("." . "~/.emacs.d/auto-saves/")) ;; dont litter FS with save files
      delete-old-versions t ;;dont keep too many old files
      kept-new-versions 10
      kept-old-versions 5
      version-controll t) ;;use versioning for backups

(setq auto-save-file-name-transforms
      '((".*" "~/.emacs.d/auto-saves/" t)))

;; tramp auto save seetings
(setq tramp-auto-save-directory "~/.emacs.d/auto-saves/")


;;load the package system. Add the melpa, marmalade, gnu archives
(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives '("marmalade" . "https://marmalde-repo.org/packages/"))
(add-to-list 'package-archives '("gnu" . "https://elapa.gnu.org/packges/"))
(package-initialize)

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
  :ensure t)
(global-set-key (kbd "C-x g") 'magit-status)

;;Theme Settings

(use-package solarized-theme
             :ensure t)
(load-theme 'solarized-dark t)

;;Font Settings
'(default ((t (:family "DejaVu Sans Mono" :foundry "PfEd" :slant normal :weight normal :height 116 :width normal))))

;; C mode settings
;; Allow for tabs to be used within lines
(setq c-tab-always-indent nil)



(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages (quote (solarized-theme which-key try use-package)))
 '(show-paren-mode t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
