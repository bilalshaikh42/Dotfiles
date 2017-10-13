;; General Settings
;; Dont show the start up screen when emacs opens
(setq inhibit-startup-message t)
(global-auto-revert-mode t) ;; automatically refresh files if there are outside changes


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
 '(show-paren-mode t))
