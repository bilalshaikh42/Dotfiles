;;; code:


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                                                       ;
;                     General Settings                                  ;
;                                                                       ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;save between sessions
(desktop-save-mode 1)

;; Don't show the start up screen/other messages  when Emacs opens
(setq inhibit-startup-message t
      inhibit-default-init t
      inhibit-startup-echo-area-message "Bilal"
      initial-scratch-message "")

;;show line numbers on left margin
(global-linum-mode t)

;;highlight matching parens
(show-paren-mode t)



;; automatically refresh files if there are outside changes. Do this for remote files too

(global-auto-revert-mode 1)
(setq auto-revert-remote-files 1)
(setq revert-without-query '(".*"))


;; Ignore modification-time-only changes in files, i.e. ones that
;; don't really change the contents.  
(defun update-buffer-modtime-if-byte-identical ()
  (let* ((size      (buffer-size))
         (byte-size (position-bytes size))
         (filename  buffer-file-name))
    (when (and byte-size (<= size 1000000))
      (let* ((attributes (file-attributes filename))
             (file-size  (nth 7 attributes)))
        (when (and file-size
                   (= file-size byte-size)
                   (string= (buffer-substring-no-properties 1 (1+ size))
                            (with-temp-buffer
                              (insert-file-contents filename)
                              (buffer-string))))
          (set-visited-file-modtime (nth 5 attributes))
          t)))))

(defun verify-visited-file-modtime--ignore-byte-identical (original &optional buffer)
  (or (funcall original buffer)
      (with-current-buffer buffer
        (update-buffer-modtime-if-byte-identical))))
(advice-add 'verify-visited-file-modtime :around #'verify-visited-file-modtime--ignore-byte-identical)

(defun ask-user-about-supersession-threat--ignore-byte-identical (original &rest arguments)
  (unless (update-buffer-modtime-if-byte-identical)
    (apply original arguments)))
(advice-add 'ask-user-about-supersession-threat :around #'ask-user-about-supers)




(fset 'yes-or-no-p 'y-or-n-p) ;; accept y/n for yes/no prompts


;;Backup and auto-save settings
(setq backup-by-copying t  ;;dont overwrite symlinks
      backup-directory-alist '(("." . "~/.emacs.d/auto-saves/")) ;; don't litter FS with save files
      delete-old-versions t ;;don't keep too many old files
      kept-new-versions 10
      kept-old-versions 5
      version-control t) ;;use versioning for backups

(setq auto-save-file-name-transforms
      '((".*" "~/.emacs.d/auto-saves/" t)))

;; tramp auto save settings
'( tramp-auto-save-directory "~/.emacs.d/auto-saves/")


;;use the ibuffer buffer management

(global-set-key (kbd "C-x C-b") 'ibuffer)


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

(use-package elpy
  :ensure t)

(use-package py-autopep8
  :ensure t)


;;install fly-check and fly-check-tip, which do syntax checking

(use-package flycheck
  :ensure t)

(use-package flycheck-pos-tip
  :ensure t)

;;smart parens, which provides IDE like paren management
(use-package smartparens
  :ensure t
  :config
  (require 'smartparens-config)
  '(sp-base-key-binding 'paraedit)
  (setq sp-autoskip-closing-pair 'always)
  (setq sp-hybrid-kill-entire-symbol nil)
  (sp-use-paredit-bindings))

;;AUCTeX for latex tools
(use-package auctex
  :ensure t
  :defer t
  :config
  (setq TeX-auto-save t)
  (setq TeX-parse-self t)
  ;;(setq-default TeX-master nil) ;;AUCTeX will prompt for master file when creating new file
  (setq global-font-lock-mode t))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                                                       ;
;                      Editing Settings                                 ;
;                                                                       ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Spell check options
(add-hook 'text-mode-hook 'flyspell-mode)
(add-hook 'prog-mode-hook 'flyspell-prog-mode)


;;syntax checking
(add-hook 'prog-mode-hook 'flycheck-mode)
(add-hook 'flycheck-mode-hook 'flycheck-pos-tip-mode)

;;Python Editing
(add-hook 'python-mode-hook 'elpy-mode)
(add-hook 'elpy-mode-hook 'py-autopep8-enable-on-save)



;;Theme Settings

;;(use-package solarized-theme
;;             :ensure t)
;;(load-theme 'solarized-dark t)

(use-package monokai-theme
  :ensure t)
(load-theme 'monokai t)

;;Font Settings
'(default ((t (:family "DejaVu Sans Mono" :foundry "PfEd" :slant normal :weight normal :height 116 :width normal))))

(provide '.emacs)
;;; .emacs ends here
