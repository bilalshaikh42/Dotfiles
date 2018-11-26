(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives '("marmalade" . "https://marmalade-repo.org/packages/"))
(add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/"))
(package-initialize)

(use-package auto-package-update
  :ensure t
  :config
  (setq auto-package-update-delete-old-versions t)
  (setq auto-package-update-hide-results t)
  (auto-package-update-maybe))

;; use bash as the shell
(setq shell-file-name "/bin/bash")


;;dont litter the init file with custom settings
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file 'noerror)

;; dont minimize on control-z
(global-unset-key "\C-z")
(global-unset-key "\C-x\C-z") 

(fset 'yes-or-no-p 'y-or-n-p) ;; accept y/n for yes/no prompts

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

;;save between sessions
;;Disabled for now, might enable at a later date
;(desktop-save-mode 1)
(setq desktop-path '("~/.emacs.d/"))
(setq desktop-dirname "~/.emacs.d/")
(setq desktop-base-file-name "emacs-desktop")

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

;; Don't show the start up screen/other messages  when Emacs opens
(setq inhibit-startup-message t
      inhibit-default-init t
      inhibit-startup-echo-area-message "Bilal"
      initial-scratch-message "")

;;show line numbers on left margin
(add-hook 'prog-mode-hook 'linum-mode)

;;highlight matching parens
(show-paren-mode t)

;; turn off the bell
(setq ring-bell-function 'ignore)

;;use the ibuffer buffer management
(global-set-key (kbd "C-x C-b") 'ibuffer)

;;switch between frames easily
(global-set-key (kbd "C-x M-o") 'next-multiframe-window)

(use-package flyspell
  :ensure t
  :hook 
  (text-mode . flyspell-mode)
  (prog-mode . flyspell-prog-mode))
(use-package flyspell-popup
  :ensure t
  :config
  (flyspell-popup-autocorrect-mode))

;;install fly-check and fly-check-tip, which do syntax checking

(use-package flycheck-pos-tip
  :ensure flycheck
  :config 
  (global-flycheck-mode)
  (add-hook 'prog-mode-hook 'flycheck-mode)
  (add-hook 'latex-mode-hook 'flycheck-mode)
  (add-hook 'flycheck-mode-hook 'flycheck-pos-tip-mode))

(use-package company
  :ensure t
  :hook 
  (after-init . global-company-mode))

;;HS mode bindings when not already in use
;; Call this function as needed through hooks
(defun hs-minor-mode-keys ()
  (local-set-key "\C-ch" 'hs-hide-block)
  (local-set-key "\C-cs" 'hs-show-block))

;;AUCTeX for latex tools
(use-package tex
  :ensure auctex
  :config
  (setq TeX-auto-save t)
  (setq TeX-parse-self t)
  ;;(setq-default TeX-master nil) ;;AUCTeX will prompt for master file when creating new file
  (setq global-font-lock-mode t)
  :bind (:map LaTeX-mode-map
  ("C-<tab>" . 'TeX-complete-symbol)))

(use-package company-auctex
  :ensure t
  :after company
  :after tex
  :config
  (company-auctex-init))

(use-package elpy
  :ensure t
  :defer t
  :config
  (setq elpy-rpc-backend "jedi")
  :bind
  (:map elpy-mode-map ("C-c C-z" . 'elpy-shell-switch-to-shell)))

(use-package py-autopep8
  :ensure t
  :defer t)

(use-package ein
  :ensure t
  :defer t)
(setenv "WORKON_HOME" "~/env/miniconda3/envs/")
(pyvenv-mode 1)


(add-hook 'python-mode-hook 'elpy-mode)
(add-hook 'elpy-mode-hook 'py-autopep8-enable-on-save)
(add-hook 'elpy-mode-hook 'hs-minor-mode)
(add-hook 'elpy-mode-hook 'hs-minor-mode-keys)

;;Install try, which lets you run a package without installing it
(use-package try
  :ensure t
  :defer t)

;; Install, which key, which brings up help on key combinations
(use-package which-key
  :ensure t
  :config
  (which-key-mode 1)
  :bind
  ("C-x w". which-key-show-top-level))

  ;;Install Magit, a git porcelain. Set key for common command
(use-package magit
  :ensure t
  :defer t
 :bind
  ("C-x g" . magit-status))





;;smart parens, which provides IDE like paren management
(use-package smartparens
  :ensure t
  :config
  (require 'smartparens-config)
  '(sp-base-key-binding 'paraedit)
  (setq sp-autoskip-closing-pair 'always)
  (setq sp-hybrid-kill-entire-symbol nil)
  (sp-use-paredit-bindings))

;;Evil to provide VIM keybindings
(use-package evil
  :ensure t
  :config
  '(evil-set-initial-state 'magit-popup-mode 'emacs)
  '(evil-set-initial-state 'org-mode 'emacs)
  (setq evil-default-state 'emacs))
  

;;docker-tramp mode which extends tramp to work within docker containers
(use-package docker-tramp
  :ensure t
  :defer t)

;; dockerfile mode which provides syntactical highlighting for dockerfiles
(use-package dockerfile-mode
  :ensure t
  :defer t)

(use-package ascii-art-to-unicode
  :ensure t
  :defer t)

(use-package pdf-tools
  :ensure t
  :defer t
  :config
  (pdf-tools-install)
  ;; open pdfs scaled to fit page
  (setq-default pdf-view-display-size 'fit-page))
(use-package org-pdfview
  :ensure t
  :defer t
  :config
  (eval-after-load 'org '(require 'org-pdfview))
  (add-to-list 'org-file-apps
               '("\\.pdf\\'" . (lambda (file link) (org-pdfview-open link)))))

;; Load org and set some key bindings and enable encryption
(use-package org
  :ensure t
  :defer t
  :init
  (require 'org-crypt)
  (require 'ox)
  (require 'ox-org)
  (org-crypt-use-before-save-magic)
  (setq org-tags-exclude-from-inheritance (quote ("crypt")))
  ;; GPG key to use for encryption
  ;; Either the Key ID or set to nil to use symmetric encryption.
  (setq org-crypt-key nil)
  (require 'org-habit)
  :config
  (add-to-list 'org-file-apps' ("\\.pdf\\'" . (lambda (file link) (org-pdfview-open link))))
  (setq org-agenda-files (quote("~/Org/")))
  (setq org-todo-keywords'((sequence  "TODO(t)" "IN-PROGRESS(p)"  "WAIT(w@/!)" "SOMEDAY(s)" "|" "DONE(d!)" "CANCELED(c@)")))
  (setq org-enforce-todo-dependencies nil)
  :hook (org-mode . visual-line-mode)
  :hook (org-mode . org-indent-mode)
  :bind(
	("C-c l" . org-store-link)
	("C-c a" . org-agenda)
	("C-c c" . org-capture))
  :bind ( :map org-mode-map
	       ("C-c d" . org-decrypt-entries)))

(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (org . t)))

(use-package org-bullets
  :ensure t
  :defer t
  ;;:hook (org-mode . org-bullets-mode)
)

;; have the ctrl-e and ctrol-a keys work better for emacs headlines
(setq org-special-ctrl-a/e t)
;; Change org elipses to something better
(setq org-ellipsis " ▼")
;;Have tab at the end of a line move to within the header so that they next tab opens up the heading 
(add-hook 'org-tab-first-hook 'org-end-of-line)

;; org-gcal to sync agenda to google calendar
(use-package org-gcal
  :ensure t
  :config
  (setq org-gcal-client-id "8240918350-f32o6lnqmbfuvcledi75ptbf7aia2iv0.apps.googleusercontent.com"
	org-gcal-client-secret "KryFDAztv4ysgsm2Cr_NyMMq" ;; Not really secret
	org-gcal-file-alist '(("bill2507733@gmail.com" .  "~/Org/Appointments.org")))
  (add-hook 'org-agenda-mode-hook (lambda () (org-gcal-sync) ))
  (add-hook 'org-capture-after-finalize-hook (lambda () (org-gcal-sync) )))

;; Org-brain
(use-package org-brain
  :ensure t
  :init
  (setq org-brain-path "~/Org/")
  ;; For Evil users
  (with-eval-after-load 'evil
    (evil-set-initial-state 'org-brain-visualize-mode 'emacs))
  :config
  (setq org-id-track-globally t)
  (setq org-id-locations-file "~/.emacs.d/.org-id-locations")
  (setq org-brain-visualize-default-choices 'all)
  (setq org-brain-title-max-length 12)
  :bind
   ("C-c v" . org-brain-visualize)
  :bind( :map org-mode-map
	      ("C-c i" . org-id-get-create)))

(use-package org-super-agenda
  :ensure t
  :config
  (setq org-super-agenda-groups
	'((:log t
		:order 0)
	  
	  (:name "Habits"
		 :habit t
		 :order 7)
	  
	  (:name "Self-paced"
		 :todo ("SOMEDAY" "TO-READ") 
		 :order 8)
	  
	  (:name "Overdue!!"
		 :deadline past
		 :order 2)

	  (:name "Missed!"
		 :scheduled past
		 :order 3)
	 
	  (:name "Today"
		 :scheduled today
		 :deadline today
		 :order 4)
	  
	  (:name "Planned"
		 :scheduled t
		 :order 5)
	  
	  (:name "Upcoming"
		 :deadline future
		 :order 6)
	

	  (:name "Schedule"
		 :time-grid t
		 :order 1)
	  ))
  :hook (org-agenda org-super-agenda-mode))

;;used to turn on ascii-art-to-unicode package
(defun aa2u-buffer ()
  (aa2u (point-min) (point-max)))
  (add-hook 'org-brain-after-visualize-hook #'aa2u-buffer)

;; org-mindmap

(use-package org-mind-map
  :ensure t
  :config
    (setq org-mind-map-engine "dot")  ; default; Directed Graph
    ;; (setq org-mind-map-engine "neato")  ; Undirected Spring Graph"
    ;; (setq org-mind-map-engine "twopi")  ; Radial Layout"
    ;; (setq org-mind-map-engine "circo")  ; Circular Layout"
    ;; (setq org-mind-map-engine "fdp")  ; Undirected Spring Force-Directed"
  )

(use-package ox-hugo
  :ensure t
  :after ox)

(use-package htmlize
   :ensure t
)

(defun my/org-inline-css-hook (exporter)
  "Insert custom inline css to automatically set the
background of code to whatever theme I'm using's background"
  (when (eq exporter 'html)
    (let* ((my-pre-bg (face-background 'default))
           (my-pre-fg (face-foreground 'default)))
      (setq
       org-html-head-extra
       (concat
        org-html-head-extra
        (format "<style type=\"text/css\">\n pre.src {background-color: %s; color: %s;}</style>\n"
                my-pre-bg my-pre-fg))))))

(add-hook 'org-export-before-processing-hook 'my/org-inline-css-hook)

(use-package solarized-theme
  :ensure t
  :defer t)
;;(load-theme 'solarized-dark t)

(use-package monokai-theme
  :ensure t
  :defer t)
;;(load-theme 'monokai t)

(use-package spacemacs-theme
  :ensure t
  :defer t)
(load-theme 'spacemacs-dark)

(setq default-frame-alist initial-frame-alist)
;;Font Settings
'(default ((t (:family "Consolas" :foundry "PfEd" :slant normal :weight normal :height 116 :width normal))))
