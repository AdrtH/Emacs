(setq warning-minimum-level :emergency)
;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(tool-bar-mode 0)
(menu-bar-mode 0)
(scroll-bar-mode 0)
(column-number-mode 1)
(show-paren-mode 1)

(global-set-key (kbd "C-l") 'recenter-top-bottom)

(setq inhibit-startup-screen t)

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(package-initialize)

(add-to-list 'custom-theme-load-path
             "~/Emacs/Theme/")
(add-to-list 'custom-theme-load-path
             "/path/to/gruber-darker-theme/")

(require 'company)
(add-hook 'after-init-hook 'global-company-mode)

(require 'yasnippet)
(add-hook 'after-init-hook 'yas-global-mode)
(global-set-key (kbd "C-*") 'yas-expand)


(require 'multiple-cursors)
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)
(global-set-key (kbd "C-$") 'mc/skip-to-next-like-this)
(global-set-key (kbd "C-£")  'mc/skip-to-previous-like-this)
(define-key mc/keymap (kbd "<return>") nil)


(require 'smex)
(require 'ido-completing-read+)

(ido-mode 1)
(ido-everywhere 1)
(ido-ubiquitous-mode 1)

(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "C-x M-x") 'execute-extended-command)

(defadvice smex (around space-inserts-hyphen activate compile)
  (let ((ido-cannot-complete-command 
	 `(lambda ()
	    (interactive)
	    (if (string= " " (this-command-keys))
		(insert ?-)
	      (funcall ,ido-cannot-complete-command)))))
    ad-do-it))

(require 'cl-lib)
(require 'magit)
(setq magit-auto-revert-mode nil)
(global-set-key (kbd "C-x g") 'magit-status)

(global-set-key (kbd "C-c m s") 'magit-status)
(global-set-key (kbd "C-c m l") 'magit-log)

(require 'move-text)
(global-set-key (kbd "M-p") 'move-text-up)
(global-set-key (kbd "M-n") 'move-text-down)


(require 'haskell-mode)
(setq haskell-interactive-popup-errors nil)


;; marker ring
(setq set-mark-command-repeat-pop t)

(setq display-line-numbers 'relative
      display-line-numbers-current-absolute t)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#242424" "#e5786d" "#95e454" "#cae682" "#8ac6f2" "#333366" "#ccaa8f" "#f6f3e8"])
 '(custom-enabled-themes '(gruber-darker))
 '(custom-safe-themes
   '("bddf21b7face8adffc42c32a8223c3cc83b5c1bbd4ce49a5743ce528ca4da2b6" default))
 '(display-line-numbers 'relative)
 '(display-line-numbers-current-absolute t)
 '(line-number-mode t)
 '(package-selected-packages
   '(magit-gh-pulls haskell-mode md-readme magit smex ido-completing-read+ gruber-darker-theme company multiple-cursors)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
