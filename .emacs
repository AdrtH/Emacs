(defadvice basic-save-buffer-2 (around fix-unwritable-save-with-sudo activate)
  "When we save a buffer which is write-protected, try to sudo-save it.

When the buffer is write-protected it is usually opened in
read-only mode.  Use \\[read-only-mode] to toggle
`read-only-mode', make your changes and \\[save-buffer] to save.
Emacs will warn you that the buffer is write-protected and asks
you to confirm if you really want to save.  If you answer yes,
Emacs will use sudo tramp method to save the file and then
reverts it, making it read-only again.  The buffer stays
associated with the original non-sudo filename."
  (condition-case err
      (progn
        ad-do-it)
    (file-error
     (when (string-prefix-p
            "Doing chmod: operation not permitted"
            (error-message-string err))
       (let ((old-buffer-file-name buffer-file-name)
             (success nil))
         (unwind-protect
             (progn
               (setq buffer-file-name (concat "/sudo::" buffer-file-name))
               (save-buffer)
               (setq success t))
           (setq buffer-file-name old-buffer-file-name)
           (when success
             (revert-buffer t t))))))))


(setq shell-command-switch "-ic")

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
(transient-mark-mode 0)

;; (set-face-attribute 'default nil :height 175) ;; default zoom (including bottom bar and minibuffer)

(global-set-key (kbd "C-l") 'recenter-top-bottom)

(setq inhibit-startup-screen t)


;; duplicate a line up or down
(defun hpx/duplicate-line-down()
    (interactive)
    (move-beginning-of-line 1)
    (kill-line)
    (yank)
    (open-line 1)
    (next-line 1)
    (yank))

(defun hpx/duplicate-line-up()
  (interactive)
  (move-beginning-of-line 1)
  (kill-line)
  (yank)
  (previous-line 1)
  (move-end-of-line 1)
  (open-line 1)
  (next-line 1)
  (yank))

(global-set-key (kbd "s-n") 'hpx/duplicate-line-down)
(global-set-key (kbd "s-p") 'hpx/duplicate-line-up)

;; evaluate current elisp expression and print it to next line
(define-key lisp-interaction-mode-map (kbd "C-c C-e") 
  (lambda () 
    (interactive)
    (let ((result (eval (read (buffer-substring 
                               (point-at-bol) (point-at-eol)))))) 
      (goto-char (point-at-eol)) 
      (insert (format "\n%s" result)))))

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

(add-hook 'html-mode-hook 'skewer-html-mode)

;; marker ring
(setq set-mark-command-repeat-pop t)

;;JAVA THINGS
;; (use-package projectile)
(use-package flycheck)
(use-package yasnippet :config (yas-global-mode))
(use-package lsp-mode :hook ((lsp-mode . lsp-enable-which-key-integration))   :config (setq lsp-completion-enable-additional-text-edit nil))

(use-package hydra)
(use-package company)
(use-package lsp-ui)
(use-package which-key :config (which-key-mode))
(use-package lsp-java :config (add-hook 'java-mode-hook 'lsp))
(use-package dap-mode :after lsp-mode :config (dap-auto-configure-mode))
(use-package dap-java :ensure nil)
;; ;; (use-package helm-lsp)
;; ;;(use-package helm
;; ;;  :config (helm-mode))
;; (use-package lsp-treemacs)

;; projectile
;; (use-package projectile
;;     :ensure t
;;     :init (projectile-mode +1)
;;     :config
;;     (define-key
;;         projectile-mode-map
;;         (kbd "C-c p")
;;       'projectile-command-map))

(use-package 'auctex)
(use-package 'tex)
(use-package 'latex)

;; (push (list 'output-pdf "Xreader") TeX-view-program-selection)

(setq display-line-numbers 'relative
      display-line-numbers-current-absolute t)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#242424" "#e5786d" "#95e454" "#cae682" "#8ac6f2" "#333366" "#ccaa8f" "#f6f3e8"])
 '(compilation-message-face 'default)
 '(custom-enabled-themes '(gruber-darker))
 '(custom-safe-themes
   '("3d9df5511048d0815b1ccc2204cc739117c1a458be92fb26c03451149a1b1c11" "d143b38de4a3d1f02077f9b53e0b9405177321d98497e27ef8d2876aaaba5b75" "bddf21b7face8adffc42c32a8223c3cc83b5c1bbd4ce49a5743ce528ca4da2b6" default))
 '(display-line-numbers 'relative)
 '(display-line-numbers-current-absolute t)
 '(fci-rule-color "#010F1D")
 '(frame-brackground-mode 'dark)
 '(highlight-changes-colors '("#EF5350" "#7E57C2"))
 '(highlight-tail-colors
   '(("#010F1D" . 0)
     ("#B44322" . 20)
     ("#34A18C" . 30)
     ("#3172FC" . 50)
     ("#B49C34" . 60)
     ("#B44322" . 70)
     ("#8C46BC" . 85)
     ("#010F1D" . 100)))
 '(ispell-dictionary nil)
 '(line-number-mode t)
 '(magit-diff-use-overlays nil)
 '(package-selected-packages
   '(web-mode company-php phps-mode auctex whitespace4r quelpa impatient-mode emmet-mode skewer-mode ccls helm-lsp which-key-posframe lsp-ui flycheck projectile lsp-java lsp-mode rust-mode magit-gh-pulls haskell-mode md-readme magit smex ido-completing-read+ gruber-darker-theme company multiple-cursors))
 '(pos-tip-background-color "#FFF9DC")
 '(pos-tip-foreground-color "#011627")
 '(vc-annotate-background nil)
 '(vc-annotate-color-map
   '((20 . "#C792EA")
     (40 . "#CF4F1F")
     (60 . "#C26C0F")
     (80 . "#FFEB95")
     (100 . "#AB8C00")
     (120 . "#A18F00")
     (140 . "#989200")
     (160 . "#8E9500")
     (180 . "#F78C6C")
     (200 . "#729A1E")
     (220 . "#609C3C")
     (240 . "#4E9D5B")
     (260 . "#3C9F79")
     (280 . "#7FDBCA")
     (300 . "#299BA6")
     (320 . "#2896B5")
     (340 . "#2790C3")
     (360 . "#82AAFF")))
 '(vc-annotate-very-old-color nil)
 '(weechat-color-list
   '(unspecified "#011627" "#010F1D" "#DC2E29" "#EF5350" "#D76443" "#F78C6C" "#D8C15E" "#FFEB95" "#5B8FFF" "#82AAFF" "#AB69D7" "#C792EA" "#AFEFE2" "#7FDBCA" "#D6DEEB" "#FFFFFF")))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(put 'narrow-to-region 'disabled nil)
