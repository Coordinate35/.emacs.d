(require 'package)

(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))
  (add-to-list 'package-archives (cons "melpa-stable" (concat proto "://stable.melpa.org/packages/")) t)
  (add-to-list 'package-archives (cons "melpa" (concat proto "://melpa.org/packages/")) t)
  
  (when (< emacs-major-version 24)
    ;; For important compatibility libraries like cl-lib
    (add-to-list 'package-archives '("gnu" . (concat proto "://elpa.gnu.org/packages/")))))

(package-initialize)

(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")
(load-theme 'molokai t)

(require 'neotree)
(global-set-key [f8] 'neotree-toggle)

(require 'company)

(require 'yasnippet)
(setq yas-snippet-dirs
      '("~/.emacs.d/snippets"
        ))
(yas-reload-all)
(add-hook 'prog-mode-hook #'yas-minor-mode)


;; golang support
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(gotest molokai-theme neotree company-go company auto-complete lsp-mode go-mode)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(add-hook 'go-mode-hook 'lsp-deferred)
(add-hook 'go-mode-hook (lambda ()
                                (setq tab-width 4)
                                (require 'company-go)
                                (add-hook 'before-save-hook 'lsp-format-buffer)
                                (add-hook 'before-save-hook 'lsp-organize-imports)))

;; (add-to-list 'load-path "~/.emacs.d/manual-plugins/go-autocomplete")
;; (require 'go-autocomplete)
;; (require 'auto-complete-config)
;; (ac-config-default)
