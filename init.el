(require 'package)

(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos)) 
		    (not (gnutls-available-p)))) 
       (proto (if no-ssl "http" "https"))) 
  (add-to-list 'package-archives (cons "melpa-stable" (concat proto
							      "://stable.melpa.org/packages/")) t) 
  (add-to-list 'package-archives (cons "melpa" (concat proto "://melpa.org/packages/")) t) 
  (when (< emacs-major-version 24)
    ;; For important compatibility libraries like cl-lib
    (add-to-list 'package-archives '("gnu" . (concat proto "://elpa.gnu.org/packages/")))))

(package-initialize)

(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")
(load-theme 'molokai t)

(setq x-select-enable-clipboard t)
(column-number-mode 1)

(require 'window-numbering)
(window-numbering-mode)

(require 'multiple-cursors)
(global-set-key (kbd "C-c C-e") 'mc/edit-lines)
(global-set-key (kbd "C-c C-n") 'mc/mark-next-like-this)
(global-set-key (kbd "C-c C-p") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-a") 'mc/mark-all-like-this)

(require 'neotree)
(setq-default neo-show-hidden-files t)
(global-set-key [f8] 'neotree-toggle)

(require 'rfc-mode)

(when (and (eq system-type 'gnu/linux)
           (string-match
            "Linux.*Microsoft.*Linux"
            (shell-command-to-string "uname -a")))
  (setq
   browse-url-generic-program  "/mnt/c/Windows/System32/cmd.exe"
   browse-url-generic-args     '("/c" "start")
   browse-url-browser-function #'browse-url-generic))
(add-to-list 'load-path "~/.emacs.d/manual-plugins/org-onenote")
(require 'org-onenote)

(setq org-log-done 'time)

(require 'elisp-format)

(require 'go-translate)
(setq gts-translate-list '(("en" "zh")))

(setq gts-default-translator (gts-translator :picker (gts-prompt-picker) 
					     :engines (list (gts-bing-engine) 
							    (gts-google-engine)) 
					     :render (gts-buffer-render)))

(require 'company)

(require 'yasnippet)
(setq yas-snippet-dirs '("~/.emacs.d/snippets"))
(yas-reload-all)
(add-hook 'prog-mode-hook #'yas-minor-mode)


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(dap-mode elisp-format impatient-mode go-translate rfc-mode window-numbering multiple-cursors gotest molokai-theme neotree company-go company auto-complete lsp-mode go-mode)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )


;; c/c++ support
(add-hook 'c-mode-hook 'lsp)
(add-hook 'c-mode-hook (lambda ()
			 (setq tab-width 4)
			 (require 'dap-cpptools)))
(add-hook 'c++-mode-hook 'lsp)


;; golang support
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


;; markdown support. Open http://localhost:8080/imp in brower like chrome to preview
(defun markdown-html (buffer) 
  (princ (with-current-buffer buffer (format
				      "<!DOCTYPE html><html><title>Impatient Markdown</title><xmp theme=\"united\" style=\"display:none;\"> %s  </xmp><script src=\"http://ndossougbe.github.io/strapdown/dist/strapdown.js\"></script></html>" 
				      (buffer-substring-no-properties 
				       (point-min) 
				       (point-max)))) 
	 (current-buffer)))
(add-hook 'markdown-mode-hook (lambda () 
				(httpd-start) 
				(impatient-mode 1) 
				(imp-set-user-filter 'markdown-html)))
