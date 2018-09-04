;;; Installing the MELPA repository.
(require 'package)
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
		    (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))
  ;; Comment/uncomment these two lines to enable/disable MELPA and MELPA Stable as desired
  (add-to-list 'package-archives (cons "melpa" (concat proto "://melpa.org/packages/")) t)
  ;;(add-to-list 'package-archives (cons "melpa-stable" (concat proto "://stable.melpa.org/packages/")) t)
  (when (< emacs-major-version 24)
    ;; For important compatibility libraries like cl-lib
    (add-to-list 'package-archives '("gnu" . (concat proto "://elpa.gnu.org/packages/")))))
(package-initialize)
;;(package-refresh-contents)



;;; Company-Mode: Modular in-buffer completion framework for Emacs.
;;; It uses company-clang as its back-end.
;;(package-install 'company)
(add-hook 'after-init-hook 'global-company-mode)


;;; Irony-Mode: A C/C++ minor mode powered by libclang.
;;; It improves code completion and provide syntax error highlighting.
;;; On the first run, build and install irony-server: M-x irony-install-server RET.
;;(package-install 'irony)
(add-hook 'c++-mode-hook 'irony-mode)
(add-hook 'c-mode-hook 'irony-mode)
(add-hook 'objc-mode-hook 'irony-mode)

(add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)

;; Windows performance tweaks
;;
(when (boundp 'w32-pipe-read-delay)
  (setq w32-pipe-read-delay 0))
;; Set the buffer size to 64K on Windows (from the original 4K)
(when (boundp 'w32-pipe-buffer-size)
  (setq irony-server-w32-pipe-buffer-size (* 64 1024)))


;;; Company Irony: A completion backend for Irony-Mode.
;;; Company C Headers: Auto-completion for C/C++ headers using Company.
;;(package-install 'company-irony)
;;(package-install 'company-c-headers)
;;
;; Load with `irony-mode` as a grouped backend.
;; Remove company-clang to ensure company-irony.
(eval-after-load 'company
  '(and
    (add-to-list
     'company-backends '(company-c-headers company-irony))
    (delete
     'company-clang company-backends)))

;; Setup search paths properly.
(setq company-c-headers-path-system
      '("/usr/include/" "/usr/local/include/" "/usr/include/c++/8.2.0"))
(defun company-c-headers-path-user-irony ()
  "Return the user include paths for the current buffer."
  (when irony-mode
    (or
     (irony--extract-user-search-paths irony--compile-options
				       irony--working-directory)
     '("."))))
(setq company-c-headers-path-user #'company-c-headers-path-user-irony)


;;; Flycheck-Irony: Flycheck checker for irony-mode.
;;(package-install 'flycheck)
;;(package-install 'flycheck-irony)
(global-flycheck-mode)

(eval-after-load 'flycheck
  '(add-hook 'flycheck-mode-hook #'flycheck-irony-setup))


;;; YASnippet: A template system for Emacs.
;;(package-install 'yasnippet)
;;(package-install 'yasnippet-snippets)
(require 'yasnippet)
(yas-reload-all)
(add-hook 'prog-mode-hook #'yas-minor-mode)


;;; Iedit - Edit multiple regions in the same way simultaneously.
;;(package-install 'iedit)
(require 'iedit)


;;; Coding style.
;;; CC-Mode.
(setq c-default-style '((c++-mode . "stroustrup")
			(c-mode . "k&r"))
      c-basic-offset 4)
(add-hook 'c-mode-common-hook (lambda () (c-toggle-electric-state +1) (c-toggle-auto-newline +1)))
;;
(electric-pair-mode 1)


;;; ClangFormat: Automatic source code formatting based on Clang.
;;(package-install 'clang-format)
(global-set-key [C-tab] 'clang-format-region)


;;; Ispell: A program that helps you to correct spelling and typographical errors.
(add-hook 'c-mode-common-hook (lambda () (ispell-change-dictionary "english") (flyspell-prog-mode)))


;;; Change Emacs appearance and behavior.
;;;
(set-default 'inhibit-startup-screen t)
(set-default 'truncate-lines t)
(load-theme 'misterioso t)
(setq make-backup-files nil) ; Stop creating backup~ files.
