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
;;(package-install 'company-irony)
;;
;; Remove company-clang to ensure company-irony.
(eval-after-load 'company
  '(and
    (add-to-list 'company-backends 'company-irony)
    (delete 'company-clang company-backends)))
