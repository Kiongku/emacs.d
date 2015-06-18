;;; init-vim.el --- initialize vim mode in Emacs

;; Author: Wai Keung Yiu Man Lung <wai.keung.yiu@gmail.com>
;; Version: 1.0
;; Keywords: vi, vim

;;; Commentary:

;; This package initializes vim mode in Emacs

;;; Code:

;; Assign C-u to vim scroll
(setq evil-want-C-u-scroll t)
(require-package 'evil)

(evil-mode 1)

;; change mode-line colour by evil state
(lexical-let ((default-color (cons (face-background 'mode-line)
                                   (face-foreground 'mode-line))))
  (add-hook 'post-command-hook
            (lambda ()
              (let ((color (cond ((minibufferp) default-color)
                                 ((evil-insert-state-p) '("#e80000" . "#ffffff"))
                                 ((evil-emacs-state-p)  '("#444488" . "#ffffff"))
                                 ((buffer-modified-p)   '("#006fa0" . "#ffffff"))
                                 (t default-color))))
                (set-face-background 'mode-line (car color))
                (set-face-foreground 'mode-line (cdr color))))))

;; vim tabs
(require-package 'elscreen)
(load "elscreen" "Elscreen" t)
(elscreen-start)
;; keybindings
(define-key evil-normal-state-map (kbd "C-w t") 'elscreen-create) ; create tab
(define-key evil-normal-state-map (kbd "C-w x") 'elscreen-kill) ; kill tab
(define-key evil-normal-state-map (kbd "gT") 'elscreen-previous) ; go to previous tab
(define-key evil-normal-state-map (kbd "gt") 'elscreen-next) ; go to next tab

;; enable vim surround
(require-package 'evil-surround)
(global-evil-surround-mode 1)

;; number increment/decrement functions
(require-package 'evil-numbers)
(define-key evil-normal-state-map (kbd "C-c +") 'evil-numbers/inc-at-pt)
(define-key evil-normal-state-map (kbd "C-c -") 'evil-numbers/dec-at-pt)

(provide 'init-vim)

;;; init-vim.el ends here
