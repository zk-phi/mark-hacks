;;; -*- lexical-binding: t -*-

(require 'cl-lib)
(autoload 'yas-expand-snippet "yasnippet")

;; custom: exchange-marks
(defvar mark-hacks-enable-exchange-makrs t)

;; custom: swap-region
(defvar mark-hacks-enable-swap-region t)
(defvar mark-hacks-swap-pending-face 'cursor)

;; custom: auto-indent
(defvar mark-hacks-enable-auto-indent t)
(defvar mark-hacks-auto-indent-inhibit-modes
  '(fundamental-mode org-mode text-mode))
(defvar mark-hacks-auto-indent-limit 100)

;; custom: visible-register
(defvar mark-hacks-enable-visible-register t)
(defvar mark-hacks-visible-register-face 'cursor)

;; custom: one-shot-yasnippet
(defvar mark-hacks-enable-oneshot-yasnippet t)

;; internal vars
(defvar mark-hacks--visible-register nil)
(defvar mark-hacks--swap-pending-overlay nil)
(defvar mark-hacks--oneshot-snippet nil)
(make-variable-buffer-local 'mark-hacks--visible-register)
(make-variable-buffer-local 'mark-hacks--swap-pending-overlay)
(make-variable-buffer-local 'mark-hacks--oneshot-snippet)

(defun mark-hacks-register-set-mark-command (fn)
  (eval `(define-advice ,fn (:around (fn &rest args) mark-hacks-set-mark)
           (cond ((and mark-hacks-enable-visible-register
                       (eq this-command last-command))
                  (deactivate-mark)
                  (cond (mark-hacks--visible-register
                         (goto-char (overlay-start mark-hacks--visible-register))
                         (delete-overlay mark-hacks--visible-register)
                         (setq mark-hacks--visible-register nil))
                        (t
                         (setq mark-hacks--visible-register
                               (make-overlay (point) (1+ (point))))
                         (overlay-put mark-hacks--visible-register
                                      'face mark-hacks-visible-register-face))))
                 ((and mark-hacks-enable-exchange-makrs
                       mark-active)
                  (setq this-command 'exchange-point-and-mark)
                  (exchange-point-and-mark))
                 (t
                  (apply fn args))))))

(defun mark-hacks-register-yank-command (fn)
  (eval `(define-advice ,fn (:around (fn &rest args) mark-hacks-yank)
           (cond ((and mark-hacks-enable-swap-region
                       (eq last-command 'kill-region))
                  (setq mark-hacks--swap-pending-overlay
                        (make-overlay (point) (1+ (point))))
                  (overlay-put mark-hacks--swap-pending-overlay
                               'face mark-hacks-swap-pending-face))
                 (t
                  (when mark-hacks--swap-pending-overlay
                    (delete-overlay mark-hacks--swap-pending-overlay)
                    (setq mark-hacks--swap-pending-overlay nil))
                  (let* ((beg (point))
                         (end (progn (apply fn args) (point))))
                    (when (and mark-hacks-enable-auto-indent
                               (not (cl-some 'derived-mode-p
                                             mark-hacks-auto-indent-inhibit-modes)))
                      (cond ((< (count-lines beg end) mark-hacks-auto-indent-limit)
                             (indent-region beg end)
                             (message "auto-indent done"))
                            (t
                             (message "auto-indent canceled.")
                             (sit-for 0.5))))))))))

(defun mark-hacks-register-kill-command (fn)
  (eval `(define-advice ,fn (:around (fn &rest args) mark-hacks-kill)
           (cond (mark-hacks--swap-pending-overlay
                  (let* ((str (buffer-substring (region-beginning) (region-end)))
                         (pending-pos (overlay-start mark-hacks--swap-pending-overlay))
                         (pos (+ (region-beginning)
                                 (if (< pending-pos (point)) (length str) 0))))
                    (delete-overlay mark-hacks--swap-pending-overlay)
                    (setq mark-hacks--swap-pending-overlay nil)
                    (delete-region (region-beginning) (region-end))
                    (goto-char pending-pos)
                    (insert str)
                    (goto-char pos)))
                 (t
                  (apply fn args)))
           (setq this-command 'kill-region))))

(defun mark-hacks-expand-oneshot-snippet ()
  (interactive)
  (cond ((not mark-hacks--oneshot-snippet)
         (error "no oneshot snippet is registered."))
        ((fboundp 'yas-expand-snippet)
         (yas-expand-snippet mark-hacks--oneshot-snippet (point) (point) nil))
        (t
         (message "yasnippet not installed"))))

(defun mark-hacks-register-copy-command (fn)
  (eval `(define-advice ,fn (:around (fn &rest args) mark-hacks-copy)
           (cond ((and (called-interactively-p 'any)
                       mark-hacks-enable-oneshot-yasnippet
                       (eq last-command this-command))
                  (let ((beg (region-beginning))
                        (end (region-end)))
                    (setq mark-hacks--oneshot-snippet (buffer-substring beg end))
                    (delete-region beg end)
                    (deactivate-mark)
                    (mark-hacks-expand-oneshot-snippet)))
                 (t
                  (apply fn args))))))

(provide 'mark-hacks)
