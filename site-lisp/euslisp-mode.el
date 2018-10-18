;;; euslisp-mode.el --- euslisp mode                 -*- lexical-binding: t; -*-

;; Copyright (C) 2018  furushchev

;; Author: Yuki Furuta <furushchev@jsk.imi.i.u-tokyo.ac.jp>
;; Keywords: lisp

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Major mode for editing euslisp code

;;; Code:

(require 'cl-indent)
(require 'lisp-mode)

;;; Constants:

(defconst euslisp-mode-version "0.0.1"
  "Euslisp mode Version")


;;; Groups:

(defgroup euslisp-mode ()
  "A mode for editing Euslisp codes in Emacs"
  :prefix "euslisp-"
  :group 'languages)

(defcustom euslisp-indent-function #'euslisp-indent-function
  "Indent function for euslisp codes"
  :type 'function
  :group 'euslisp-mode)


;;; Functions:

(defun euslisp-indent-line (&optional indent)
  (interactive)
  (let ((lisp-indent-function 'euslisp-indent-function))
    (lisp-indent-line indent)))

(defun euslisp-indent-function (indent-point state)
  (let ((normal-indent (current-column)))
    (goto-char (1+ (elt state 1)))
    (parse-partial-sexp (point) calculate-lisp-indent-last-sexp 0 t)
    (if (and (elt state 2)
             (not (looking-at "\\sw\\|\\s_")))
        (progn
          (if (not (> (save-excursion (forward-line 1) (point))
                      calculate-lisp-indent-last-sexp))
              (progn (goto-char calculate-lisp-indent-last-sexp)
                     (beginning-of-line)
                     (parse-partial-sexp (point)
                                         calculate-lisp-indent-last-sexp 0 t)))
          (backward-prefix-chars)
          (current-column))
      (let ((function (buffer-substring (point)
                                        (progn (forward-sexp 1) (point))))
            (up-func (ignore-errors
                       (buffer-substring
                        (save-excursion (backward-up-list 2) (forward-char) (point))
                        (save-excursion (backward-up-list 2) (forward-char) (forward-sexp) (point)))))
            method)
        (setq method (or (function-get (intern-soft function) 'euslisp-indent-function)
                         (function-get (intern-soft function) 'lisp-indent-function)
                         (get (intern-soft function) 'lisp-indent-hook)))
        (cond ((and up-func
                    (string-match "\\`defclass" up-func))
               (euslisp-indent-defclass-slots-form state indent-point normal-indent))
              ((and up-func
                    (string-match "\\`defmethod" up-func))
               (euslisp-indent-defmethod-method-form state indent-point normal-indent))
              ((or (eq method 'defun)
                   (and (null method)
                        (> (length function) 3)
                        (string-match "\\`def" function)))
               (lisp-indent-defform state indent-point))
              ((integerp method)
               (lisp-indent-specform method state
                                     indent-point normal-indent))
              (method
               (funcall method indent-point state)))))))

(defun euslisp-indent-defclass-slots-form (state _indent-point _normal-indent)
  (let ((prev-sexp (buffer-substring
                    (save-excursion (backward-up-list) (backward-char) (backward-sexp) (point))
                    (save-excursion (backward-up-list) (backward-char) (point)))))
    (when (string-match "\\`:slots" prev-sexp)
      (goto-char (cadr state))
      (1+ (current-column)))))

(defun euslisp-indent-defmethod-method-form (state _indent-point _normal-indent)
  (lisp-indent-defform state _indent-point))

;;; Autoloads:

;;;###autoload
(define-derived-mode euslisp-mode common-lisp-mode "Euslisp"
  "Euslisp mode"
  (set (make-local-variable 'indent-line-function) #'euslisp-indent-line)
  (make-local-variable 'indent-defclass-inside-p)
  (make-local-variable 'indent-defmethod-inside-p))

;;;###autoload
(progn
  (add-to-list 'auto-mode-alist '("\\.l\\'" . euslisp-mode))
  (add-to-list 'interpreter-mode-alist '("eus" . euslisp-mode))
  (add-to-list 'interpreter-mode-alist '("roseus" . euslisp-mode))
  (add-to-list 'interpreter-mode-alist '("irteusgl" . euslisp-mode))
  (add-to-list 'interpreter-mode-alist '("jskrbeusgl" . euslisp-mode)))


;;; Syntax Highlighting:

(font-lock-add-keywords
 'euslisp-mode
 (list
  ;; defforeign
  (list (concat "(\\_<" (regexp-opt '("defforeign") t) "\\_>")
        '(1 font-lock-keyword-face nil t))
  ;; self
  (list "\\_<\\(self\\)\\_>"
        '(1 font-lock-constant-face nil t))
  (list "\\(\\*\\w\+\\*\\)\\>"
        '(1 font-lock-constant-face nil t))
  (list "\\(#\\(\\+\\|\\-\\)\.\*\\)"
        '(1 font-lock-variable-name-face))
  ;; throw-error
  (list "\\(throw-error\\)"
        '(1 font-lock-warning-face nil t))
  ;; warn
  (list (concat "(\\_<" (regexp-opt '("warn" "warning-message") t) "\\_>")
        '(1 font-lock-warning-face nil t))
  ;; send
  (list (concat "(\\_<" (regexp-opt '("send" "send*" "send-all" "send-super" "send-super*") t) "\\_>")
        '(1 font-lock-builtin-face nil t))
  ;; forward-message-to
  (list (concat "(\\_<" (regexp-opt '("forward-message-to" "forward-message-to-all") t) "\\>")
        '(1 font-lock-builtin-face nil t))
  ;; 
  (list "\\(\\*[^ ]*\\*\\)"
        '(1 font-lock-constant-face nil t))
  ;; load
  (list (concat "(\\_<" (regexp-opt '("load") t) "\\_>")
        '(1 font-lock-keyword-face nil t))
  ;; setq
  (list (concat "(\\_<" (regexp-opt '("setq") t) "\\_>")
        '(1 font-lock-type-face nil t))))

(provide 'euslisp-mode)
;;; euslisp-mode.el ends here
