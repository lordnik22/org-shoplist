;;; test-helper.el --- Helpers for org-shoplist tests
;;; Commentary:
;; Helps out org-shoplist tests
;;; Code:
(require 'org-shoplist)
(require 'calc-units)

(defconst org-shoplist-test-default-buffer "*Org-Shoplist-Test*")
(defconst org-shoplist-test-default-result-buffer "*Org-Shoplist-Test-Result*")

(defvar org-shoplist-test-backup-vars
  (list (cons org-shoplist-keyword 'org-shoplist-keyword)
	(cons org-shoplist-ing-unit-regex 'org-shoplist-ing-unit-regex)
	(cons org-shoplist-ing-amount-regex 'org-shoplist-ing-amount-regex)
	(cons org-shoplist-ing-regex 'org-shoplist-ing-regex)
	(cons org-shoplist-additional-units 'org-shoplist-additional-units)
	(cons org-shoplist-table-header 'org-shoplist-table-header)
	(cons org-shoplist-explict-keyword 'org-shoplist-explict-keyword)
	(cons math-simplifying-units 'math-simplifying-units)))

(defun org-shoplist-test-load-custom-var ()
  "Make a save state of the current values of the custom variables."
  (let ((loaded-vars (list)))
    (dolist (i org-shoplist-test-backup-vars loaded-vars)
      (setq loaded-vars (append loaded-vars (list (cons (eval (cdr i)) (cdr i))))))
    (setq org-shoplist-test-backup-vars loaded-vars)))

(defun org-shoplist-test-reset-custom-var ()
  "Reset custom variables that might got editted by a test."
  (dolist (i org-shoplist-test-backup-vars)
    (set (cdr i) (car i))))

(defun org-shoplist-test-set-additioanl-units (units)
  "Add ‘UNITS’ as additional units for current test."
  (apply 'add-to-list 'org-shoplist-additional-units units)
  (apply 'add-to-list 'math-additional-units org-shoplist-additional-units)
  (setq math-units-table nil))

(defun org-shoplist-test-test-in-buffer (func-in-buffer)
  "Execute a test in temp-buffer and leave everthing in same state as before.
'FUNC' is what should be done in the temp-buffer."
  (unwind-protect
      (with-current-buffer (get-buffer-create org-shoplist-test-default-buffer)
	(org-shoplist-test-load-custom-var)
	(funcall func-in-buffer))
    (kill-buffer org-shoplist-test-default-buffer)
    (org-shoplist-test-reset-custom-var)))
(defun org-shoplist-test-test-in-org-buffer (func-in-org-buffer)
  "Execute a test in temp-buffer and leave everthing in same state as before.
'FUNC' is what should be done in the temp-buffer."
  (org-shoplist-test-test-in-buffer
   (lambda ()
     (org-mode)
     (funcall func-in-org-buffer))))
;;; test-helper.el ends here
