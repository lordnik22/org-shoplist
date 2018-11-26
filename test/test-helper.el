;;; test-helper.el --- Helpers for org-shoplist tests
;;; Commentary:
;; Helps out org-shoplist tests
;;; Code:
(require 'org-shoplist)
(require 'calc-units)

(defconst org-shoplist-test-default-buffer "*Org-Shoplist-Test*")
(defconst org-shoplist-test-default-result-buffer "*Org-Shoplist-Test-Result*")

(defvar org-shoplist-test-keyword-b org-shoplist-keyword
  "Backup constant for resting the custom variable: ‘org-shoplist-test-keyword’.")

(defvar org-shoplist-test-ing-unit-regex-b org-shoplist-ing-unit-regex
  "Backup constant for resting the custom variable: ‘org-shoplist-ing-unit-regex’.")

(defvar org-shoplist-test-ing-amount-regex-b org-shoplist-ing-amount-regex
  "Backup constant for resting the custom variable: ‘org-shoplist-ing-amount-regex’.")

(defvar org-shoplist-test-ing-regex-b org-shoplist-ing-regex
  "Backup constant for resting the custom variable: ‘org-shoplist-ing-regex’.")

(defvar org-shoplist-test-additional-units-b org-shoplist-additional-units
  "Backup constant for resting the custom variable: ‘org-shoplist-additional-units’.")

(defvar org-shoplist-test-table-header-b org-shoplist-table-header
  "Backup constant for resting the custom variable: ‘org-shoplist-table-header’.")

(defun org-shoplist-test-load-custom-var ()
  "Make a save state of the current values of the custom variables."
  (setq org-shoplist-test-keyword-b org-shoplist-keyword)
  (setq org-shoplist-test-ing-unit-regex-b org-shoplist-ing-unit-regex)
  (setq org-shoplist-test-ing-amount-regex-b org-shoplist-ing-amount-regex)
  (setq org-shoplist-test-ing-regex-b org-shoplist-ing-regex)
  (setq org-shoplist-test-additional-units-b org-shoplist-additional-units)
  (setq org-shoplist-test-table-header-b org-shoplist-table-header))

(defun org-shoplist-test-reset-custom-var ()
  "Reset custom variables that might got editted by a test."
  (setq org-shoplist-keyword org-shoplist-test-keyword-b)
  (setq org-shoplist-ing-unit-regex org-shoplist-test-ing-unit-regex-b)
  (setq org-shoplist-ing-amount-regex org-shoplist-test-ing-amount-regex-b)
  (setq org-shoplist-ing-regex org-shoplist-test-ing-regex-b)
  (setq org-shoplist-additional-units org-shoplist-test-additional-units-b)
  (setq math-additional-units org-shoplist-test-additional-units-b)
  (setq org-shoplist-table-header org-shoplist-test-table-header-b))

(defun org-shoplist-test-set-additioanl-units (units)
  "Add ‘UNITS’ as additional units for current test."
  (apply 'add-to-list 'org-shoplist-additional-units units)
  (apply 'add-to-list 'math-additional-units org-shoplist-additional-units)
  (setq math-units-table nil)
  (message "%s" math-additional-units))

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
