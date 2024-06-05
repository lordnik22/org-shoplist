;;; test-helper.el --- Helpers for org-shoplist tests
;;; Commentary:
;; Helps out org-shoplist tests
;;; Code:
(require 'calc-units)
(load (concat default-directory "../org-shoplist.el"))
(require 'org-shoplist)

(defun org-shoplist-test-load-default-env ()
  "Set all cusotm var to there default."
  (setq org-shoplist-inital-factor 1
	org-shoplist-ing-start-char "("
	org-shoplist-ing-end-char ")"
	org-todo-keywords (append org-todo-keywords (list (list 'sequence org-shoplist-keyword "BOUGHT")))
	org-shoplist-search-type (list 'keyword "TOBUY")))

(defconst org-shoplist-test-default-buffer "*Org-Shoplist-Test*")
(defconst org-shoplist-test-default-result-buffer "*Org-Shoplist-Test-Result*")

(defvar org-shoplist-test-files
  (list "./org-shoplist-ing-test.el"
        "./org-shoplist-recipe-test.el"
        "./org-shoplist-test.el"))

(org-shoplist-test-load-default-env)
(defvar org-shoplist-test-backup-vars
  (list (cons org-shoplist-search-type 'org-shoplist-search-type)
        (cons org-shoplist-ing-regex 'org-shoplist-ing-regex)
        (cons org-shoplist-additional-units 'org-shoplist-additional-units)
        (cons org-shoplist-ing-start-char 'org-shoplist-ing-start-char)
        (cons org-shoplist-ing-end-char 'org-shoplist-ing-end-char)
        (cons math-additional-units 'math-additional-units)
        (cons org-shoplist-explicit-keyword 'org-shoplist-explicit-keyword)
        (cons org-shoplist-default-format 'org-shoplist-default-format)
        (cons math-simplifying-units 'math-simplifying-units)
        (cons org-shoplist-ing-default-separator 'org-shoplist-ing-default-separator)
        (cons org-shoplist-auto-add-unit 'org-shoplist-auto-add-unit)
        (cons org-shoplist-precision 'org-shoplist-precision)
        (cons org-shoplist-inital-factor 'org-shoplist-inital-factor)))

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
  (dolist (i units)
    (add-to-list 'org-shoplist-additional-units i))
  (dolist (i org-shoplist-additional-units)
    (add-to-list 'math-additional-units i))
  (setq math-units-table nil))

(defun org-shoplist-test-test (func)
  "Execute a test in temp-buffer and leave everthing in same state as before.
'FUNC' is what should be done in the temp-buffer."
  (unwind-protect
      (progn (org-shoplist-test-load-custom-var)
             (funcall func))
    (org-shoplist-test-reset-custom-var)))

(defun org-shoplist-test-test-in-buffer (func-in-buffer)
  "Execute a test in temp-buffer and leave everthing in same state as before.
‘FUNC-IN-BUFFER’ is what should be done in the temp-buffer."
  (org-shoplist-test-test
   (lambda ()
     (unwind-protect
         (with-current-buffer (get-buffer-create org-shoplist-test-default-buffer)
           (funcall func-in-buffer))
       (kill-buffer org-shoplist-test-default-buffer)))))

(defun org-shoplist-test-test-in-org-buffer (func-in-org-buffer)
  "Execute a test in temp-buffer and leave everthing in same state as before.
‘FUNC-IN-ORG-BUFFER’ is what should be done in the temp-buffer."
  (org-shoplist-test-test-in-buffer
   (lambda ()
     (org-mode)
     (funcall func-in-org-buffer))))

(dolist (f org-shoplist-test-files)
  (load (concat default-directory f)))

;;; test-helper.el ends here
