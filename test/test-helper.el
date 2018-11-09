;;; test-helper.el --- Helpers for org-shoplist tests
;;; Commentary:
;; Helps out org-shoplist tests
;;; Code:
(defconst org-shoplist-test-default-buffer "*Org-Shoplist-Test*")
(defun org-shoplist-test-test-in-buffer (func)
  "Execute a test in temp-buffer and leave everthing in same state as before.
'FUNC' is what should be done in the temp-buffer."
  (unwind-protect
      (with-current-buffer (get-buffer-create org-shoplist-test-default-buffer)
	(funcall func))
    (kill-buffer org-shoplist-test-default-buffer)))
(defun org-shoplist-test-test-in-org-buffer (func)
  "Execute a test in temp-buffer and leave everthing in same state as before.
'FUNC' is what should be done in the temp-buffer."
  (unwind-protect
      (with-current-buffer (get-buffer-create org-shoplist-test-default-buffer)
	(org-mode)
	(funcall func))
    (kill-buffer org-shoplist-test-default-buffer)))
;;; test-helper.el ends here
