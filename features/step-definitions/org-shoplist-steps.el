;;; Commentary:
;; This file contains the project specific step definitions. All
;; files in this directory whose names end with "-steps.el" will be
;; loaded automatically by Ecukes.
;;; Code:
(Given "^This org file:$"
  (lambda (content)
    (insert content)))

(And "^I execute \"\\(.*\\)\""
  (lambda (cmd)
    (eval-expression cmd)))
;; org-shoplist-steps.el ends here
