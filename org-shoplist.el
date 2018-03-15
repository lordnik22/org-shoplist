;;; org-shoplist.el --- Eat the world

;; Copyright (C) 2017 Free Software Foundation, Inc.

;; Author: lordnik22
;; Version: 1.0.0
;; Keywords: org-mode, shopping-list, eating-plan, recipe-list
;; URL: https://github.com/lordnik22

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;; Commentary:
;; There is nothing done, yet.
;;; Code:
(require 'calc-ext)

(defun org-shoplist-ing-create (amount name)
  "Create a ingredient.
'AMOUNT' can be a string, a number or a valid sequence.
'NAME' is a string.
If one constraint gets disregarded throw error."
  (when (not (stringp name)) (error "Invalid name for ingredient"))
  (when (eq amount nil) (setq amount "0"))
  (when (numberp amount) (setq amount (number-to-string amount)))
  (if (and (stringp amount) (string-match "^[0-9]" amount))
      (list name (math-read-expr amount))
    (if (and (sequencep amount) (eq (car amount) (intern "*")) (sequencep (cdr amount)))
	(list name amount)
      (error "Invalid amount for ingredient"))))

(defun org-shoplist-recipe-create (name &rest ings)
  "Create a recipe.
'NAME' must be a string.
'INGS' must be vallid ingredients.
Use ´(org-shoplist-ing-create)' to create valid ingredients."
  (when (eq name nil) (error "Invalid name for recipe"))
  (cons name ings))
(provide 'org-shoplist)
;;; org-shoplist.el ends here
