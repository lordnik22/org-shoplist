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
(require 'org)

(defconst org-shoplist-ing-re "(\\([1-9][0-9]*\\)\\(.*\\) \\(.+\\))"
  "Match an ingredient.
group 1: number
group 2: unit
group 3: ingredient-name")

(defun org-shoplist-ing-create (amount name)
  "Create an ingredient.
`AMOUNT' can be a string, a number or a valid sequence.
`NAME' is a string.
If one constraint gets disregarded throw error."
  (when (not (stringp name)) (error "Invalid `NAME' for ingredient"))
  (when (eq amount nil) (setq amount "0"))
  (when (numberp amount) (setq amount (number-to-string amount)))
  (save-match-data
    (if (and (stringp amount) (string-match "^[0-9]" amount))
	(list name (math-read-expr amount))
      (if (and (listp amount) (eq (car amount) (intern "*")) (listp (cdr amount)))
	  (list name amount)
	(error "Invalid `AMOUNT' for ingredient")))))

(defun org-shoplist-ing-name (ing)
  "Get name of `ING'."
  (car ing))

(defun org-shoplist-ing-amount (ing)
  "Get amount of `ING'."
  (car (cdr ing)))

(defun org-shoplist-ing-unit (ing)
  "Get unit of `ING'."
  (let ((amnt (org-shoplist-ing-amount ing)))
    (if (numberp amnt)
	nil
      (elt (elt amnt 2) 1))))

;;(require 'calc-ext)  (math-read-expr "100g")(* 100 (var g var-g))
;;calc-aent
;;(math-evaluate-expr (math-read-expr "100g+100g"))

(defun org-shoplist-recipe-create (name &rest ings)
  "Create a recipe.
`NAME' must be a string.
`INGS' must be valid ingredients.
Use `org-shoplist-ing-create' to create valid ingredients."
  (when (eq name nil) (error "Invalid name for recipe"))
  (when (listp (car (car ings))) (setq ings (car ings)))
  (if (or (eq ings nil) (equal ings '(nil)))
      (list name)
    (list name ings)))

(defun org-shoplist-recipe-name (recipe)
  "Get name of `RECIPE'."
  (car recipe))

(defun org-shoplist-recipe-first-ing (recipe)
  "Get first ingredient of `RECIPE'."
  (car (cdr recipe)))

(defun org-shoplist-recipe-get-all-ing (recipe)
  "Get all ingredients of `RECIPE'."
  (cdr recipe))

(defun org-shoplist-recipe-get-N-ing (recipe n)
  "Get from `RECIPE' the `N'th ingredient.
First = `n' = 1"
  (elt recipe n))

(defun org-shoplist-ing-read (&optional str)
  "Parse given `STR' and return a list of found ingredients.
Whenn `STR' is nil read line where point is and parse that line."
  (when (eq str nil) (setq str (thing-at-point 'line t)))
  (if (or (eq nil str) (string= str ""))
      nil
    (org-shoplist--ing-read-loop str 0 '())))

(defun org-shoplist--ing-read-loop (str start-pos ings)
  "Helper functions for (org-shoplist-read) which does the recursive matching.
`STR' is a string where regex is getting matched against.
`START-POS' is where in string should start.
`INGS' is a list of the found ingredients."
  (if (string-match "(\\([1-9][0-9]*\\)\\(\\w+\\)? \\(\\w+\\))" str start-pos)
      (org-shoplist--ing-read-loop
       str
       (match-end 0)
       (if (eq ings nil)
	   (list (org-shoplist-ing-create (concat (match-string 1 str) (match-string 2 str)) (match-string 3 str)))
	 (add-to-list 'ings
		      (org-shoplist-ing-create (concat (match-string 1 str) (match-string 2 str)) (match-string 3 str)))))
    (reverse ings)))

(defun org-shoplist--recipe-read-all-ing (stars)
  "Return a list of ingredient-structures of recipe where point is at.
`STARS' are the stars of the recipe heading."
  (beginning-of-line 2)
  (save-excursion
    (let* ((ing (org-shoplist-ing-read))
	   (ing-list nil))
      (while (and (not (looking-at-p (concat "^" (regexp-quote stars) " ")))
		  (not (= (point) (point-max))))
	(setq ing-list (append ing-list ing))
	(beginning-of-line 2)
	(setq ing (org-shoplist-ing-read)))
      ing-list)))

(defun org-shoplist-recipe-read ()
  "Assums that at beginning of recipe.
Which is at (beginning-of-line) at heading (╹* Nut Salat...).
Return a recipe structure or throw error.
To read a recipe there must be at least a org-heading (name of the recipe).
See `org-shoplist-recipe-create' for more details on creating general recipes."
  (save-match-data
    (when (not (looking-at org-heading-regexp)) (error "Not at beginning of recipe"))
    (save-excursion
      (let ((recipe-name (if (or (eq (match-string 2) nil) (string= (match-string 2) ""))
			     (error "No recipe-name provided")
			   (match-string 2)))
	    (ings (progn
		    (org-shoplist--recipe-read-all-ing (match-string 1)))))
	(org-shoplist-recipe-create recipe-name ings)))))

(defun org-shoplist-shoplist-create (shop-date &rest recipes)
  "Create a shoplist.
`SHOP-DATE' a string or nil containing shopping day.
`RECIPES' initial recipes in the shoplist."
  (list shop-date recipes))

(defun org-shoplist-shoplist-shopdate (shoplist)
  "Create a shoplist.
`SHOPLIST' a string or nil containing shopping day."
(car shoplist))

(defun org-shoplist-shoplist-recipes (shoplist)
  "Create a shoplist.
`SHOPLIST' a string or nil containing shopping day."
  (if (eq nil (car (cdr shoplist)))
      nil
    (car (cdr shoplist))))

(provide 'org-shoplist)
;;; org-shoplist.el ends here
