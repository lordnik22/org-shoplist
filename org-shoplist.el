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
(require 'subr-x)
(require 'seq)
(require 'calc-ext)
(require 'calc-units)
(require 'org)

(defcustom org-shoplist-keyword "TOBUY"
  "Keyword to mark recies for shopping."
  :type 'string
  :group 'org-shoplist)

(defcustom org-shoplist-ing-unit-regex "\\([^0-9 ]+\\)"
  "Match a unit in a string."
  :type 'string
  :group 'org-shoplist)

(defcustom org-shoplist-ing-amount-regex "\\([1-9]?[0-9e\\.]*\\(\\.\\|-?\\)[0-9]*\\)[ ]?\\([^-+\n\t.]*\\)"
  "Match an amount in a string."
  :type 'string
  :group 'org-shoplist)

(defcustom org-shoplist-ing-regex "(\\([1-9]?[0-9e\\.]*\\(\\.\\|-?\\)[0-9]*\\)[ ]?\\([^-+\n\t)(]+?\\)[ ]\\(.+?\\))"
  "Match an ingredient.
group 1: number
group 2: unit
group 3: ingredient-name"
  :type 'string
  :group 'org-shoplist)

(defcustom org-shoplist-additional-units nil
  "Additional units that are needed for recipes with special units.
Beaware that the unit can't contain dots."
  :type '(repeat (symbol string string))
  :group 'org-shoplist)

;; Inject custom units
(when (not (eq nil org-shoplist-additional-units))
  (eval-after-load "calc-units" '(progn (apply 'add-to-list 'math-additional-units org-shoplist-additional-units))))

(defun org-shoplist-ing-name (ing)
  "Get name of ‘ING’."
  (car ing))

(defun org-shoplist-ing-amount (ing)
  "Get amount of ‘ING’."
  (car (cdr ing)))

(defun org-shoplist--ing-validate-amount (amount)
  "Return ‘AMOUNT’ when it’s valid else throw error."
  (when (eq amount nil) (setq amount "0"))
  (when (numberp amount) (setq amount (number-to-string amount)))
  (if (and (stringp amount)
	   (string-match (concat "^" org-shoplist-ing-amount-regex "$") amount)
	   (or (string= "" (match-string 1 amount))
	       (< 0 (string-to-number (match-string 1 amount)))))
      (save-match-data (calc-eval amount))
    (user-error "Invalid ‘AMOUNT’(%s) for ingredient" amount)))

(defun org-shoplist-ing-unit (ing)
  "Get unit of ‘ING’."
  (let ((amount (org-shoplist-ing-amount ing)))
    (if (string-match org-shoplist-ing-unit-regex amount)
	(match-string 1 amount)
      nil)))

(defun org-shoplist-ing-group (ing)
  "Get group of ‘ING’."
  (car (cdr (cdr ing))))

(defun org-shoplist--ing-find-unit-group (amount)
  "Find the ground unit of ‘AMOUNT’s unit.
When ‘AMOUNT’ nil, return nil"
  (calc-eval (math-extract-units (math-to-standard-units (math-read-expr amount) nil))))

(defun org-shoplist-ing-create (amount name)
  "Create an ingredient.
‘AMOUNT’ can be a string, a number or a valid sequence.
‘NAME’ is a string.
If one constraint gets disregarded throw error."
  (save-match-data
    (when (not (stringp name)) (user-error "Invalid ‘NAME’(%s) for ingredient" name))
    (let ((valid-amount (org-shoplist--ing-validate-amount amount)))
      (list name
	    valid-amount
	    (org-shoplist--ing-find-unit-group valid-amount)))))

(defun org-shoplist-ing-+ (&rest amounts)
  "Add ‘AMOUNTS’ toghether return the sum."
  (org-shoplist--ing-validate-amount
   (calc-eval
    (math-to-standard-units
     (math-read-expr
      (mapconcat (lambda (x)
		   (cond ((stringp x) x)
			 ((integerp x) (number-to-string x))
			 ((eq nil x) "0")
			 ((listp x) (org-shoplist-ing-amount x))
			 (t (user-error "Given ‘AMOUNT’(%s) can’t be converted" x))))
		 amounts "+"))
     nil))))

(defun org-shoplist-ing-* (ing factor)
  "Multiply the amount of ‘ING’ with given ‘FACTOR’.
Return new ingredient with modified amount."
  (if (= factor 0) nil
    (org-shoplist-ing-create
     (calc-eval (math-to-standard-units
		 (math-read-expr (concat (number-to-string factor) "*" (org-shoplist-ing-amount ing)))
		 nil))
     (org-shoplist-ing-name ing))))

(defun org-shoplist-ing-aggregate (&rest ings)
  "Aggregate ‘INGS’."
  (let ((group-ings (seq-group-by (lambda (x) (list (org-shoplist-ing-name x) (org-shoplist-ing-group x))) ings))
	(aggregate-ings (list)))
    (while (not (eq nil (car group-ings)))
      (setq aggregate-ings (cons (org-shoplist-ing-create (apply 'org-shoplist-ing-+ (cdr (car group-ings)))
					      (car (car (car group-ings))))
				 aggregate-ings))
      (setq group-ings (cdr group-ings)))
    aggregate-ings))

(defun org-shoplist-ing-read (&optional aggregate str)
  "‘AGGREGATE’ output when t else return parsed ‘STR’ raw.
Whenn ‘STR’ is nil read line where point is at."
  (when (eq str nil) (setq str (thing-at-point 'line)))
  (if (or (eq nil str) (string= str ""))
      nil
    (let ((read-ings (org-shoplist--ing-read-loop str 0 '())))
      (if aggregate
	  (apply 'org-shoplist-ing-aggregate read-ings)
	(reverse read-ings)))))

(defun org-shoplist--ing-read-loop (str start-pos ings)
  "Helper functions for (org-shoplist-read) which does the recursive matching.
‘STR’ is a string where regex is getting matched against.
‘START-POS’ is where in string should start.
‘INGS’ is a list of the found ingredients."
  (if (string-match org-shoplist-ing-regex str start-pos)
      (org-shoplist--ing-read-loop
       str
       (match-end 0)
       (let ((new-ing (org-shoplist-ing-create (concat (match-string 1 str) (match-string 3 str)) (match-string 4 str))))
	 (if (eq ings nil)
	     (list new-ing)
	   (push new-ing ings))))
    ings))

(defun org-shoplist-recipe-create (name &rest ings)
  "Create a recipe.
‘NAME’ must be a string.
‘INGS’ must be valid ingredients.
Use ‘org-shoplist-ing-create’ to create valid ingredients."
  (when (or (eq name nil) (string= name "")) (user-error "Invalid name for recipe: ‘%s’" name))
  (when (listp (car (car ings))) (setq ings (car ings)))
  (if (or (eq ings nil) (equal ings '(nil)))
      nil
    (list name ings)))

(defun org-shoplist-recipe-name (recipe)
  "Get name of ‘RECIPE’."
  (car recipe))

(defun org-shoplist-recipe-first-ing (recipe)
  "Get first ingredient of ‘RECIPE’."
  (car (cdr recipe)))

(defun org-shoplist-recipe-get-all-ing (recipe)
  "Get all ingredients of ‘RECIPE’."
  (cdr recipe))

(defun org-shoplist-recipe-get-N-ing (recipe n)
  "Get from ‘RECIPE’ the ‘N’th ingredient.
First = ‘n’ = 0"
  (elt recipe n))

(defun org-shoplist--recipe-read-all-ing (stars)
  "Assums that at beginning of recipe.
Return a list of ingredient-structures of recipe where point is at.
‘STARS’ are the stars of the recipe heading."
   (let ((ing-list nil))
    (beginning-of-line 2)
    (while (and (not (looking-at-p (concat "^" (regexp-quote stars) " ")))
		(not (= (point) (point-max))))
      (setq ing-list (append ing-list (org-shoplist-ing-read)))
      (beginning-of-line 2))
    ing-list))

(defun org-shoplist-recipe-read (&optional aggregate)
  "Assums that at beginning of recipe.
Which is at (beginning-of-line) at heading (╹* Nut Salat...).
Return a recipe structure or throw error.  To read a recipe there
must be at least a org-heading (name of the recipe) and one
ingredient.
‘AGGREGATE’ ingredients when t.
See ‘org-shoplist-recipe-create’ for more details on creating general
recipes."
  (save-match-data
    (when (not (looking-at org-heading-regexp)) (user-error "Not at beginning of recipe"))
    (let ((read-ings (save-match-data (org-shoplist--recipe-read-all-ing (match-string 1)))))
      (org-shoplist-recipe-create (string-trim (replace-regexp-in-string org-todo-regexp "" (match-string 2)))
		      (if aggregate (apply 'org-shoplist-ing-aggregate read-ings) read-ings)))))

(defun org-shoplist-shoplist-create (&rest recipes)
  "Create a shoplist.
‘RECIPES’ initial recipes in the shoplist."
  (list (calendar-current-date) recipes))

(defun org-shoplist-shoplist-shopdate (shoplist)
  "Get shopdate of shoplist.
‘SHOPLIST’ of which the date should be extracted."
  (car shoplist))

(defun org-shoplist-shoplist-recipes (shoplist)
  "Get recipes of shoplist.
‘SHOPLIST’ a string or nil containing shopping day."
  (if (eq nil (car (cdr shoplist)))
      nil
    (car (cdr shoplist))))

(defun org-shoplist-shoplist-read (&optional aggregate)
  "Return a shoplist structure or throw error.
To read a recipe there must be at least a org-heading (name of the recipe).
See ‘org-shoplist-recipe-create’ for more details on creating general recipes.
‘AGGREGATE’ ingredients when t."
  (let ((recipe-list nil))
    (beginning-of-line 2)
    (while (and (not (= (point-max) (point)))
		(search-forward-regexp org-heading-regexp nil t 1)
		(progn (beginning-of-line 1) (looking-at-p (concat ".+" org-shoplist-keyword))))
      (setq recipe-list (append recipe-list (org-shoplist-recipe-read))))
    (org-shoplist-shoplist-create (calendar-current-date) recipe-list)))

(provide 'org-shoplist)
;;; org-shoplist.el ends here
