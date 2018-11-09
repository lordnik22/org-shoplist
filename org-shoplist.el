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

(defcustom org-shoplist-ing-amount-regex "\\([1-9][0-9]*\\)\\([^0-9+\\-\\*\\/]*?\\)"
  "Match an amount in a string."
  :type 'string
  :group 'org-shoplist)

(defcustom org-shoplist-ing-regex "(\\([1-9][0-9]*\\)\\([^0-9 ]*?\\)[ ]\\(.+?\\))"
  "Match an ingredient.
group 1: number
group 2: unit
group 3: ingredient-name"
  :type 'string
  :group 'org-shoplist)

(defcustom org-shoplist-additional-units '(())
  "Additional units that are needed for recipes with special units."
  :type '(repeat (symbol string string))
  :group 'org-shoplist)

;; Inject custom units
(eval-after-load "calc-units"
  '(progn
     (setq math-additional-units
	   org-shoplist-additional-units
           math-units-table nil)))

(defun org-shoplist-ing-create (amount name)
  "Create an ingredient.
‘AMOUNT’ can be a string, a number or a valid sequence.
‘NAME’ is a string.
If one constraint gets disregarded throw error."
  (when (not (stringp name)) (error "Invalid ‘NAME’ for ingredient"))
  (condition-case ex
      (list name (org-shoplist-ing-amount-validate amount))
    ('error (error (error-message-string ex)))))

(defun org-shoplist-ing-name (ing)
  "Get name of ‘ING’."
  (car ing))

(defun org-shoplist-ing-amount (ing)
  "Get amount of ‘ING’."
  (car (cdr ing)))

(defun org-shoplist-ing-amount-validate (amount)
  "Return ‘AMOUNT’ when it’s valid else throw error."
  (when (eq amount nil) (setq amount "0"))
  (when (numberp amount) (setq amount (number-to-string amount)))
  (save-match-data
    (if (and (stringp amount) (string-match (concat "^" org-shoplist-ing-amount-regex "$") amount))
	(calc-eval amount)
      (error "Invalid ‘AMOUNT’(%s) for ingredient" amount))))

(defun org-shoplist-ing-unit (ing)
  "Get unit of ‘ING’."
  (let ((amount (org-shoplist-ing-amount ing)))
    (if (string-match org-shoplist-ing-unit-regex amount)
	(match-string 1 amount)
      nil)))

(defun org-shoplist-ing-+ (&rest amounts)
  "Add ‘AMOUNTS’ toghether return the sum."
  (condition-case ex
      (org-shoplist-ing-amount-validate
       (calc-eval
	(math-simplify-units
	 (math-read-expr
	  (mapconcat (lambda (x)
		       (cond ((stringp x) x)
			     ((integerp x) (int-to-string x))
			     ((eq nil x) "0")
			     ((listp x) (org-shoplist-ing-amount x))
			     (t (error "Given ‘AMOUNT’(%s) can’t be converted" x))))
		     amounts "+")))))
    ('error (error (error-message-string ex)))))

(defun org-shoplist-ing-* (ing factor)
  "Multiply the amount of ‘ING’ with given ‘FACTOR’.
Return new ingredient with modified amount."
  (if (= factor 0) nil
    (org-shoplist-ing-create
     (calc-eval (math-simplify-units (math-read-expr (concat (int-to-string factor) "*" (org-shoplist-ing-amount ing)))))
     (org-shoplist-ing-name ing))))

(defun org-shoplist-recipe-create (name &rest ings)
  "Create a recipe.
‘NAME’ must be a string.
‘INGS’ must be valid ingredients.
Use ‘org-shoplist-ing-create’ to create valid ingredients."
  (when (or (eq name nil) (string= name "")) (error "Invalid name for recipe: ‘%s’" name))
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

(defun org-shoplist-ing-read (&optional str)
  "Parse given ‘STR’ and return a list of found ingredients.
Whenn ‘STR’ is nil read line where point is and parse that line."
  (when (eq str nil) (setq str (thing-at-point 'line)))
  (if (or (eq nil str) (string= str ""))
      nil
    (org-shoplist--ing-read-loop str 0 '())))

(defun org-shoplist--ing-read-loop (str start-pos ings)
  "Helper functions for (org-shoplist-read) which does the recursive matching.
‘STR’ is a string where regex is getting matched against.
‘START-POS’ is where in string should start.
‘INGS’ is a list of the found ingredients."
  (if (string-match org-shoplist-ing-regex str start-pos)
      (org-shoplist--ing-read-loop
       str
       (match-end 0)
       (if (eq ings nil)
	   (list (org-shoplist-ing-create (concat (match-string 1 str) (match-string 2 str)) (match-string 3 str)))
	 (add-to-list 'ings
		      (org-shoplist-ing-create (concat (match-string 1 str) (match-string 2 str)) (match-string 3 str)))))
    (reverse ings)))

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

(defun org-shoplist-recipe-read ()
  "Assums that at beginning of recipe.
Which is at (beginning-of-line) at heading (╹* Nut Salat...).
Return a recipe structure or throw error.  To read a recipe there
must be at least a org-heading (name of the recipe) and one
ingredient.
See ‘org-shoplist-recipe-create’ for more details on creating general
recipes."
  (save-match-data
    (when (not (looking-at org-heading-regexp)) (error "Not at beginning of recipe"))
    (org-shoplist-recipe-create (string-trim (replace-regexp-in-string org-todo-regexp "" (match-string 2)))
		    (org-shoplist--recipe-read-all-ing (match-string 1)))))

(defun org-shoplist-shoplist-create (shop-date &rest recipes)
  "Create a shoplist.
‘SHOP-DATE’ a string or nil containing shopping day.
‘RECIPES’ initial recipes in the shoplist."
  (list shop-date recipes))

(defun org-shoplist-shoplist-shopdate (shoplist)
  "Get shopdate of shoplist.
‘SHOPLIST’ a string or nil containing shopping day."
  (car shoplist))

(defun org-shoplist-shoplist-recipes (shoplist)
  "Get recipes of shoplist.
‘SHOPLIST’ a string or nil containing shopping day."
  (if (eq nil (car (cdr shoplist)))
      nil
    (car (cdr shoplist))))

(defun org-shoplist-shoplist-read ()
  "Return a shoplist structure or throw error.
To read a recipe there must be at least a org-heading (name of the recipe).
See ‘org-shoplist-recipe-create’ for more details on creating general recipes."
  (let ((recipe-list nil))
    (beginning-of-line 2)
    (while (and (not (= (point-max) (point)))
		(search-forward-regexp org-heading-regexp nil t 1)
		(progn (beginning-of-line 1) (looking-at-p (concat ".+" org-shoplist-keyword))))
      (setq recipe-list (append recipe-list (org-shoplist-recipe-read))))
    (org-shoplist-shoplist-create (calendar-current-date) recipe-list)))

(provide 'org-shoplist)
;;; org-shoplist.el ends here
