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

(defcustom org-shoplist-ing-amount-regex '(concat
			       "\\([1-9]?[0-9]*\\.?[0-9]*\\)\\([ ]?\\)\\("
			       "[YZEPTGMkKhHDdcmunpfazy]?\\("
			       (mapconcat (lambda (x) (symbol-name (car x))) (append math-standard-units org-shoplist-additional-units) "\\|")
			       "\\)\\)?")
  "Match an amount in a string."
  :type 'string
  :group 'org-shoplist)

(defcustom org-shoplist-ing-regex '(concat
			"(\\([1-9]?[0-9]*\\.?[0-9]*\\)\\([ ]?\\)\\("
			"[YZEPTGMkKhHDdcmunpfazy]?\\("
			(mapconcat (lambda (x) (symbol-name (car x))) (append math-standard-units org-shoplist-additional-units) "\\|")
			"\\)\\)?[ ]\\(.+?\\))")
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
(eval-after-load "calc-units" '(progn (add-to-list 'math-additional-units org-shoplist-additional-units)))

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
  (if (and (stringp amount) (string-match (concat "^" (eval org-shoplist-ing-amount-regex) "$") amount))
      (save-match-data (calc-eval amount))
    (error "Invalid ‘AMOUNT’(%s) for ingredient" amount)))

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
  (let ((unit (match-string 4 amount)))
    (if (eq unit nil)
	nil
      (let ((next-unit (car (last (seq-find (lambda (x) (equal (intern unit) (car x))) math-units-table)))))
	(if (eq next-unit nil)
	    unit
	  (string-match (concat "^" (eval org-shoplist-ing-amount-regex) "$") next-unit)
	  (org-shoplist--ing-find-unit-group next-unit))))))

(defun org-shoplist-ing-create (amount name)
  "Create an ingredient.
‘AMOUNT’ can be a string, a number or a valid sequence.
‘NAME’ is a string.
If one constraint gets disregarded throw error."
  (save-match-data
    (when (not (stringp name)) (error "Invalid ‘NAME’(%s) for ingredient" name))
    (list name
	  (org-shoplist--ing-validate-amount amount)
	  (org-shoplist--ing-find-unit-group amount))))

(defun org-shoplist-ing-+ (&rest amounts)
  "Add ‘AMOUNTS’ toghether return the sum."
  (org-shoplist--ing-validate-amount
   (calc-eval
    (math-simplify-units
     (math-read-expr
      (mapconcat (lambda (x)
		   (cond ((stringp x) x)
			 ((integerp x) (int-to-string x))
			 ((eq nil x) "0")
			 ((listp x) (org-shoplist-ing-amount x))
			 (t (error "Given ‘AMOUNT’(%s) can’t be converted" x))))
		 amounts "+"))))))

(defun org-shoplist-ing-* (ing factor)
  "Multiply the amount of ‘ING’ with given ‘FACTOR’.
Return new ingredient with modified amount."
  (if (= factor 0) nil
    (org-shoplist-ing-create
     (calc-eval (math-simplify-units (math-read-expr (concat (int-to-string factor) "*" (org-shoplist-ing-amount ing)))))
     (org-shoplist-ing-name ing))))

(defun org-shoplist-ing-read (&optional aggregate str)
  "‘AGGREGATE’ output when t else return parsed ‘STR’ raw.
Whenn ‘STR’ is nil read line where point is at."
  (when (eq str nil) (setq str (thing-at-point 'line)))
  (if (or (eq nil str) (string= str ""))
      nil
    (org-shoplist--ing-read-loop str 0 '() aggregate)))

(defun org-shoplist-ing-+-p (ing1 ing2)
  "Return t when ‘ING1’ and ‘ING2’ can be summend else nil."
  (condition-case nil
      (progn
	(if (and (string= (org-shoplist-ing-name ing1) (org-shoplist-ing-name ing2)) (org-shoplist-ing-+ ing1 ing2))
	    t
	  nil))
    (error nil)))

(defun org-shoplist--ing-read-loop (str start-pos ings &optional aggregate)
  "Helper functions for (org-shoplist-read) which does the recursive matching.
‘STR’ is a string where regex is getting matched against.
‘START-POS’ is where in string should start.
‘INGS’ is a list of the found ingredients.
‘AGGREGATE’ when t"
  (if (string-match (eval org-shoplist-ing-regex) str start-pos)
      (org-shoplist--ing-read-loop
       str
       (match-end 0)
       (let ((new-ing (org-shoplist-ing-create (concat (match-string 1 str) (match-string 3 str)) (match-string 5 str))))
	 (if (eq ings nil)
	     (list new-ing)
	   (if (not (eq nil aggregate))
	       (let* ((same-ings (seq-filter (lambda (x) (org-shoplist-ing-+-p new-ing x)) ings))
		      (new-ings (seq-difference ings same-ings)))
		 (push (org-shoplist-ing-create (apply 'org-shoplist-ing-+ (push new-ing same-ings)) (org-shoplist-ing-name new-ing))
		       new-ings))
	     (push new-ing ings))))
       aggregate)
    (reverse ings)))

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

(defun org-shoplist--recipe-read-all-ing (stars &optional aggregate)
  "Assums that at beginning of recipe.
Return a list of ingredient-structures of recipe where point is at.
‘STARS’ are the stars of the recipe heading.
‘AGGREGATE’ ingredients when set."
   (let ((ing-list nil))
    (beginning-of-line 2)
    (while (and (not (looking-at-p (concat "^" (regexp-quote stars) " ")))
		(not (= (point) (point-max))))
      (setq ing-list (append ing-list (org-shoplist-ing-read aggregate)))
      (beginning-of-line 2))
    ing-list))

(defun org-shoplist-recipe-read (&optional aggregate)
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
