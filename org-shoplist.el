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
(require 'calendar)

(defcustom org-shoplist-buffer-name "*Shopping List*"
  "Name of buffer when generating a shopping list."
  :type 'string
  :group 'org-shoplist)

(defcustom org-shoplist-keyword "TOBUY"
  "Keyword to mark recies for shopping."
  :type 'string
  :group 'org-shoplist)

(defcustom org-shoplist-table-header (list "Ingredient" "Amount")
  "This varaible defines the header of the standard ingredient header."
  :type '(repeat string)
  :group 'shoplist)

(defcustom org-shoplist-additional-units nil
  "Additional units that are needed for recipes with special units.
Beaware that the unit can't contain dots."
  :type '(repeat (symbol string string))
  :group 'org-shoplist)

(defconst org-shoplist-ing-unit-regex "\\([^0-9 ]+\\)"
  "Match a unit in a string.")

(defconst org-shoplist-ing-amount-regex "\\([0-9]*\\(\\.\\|e-\\)?[0-9]*\\(\\.\\|e-\\)?[0-9]+[.]?[ ]?\\([^-+*\\\n .]*\\)\\)"
  "Match an amount in a string.")

(defconst org-shoplist-ing-regex "(\\([1-9]?[0-9e\\.]*\\(\\.\\|-?\\)[0-9]*\\)[ ]?\\([^-+\n\t)(]+?\\)[ ]\\(.+?\\))"
  "Match an ingredient.")

;; Inject custom units
(when (not (eq nil org-shoplist-additional-units))
  (eval-after-load "calc-units" '(progn (apply 'add-to-list 'math-additional-units org-shoplist-additional-units))))

(defun org-shoplist-calc-eval (str &optional separator &rest args)
  "Pass ‘STR’, ‘SEPARATOR’ and ‘ARGS’ to (calc-eval) than check if it is valid."
  (when (eq nil str) (setq str "0"))
  (let* ((c-eval-str (calc-eval str separator args))
	 (f-char (substring c-eval-str 0 1)))
    (if (or (string= "0" f-char)
	    (< 0 (string-to-number f-char)))
	c-eval-str
      (concat "1" c-eval-str))))

(defun org-shoplist--ing-find-unit-group (amount)
  "Find the ground unit of ‘AMOUNT’s unit.
When ‘AMOUNT’ nil, return nil"
  (calc-eval (math-extract-units (math-to-standard-units (math-read-expr amount) nil))))

(defun org-shoplist--ing-transform-when-valid (amount)
  "Transform ‘AMOUNT’ to a valid form when possible else throw an error."
  (if (and (stringp amount)
	   (string-match (concat "^" org-shoplist-ing-amount-regex "$") amount))
      (save-match-data (org-shoplist-calc-eval amount))
    (when (eq amount nil) (setq amount "0"))
    (let ((math-backup math-simplifying-units))
      (unwind-protect
	  (progn (setq math-simplifying-units t)
		 (let ((c-eval-a (save-match-data (ignore-errors (org-shoplist-calc-eval amount)))))
		   (if (and (stringp c-eval-a)
			    (string-match (concat "^" org-shoplist-ing-amount-regex "$") c-eval-a))
		       c-eval-a
		     (user-error "Invalid ‘AMOUNT’(%s) for ingredient" amount))))
	(setq math-simplifying-units math-backup)
	nil))))

(defun org-shoplist-ing-name (ing)
  "Get name of ‘ING’."
  (car ing))

(defun org-shoplist-ing-amount (ing)
  "Get amount of ‘ING’."
  (car (cdr ing)))


(defun org-shoplist-ing-unit (ing)
  "Get unit of ‘ING’."
  (let ((amount (org-shoplist-ing-amount ing)))
    (if (string-match org-shoplist-ing-unit-regex amount)
	(match-string 1 amount)
      nil)))

(defun org-shoplist-ing-group (ing)
  "Get group of ‘ING’."
  (car (cdr (cdr ing))))

(defun org-shoplist-ing-create (amount name)
  "Create an ingredient.
‘AMOUNT’ can be a string, a number or a valid sequence.
‘NAME’ is a string.
If one constraint gets disregarded throw error."
  (save-match-data
    (when (not (stringp name)) (user-error "Invalid ‘NAME’(%s) for ingredient" name))
    (let ((valid-amount (org-shoplist--ing-transform-when-valid amount)))
      (list name
	    valid-amount
	    (org-shoplist--ing-find-unit-group valid-amount)))))

(defun org-shoplist-ing-+ (&rest amounts)
  "Add ‘AMOUNTS’ toghether return the sum."
  (org-shoplist--ing-transform-when-valid
   (mapconcat (lambda (x)
		(cond ((stringp x) x)
		      ((integerp x) (number-to-string x))
		      ((eq nil x) "0")
		      ((listp x) (org-shoplist-ing-amount x))
		      (t (user-error "Given ‘AMOUNT’(%s) can’t be converted" x))))
	      amounts "+")))

(defun org-shoplist-ing-* (ing factor)
  "Multiply the amount of ‘ING’ with given ‘FACTOR’.
Return new ingredient with modified amount."
  (if (= factor 0) nil
    (org-shoplist-ing-create
     (concat (number-to-string factor) "*" (org-shoplist-ing-amount ing))
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
  "‘AGGREGATE’ output when non-nil else return parsed ‘STR’ raw.
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
       (cons (org-shoplist-ing-create
	      (concat (match-string 1 str) (match-string 3 str))
	      (match-string 4 str))
	      ings))
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

(defun org-shoplist-recipe-get-all-ing (recipe)
  "Get all ingredients of ‘RECIPE’."
  (cdr recipe))

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
‘AGGREGATE’ ingredients when non-nil.
See ‘org-shoplist-recipe-create’ for more details on creating general
recipes."
  (save-match-data
    (when (not (looking-at org-heading-regexp)) (user-error "Not at beginning of recipe"))
    (let ((read-ings (save-match-data (org-shoplist--recipe-read-all-ing (match-string 1)))))
      (org-shoplist-recipe-create (string-trim (replace-regexp-in-string org-todo-regexp "" (match-string 2)))
		      (if aggregate (apply 'org-shoplist-ing-aggregate read-ings) read-ings)))))

(defun org-shoplist-shoplist-create (&rest recipes)
  "Create a shoplist.
‘RECIPES’ is a sequence of recipes."
  (if (or (eq recipes nil)
	  (eq nil (car recipes)))
      nil
    (list (calendar-current-date)
	  recipes
	  (reverse (apply 'org-shoplist-ing-aggregate
			  (apply 'append (apply 'append (mapcar 'org-shoplist-recipe-get-all-ing recipes))))))))

(defun org-shoplist-shoplist-creation-date (shoplist)
  "Get shopdate of shoplist.
‘SHOPLIST’ of which the date should be extracted."
  (car shoplist))

(defun org-shoplist-shoplist-recipes (shoplist)
  "Get recipes of shoplist.
‘SHOPLIST’ a."
  (if (eq nil (car (cdr shoplist)))
      nil
    (car (cdr shoplist))))

(defun org-shoplist-shoplist-ings (shoplist)
  "Get recipes of shoplist.
‘SHOPLIST’ a."
  (if (eq nil (car (cdr (cdr shoplist))))
      nil
    (car (cdr (cdr shoplist)))))

(defun org-shoplist--shoplist-read-all-recipes (&optional aggregate)
  "Read all recipes in buffer.
‘AGGREGATE’ when t"
  (let ((recipe-list nil))
    (while (and (not (= (point-max) (point)))
		(search-forward-regexp org-heading-regexp nil t 1))
      (when (save-excursion (beginning-of-line 1) (looking-at-p (concat ".+" org-shoplist-keyword)))
	(beginning-of-line 1)
	 (if (eq recipe-list nil)
	     (setq recipe-list (list (org-shoplist-recipe-read aggregate)))
	   (push (org-shoplist-recipe-read aggregate) recipe-list))))
    recipe-list))

(defun org-shoplist-shoplist-read (&optional aggregate)
  "Return a shoplist structure or throw error.
To read a recipe there must be at least a org-heading (name of the recipe).
See ‘org-shoplist-recipe-create’ for more details on creating general recipes.
‘AGGREGATE’ ingredients when non-nil."
  (let ((recipe-list (save-match-data (org-shoplist--shoplist-read-all-recipes aggregate))))
    (apply 'org-shoplist-shoplist-create (reverse recipe-list))))

(defun org-shoplist-shoplist-insert (shoplist)
  "Insert ‘SHOPLIST’ in current buffer."
  (insert "|" (mapconcat 'identity org-shoplist-table-header "|") "|" ?\n
	  (mapconcat (lambda (i) (concat "|" (org-shoplist-ing-name i) "|" (org-shoplist-ing-amount i)))
		     (org-shoplist-shoplist-ings shoplist) "|\n") "|" ?\n))

(defun org-shoplist ()
  "Generate a shoplist with recipes from current buffer."
  (interactive)
  (let ((sl (with-current-buffer (current-buffer) (org-shoplist-shoplist-read))))
    (with-current-buffer (switch-to-buffer org-shoplist-buffer-name)
      (when (>= (buffer-size) 0) (erase-buffer))
      (org-shoplist-shoplist-insert sl)
      (funcall 'org-mode)
      (org-table-align))))

(provide 'org-shoplist)
;;; org-shoplist.el ends here
