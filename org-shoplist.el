;;; org-shoplist.el --- Eat the world -*- lexical-binding: t -*-

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
(eval-when-compile (require 'cl))

(defcustom org-shoplist-buffer-name "*Shopping List*"
  "Name of buffer when generating a shopping list."
  :type 'string
  :group 'org-shoplist)

(defcustom org-shoplist-keyword "TOBUY"
  "Keyword to mark recies for shopping."
  :type 'string
  :group 'org-shoplist)

(defcustom org-shoplist-factor-property-name "FACTOR"
  "The default name for the factor-property of headers."
  :type 'string
  :group 'org-shoplist)

(defcustom org-shoplist-table-header (list "Ingredient" "Amount")
  "This varaible defines the header of the standard ingredient header."
  :type '(repeat string)
  :group 'shoplist)

(defcustom org-shoplist-additional-units nil
  "Additional units that are needed for recipes with special units.
Beaware that the unit can't contain dots."
  :type '(repeat (list (symbol)
		       (string :tag "Definition")
		       (string :tag "Description")))
  :group 'org-shoplist)

(defcustom org-shoplist-explicit-keyword nil
  "When non-nil, only striclty include ingredients of marked headings.
Meaning: When for example a level-1-header is marked, the
ingredients defined in subheadings which aren’t marked don’t get
included in the shoplist."
  :type 'boolean
  :group 'org-shoplist)

(defcustom org-shoplist-aggregate t
  "When non-nil will aggregate the ingredient of the generated shoplist.
When nil won’t aggregate."
  :type 'boolean
  :group 'org-shoplist)

(defcustom org-shoplist-ing-start-char "("
  "Start char which introduces a ingredient."
  :type 'string
  :group 'org-shoplist)

(defcustom org-shoplist-ing-end-char ")"
  "End char which terminats a ingredient."
  :type 'string
  :group 'org-shoplist)

(defcustom org-shoplist-default-format 'org-shoplist-shoplist-as-table
  "Function name with one parameter which formats the shoplist."
  :type 'symbol
  :group 'org-shoplist)

(defcustom org-shoplist-ing-default-separator " "
  "Default separator for a ing parts."
  :type 'string
  :group 'org-shoplist)

(defconst org-shoplist-ing-unit-regex "\\([^0-9 ]+\\)"
  "Match a unit in a string.")

(defconst org-shoplist-ing-amount-regex "\\(\\([0-9]+\\(\\.\\|e-\\)\\)?\\([0-9]*\\(\\.\\|e-\\)\\)?[0-9]+[.]?[ ]?\\([^-+*\\\n .()]*\\)\\)"
  "Match an amount in a string in a exact fashion.")

(defconst org-shoplist--ing-first-part-regex '(format "\\([^%s%s]+?[^[:space:]%s%s]?\\)"
					  (regexp-quote org-shoplist-ing-start-char)
					  (regexp-quote org-shoplist-ing-end-char)
					  (regexp-quote org-shoplist-ing-start-char)
					  (regexp-quote org-shoplist-ing-end-char))
  "A regex which matches first part of ingredient the amount.")

(defconst org-shoplist--ing-second-part-regex '(format "\\([^[:space:]%s%s]?[^%s%s]+?\\)"
					   (regexp-quote org-shoplist-ing-start-char)
					   (regexp-quote org-shoplist-ing-end-char)
					   (regexp-quote org-shoplist-ing-start-char)
					   (regexp-quote org-shoplist-ing-end-char))
  "A regex which matches second part of the ingredient the name.")

(defconst org-shoplist--ing-content-spliter-regex "\\([[:space:]]+\\)"
  "A regex which matches whitespace which splits the date of ingredient.")

(defconst org-shoplist--ing-optional-content-spliter-regex "\\([[:space:]]*\\)"
  "A regex which matches whitespace which splits the date of ingredient.")

(defconst org-shoplist-ing-regex '(concat (regexp-quote org-shoplist-ing-start-char)
			      (eval org-shoplist--ing-first-part-regex)
			      (eval org-shoplist--ing-content-spliter-regex)
			      (eval org-shoplist--ing-second-part-regex)
			      (regexp-quote org-shoplist-ing-end-char))
  "Match an ingredient.")

;; Inject custom units
(when (not (eq nil org-shoplist-additional-units))
  (eval-after-load "calc-units" '(dolist (i org-shoplist-additional-units) (add-to-list 'math-additional-units i))))

(defun org-shoplist--ing-find-unit-group (amount)
  "Find the ground unit of ‘AMOUNT’s unit.
When ‘AMOUNT’ nil, return nil"
  (calc-eval (math-extract-units (math-to-standard-units (math-read-expr amount) nil))))

(defun org-shoplist-ing--transform-amount (amount)
  "Transform ‘AMOUNT’ to a valid form when possible else throw an error."
  (defun org-shoplist--calc-eval (str &optional separator &rest args)
    "Pass ‘STR’, ‘SEPARATOR’ and ‘ARGS’ to (calc-eval) than check if it is valid."
    (when (eq nil str) (setq str "0"))
    (let ((e-str (save-match-data (ignore-errors (eval (calc-eval str separator args))))))
      (when (or (eq e-str nil)
		(string-match "[<>+*/-]" e-str))
	(user-error "Invalid ‘AMOUNT’(%s) for ingredient" amount))
      (when (string= "0" e-str) (setq e-str (concat e-str (calc-eval (math-extract-units (math-read-expr str))))))
      (when (> 0 (string-to-number (substring e-str 0 1))) (setq e-str (concat "1" e-str )))
      (when (string-match "\\(\\.\\)\\([^0-9]\\|$\\)" e-str) (setq e-str (replace-match "" t t e-str 1)))
      (apply 'concat
	     (split-string
	      (concat (math-round (calc-eval (math-remove-units (math-read-expr e-str))))
		      (when-let ((unit (calc-eval (math-extract-units (math-read-expr e-str))))
				 (ex (not (string= "1" unit))))
			unit))))))
  (let ((math-backup math-simplifying-units))
    (unwind-protect
	(progn
	  (setq math-simplifying-units t)
	  (org-shoplist--calc-eval amount))
      (setq math-simplifying-units math-backup))))

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

(defun org-shoplist-ing-separator (ing)
  "Get separator of ‘ING’."
  (car (cdr (cdr (cdr ing)))))

(defun org-shoplist-ing-string (ing)
  "Return ‘ING’ as follow: “(amount name)”.
‘SEPARATOR’ is by default a space.  Can be any string."
  (concat org-shoplist-ing-start-char (org-shoplist-ing-amount ing) (org-shoplist-ing-separator ing) (org-shoplist-ing-name ing) org-shoplist-ing-end-char))

(defun org-shoplist-ing-create (amount name &optional separator)
  "Create an ingredient.
‘AMOUNT’ can be a string, a number or a valid sequence.
‘NAME’ is a string.
‘SEPARATOR’ a string by which ‘NAME’ and ‘AMOUNT’ is separated.
If one constraint gets disregarded throw error."
  (save-match-data
    (when (not (stringp name)) (user-error "Invalid ‘NAME’(%S) for ingredient" name))
    (let ((transform-amount (org-shoplist-ing--transform-amount amount)))
      (list name
	    transform-amount
	    (org-shoplist--ing-find-unit-group transform-amount)
	    (if (eq nil separator) org-shoplist-ing-default-separator separator)))))

(defun org-shoplist-ing-+ (&rest amounts)
  "Add ‘AMOUNTS’ toghether return the sum."
  (let ((sum-amount (mapconcat (lambda (x)
				 (cond ((stringp x) x)
				       ((integerp x) (number-to-string x))
				       ((eq nil x) "0")
				       ((listp x) (org-shoplist-ing-amount x))
				       (t (user-error "Given ‘AMOUNT’(%S) can’t be converted" x))))
			       amounts "+")))
    (if-let ((t-sum-amount (ignore-errors (org-shoplist-ing--transform-amount sum-amount))))
	t-sum-amount
      (user-error "Incompatible units while aggregating(%S)" amounts))))

(defun org-shoplist-ing-* (ing factor)
  "Multiply the amount of ‘ING’ with given ‘FACTOR’.
Return new ingredient with modified amount."
  (if (= factor 0) nil
    (org-shoplist-ing-create
     (concat (number-to-string factor) "*" (org-shoplist-ing-amount ing))
     (org-shoplist-ing-name ing)
     (org-shoplist-ing-separator ing))))

(defun org-shoplist-ing-aggregate (&rest ings)
  "Aggregate ‘INGS’."
  (let ((group-ings (seq-group-by (lambda (x) (list (org-shoplist-ing-name x) (org-shoplist-ing-group x))) ings))
	(aggregate-ings (list)))
    (while (not (eq nil (car group-ings)))
      (setq aggregate-ings (cons (org-shoplist-ing-create (apply 'org-shoplist-ing-+ (cdr (car group-ings)))
					      (org-shoplist-ing-name (car (car group-ings)))
					      (org-shoplist-ing-separator (car (car group-ings))))
				 aggregate-ings))
      (setq group-ings (cdr group-ings)))
    aggregate-ings))

(defun org-shoplist-ing-read (&optional aggregate str)
  "‘AGGREGATE’ output when non-nil else return parsed ‘STR’ raw.
Whenn ‘STR’ is nil read line where point is at."
  (defun org-shoplist--ing-read-loop (str start-pos ings)
    "Helper functions for (org-shoplist-read) which does the recursive matching.
‘STR’ is a string where regex is getting matched against.
‘START-POS’ is where in string should start.
‘INGS’ is a list of the found ingredients."
    (if (string-match (eval org-shoplist-ing-regex) str start-pos)
	(org-shoplist--ing-read-loop
	 str
	 (match-end 0)
	 (cons (org-shoplist-ing-create
		(match-string 1 str)
		(match-string 3 str)
		(match-string 2 str))
	       ings))

      ings))
  (defun org-shoplist--concat-when-broken (last-pos)
    "Concat broken ing when it’s splitted into two by newline."
    (when-let (ing-start (when (string-match (concat (regexp-quote org-shoplist-ing-start-char)
						     (eval org-shoplist--ing-first-part-regex)
						     (eval org-shoplist--ing-content-spliter-regex)
						     "$")
					     str last-pos)
			   (match-string 0 str)))
      (beginning-of-line 2)
      (let ((nl (thing-at-point 'line)))
	(when-let (ing-end (when (string-match (concat "^"
						       (eval org-shoplist--ing-optional-content-spliter-regex)
						       (eval org-shoplist--ing-second-part-regex)
						       (regexp-quote org-shoplist-ing-end-char))
					       nl)
			     (match-string 0 nl)))
	  (concat ing-start ing-end)))))
  (when (eq str nil) (setq str (thing-at-point 'line)))
  (when (not (or (eq nil str) (string= str "")))
    (let ((read-ings (org-shoplist--ing-read-loop str 0 '())))
      (when-let ((breaked-ing (save-excursion (org-shoplist--concat-when-broken (if (eq nil read-ings) 0 (match-end 0))))))
	(setq read-ings (org-shoplist--ing-read-loop breaked-ing 0 read-ings)))
      (if aggregate
	  (apply 'org-shoplist-ing-aggregate read-ings)
	(reverse read-ings)))))

(defun org-shoplist-recipe-create (name &rest ings)
  "Create a recipe.
‘NAME’ must be a string.
‘INGS’ must be valid ingredients.
Use ‘org-shoplist-ing-create’ to create valid ingredients."
  (when (and (stringp name) (string= name "")) (user-error "Invalid name for recipe: ‘%s’" name))
  (when (listp (car (car ings))) (setq ings (car ings)))
  (if (or (eq name nil) (eq ings nil) (equal ings '(nil)))
      nil
    (list name ings)))

(defun org-shoplist-recipe-name (recipe)
  "Get name of ‘RECIPE’."
  (car recipe))

(defun org-shoplist-recipe-get-all-ing (recipe)
  "Get all ingredients of ‘RECIPE’."
  (car (cdr recipe)))

(defun org-shoplist-recipe-* (recipe factor)
  "Multiply all ingredients of ‘RECIPE’ by given ‘FACTOR’."
  (if (eq factor nil)
      recipe
    (let (f-ing-list)
      (dolist (i (org-shoplist-recipe-get-all-ing recipe) f-ing-list)
	(push (org-shoplist-ing-* i factor) f-ing-list))
      (org-shoplist-recipe-create (org-shoplist-recipe-name recipe) (reverse f-ing-list)))))

(defun org-shoplist--recipe-read-factor ()
  "Read the value of ‘ORG-SHOPLIST-FACTOR-PROPERTY-NAME’ in recipe where point is at."
  (when (not (ignore-errors (org-back-to-heading t)))
    (user-error "Not in recipe"))
  (let ((v (ignore-errors
	     (string-to-number
	      (org-entry-get (point) org-shoplist-factor-property-name)))))
    v))

(defun org-shoplist-recipe-read (&optional aggregate explicit-match)
  "Assums that at beginning of recipe.
Which is at (beginning-of-line) at heading (╹* Nut Salat...).
Return a recipe structure or throw error.  To read a recipe there
must be at least a org-heading (name of the recipe) and one
ingredient.
‘AGGREGATE’ ingredients when non-nil.
‘EXPLICIT-MATCH’ when is non-nil only marked headings will be included.
See ‘org-shoplist-recipe-create’ for more details on creating general
recipes."
  (save-match-data
    (when (not (looking-at org-heading-regexp)) (user-error "Not at beginning of recipe"))
    (let ((read-ings
	   (save-match-data
	     (let ((ing-list nil)
		   (h (org-get-heading))		;current header
		   (l (org-current-level)))
	       (beginning-of-line 2)
	       (while (and (or (string= h (org-get-heading))
			       (> (org-current-level) l))
			   (not (>= (point) (point-max))))
		 (if explicit-match
		     (if (string= (org-get-todo-state) org-shoplist-keyword)
			 (setq ing-list (append ing-list (org-shoplist-ing-read))))
		   (setq ing-list (append ing-list (org-shoplist-ing-read))))
		 (beginning-of-line 2))
	       ing-list))))
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
			  (apply 'append (mapcar 'org-shoplist-recipe-get-all-ing recipes)))))))

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

(defun org-shoplist-shoplist-read (&optional aggregate explicit-match)
  "Return a shoplist structure or throw error.
To read a recipe there must be at least a org-heading (name of the recipe).
See ‘org-shoplist-recipe-create’ for more details on creating general recipes.
‘AGGREGATE’ ingredients when non-nil.
‘EXPLICIT-MATCH’ when is non-nil only marked headings will be included."
  (let ((recipe-list
	 (save-match-data
	   (let ((recipe-list nil))
	     (while (and (not (= (point-max) (point)))
			 (search-forward-regexp org-heading-regexp nil t 1))
	       (when (save-excursion (beginning-of-line 1) (looking-at-p (concat ".+" org-shoplist-keyword)))
		 (beginning-of-line 1)
		 (if (eq recipe-list nil)
		     (setq recipe-list (list (org-shoplist-recipe-read aggregate explicit-match)))
		   (push (org-shoplist-recipe-read aggregate explicit-match) recipe-list))))
	     recipe-list))))
    (apply 'org-shoplist-shoplist-create (reverse recipe-list))))

(defun org-shoplist-shoplist-as-table (shoplist)
  "Format ‘SHOPLIST’ as table."
  (concat "|" (mapconcat 'identity org-shoplist-table-header "|")
	  "|\n"
	  (mapconcat (lambda (i) (concat "|" (org-shoplist-ing-name i) "|" (org-shoplist-ing-amount i)))
		     (org-shoplist-shoplist-ings shoplist)
		     "|\n")
	  "|\n"))

(defun org-shoplist-shoplist-as-todo-list (shoplist)
  "Format ‘SHOPLIST’ as todo-list."
  (concat
   (concat "#+SEQ_TODO:\s" org-shoplist-keyword "\s|\sBOUGHT\n")
   (mapconcat (lambda (i)
		(concat "*\s" org-shoplist-keyword "\s"
			org-shoplist-ing-start-char (org-shoplist-ing-amount i) "\s" (org-shoplist-ing-name i) org-shoplist-ing-end-char))
	      (org-shoplist-shoplist-ings shoplist)
	      "\n")))

(defun org-shoplist-shoplist-insert (as-format)
  "Insert a shoplist with given format(‘AS-FORMAT’)."
  (save-excursion
    (funcall 'org-mode)
    (insert as-format)
    (goto-char (point-min))
    (when (org-at-table-p) (org-table-align))))

(defun org-shoplist (formatter)
  "Generate a shoplist from current buffer with ‘FORMATTER’."
  (interactive "aFormatter-Name: ")
  (let ((sl (with-current-buffer (current-buffer)
	      (save-excursion (goto-char (point-min)) (org-shoplist-shoplist-read org-shoplist-aggregate org-shoplist-explicit-keyword)))))
    (with-current-buffer (switch-to-buffer org-shoplist-buffer-name)
      (when (>= (buffer-size) 0) (erase-buffer))
      (when (eq formatter '##) (setq formatter org-shoplist-default-format))
      (org-shoplist-shoplist-insert (funcall formatter sl)))))

(defun org-shoplist-init ()
  "Setting the todo-keywords for current file."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (when (not (looking-at-p "#\\+SEQ_TODO:")) )
    (funcall 'org-mode)))

(defun org-shoplist-unmark-all ()
  "Unmark all recipes which are marked with ‘ORG-SHOPLIST-KEYWORD’."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (beginning-of-line 2)
    (while (re-search-forward (concat " " org-shoplist-keyword) nil t)
      (replace-match "" nil nil))))

(defun org-shoplist-recipe-set-factor (new-factor)
  "Set ‘NEW-FACTOR’ as value of the factor-property of current header.
When there is no value, set value as inital value."
  (interactive "NValue: " )
  (let ((old-factor (org-shoplist--recipe-read-factor)))
    (when (eq new-factor nil) (user-error "No inital value for %s defined"
					  org-shoplist-factor-property-name))
    (when (< new-factor 1) (user-error "Can’t decrement under 1"))
    (when old-factor
      (let* ((current-recipe (save-excursion (org-shoplist-recipe-read)))
	     (new-recipe (org-shoplist-recipe-*
			  current-recipe
			  (ignore-errors (/ (float new-factor) old-factor)))))
	(when (eq nil new-recipe) (user-error "No ingredients to apply factor"))
	;; replace current with new
	(save-excursion
	  (cl-mapc
	   (lambda (new old)
	     (search-forward (org-shoplist-ing-string old) nil t 1)
	     (replace-match (org-shoplist-ing-string new) t))
	   (org-shoplist-recipe-get-all-ing new-recipe)
	   (org-shoplist-recipe-get-all-ing current-recipe)))))
    (org-set-property org-shoplist-factor-property-name (number-to-string new-factor))))

(defun org-shoplist-recipe-factor-down ()
  "Decrement the factor-property of current header and adjust ingredients amounts."
  (interactive)
  (save-excursion (org-shoplist-recipe-set-factor (ignore-errors (1- (org-shoplist--recipe-read-factor))))))

(defun org-shoplist-recipe-factor-up ()
  "Increment the factor-property of current header."
  (interactive)
  (save-excursion (org-shoplist-recipe-set-factor (ignore-errors (1+ (org-shoplist--recipe-read-factor))))))

(provide 'org-shoplist)
;;; org-shoplist.el ends here
