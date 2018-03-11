;;; org-shoplist.el --- Eat the world

;; Copyright (C) 2017 Free Software Foundation, Inc.

;; Author: lordnik22
;; Version: 1.0.0
;; Package-Requires: ((org "9.0.6"))
;; Keywords: org-mode, shopping list, moneyflow
;; URL: https://github.com/lordnik22

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;; Commentary:
;; There is nothing done, yet.
;;; Code:
(defcustom org-shoplist-ingredient-units '()
  "Units for specifing the amount of a ingredient."
  :type '(repeat string)
  :group 'org-shoplist)

(defconst org-shoplist-ingredient-regex
  (concat "(\\([1-9][0-9]*\\)"
	  "\\("
	  (mapconcat 'identity
		     org-shoplist-ingredient-units
		     "\\|")
	  "\\)?"
	  " \\(.+?\\))")
  "Regex for finding ingredients in strings.")

(defun org-shoplist-str-to-ing (str)
  "Convert 'STR' to a ing struct.
When 'STR' is empty or can't be converted returns nil.
Empty ings are nil or nothing."
  (if (eq nil (string-match org-shoplist-ingredient-regex str))
      '()
    (org-shoplist--str-to-ing-extract-match '() str)))

(defun org-shoplist--str-to-ing-extract-match (ing-list str)
  "Extracts every ingredient in (match-string).
'ING-LIST' contains the found ingredients.
'START-INDEX' defines the starting index where ing starts
'STR' defines the string which contains the ingredients.
A ingredient is a list of three elements."
  (setq ing-list
	(cons (list (string-to-number (match-string 1 str))
		    (if (string= (match-string 2 str) "")
			nil
		      (match-string 2 str))
		    (match-string 3 str))
	      ing-list))
  (if (eq nil (string-match org-shoplist-ingredient-regex str (match-end 0)))
      (reverse ing-list)
    (org-shoplist--str-to-ing-extract-match ing-list str)))

(defun org-shoplist-ing-get-amount (ing)
  "Get amount of ingredient as a number.
'ING' is a list representing a ingredient."
  (car ing))

(defun org-shoplist-ing-get-unit (ing)
  "Get unit of ingredient as a string.
'ING' is a list representing a ingredient."
  (car (cdr ing)))

(defun org-shoplist-ing-get-name (ing)
  "Get name of ingredient as a string.
'ING' is a list representing a ingredient.
Beaware that the name also includes adjectives.
Exmaple: '(100 'g' 'crushed Nuts) => 'crushed Nuts'"
  (car (cdr (cdr ing))))
(provide 'org-shoplist)
;;; org-shoplist.el ends here
