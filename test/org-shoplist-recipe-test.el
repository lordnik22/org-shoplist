;;; org-shoplist-recipe-test.el --- Tests for org-shoplist
;;; Commentary:
;; Tests the data structures and functions of org-shoplist
;;; Code:
(ert-deftest org-shoplist-test/feeling-better? ()
  "Checks if it's a good day to program."
  (should (= 1 1)))

(ert-deftest org-shoplist-test/recipe-create-nil ()
  "From nothing comes nothing."
  (should (eq nil (org-shoplist-recipe-create nil))))

(ert-deftest org-shoplist-test/recipe-create-empty-string-name ()
  "Should error when passing no name for recipe."
  (should (equal '(user-error "Invalid name for recipe: ‘’")
		 (should-error (org-shoplist-recipe-create "")))))


(ert-deftest org-shoplist-test/recipe-create-normal-name-ing-nil ()
  "Create no recipe when there are no ingredients."
  (should (equal nil (org-shoplist-recipe-create "Nut Salat"))))

(ert-deftest org-shoplist-test/recipe-create-normal-name-one-ing ()
  "Create a recipe with one ingredient."
  (should (equal (list "Nut Salat" (list (org-shoplist-ing-create "100g" "Nuts")))
		 (org-shoplist-recipe-create "Nut Salat" (org-shoplist-ing-create "100g" "Nuts")))))

(ert-deftest org-shoplist-test/recipe-create-normal-name-two-ing ()
  "Create a recipe with two ingredients."
  (should (equal (list "Nut Salat"
		       (list (org-shoplist-ing-create "100g" "Nuts")
			     (org-shoplist-ing-create "100g" "Nuts")))
		 (org-shoplist-recipe-create "Nut Salat"
			  (org-shoplist-ing-create "100g" "Nuts")
			  (org-shoplist-ing-create "100g" "Nuts")))))

(ert-deftest org-shoplist-test/recipe-create-normal-name-two-diff-ing ()
  "Create a recipe with two different ingredients."
  (should (equal (list "Nut Salat"
		       (list (org-shoplist-ing-create "100g" "Nuts")
			     (org-shoplist-ing-create "200g" "Salat")))
		 (org-shoplist-recipe-create "Nut Salat"
			  (org-shoplist-ing-create "100g" "Nuts")
			  (org-shoplist-ing-create "200g" "Salat")))))

(ert-deftest org-shoplist-test/recipe-create-normal-name-three-diff-ing ()
  "Create a recipe with three different ingredients."
  (should (equal (list "Nut Salat"
		       (list
			(org-shoplist-ing-create "100g" "Nuts")
			(org-shoplist-ing-create "200g" "Salat")
			(org-shoplist-ing-create "1tsp" "Pepper")))
		 (org-shoplist-recipe-create "Nut Salat"
			  (org-shoplist-ing-create "100g" "Nuts")
			  (org-shoplist-ing-create "200g" "Salat")
			  (org-shoplist-ing-create "1tsp" "Pepper")))))

(ert-deftest org-shoplist-test/recipe-create-passing-list-of-ings ()
  "Create a recipe with by passing a list of ingredients."
  (should (equal (list "Nut Salat"
		   (list (org-shoplist-ing-create "100g" "Nuts")
			 (org-shoplist-ing-create "200g" "Salat")
			 (org-shoplist-ing-create "1tsp" "Pepper")))
		 (org-shoplist-recipe-create "Nut Salat"
				 (list
				  (org-shoplist-ing-create "100g" "Nuts")
				  (org-shoplist-ing-create "200g" "Salat")
				  (org-shoplist-ing-create "1tsp" "Pepper"))))))

(ert-deftest org-shoplist-test/recipe-read-empty-buffer ()
  "Throw an error when buffer is empty."
  (org-shoplist-test-test-in-org-buffer
   (lambda ()
     (goto-char (point-min)) ;;move point that buffer don't get terminated
     (should (equal '(user-error "Not at beginning of recipe")
		    (should-error (org-shoplist-recipe-read))))
     (should (= (point) (point-min))))))

(ert-deftest org-shoplist-test/recipe-read-no-name ()
  "Read a recipe with no ingredients."
  (org-shoplist-test-test-in-org-buffer
   (lambda ()
     (insert "* ")
     (goto-char (point-min))
     (should (equal '(user-error "Invalid name for recipe: ‘’")
		    (should-error (org-shoplist-recipe-read)))))))

(ert-deftest org-shoplist-test/recipe-read-no-ings ()
  "Read a recipe with no ingredients."
  (org-shoplist-test-test-in-org-buffer
   (lambda ()
     (insert "* Test")
     (goto-char (point-min))
     (should (equal nil (org-shoplist-recipe-read))))))

(ert-deftest org-shoplist-test/recipe-read-all-ing ()
  "Read all ingredients of recipe."
  (org-shoplist-test-test-in-org-buffer
   (lambda ()
     (setq org-shoplist-explicit-keyword nil)
     (insert "* Test
- (200g Nuts) mahlen")
     (goto-char (point-min))
     (should (equal (org-shoplist-recipe-create "Test" (list (org-shoplist-ing-create "200g" "Nuts")))
		    (org-shoplist-recipe-read))))))

(ert-deftest org-shoplist-test/recipe-read-all-two-ing ()
  "Read all ingredients of recipe."
  (org-shoplist-test-test-in-org-buffer
   (lambda ()
     (insert "* Test
- (200g Nuts) mahlen
- (200g Nuts) mahlen")
     (goto-char (point-min))
     (should (equal (org-shoplist-recipe-create "Test"
			     (list (org-shoplist-ing-create "200g" "Nuts")
				   (org-shoplist-ing-create "200g" "Nuts")))
		    (org-shoplist-recipe-read)))
     (should (= 49 (point))))))


(ert-deftest org-shoplist-test/recipe-read-one-ing ()
  "Read a recipe with one full ingredient.
Full means with unit, amount and ing-name."
  (org-shoplist-test-test-in-org-buffer
   (lambda ()
     (insert "* Test
- (200g Nuts) mahlen")
     (goto-char (point-min))
     (should (equal (list "Test" (list (org-shoplist-ing-create "200g" "Nuts")))
		    (org-shoplist-recipe-read))))))

(ert-deftest org-shoplist-test/recipe-read-two-ing ()
  "Read a recipe with two full ingredients.
Full means with unit, amount and ing-name."
  (org-shoplist-test-test-in-org-buffer
   (lambda ()
     (insert "* Test
- (200g Nuts) mahlen
- (200g Nuts) mahlen")
     (goto-char (point-min))
     (should (equal (list "Test"
			  (list (org-shoplist-ing-create "200g" "Nuts")
				(org-shoplist-ing-create "200g" "Nuts")))
		    (org-shoplist-recipe-read))))))

(ert-deftest org-shoplist-test/recipe-read-three-ing ()
  "Read a recipe with three full ingredients.
Full means with unit, amount and ing-name."
  (org-shoplist-test-test-in-org-buffer
   (lambda ()
     (insert "* Test
- (200g Nuts) mahlen
- (200g Nuts) mahlen
- (200g Nuts) mahlen")
     (goto-char (point-min))
     (should (equal (list "Test"
		      (list (org-shoplist-ing-create "200g" "Nuts")
			    (org-shoplist-ing-create "200g" "Nuts")
			    (org-shoplist-ing-create "200g" "Nuts")))
		    (org-shoplist-recipe-read))))))

(ert-deftest org-shoplist-test/recipe-read-two-ing-with-star-trash-inbetween ()
  "Read a recipe with two full ingredients but trash inbetween.
Full means with unit, amount and ing-name.
Trash means any text that contains no ingredient."
  (org-shoplist-test-test-in-org-buffer
   (lambda ()
     (insert "* Test
- (200g Nuts) mahlen
*Sauce:*
- (200g Nuts) mahlen")
     (goto-char (point-min))
     (should (equal (list "Test"
			  (list (org-shoplist-ing-create "200g" "Nuts")
				(org-shoplist-ing-create "200g" "Nuts")))
		    (org-shoplist-recipe-read))))))

(ert-deftest org-shoplist-test/recipe-read-two-ing-with-trash-inbetween ()
  "Read a recipe with two full ingredients but trash inbetween.
Full means with unit, amount and ing-name.
Trash means any text that contains no ingredient."
  (org-shoplist-test-test-in-org-buffer
   (lambda ()
     (insert "* Test
- (200g Nuts) mahlen
Achtung: doppelt!
- (200g Nuts) mahlen")
     (goto-char (point-min))
     (should (equal (list "Test"
			  (list (org-shoplist-ing-create "200g" "Nuts")
				(org-shoplist-ing-create "200g" "Nuts")))
		    (org-shoplist-recipe-read))))))

(ert-deftest org-shoplist-test/recipe-read-between-two-headers ()
  "Read a recipe with two full ingredients.
Full means with unit, amount and ing-name."
  (org-shoplist-test-test-in-org-buffer
   (lambda ()
     (insert "* Rezept 1
Die (200g Nuts) mahlen.
Nuts haben einen hohen Protain gehalt.
Für die Sauce brauchen wir:
- (200g Nuts)
* Rezept 2")
     (goto-char (point-min))
     (should (equal (list "Rezept 1"
			  (list (org-shoplist-ing-create "200g" "Nuts")
				(org-shoplist-ing-create "200g" "Nuts")))
		    (org-shoplist-recipe-read))))))

(ert-deftest org-shoplist-test/recipe-read-only-one-header ()
  "Read the recipe which is marked."
  (org-shoplist-test-test-in-org-buffer
   (lambda ()
     (insert "* Rezept 1
Die (200g Nuts) mahlen.
Nuts haben einen hohen Protain gehalt.
Für die Sauce brauchen wir:
- (200g Nuts)
* Rezept 2
- (200g Flour)")
     (goto-char (point-min))
     (should (equal (list "Rezept 1"
			  (list (org-shoplist-ing-create "200g" "Nuts")
				(org-shoplist-ing-create "200g" "Nuts")))
		    (org-shoplist-recipe-read))))))

(ert-deftest org-shoplist-test/recipe-read-aggregate-100-same-ingredients ()
  "Read a recipe with 100 ing to see performance of regex."
  (org-shoplist-test-test-in-org-buffer
   (lambda ()
     (setq org-shoplist-explicit-keyword nil)
     (insert-file-contents "./file/recipe-with-100-ing.org")
     (goto-char (point-min))
     (should (equal (list "Recipe 1"
			  (list (org-shoplist-ing-create "100g" "Nuts")))
		    (org-shoplist-recipe-read t))))))

(ert-deftest org-shoplist-test/recipe-read-invalid-amount ()
  "Read the recipe which is marked."
  (org-shoplist-test-test-in-org-buffer
   (lambda ()
     (insert "* Rezept 1
Die (200mg. Milk) mahlen.
Nuts haben einen hohen Protain gehalt.
Für die Sauce brauchen wir:
- (200g Nuts)
* Rezept 2
- (200g Flour)")
     (goto-char (point-min))
     (should (equal '(user-error "Invalid ‘AMOUNT’(200mg.) for ingredient")
		    (should-error (org-shoplist-recipe-read)))))))

(ert-deftest org-shoplist-test/recipe-read-three-ings-two-broken ()
  "Read broken ings as normal when reading recipes."
  (org-shoplist-test-test-in-org-buffer
   (lambda ()
     (insert "* Rezept 1
Die (100g Nuts) (200ml
 Milk) (100g
Salad) mahlen.")
     (goto-char (point-min))
     (should (equal (org-shoplist-recipe-create "Rezept 1"
			     (org-shoplist-ing-create "100g" "Nuts" " ")
			     (org-shoplist-ing-create "200ml" "Milk" "\n ")
			     (org-shoplist-ing-create "100g" "Salad" "\n"))
		    (org-shoplist-recipe-read))))))

(ert-deftest org-shoplist-test/recipe-*-nil-nil ()
  "From nothing comes nothing."
  (should (eq nil (org-shoplist-recipe-* nil nil))))

(ert-deftest org-shoplist-test/recipe-*-recipe-factor-nil ()
  "Return the same recipe when factor is nil."
  (should (equal (org-shoplist-recipe-create "Test" (org-shoplist-ing-create "400g" "Nuts"))
		 (org-shoplist-recipe-* (org-shoplist-recipe-create "Test" (org-shoplist-ing-create "400g" "Nuts")) nil))))

(ert-deftest org-shoplist-test/recipe-*-recipe-nil-factor-2 ()
  "From nothing comes nothing."
  (should (eq nil (org-shoplist-recipe-* nil 2))))

(ert-deftest org-shoplist-test/recipe-*-2-200g ()
  "Multiply all ingredients of recipe by given factor. "
  (should (equal (org-shoplist-recipe-create "Test" (org-shoplist-ing-create "400g" "Nuts"))
		 (org-shoplist-recipe-* (org-shoplist-recipe-create "Test" (org-shoplist-ing-create "200g" "Nuts")) 2))))

(ert-deftest org-shoplist-test/recipe-*-2/3-200g ()
  "Multiply all ingredients of recipe by given factor. "
  (should (equal (org-shoplist-recipe-create "Test" (org-shoplist-ing-create "133.g" "Nuts"))
		 (org-shoplist-recipe-* (org-shoplist-recipe-create "Test" (org-shoplist-ing-create "200g" "Nuts")) (/ 2.0 3)))))

(ert-deftest org-shoplist-test/recipe-*-2/3-200g ()
  "Multiply all ingredients of recipe by given factor. "
  (should (equal (org-shoplist-recipe-create "Test"
			  (org-shoplist-ing-create "200g" "Nuts")
			  (org-shoplist-ing-create "400ml" "Milk"))
		 (org-shoplist-recipe-* (org-shoplist-recipe-create "Test"
			      (org-shoplist-ing-create "100g" "Nuts")
			      (org-shoplist-ing-create "200ml" "Milk"))
		     2))))

(ert-deftest org-shoplist-test/recipe-explict-keyword ()
  "Only read the explict marked org-headings when org-shoplist-explicit-keyword it t."
  (org-shoplist-test-test-in-org-buffer
   (lambda ()
     (insert "* " org-shoplist-keyword " Rezept 2
- (200g Nuts)
** More ingredients
- (200g Nuts)")
     (goto-char (point-min))
     (should (equal (org-shoplist-recipe-create "Rezept 2" (org-shoplist-ing-create "200g" "Nuts"))
		    (org-shoplist-recipe-read nil t))))))

(ert-deftest org-shoplist-test/recipe-explict-keyword-read-one-marked-subheading ()
  "Only read the explict marked org-headings when org-shoplist-explicit-keyword it t."
  (org-shoplist-test-test-in-org-buffer
   (lambda ()
     (setq org-shoplist-explicit-keyword t)
     (insert "* " org-shoplist-keyword " Rezept 2
- (200g Nuts)
** " org-shoplist-keyword " More ingredients
- (200g Nuts)")
     (goto-char (point-min))
     (should (equal (org-shoplist-recipe-create "Rezept 2"
			     (org-shoplist-ing-create "200g" "Nuts")
			     (org-shoplist-ing-create "200g" "Nuts"))
		    (org-shoplist-recipe-read))))))

(ert-deftest org-shoplist-test/recipe-explict-keyword-read-two-marked-subheading-after-unmarked ()
  "Only read the explict marked org-headings when org-shoplist-explicit-keyword it t."
  (org-shoplist-test-test-in-org-buffer
   (lambda ()
     (insert "* " org-shoplist-keyword " Rezept 2
- (200g Nuts)
** Other ingredients 1
- (200g Floor)
** " org-shoplist-keyword " More ingredients
- (200g Salt)
** " org-shoplist-keyword " Other ingredients 2
- (200g Apple)")
     (goto-char (point-min))
     (should (equal (org-shoplist-recipe-create "Rezept 2"
			     (org-shoplist-ing-create "200g" "Nuts")
			     (org-shoplist-ing-create "200g" "Salt")
			     (org-shoplist-ing-create "200g" "Apple"))
		    (org-shoplist-recipe-read nil t))))))

(ert-deftest org-shoplist-test/recipe-explict-keyword-read-marked-subheading-inbetween-two-unmarked ()
  "Only read the explict marked org-headings when org-shoplist-explicit-keyword it t."
  (org-shoplist-test-test-in-org-buffer
   (lambda ()
     (insert "* " org-shoplist-keyword " Rezept 2
- (200g Nuts)
** Other ingredients 1
- (200g Floor)
** " org-shoplist-keyword " More ingredients
- (200g Salt)
** Other ingredients 2
- (200g Apple)")
     (goto-char (point-min))
     (should (equal (org-shoplist-recipe-create "Rezept 2"
			     (org-shoplist-ing-create "200g" "Nuts")
			     (org-shoplist-ing-create "200g" "Salt"))
		    (org-shoplist-recipe-read nil t))))))


(ert-deftest org-shoplist-test/factor-up-ones-buffer-empty ()
  (org-shoplist-test-test-in-org-buffer
   (lambda ()
     (should (equal '(user-error "Not in recipe")
		    (should-error (org-shoplist-recipe-factor-up)))))))

(ert-deftest org-shoplist-test/factor-up-ones-header-no-property ()
  (org-shoplist-test-test-in-org-buffer
   (lambda ()
     (insert "* Test Header")
     (goto-char (point-min))
     (should (equal (list 'user-error
			  (format "No inital value for %s defined"
				  org-shoplist-factor-property-name))
		    (should-error (org-shoplist-recipe-factor-up)))))))

(ert-deftest org-shoplist-test/factor-up-ones-header-no-ingredients ()
  (org-shoplist-test-test-in-org-buffer
   (lambda ()
     (insert "* Test Header
  :PROPERTIES:
  :" org-shoplist-factor-property-name ":   1
  :END:")
     (goto-char (point-min))
     (should (equal '(user-error "No ingredients to apply factor")
		    (should-error (org-shoplist-recipe-factor-up)))))))

(ert-deftest org-shoplist-test/factor-up-five-times-600g-amount ()
  (org-shoplist-test-test-in-org-buffer
   (lambda ()
     (insert "* Test Header
  :PROPERTIES:
  :" org-shoplist-factor-property-name ":   1
  :END:
- (100g Nuts)")
     (goto-char (point-min))
     (org-shoplist-recipe-factor-up)
     (org-shoplist-recipe-factor-up)
     (org-shoplist-recipe-factor-up)
     (org-shoplist-recipe-factor-up)
     (org-shoplist-recipe-factor-up)
     (should (string= (buffer-string)
		      (concat "* Test Header
  :PROPERTIES:
  :" org-shoplist-factor-property-name ":   6
  :END:
- (600g Nuts)"))))))

(ert-deftest org-shoplist-test/factor-up-and-down-five-times-same-amount ()
  (org-shoplist-test-test-in-org-buffer
   (lambda ()
     (insert "* Test Header
  :PROPERTIES:
  :" org-shoplist-factor-property-name ":   1
  :END:
- (100g Nuts)")
     (goto-char (point-min))
     (org-shoplist-recipe-factor-up)
     (org-shoplist-recipe-factor-up)
     (org-shoplist-recipe-factor-up)
     (org-shoplist-recipe-factor-up)
     (org-shoplist-recipe-factor-up)
     (org-shoplist-recipe-factor-down)
     (org-shoplist-recipe-factor-down)
     (org-shoplist-recipe-factor-down)
     (org-shoplist-recipe-factor-down)
     (org-shoplist-recipe-factor-down)
     (should (string= (buffer-string)
		      (concat "* Test Header
  :PROPERTIES:
  :" org-shoplist-factor-property-name ":   1
  :END:
- (100g Nuts)"))))))

(ert-deftest org-shoplist-test/factor-up-1-2-one-header-one-ingredient ()
  (org-shoplist-test-test-in-org-buffer
   (lambda ()
     (insert "* Test Header
  :PROPERTIES:
  :" org-shoplist-factor-property-name ":   1
  :END:
- (200g Nuts)")
     (goto-char (point-min))
     (org-shoplist-recipe-factor-up)
     (should (string= (buffer-string)
		      (concat "* Test Header
  :PROPERTIES:
  :" org-shoplist-factor-property-name ":   2
  :END:
- (400g Nuts)")))
     (should (= (point) (point-min))))))

(ert-deftest org-shoplist-test/factor-up-2-3-one-header-one-ingredient ()
  (org-shoplist-test-test-in-org-buffer
   (lambda ()
     (insert "* Test Header
  :PROPERTIES:
  :" org-shoplist-factor-property-name ":   2
  :END:
- (200g Nuts)")
     (goto-char (point-min))
     (org-shoplist-recipe-factor-up)
     (should (string= (buffer-string)
		      (concat "* Test Header
  :PROPERTIES:
  :" org-shoplist-factor-property-name ":   3
  :END:
- (300g Nuts)")))
     (should (= (point) (point-min))))))

(ert-deftest org-shoplist-test/factor-up-1-2-one-header-two-ingredient ()
  (org-shoplist-test-test-in-org-buffer
   (lambda ()
     (insert "* Test Header
  :PROPERTIES:
  :" org-shoplist-factor-property-name ":   1
  :END:
- (200g Nuts)
- (100ml Milk)")
     (goto-char (point-min))
     (org-shoplist-recipe-factor-up)
     (should (string= (buffer-string)
		      (concat "* Test Header
  :PROPERTIES:
  :" org-shoplist-factor-property-name ":   2
  :END:
- (400g Nuts)
- (200ml Milk)")))
     (should (= (point) (point-min))))))

(ert-deftest org-shoplist-test/factor-up-1-2-one-header-one-broken-ing ()
  (org-shoplist-test-test-in-org-buffer
   (lambda ()
     (insert "* Test Header
  :PROPERTIES:
  :" org-shoplist-factor-property-name ":   1
  :END:
- (200g
Nuts)")
     (goto-char (point-min))
     (org-shoplist-recipe-factor-up)
     (should (string= (buffer-string)
		      (concat "* Test Header
  :PROPERTIES:
  :" org-shoplist-factor-property-name ":   2
  :END:
- (400g
Nuts)")))
     (should (= (point) (point-min))))))

(ert-deftest org-shoplist-test/factor-up-1-2-one-header-one-ing-without-unit ()
  (org-shoplist-test-test-in-org-buffer
   (lambda ()
     (insert "* Test Header
  :PROPERTIES:
  :" org-shoplist-factor-property-name ":   1
  :END:
- (1 Orange)")
     (goto-char (point-min))
     (org-shoplist-recipe-factor-up)
     (should (string= (buffer-string)
		      (concat "* Test Header
  :PROPERTIES:
  :" org-shoplist-factor-property-name ":   2
  :END:
- (2 Orange)")))
     (should (= (point) (point-min))))))

(ert-deftest org-shoplist-test/factor-up-1-2-one-header-with-nested-non-marked-header-one-ing-each-exlicity-nil ()
  (org-shoplist-test-test-in-org-buffer
   (lambda ()
     (insert "* Test Header
  :PROPERTIES:
  :" org-shoplist-factor-property-name ":   1
  :END:
- (100g Nuts)
** other Test-Header
- (100g Nuts)")
     (goto-char (point-min))
     (setq org-shoplist-explicit-keyword nil)
     (org-shoplist-recipe-factor-up)
     (should (string= (buffer-string)
		      (concat"* Test Header
  :PROPERTIES:
  :" org-shoplist-factor-property-name ":   2
  :END:
- (200g Nuts)
** other Test-Header
- (200g Nuts)")))
     (should (= (point) (point-min))))))

(ert-deftest org-shoplist-test/factor-up-1-2-one-header-with-nested-marked-header-one-ing-each-explicity-nil ()
  (org-shoplist-test-test-in-org-buffer
   (lambda ()
     (insert "* Test Header
  :PROPERTIES:
  :" org-shoplist-factor-property-name ":   1
  :END:
- (100g Nuts)
** " org-shoplist-keyword " other Test-Header
- (100g Nuts)")
     (goto-char (point-min))
     (setq org-shoplist-explicit-keyword nil)
     (org-shoplist-recipe-factor-up)
     (should (string= (buffer-string)
		      (concat"* Test Header
  :PROPERTIES:
  :" org-shoplist-factor-property-name ":   2
  :END:
- (200g Nuts)
** " org-shoplist-keyword " other Test-Header
- (200g Nuts)")))
     (should (= (point) (point-min))))))

(ert-deftest org-shoplist-test/factor-up-1-2-one-header-with-nested-marked-header-one-ing-each-explicity-t ()
  (org-shoplist-test-test-in-org-buffer
   (lambda ()
     (insert "* " org-shoplist-keyword " Test Header
  :PROPERTIES:
  :" org-shoplist-factor-property-name ":   1
  :END:
- (100g Nuts)
** " org-shoplist-keyword " other Test-Header
- (100g Nuts)")
     (goto-char (point-min))
     (setq org-shoplist-explicit-keyword t)
     (org-shoplist-recipe-factor-up)
     (should (string= (buffer-string)
		      (concat"* " org-shoplist-keyword " Test Header
  :PROPERTIES:
  :" org-shoplist-factor-property-name ":   2
  :END:
- (200g Nuts)
** " org-shoplist-keyword " other Test-Header
- (200g Nuts)")))
     (should (= (point) (point-min))))))

(ert-deftest org-shoplist-test/factor-up-1-2-one-header-with-nested-non-marked-header-one-ing-each-exlicity-t ()
  (org-shoplist-test-test-in-org-buffer
   (lambda ()
     (insert "* " org-shoplist-keyword " Test Header
  :PROPERTIES:
  :" org-shoplist-factor-property-name ":   1
  :END:
- (100g Nuts)
** other Test-Header
- (100g Nuts)")
     (goto-char (point-min))
     (setq org-shoplist-explicit-keyword t)
     (org-shoplist-recipe-factor-up 1)
     (should (string= (buffer-string)
		      (concat "* " org-shoplist-keyword " Test Header
  :PROPERTIES:
  :" org-shoplist-factor-property-name ":   2
  :END:
- (200g Nuts)
** other Test-Header
- (100g Nuts)")))
     (should (= (point) (point-min))))))

(ert-deftest org-shoplist-test/factor-up-1-2-two-header-only-factor-one ()
  (org-shoplist-test-test-in-org-buffer
   (lambda ()
     (insert "* Test Header
  :PROPERTIES:
  :" org-shoplist-factor-property-name ":   1
  :END:
- (100g Nuts)
* other Test-Header
  :PROPERTIES:
  :" org-shoplist-factor-property-name ":   1
  :END:
- (100g Nuts)")
     (goto-char (point-min))
     (setq org-shoplist-explicit-keyword nil)
     (org-shoplist-recipe-factor-up)
     (should (string= (buffer-string)
		      (concat"* Test Header
  :PROPERTIES:
  :" org-shoplist-factor-property-name ":   2
  :END:
- (200g Nuts)
* other Test-Header
  :PROPERTIES:
  :" org-shoplist-factor-property-name ":   1
  :END:
- (100g Nuts)")))
     (should (= (point) (point-min))))))

(ert-deftest org-shoplist-test/factor-up-1-2-nested-header-with-factor-prop ()
  (org-shoplist-test-test-in-org-buffer
   (lambda ()
     (insert "* Test Header
  :PROPERTIES:
  :" org-shoplist-factor-property-name ":   1
  :END:
- (100g Nuts)
** other Test-Header
  :PROPERTIES:
  :" org-shoplist-factor-property-name ":   1
  :END:
- (100g Nuts)")
     (goto-char (point-min))
     (setq org-shoplist-explicit-keyword nil)
     (org-shoplist-recipe-factor-up)
     (should (string= (buffer-string)
		      (concat "* Test Header
  :PROPERTIES:
  :" org-shoplist-factor-property-name ":   2
  :END:
- (200g Nuts)
** other Test-Header
  :PROPERTIES:
  :" org-shoplist-factor-property-name ":   2
  :END:
- (200g Nuts)")))
     (should (= (point) (point-min))))))
;;; org-shoplist-recipe-test.el ends here
