;;; org-shoplist-recipe-test.el --- Tests for org-shoplist
;;; Commentary:
;; Tests the data structures and functions of org-shoplist
;;; Code:
(ert-deftest org-shoplist-test/feeling-better? ()
  "Checks if it's a good day to program."
  (should (= 1 1)))

(ert-deftest org-shoplist-test/recipe-create-nil ()
  "From nothing comes nothing."
  (should (eq nil (org-shoplist-recipe-create nil nil nil nil))))

(ert-deftest org-shoplist-test/recipe-create-empty-string-name ()
  "Should error when passing no name for recipe."
  (should (equal '(user-error "Invalid name for recipe: ‘’")
		 (should-error (org-shoplist-recipe-create "" nil nil nil)))))

(ert-deftest org-shoplist-test/recipe-create-normal-name-one-ing ()
  "Create a recipe with one ingredient."
  (should (equal (list "Nut Salat" 1 nil (list (org-shoplist-ing-create "100g" "Nuts")))
		 (org-shoplist-recipe-create "Nut Salat" 1 nil (list (org-shoplist-ing-create "100g" "Nuts"))))))

(ert-deftest org-shoplist-test/recipe-create-factor-nil-one-ing ()
  "Create a recipe with factor nil."
  (should (equal (list "Nut Salat" nil 'org-shoplist--recipe-read-ings-marked-tree
                       (list (org-shoplist-ing-create "100g" "Nuts")))
		 (org-shoplist-recipe-create "Nut Salat" nil 'org-shoplist--recipe-read-ings-marked-tree
                          (list (org-shoplist-ing-create "100g" "Nuts"))))))

(ert-deftest org-shoplist-test/recipe-create-factor-empty-string-one-ing ()
  "Should error when factor not number."
  (should (equal '(user-error "Invalid factor for recipe(Nut Salat): ‘’")
		 (should-error (org-shoplist-recipe-create "Nut Salat" "" nil (list (org-shoplist-ing-create "100g" "Nuts")))))))

(ert-deftest org-shoplist-test/recipe-create-factor-number-string-one-ing ()
  "Should error when factor number in string."
  (should (equal '(user-error "Invalid factor for recipe(Nut Salat): ‘2a’")
		 (should-error (org-shoplist-recipe-create "Nut Salat" "2a" nil (list (org-shoplist-ing-create "100g" "Nuts")))))))

(ert-deftest org-shoplist-test/recipe-create-normal-name-two-ing ()
  "Create a recipe with two ingredients."
  (should (equal (list "Nut Salat"
                       nil
                       nil
		       (list (org-shoplist-ing-create "100g" "Nuts")
			     (org-shoplist-ing-create "100g" "Nuts")))
		 (org-shoplist-recipe-create "Nut Salat"
                          nil
                          nil
                          (list
                           (org-shoplist-ing-create "100g" "Nuts")
                           (org-shoplist-ing-create "100g" "Nuts"))))))

(ert-deftest org-shoplist-test/recipe-create-normal-name-two-diff-ing ()
  "Create a recipe with two different ingredients."
  (should (equal (list "Nut Salat"
                       nil
                       nil
		       (list (org-shoplist-ing-create "100g" "Nuts")
			     (org-shoplist-ing-create "200g" "Salat")))
		 (org-shoplist-recipe-create "Nut Salat"
                          nil
                          nil
			  (list (org-shoplist-ing-create "100g" "Nuts")
                                (org-shoplist-ing-create "200g" "Salat"))))))

(ert-deftest org-shoplist-test/recipe-create-normal-name-three-diff-ing ()
  "Create a recipe with three different ingredients."
  (should (equal (list "Nut Salat"
                       nil
                       nil
		       (list
			(org-shoplist-ing-create "100g" "Nuts")
			(org-shoplist-ing-create "200g" "Salat")
			(org-shoplist-ing-create "1tsp" "Pepper")))
		 (org-shoplist-recipe-create "Nut Salat"
                          nil
                          nil
			  (list (org-shoplist-ing-create "100g" "Nuts")
                                (org-shoplist-ing-create "200g" "Salat")
                                (org-shoplist-ing-create "1tsp" "Pepper"))))))

(ert-deftest org-shoplist-test/recipe-create-passing-list-of-ings ()
  "Create a recipe with by passing a list of ingredients."
  (should (equal (list "Nut Salat"
                       nil
                       nil
		   (list (org-shoplist-ing-create "100g" "Nuts")
			 (org-shoplist-ing-create "200g" "Salat")
			 (org-shoplist-ing-create "1tsp" "Pepper")))
		 (org-shoplist-recipe-create "Nut Salat"
                          nil
                          nil
                          (list (org-shoplist-ing-create "100g" "Nuts")
                                (org-shoplist-ing-create "200g" "Salat")
                                (org-shoplist-ing-create "1tsp" "Pepper"))))))

(ert-deftest org-shoplist-test/recipe-read-empty-buffer ()
  "Throw an error when buffer is empty."
  (org-shoplist-test-test-in-org-buffer
   (lambda ()
     (goto-char (point-min)) ;;move point that buffer don't get terminated
     (should (equal '(user-error "Not at beginning of recipe")
		    (should-error (org-shoplist-recipe-read 'org-shoplist--recipe-read-ings-current))))
     (should (= (point) (point-min))))))

(ert-deftest org-shoplist-test/recipe-read-no-name ()
  "Read a recipe with no ingredients."
  (org-shoplist-test-test-in-org-buffer
   (lambda ()
     (insert "* ")
     (goto-char (point-min))
     (should (equal '(user-error "Invalid name for recipe: ‘’")
		    (should-error (org-shoplist-recipe-read 'org-shoplist--recipe-read-ings-current)))))))

(ert-deftest org-shoplist-test/recipe-read-all-ing ()
  "Read all ingredients of recipe."
  (org-shoplist-test-test-in-org-buffer
   (lambda ()
     (setq org-shoplist-inital-factor nil)
     (setq org-shoplist-explicit-keyword nil)
     (insert "* Test
- (200g Nuts) mahlen")
     (goto-char (point-min))
     (should (equal (org-shoplist-recipe-create "Test" nil 'org-shoplist--recipe-read-ings-current
                             (list (org-shoplist-ing-create "200g" "Nuts")))
		    (org-shoplist-recipe-read 'org-shoplist--recipe-read-ings-current))))))

(ert-deftest org-shoplist-test/recipe-read-all-two-ing ()
  "Read all ingredients of recipe."
  (org-shoplist-test-test-in-org-buffer
   (lambda ()
     (setq org-shoplist-inital-factor nil)
     (insert "* Test
- (200g Nuts) mahlen
- (200g Nuts) mahlen")
     (goto-char (point-min))
     (should (equal (org-shoplist-recipe-create "Test"
                             nil
                             'org-shoplist--recipe-read-ings-current
			     (list (org-shoplist-ing-create "200g" "Nuts")
				   (org-shoplist-ing-create "200g" "Nuts")))
		    (org-shoplist-recipe-read 'org-shoplist--recipe-read-ings-current)))
     (should (= 49 (point))))))


(ert-deftest org-shoplist-test/recipe-read-one-ing ()
  "Read a recipe with one full ingredient.
Full means with unit, amount and ing-name."
  (org-shoplist-test-test-in-org-buffer
   (lambda ()
   (setq org-shoplist-inital-factor nil)
   (insert "* Test
- (200g Nuts) mahlen")
     (goto-char (point-min))
     (should (equal (list "Test" nil 'org-shoplist--recipe-read-ings-current
                          (list (org-shoplist-ing-create "200g" "Nuts")))
		    (org-shoplist-recipe-read 'org-shoplist--recipe-read-ings-current))))))

(ert-deftest org-shoplist-test/recipe-read-two-ing ()
  "Read a recipe with two full ingredients.
Full means with unit, amount and ing-name."
  (org-shoplist-test-test-in-org-buffer
   (lambda ()
     (setq org-shoplist-inital-factor nil)
     (insert "* Test
- (200g Nuts) mahlen
- (200g Nuts) mahlen")
     (goto-char (point-min))
     (should (equal (list "Test" nil 'org-shoplist--recipe-read-ings-current
			  (list (org-shoplist-ing-create "200g" "Nuts")
				(org-shoplist-ing-create "200g" "Nuts")))
		    (org-shoplist-recipe-read 'org-shoplist--recipe-read-ings-current))))))

(ert-deftest org-shoplist-test/recipe-read-three-ing ()
  "Read a recipe with three full ingredients.
Full means with unit, amount and ing-name."
  (org-shoplist-test-test-in-org-buffer
   (lambda ()
     (setq org-shoplist-inital-factor nil)
     (insert "* Test
- (200g Nuts) mahlen
- (200g Nuts) mahlen
- (200g Nuts) mahlen")
     (goto-char (point-min))
     (should (equal (list "Test" nil 'org-shoplist--recipe-read-ings-current
		      (list (org-shoplist-ing-create "200g" "Nuts")
			    (org-shoplist-ing-create "200g" "Nuts")
			    (org-shoplist-ing-create "200g" "Nuts")))
		    (org-shoplist-recipe-read 'org-shoplist--recipe-read-ings-current))))))

(ert-deftest org-shoplist-test/recipe-read-two-ing-with-star-trash-inbetween ()
  "Read a recipe with two full ingredients but trash inbetween.
Full means with unit, amount and ing-name.
Trash means any text that contains no ingredient."
  (org-shoplist-test-test-in-org-buffer
   (lambda ()
     (setq org-shoplist-inital-factor nil)
     (insert "* Test
- (200g Nuts) mahlen
*Sauce:*
- (200g Nuts) mahlen")
     (goto-char (point-min))
     (should (equal (list "Test" nil 'org-shoplist--recipe-read-ings-current
			  (list (org-shoplist-ing-create "200g" "Nuts")
				(org-shoplist-ing-create "200g" "Nuts")))
		    (org-shoplist-recipe-read 'org-shoplist--recipe-read-ings-current))))))

(ert-deftest org-shoplist-test/recipe-read-two-ing-with-trash-inbetween ()
  "Read a recipe with two full ingredients but trash inbetween.
Full means with unit, amount and ing-name.
Trash means any text that contains no ingredient."
  (org-shoplist-test-test-in-org-buffer
   (lambda ()
     (setq org-shoplist-inital-factor nil)
     (insert "* Test
- (200g Nuts) mahlen
Achtung: doppelt!
- (200g Nuts) mahlen")
     (goto-char (point-min))
     (should (equal (list "Test" nil
                          'org-shoplist--recipe-read-ings-current
			  (list (org-shoplist-ing-create "200g" "Nuts")
				(org-shoplist-ing-create "200g" "Nuts")))
		    (org-shoplist-recipe-read 'org-shoplist--recipe-read-ings-current))))))

(ert-deftest org-shoplist-test/recipe-read-between-two-headers ()
  "Read a recipe with two full ingredients.
Full means with unit, amount and ing-name."
  (org-shoplist-test-test-in-org-buffer
   (lambda ()
     (setq org-shoplist-inital-factor nil)
     (insert "* Rezept 1
Die (200g Nuts) mahlen.
Nuts haben einen hohen Protain gehalt.
Für die Sauce brauchen wir:
- (200g Nuts)
* Rezept 2")
     (goto-char (point-min))
     (should (equal (list "Rezept 1" nil
                          'org-shoplist--recipe-read-ings-current
			  (list (org-shoplist-ing-create "200g" "Nuts")
				(org-shoplist-ing-create "200g" "Nuts")))
		    (org-shoplist-recipe-read 'org-shoplist--recipe-read-ings-current))))))

(ert-deftest org-shoplist-test/recipe-read-only-one-header ()
  "Read the recipe which is marked."
  (org-shoplist-test-test-in-org-buffer
   (lambda ()
     (setq org-shoplist-inital-factor nil)
     (insert "* Rezept 1
Die (200g Nuts) mahlen.
Nuts haben einen hohen Protain gehalt.
Für die Sauce brauchen wir:
- (200g Nuts)
* Rezept 2
- (200g Flour)")
     (goto-char (point-min))
     (should (equal (list "Rezept 1" nil
                          'org-shoplist--recipe-read-ings-current
			  (list (org-shoplist-ing-create "200g" "Nuts")
				(org-shoplist-ing-create "200g" "Nuts")))
		    (org-shoplist-recipe-read 'org-shoplist--recipe-read-ings-current))))))

(ert-deftest org-shoplist-test/recipe-read-aggregate-100-same-ingredients ()
  "Read a recipe with 100 ing to see performance of regex."
  (org-shoplist-test-test-in-org-buffer
   (lambda ()
     (setq org-shoplist-inital-factor nil)
     (insert-file-contents "./file/recipe-with-100-ing.org")
     (goto-char (point-min))
     (should (equal (list "Recipe 1" nil
                          'org-shoplist--recipe-read-ings-current
			  (list (org-shoplist-ing-create "100g" "Nuts")))
		    (org-shoplist-recipe-read 'org-shoplist--recipe-read-ings-current t))))))

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
		    (should-error (org-shoplist-recipe-read 'org-shoplist--recipe-read-ings-current)))))))

(ert-deftest org-shoplist-test/recipe-read-three-ings-two-broken ()
  "Read broken ings as normal when reading recipes."
  (org-shoplist-test-test-in-org-buffer
   (lambda ()
     (setq org-shoplist-inital-factor nil)
     (insert "* Rezept 1
Die (100g Nuts) (200ml
 Milk) (100g
Salad) mahlen.")
     (goto-char (point-min))
     (should (equal (org-shoplist-recipe-create "Rezept 1" nil
                             'org-shoplist--recipe-read-ings-current
			     (list (org-shoplist-ing-create "100g" "Nuts" " ")
                                   (org-shoplist-ing-create "200ml" "Milk" "\n ")
                                   (org-shoplist-ing-create "100g" "Salad" "\n")))
		    (org-shoplist-recipe-read 'org-shoplist--recipe-read-ings-current))))))

(ert-deftest org-shoplist-test/recipe-*-nil-nil ()
  "From nothing comes nothing."
  (should (eq nil (org-shoplist-recipe-* nil nil))))

(ert-deftest org-shoplist-test/recipe-*-factor-nil ()
  "Return the same recipe when factor is nil."
  (should (equal (org-shoplist-recipe-create "Test" nil nil (list (org-shoplist-ing-create "400g" "Nuts")))
		 (org-shoplist-recipe-* (org-shoplist-recipe-create "Test" nil nil (list (org-shoplist-ing-create "400g" "Nuts"))) nil))))

(ert-deftest org-shoplist-test/recipe-*-factor-2-recipe-factor-nil ()
  "Return the same recipe when factor is nil."
  (should (equal (org-shoplist-recipe-create "Test" nil nil (list (org-shoplist-ing-create "400g" "Nuts")))
		 (org-shoplist-recipe-* (org-shoplist-recipe-create "Test" nil nil (list (org-shoplist-ing-create "200g" "Nuts"))) 2))))

(ert-deftest org-shoplist-test/recipe-*-recipe-nil-factor-2 ()
  "From nothing comes nothing."
  (should (eq nil (org-shoplist-recipe-* nil 2))))

(ert-deftest org-shoplist-test/recipe-*-apply-factor-to-recipe-factor ()
  "Return recipe with adjusted internal factor."
  (should (equal (org-shoplist-recipe-create "Test" 6 nil (list (org-shoplist-ing-create "200g" "Nuts")))
		 (org-shoplist-recipe-* (org-shoplist-recipe-create "Test" 3 nil (list (org-shoplist-ing-create "100g" "Nuts"))) 2))))

(ert-deftest org-shoplist-test/recipe-*-2-200g ()
  "Multiply all ingredients of recipe by given factor. "
  (should (equal (org-shoplist-recipe-create "Test" nil nil (list (org-shoplist-ing-create "400g" "Nuts")))
		 (org-shoplist-recipe-* (org-shoplist-recipe-create "Test" nil nil (list (org-shoplist-ing-create "200g" "Nuts"))) 2))))

(ert-deftest org-shoplist-test/recipe-*-2/3-200g ()
  "Multiply all ingredients of recipe by given factor. "
  (should (equal (org-shoplist-recipe-create "Test" nil nil (list (org-shoplist-ing-create "133g" "Nuts")))
		 (org-shoplist-recipe-* (org-shoplist-recipe-create "Test" nil nil (list (org-shoplist-ing-create "200g" "Nuts"))) (/ 2.0 3)))))

(ert-deftest org-shoplist-test/recipe-*-2/3-200g ()
  "Multiply all ingredients of recipe by given factor. "
  (should (equal (org-shoplist-recipe-create "Test" nil
                          nil
			  (list (org-shoplist-ing-create "200g" "Nuts")
                                (org-shoplist-ing-create "400ml" "Milk")))
		 (org-shoplist-recipe-* (org-shoplist-recipe-create "Test" nil
                              nil
			      (list (org-shoplist-ing-create "100g" "Nuts")
                                    (org-shoplist-ing-create "200ml" "Milk")))
		     2))))

(ert-deftest org-shoplist-test/recipe-explict-keyword ()
  "Only read the explict marked org-headings."
  (org-shoplist-test-test-in-org-buffer
   (lambda ()
     (setq org-shoplist-inital-factor nil)
     (insert "* " (org-shoplist--search-type-keyword-value) " Rezept 2
- (200g Nuts)
** More ingredients
- (200g Nuts)")
     (goto-char (point-min))
     (should (equal (org-shoplist-recipe-create "Rezept 2" nil 'org-shoplist--recipe-read-ings-keyword-tree
                             (list (org-shoplist-ing-create "200g" "Nuts")))
		    (org-shoplist-recipe-read 'org-shoplist--recipe-read-ings-keyword-tree))))))

(ert-deftest org-shoplist-test/factor-up-buffer-empty ()
  (org-shoplist-test-test-in-org-buffer
   (lambda ()
     (should (equal '(user-error "Recipe not found")
		    (should-error (org-shoplist-factor-up)))))))

(ert-deftest org-shoplist-test/factor-up-ones-header-no-property ()
  (org-shoplist-test-test-in-org-buffer
   (lambda ()
     (setq org-shoplist-inital-factor nil)
     (insert "* Test Header
- (200g nuts)")
     (goto-char (point-min))
     (should (equal (list 'user-error (format "Property %s not defined" org-shoplist-factor-property-name))
		    (should-error (org-shoplist-factor-up)))))))

(ert-deftest org-shoplist-test/factor-up-ones-header-no-property-but-custom-var-set ()
  (org-shoplist-test-test-in-org-buffer
   (lambda ()
     (setq org-shoplist-inital-factor 1)
     (insert "* Test Header
- (200g Nuts)")
     (goto-char (point-min))
     (save-excursion (org-shoplist-factor-up))
     (should (equal (org-shoplist-recipe-create "Test Header" 2 'org-shoplist--recipe-read-ings-tree
                             (list (org-shoplist-ing-create "400g" "Nuts" " ")))
                    (org-shoplist-recipe-read 'org-shoplist--recipe-read-ings-tree))))))

(ert-deftest org-shoplist-test/factor-up-99-times-10000g-amount ()
  (org-shoplist-test-test-in-org-buffer
   (lambda ()
     (insert "* Test Header
  :PROPERTIES:
  :" org-shoplist-factor-property-name ":   1
  :END:
- (100g Nuts)")
     (goto-char (point-min))
     (dotimes (i 99)
       (org-shoplist-factor-up))
     (should (string= (buffer-string)
		      (concat "* Test Header
  :PROPERTIES:
  :" org-shoplist-factor-property-name ":   100
  :END:
- (10000g Nuts)"))))))

(ert-deftest org-shoplist-test/factor-up-and-down-99-times-same-amount ()
  (org-shoplist-test-test-in-org-buffer
   (lambda ()
     (insert "* Test Header
  :PROPERTIES:
  :" org-shoplist-factor-property-name ":   1
  :END:
- (100g Nuts)")
     (goto-char (point-min))
     (dotimes (i 99)
       (org-shoplist-factor-up))
     (dotimes (i 99)
       (org-shoplist-factor-down))
     (should (string= (buffer-string)
		      (concat "* Test Header
  :PROPERTIES:
  :" org-shoplist-factor-property-name ":   1
  :END:
- (100g Nuts)"))))))

(ert-deftest org-shoplist-test/factor-up-1-2-one-header-one-ingredient ())
(org-shoplist-test-test-in-org-buffer
 (lambda ()
   (insert "* Test Header
  :PROPERTIES:
  :" org-shoplist-factor-property-name ":   1
  :END:
- (200g Nuts)")
   (goto-char (point-min))
   (org-shoplist-factor-up)
   (should (string= (buffer-string)
                    (concat "* Test Header
  :PROPERTIES:
  :" org-shoplist-factor-property-name ":   2
  :END:
- (400g Nuts)")))
   (should (= (point) (point-min)))))

(ert-deftest org-shoplist-test/factor-up-2-3-one-header-one-ingredient ()
  (org-shoplist-test-test-in-org-buffer
   (lambda ()
     (insert "* Test Header
  :PROPERTIES:
  :" org-shoplist-factor-property-name ":   2
  :END:
- (200g Nuts)")
     (goto-char (point-min))
     (org-shoplist-factor-up)
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
     (org-shoplist-factor-up)
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
     (org-shoplist-factor-up)
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
     (org-shoplist-factor-up)
     (should (string= (buffer-string)
		      (concat "* Test Header
  :PROPERTIES:
  :" org-shoplist-factor-property-name ":   2
  :END:
- (2 Orange)")))
     (should (= (point) (point-min))))))

(ert-deftest org-shoplist-test/factor-up-1-2-one-header-with-one-nested-header-one-ing-each ()
  (org-shoplist-test-test-in-org-buffer
   (lambda ()
     (setq org-shoplist-inital-factor 1)
     (insert "* Test Header
:PROPERTIES:
:" org-shoplist-factor-property-name ":   1
:END:
- (100g Nuts)
** other Test-Header
- (100g Nuts)")
     (goto-char (point-min))
     (setq org-shoplist-explicit-keyword nil)
     (org-shoplist-factor-up)
     (should (string= (buffer-string)
		      (concat"* Test Header
:PROPERTIES:
:" org-shoplist-factor-property-name ":   2
:END:
- (200g Nuts)
** other Test-Header
:PROPERTIES:
:" org-shoplist-factor-property-name ":   2.0
:END:
- (200g Nuts)")))
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
     (org-shoplist-factor-up)
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
     (org-shoplist-factor-up)
     (should (string= (buffer-string)
		      (concat "* Test Header
  :PROPERTIES:
  :" org-shoplist-factor-property-name ":   2
  :END:
- (200g Nuts)
** other Test-Header
  :PROPERTIES:
  :" org-shoplist-factor-property-name ":   2.0
  :END:
- (200g Nuts)")))
     (should (= (point) (point-min))))))

(ert-deftest org-shoplist-test/factor-up-1-2-deep-nested-headers-with-factor-prop-test-proportional ()
  (org-shoplist-test-test-in-org-buffer
   (lambda ()
     (setq org-shoplist-explicit-keyword nil)
     (insert "* Test Header
  :PROPERTIES:
  :" org-shoplist-factor-property-name ":   1
  :END:
- (100g Nuts)
** other Test-Header
  :PROPERTIES:
  :" org-shoplist-factor-property-name ":   2
  :END:
- (350g Salad)
*** another Test-Header
  :PROPERTIES:
  :" org-shoplist-factor-property-name ":   3
  :END:
- (300ml Cream)")
     (goto-char (point-min))
     (setq org-shoplist-explicit-keyword nil)
     (org-shoplist-factor-up)
     (should (string= (buffer-string)
		      (concat "* Test Header
  :PROPERTIES:
  :" org-shoplist-factor-property-name ":   2
  :END:
- (200g Nuts)
** other Test-Header
  :PROPERTIES:
  :" org-shoplist-factor-property-name ":   4.0
  :END:
- (700g Salad)
*** another Test-Header
  :PROPERTIES:
  :" org-shoplist-factor-property-name ":   6.0
  :END:
- (600ml Cream)")))
     (should (= (point) (point-min))))))

(ert-deftest org-shoplist-test/factor-up-twice-deep-nested-headers-with-factor-prop-test-proportional ()
  (org-shoplist-test-test-in-org-buffer
   (lambda ()
     (setq org-shoplist-explicit-keyword nil)
     (insert "* Test Header
  :PROPERTIES:
  :" org-shoplist-factor-property-name ":   1
  :END:
- (100g Nuts)
** other Test-Header
  :PROPERTIES:
  :" org-shoplist-factor-property-name ":   2
  :END:
- (350g Salad)
*** another Test-Header
  :PROPERTIES:
  :" org-shoplist-factor-property-name ":   3
  :END:
- (300ml Cream)")
     (goto-char (point-min))
     (setq org-shoplist-explicit-keyword nil)
     (org-shoplist-factor-up)
     (org-shoplist-factor-up)
     (should (string= (buffer-string)
		      (concat "* Test Header
  :PROPERTIES:
  :" org-shoplist-factor-property-name ":   3
  :END:
- (300g Nuts)
** other Test-Header
  :PROPERTIES:
  :" org-shoplist-factor-property-name ":   6.0
  :END:
- (1050g Salad)
*** another Test-Header
  :PROPERTIES:
  :" org-shoplist-factor-property-name ":   9.0
  :END:
- (900ml Cream)")))
     (should (= (point) (point-min))))))

(ert-deftest org-shoplist-test/factor-up-twice-nested-header-with-lower-factor-than-upper ()
  (org-shoplist-test-test-in-org-buffer
   (lambda ()
     (setq org-shoplist-explicit-keyword nil)
     (insert "* Test Header
  :PROPERTIES:
  :" org-shoplist-factor-property-name ":   2
  :END:
- (100g Nuts)
** other Test-Header
  :PROPERTIES:
  :" org-shoplist-factor-property-name ":   1
  :END:
- (350g Salad)")
     (goto-char (point-min))
     (setq org-shoplist-explicit-keyword nil)
     (org-shoplist-factor-up)
     (should (string= (buffer-string)
		      (concat "* Test Header
  :PROPERTIES:
  :" org-shoplist-factor-property-name ":   3
  :END:
- (150g Nuts)
** other Test-Header
  :PROPERTIES:
  :" org-shoplist-factor-property-name ":   1.5
  :END:
- (525g Salad)")))
     (should (= (point) (point-min))))))

;;; org-shoplist-recipe-test.el ends here
