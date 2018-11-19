;;; org-shoplist-test.el --- Tests for org-shoplist
;;; Commentary:
;; Tests the data structures and functions of org-shoplist
;;; Code:
(ert-deftest org-shoplist-test/feeling-better? ()
  "Checks if it's a good day to program."
  (should (= 1 1)))

(ert-deftest org-shoplist-test/shoplist-create-not-even-nil ()
  "From nothing comes nothing."
  (should (eq nil
	      (org-shoplist-shoplist-create))))

(ert-deftest org-shoplist-test/shoplist-create-nil ()
  "From nothing comes nothing."
  (should (eq nil
	      (org-shoplist-shoplist-create nil))))

(ert-deftest org-shoplist-test/shoplist-create-with-one-recipe ()
  "Get a shopping list containing one recipe."
  (should (equal (list (calendar-current-date)
		       (list (org-shoplist-recipe-create "Applepie"
					     (org-shoplist-ing-create "200g" "Apple"))))
		 (org-shoplist-shoplist-create
		  (org-shoplist-recipe-create "Applepie"
				  (org-shoplist-ing-create "200g" "Apple"))))))

(ert-deftest org-shoplist-test/shoplist-create-with-two-recipes ()
  "Get a shopping list containing two recipe."
  (should (equal (list (calendar-current-date)
		       (list (org-shoplist-recipe-create "Applepie"
					     (org-shoplist-ing-create "200g" "Apple"))
			     (org-shoplist-recipe-create "Nut Salat"
					     (org-shoplist-ing-create "200g" "Nuts")
					     (org-shoplist-ing-create "100g" "Salat"))))
		 (org-shoplist-shoplist-create
		  (org-shoplist-recipe-create "Applepie"
				  (org-shoplist-ing-create "200g" "Apple"))
		  (org-shoplist-recipe-create "Nut Salat"
				  (org-shoplist-ing-create "200g" "Nuts")
				  (org-shoplist-ing-create "100g" "Salat"))))))

(ert-deftest org-shoplist-test/shoplist-create-with-list-of-two-recipes ()
  "Get a shopping list containing two recipe."
  (should (equal (list (calendar-current-date)
		       (list (org-shoplist-recipe-create "Applepie"
					     (org-shoplist-ing-create "200g" "Apple"))
			     (org-shoplist-recipe-create "Nut Salat"
					     (org-shoplist-ing-create "200g" "Nuts")
					     (org-shoplist-ing-create "100g" "Salat"))))
		 (apply 'org-shoplist-shoplist-create
		  (list (org-shoplist-recipe-create "Applepie"
					(org-shoplist-ing-create "200g" "Apple"))
			(org-shoplist-recipe-create "Nut Salat"
					(org-shoplist-ing-create "200g" "Nuts")
					(org-shoplist-ing-create "100g" "Salat")))))))

(ert-deftest org-shoplist-test/shoplist-creation-date-nil ()
  "From nothing comes nothing"
  (should (equal nil (org-shoplist-shoplist-creation-date nil))))

(ert-deftest org-shoplist-test/shoplist-creation-date-get-current-date ()
  "Get current date when passing a shoplist which got created when test is executed."
  (should (equal (calendar-current-date) (org-shoplist-shoplist-creation-date (org-shoplist-shoplist-create
					     (org-shoplist-recipe-create "Applepie"
							     (org-shoplist-ing-create "200g" "Apple")))))))

(ert-deftest org-shoplist-test/shoplist-creation-date-get-current-date ()
  "Get current date when passing a shoplist which got created when test is executed."
  (should (equal (calendar-current-date) (org-shoplist-shoplist-creation-date (org-shoplist-shoplist-create
					     (org-shoplist-recipe-create "Applepie"
							     (org-shoplist-ing-create "200g" "Apple")))))))

(ert-deftest org-shoplist-test/shoplist-recipes-nil ()
  "From nothing comes nothing"
  (should (equal nil (org-shoplist-shoplist-recipes nil))))

(ert-deftest org-shoplist-test/shoplist-recipes-shoplist-with-one-recipe ()
  "Get all recipes "
  (should (equal (list (org-shoplist-recipe-create "Applepie" (org-shoplist-ing-create "200g" "Apple")))
		 (org-shoplist-shoplist-recipes
		  (org-shoplist-shoplist-create
		   (org-shoplist-recipe-create "Applepie" (org-shoplist-ing-create "200g" "Apple")))))))

(ert-deftest org-shoplist-test/shoplist-recipes-shoplist-with-two-recipe ()
  "From nothing comes nothing"
  (should (equal (list (org-shoplist-recipe-create "Applepie" (org-shoplist-ing-create "200g" "Apple"))
		       (org-shoplist-recipe-create "Applepie" (org-shoplist-ing-create "200g" "Apple")))
		 (org-shoplist-shoplist-recipes
		  (org-shoplist-shoplist-create
		   (org-shoplist-recipe-create "Applepie" (org-shoplist-ing-create "200g" "Apple"))
		   (org-shoplist-recipe-create "Applepie" (org-shoplist-ing-create "200g" "Apple")))))))

(ert-deftest org-shoplist-test/shoplist-read-nil ()
  "From nothing comes nothing"
  (should (equal nil (org-shoplist-shoplist-read nil))))

(ert-deftest org-shoplist-test/recipe-read-one-marked-recipe ()
  "Read the recipe which is marked."
  (org-shoplist-test-test-in-org-buffer
   (lambda ()
     (insert "* " org-shoplist-keyword " Rezept 1" "
Die (200g Nuts) mahlen.
Nuts haben einen hohen Protain gehalt.
Für die Sauce brauchen wir:
- (200g Nuts)")
     (goto-char (point-min))
     (should (equal (org-shoplist-shoplist-create
		     (org-shoplist-recipe-create "Rezept 1"
				     (org-shoplist-ing-create "200g" "Nuts")
				     (org-shoplist-ing-create "200g" "Nuts")))
		    (org-shoplist-shoplist-read))))))

(ert-deftest org-shoplist-test/recipe-read-only-one-marked-recipe ()
  "Read the recipe which is marked."
  (org-shoplist-test-test-in-org-buffer
   (lambda ()
     (insert "* " org-shoplist-keyword " Rezept 1" "
Die (200g Nuts) mahlen.
Nuts haben einen hohen Protain gehalt.
Für die Sauce brauchen wir:
* Rezept 2
- (200g Nuts)")
     (goto-char (point-min))
     (should (equal (org-shoplist-shoplist-create
		     (org-shoplist-recipe-create "Rezept 1"
				     (org-shoplist-ing-create "200g" "Nuts")))
		    (org-shoplist-shoplist-read))))))

(ert-deftest org-shoplist-test/recipe-read-only-one-marked-recipe-after-non-marked ()
  "Read the recipe which is marked."
  (org-shoplist-test-test-in-org-buffer
   (lambda ()
     (insert "* Rezept 1
Die (200g Nuts) mahlen.
Nuts haben einen hohen Protain gehalt.
Für die Sauce brauchen wir:
* " org-shoplist-keyword " Rezept 2
- (200g Nuts)")
     (goto-char (point-min))
     (should (equal (org-shoplist-shoplist-create
		     (org-shoplist-recipe-create "Rezept 2"
				     (org-shoplist-ing-create "200g" "Nuts")))
		    (org-shoplist-shoplist-read))))))

(ert-deftest org-shoplist-test/recipe-read-two-marked-recipes ()
  "Read the marked recipes."
  (org-shoplist-test-test-in-org-buffer
   (lambda ()
     (insert "* " org-shoplist-keyword " Rezept 1" "
Die (200g Nuts) mahlen.
Nuts haben einen hohen Protain gehalt.
Für die Sauce brauchen wir:
* " org-shoplist-keyword " Rezept 2
- (200g Nuts)")
     (goto-char (point-min))
     (should (equal (org-shoplist-shoplist-create
		     (org-shoplist-recipe-create "Rezept 1"
				     (org-shoplist-ing-create "200g" "Nuts"))
		     (org-shoplist-recipe-create "Rezept 2"
				     (org-shoplist-ing-create "200g" "Nuts")))
		    (org-shoplist-shoplist-read))))))

(ert-deftest org-shoplist-test/recipe-read-three-diff-marked-recipes ()
  "Read the marked recipes."
  (org-shoplist-test-test-in-org-buffer
   (lambda ()
     (insert "* " org-shoplist-keyword " Rezept 1" "
Die (200ml Milk) mahlen.
Nuts haben einen hohen Protain gehalt.
Für die Sauce brauchen wir:
* " org-shoplist-keyword " Rezept 2
- (200g Nuts)
* " org-shoplist-keyword " Rezept 3
- (2tsp Butter)")
     (goto-char (point-min))
     (should (equal (org-shoplist-shoplist-create
		     (org-shoplist-recipe-create "Rezept 1"
				     (org-shoplist-ing-create "200ml" "Milk"))
		     (org-shoplist-recipe-create "Rezept 2"
				     (org-shoplist-ing-create "200g" "Nuts"))
		     (org-shoplist-recipe-create "Rezept 3"
				     (org-shoplist-ing-create "2tsp" "Butter")))
		    (org-shoplist-shoplist-read))))))

(ert-deftest org-shoplist-test/recipe-read-one-marked-inbetween-two-non-marked-recipes ()
  "Read the recipe which is marked."
  (org-shoplist-test-test-in-org-buffer
   (lambda ()
     (insert "* Rezept 1
Die (200ml Milk) mahlen.
Nuts haben einen hohen Protain gehalt.
Für die Sauce brauchen wir:
* " org-shoplist-keyword " Rezept 2
- (200g Nuts)
* Rezept 3
- (2tsp Butter)")
     (goto-char (point-min))
     (should (equal (org-shoplist-shoplist-create
		     (org-shoplist-recipe-create "Rezept 2" (org-shoplist-ing-create "200g" "Nuts")))
		    (org-shoplist-shoplist-read))))))
;;; org-shoplist-test.el ends here
