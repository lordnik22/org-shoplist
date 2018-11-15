;;; org-shoplist-test.el --- Tests for org-shoplist
;;; Commentary:
;; Tests the data structures and functions of org-shoplist
;;; Code:
(ert-deftest org-shoplist-test/feeling-better? ()
  "Checks if it's a good day to program."
  (should (= 1 1)))

(ert-deftest org-shoplist-test/shoplist-create-nil ()
  "From nothing comes nothing."
  (should (eq nil (org-shoplist-shoplist-create nil))))

(ert-deftest org-shoplist-test/shoplist-create-with-one-recipe ()
  "Get a shopping list containing one recipe."
  (should (equal (list (calendar-current-date)
		       (list (org-shoplist-recipe-create "Applepie" (org-shoplist-ing-create "200g" "Apple")))
		       (list (org-shoplist-ing-create "200g" "Apple")))
		 (org-shoplist-shoplist-create (org-shoplist-recipe-create "Applepie" (org-shoplist-ing-create "200g" "Apple"))))))

(ert-deftest org-shoplist-test/shoplist-create-with-two-recipes ()
  "Get a shopping list containing two recipe."
  (should (equal (list (calendar-current-date)
		       (list (org-shoplist-recipe-create "Applepie"
					     (org-shoplist-ing-create "200g" "Apple"))
			     (org-shoplist-recipe-create "Nut Salat"
					     (org-shoplist-ing-create "200g" "Nuts")
					     (org-shoplist-ing-create "100g" "Salat")))
		       (list (org-shoplist-ing-create "200g" "Apple")
			     (org-shoplist-ing-create "200g" "Nuts")
			     (org-shoplist-ing-create "100g" "Salat")))
		 (org-shoplist-shoplist-create (org-shoplist-recipe-create "Applepie"
						   (org-shoplist-ing-create "200g" "Apple"))
				   (org-shoplist-recipe-create "Nut Salat"
						   (org-shoplist-ing-create "200g" "Nuts")
						   (org-shoplist-ing-create "100g" "Salat"))))))


(ert-deftest org-shoplist-test/shoplist-create-aggregate-recipe-with-duplicate-ings ()
  "Get a shopping list containing one recipe that’s fully aggregated."
  (should (equal (list (calendar-current-date)
		       (list (org-shoplist-recipe-create "Applepie"
					     (org-shoplist-ing-create "200g" "Apple")
					     (org-shoplist-ing-create "200g" "Apple")))
		       (list (org-shoplist-ing-create "400g" "Apple")))
		 (org-shoplist-shoplist-create
		  (org-shoplist-recipe-create "Applepie"
				  (org-shoplist-ing-create "200g" "Apple")
				  (org-shoplist-ing-create "200g" "Apple"))))))

(ert-deftest org-shoplist-test/shoplist-create-aggregate-two-diff-recipes-with-same-ings ()
  "Get a shopping list containing one recipe that’s fully aggregated."
  (should (equal (list (calendar-current-date)
		       (list (org-shoplist-recipe-create "Applepie" (org-shoplist-ing-create "200ml" "Milk"))
			     (org-shoplist-recipe-create "Nut Salat" (org-shoplist-ing-create "200ml" "Milk")))
		       (list (org-shoplist-ing-create "4e-4 m^3" "Milk")))
		 (org-shoplist-shoplist-create
		  (org-shoplist-recipe-create "Applepie"
				  (org-shoplist-ing-create "200ml" "Milk"))
		  (org-shoplist-recipe-create "Nut Salat"
				  (org-shoplist-ing-create "200ml" "Milk"))))))

(ert-deftest org-shoplist-test/shoplist-creation-date-nil ()
  "From nothing comes nothing"
  (should (equal nil (org-shoplist-shoplist-creation-date nil))))

(ert-deftest org-shoplist-test/shoplist-creation-date-get-current-date ()
  "Get current date when passing a shoplist which got created when test is executed."
  (should (equal (calendar-current-date)
		 (org-shoplist-shoplist-creation-date
		  (org-shoplist-shoplist-create (list (org-shoplist-recipe-create "Applepie"
							  (org-shoplist-ing-create "200g" "Apple"))))))))

(ert-deftest org-shoplist-test/shoplist-creation-date-get-current-date ()
  "Get current date when passing a shoplist which got created when test is executed."
  (should (equal (calendar-current-date) (org-shoplist-shoplist-creation-date
					  (org-shoplist-shoplist-create (list
					     (org-shoplist-recipe-create "Applepie"
							     (org-shoplist-ing-create "200g" "Apple"))))))))

(ert-deftest org-shoplist-test/shoplist-recipes-nil ()
  "From nothing comes nothing"
  (should (equal nil (org-shoplist-shoplist-recipes nil))))

(ert-deftest org-shoplist-test/shoplist-recipes-shoplist-with-one-recipe ()
  "Get all recipes "
  (should (equal (list (org-shoplist-recipe-create "Applepie" (org-shoplist-ing-create "200g" "Apple")))
		 (org-shoplist-shoplist-recipes
		  (org-shoplist-shoplist-create (org-shoplist-recipe-create "Applepie" (org-shoplist-ing-create "200g" "Apple")))))))

(ert-deftest org-shoplist-test/shoplist-recipes-shoplist-with-two-recipe ()
  "From nothing comes nothing"
  (should (equal (list (org-shoplist-recipe-create "Applepie" (org-shoplist-ing-create "200g" "Apple"))
		       (org-shoplist-recipe-create "Applepie" (org-shoplist-ing-create "200g" "Apple")))
		 (org-shoplist-shoplist-recipes
		  (org-shoplist-shoplist-create (org-shoplist-recipe-create "Applepie" (org-shoplist-ing-create "200g" "Apple"))
				    (org-shoplist-recipe-create "Applepie" (org-shoplist-ing-create "200g" "Apple")))))))


(ert-deftest org-shoplist-test/shoplist-ings-nil ()
  "From nothing comes nothing"
  (should (equal nil (org-shoplist-shoplist-ings nil))))

(ert-deftest org-shoplist-test/shoplist-ings-shoplist-with-one-recipe ()
  "Get all recipes "
  (should (equal (list (org-shoplist-ing-create "200g" "Apple"))
		 (org-shoplist-shoplist-ings
		  (org-shoplist-shoplist-create (org-shoplist-recipe-create "Applepie" (org-shoplist-ing-create "200g" "Apple")))))))

(ert-deftest org-shoplist-test/shoplist-ings-shoplist-with-two-recipe-and-diff-ing ()
  "From nothing comes nothing"
  (should (equal (list (org-shoplist-ing-create "2e-4 m^3" "Cream") (org-shoplist-ing-create "200g" "Apple"))
		 (org-shoplist-shoplist-ings
		  (org-shoplist-shoplist-create (org-shoplist-recipe-create "Applepie" (org-shoplist-ing-create "200ml" "Cream"))
				    (org-shoplist-recipe-create "Applepie" (org-shoplist-ing-create "200g" "Apple")))))))

(ert-deftest org-shoplist-test/shoplist-ings-shoplist-with-two-recipe ()
  "From nothing comes nothing"
  (should (equal (list (org-shoplist-ing-create "400g" "Apple"))
		 (org-shoplist-shoplist-ings
		  (org-shoplist-shoplist-create (org-shoplist-recipe-create "Applepie" (org-shoplist-ing-create "200g" "Apple"))
				    (org-shoplist-recipe-create "Applepie" (org-shoplist-ing-create "200g" "Apple")))))))


(ert-deftest org-shoplist-test/shoplist-read-nil ()
  "From nothing comes nothing"
  (should (equal nil (org-shoplist-shoplist-read nil))))

(ert-deftest org-shoplist-test/shoplist-read-one-marked-recipe ()
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
		    (org-shoplist-shoplist-read nil))))))

(ert-deftest org-shoplist-test/shoplist-read-only-one-marked-recipe ()
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
     (should (equal (org-shoplist-shoplist-create (org-shoplist-recipe-create "Rezept 1" (org-shoplist-ing-create "200g" "Nuts")))
		    (org-shoplist-shoplist-read))))))

(ert-deftest org-shoplist-test/shoplist-read-only-one-marked-recipe-after-non-marked ()
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
     (should (equal (org-shoplist-shoplist-create (org-shoplist-recipe-create "Rezept 2"
						      (org-shoplist-ing-create "200g" "Nuts")))
		    (org-shoplist-shoplist-read))))))

(ert-deftest org-shoplist-test/shpoplist-read-two-marked-recipes ()
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
     (should (equal (org-shoplist-shoplist-create (org-shoplist-recipe-create "Rezept 1"
						      (org-shoplist-ing-create "200g" "Nuts"))
				      (org-shoplist-recipe-create "Rezept 2"
						      (org-shoplist-ing-create "200g" "Nuts")))
		    (org-shoplist-shoplist-read))))))

(ert-deftest org-shoplist-test/shoplist-read-three-diff-marked-recipes ()
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

(ert-deftest org-shoplist-test/shoplist-read-one-marked-inbetween-two-non-marked-recipes ()
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
     (should (equal (org-shoplist-shoplist-create (org-shoplist-recipe-create "Rezept 2" (org-shoplist-ing-create "200g" "Nuts")))
		    (org-shoplist-shoplist-read))))))

(ert-deftest org-shoplist-test/shoplist-read-aggregate-duplicate-ings-in-one-recipe ()
  "Read the recipe which is marked."
  (org-shoplist-test-test-in-org-buffer
   (lambda ()
     (insert "* " org-shoplist-keyword " Rezept 2
- (200g Nuts)
- (200g Nuts)")
     (goto-char (point-min))
     (should (equal (org-shoplist-shoplist-create (org-shoplist-recipe-create "Rezept 2" (org-shoplist-ing-create "400g" "Nuts")))
		    (org-shoplist-shoplist-read t))))))

(ert-deftest org-shoplist-test/shoplist-insert-400g-Nuts ()
  "Insert a simple shoplist in table format."
  (org-shoplist-test-test-in-org-buffer
   (lambda ()
     (goto-char (point-min))
     (org-shoplist-shoplist-insert (org-shoplist-shoplist-create (org-shoplist-recipe-create "Rezept 2" (org-shoplist-ing-create "400g" "Nuts"))))
     (should (equal (buffer-string) "|Nuts|400 g|")))))

(ert-deftest org-shoplist-test/shoplist-insert-400ml-Milk ()
  "Insert a simple shoplist in table format with a readable unit."
  (org-shoplist-test-test-in-org-buffer
   (lambda ()
     (goto-char (point-min))
     (org-shoplist-shoplist-insert (org-shoplist-shoplist-create (org-shoplist-recipe-create "Rezept 2" (org-shoplist-ing-create "400ml" "Milk"))))
     (should (equal (buffer-string) "|Milk|400 ml|")))))
;;; org-shoplist-test.el ends here
