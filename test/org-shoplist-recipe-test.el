;;; org-shoplist-recipe-test.el --- Tests for org-shoplist
;;; Commentary:
;; Tests the data structures and functions of org-shoplist
;;; Code:
(ert-deftest org-shoplist-test/feeling-better? ()
  "Checks if it's a good day to program."
  (should (= 1 1)))

(ert-deftest org-shoplist-test/recipe-create-nil ()
  "Should error when passing no name for recipe."
  (should (equal '(error "Invalid name for recipe: ’’")
		 (should-error (org-shoplist-recipe-create nil)))))

(ert-deftest org-shoplist-test/recipe-create-empty-string-name ()
  "Should error when passing no name for recipe."
  (should (equal '(error "Invalid name for recipe: ’’")
		 (should-error (org-shoplist-recipe-create "")))))


(ert-deftest org-shoplist-test/recipe-create-normal-name-ing-nil ()
  "Create a recipe without ingredients."
  (should (equal '("Nut Salat") (org-shoplist-recipe-create "Nut Salat"))))

(ert-deftest org-shoplist-test/recipe-create-normal-name-one-ing ()
  "Create a recipe with one ingredient."
  (should (equal '("Nut Salat" (("Nuts" (* 100 (var g var-g)))))
		 (org-shoplist-recipe-create "Nut Salat" (org-shoplist-ing-create "100g" "Nuts")))))

(ert-deftest org-shoplist-test/recipe-create-normal-name-two-ing ()
  "Create a recipe with two ingredients."
  (should (equal '("Nut Salat"
		   (("Nuts" (* 100 (var g var-g)))
		    ("Nuts" (* 100 (var g var-g)))))
		 (org-shoplist-recipe-create "Nut Salat"
				 (org-shoplist-ing-create "100g" "Nuts")
				 (org-shoplist-ing-create "100g" "Nuts")))))

(ert-deftest org-shoplist-test/recipe-create-normal-name-two-diff-ing ()
  "Create a recipe with two different ingredients."
  (should (equal '("Nut Salat"
		   (("Nuts" (* 100 (var g var-g)))
		    ("Salat" (* 200 (var g var-g)))))
		 (org-shoplist-recipe-create "Nut Salat"
				 (org-shoplist-ing-create "100g" "Nuts")
				 (org-shoplist-ing-create "200g" "Salat")))))

(ert-deftest org-shoplist-test/recipe-create-normal-name-three-diff-ing ()
  "Create a recipe with three different ingredients."
  (should (equal '("Nut Salat"
		   (("Nuts" (* 100 (var g var-g)))
		    ("Salat" (* 200 (var g var-g)))
		    ("Pepper" (* 1 (var tsp var-tsp)))))
		 (org-shoplist-recipe-create "Nut Salat"
				 (org-shoplist-ing-create "100g" "Nuts")
				 (org-shoplist-ing-create "200g" "Salat")
				 (org-shoplist-ing-create "1tsp" "Pepper")))))

(ert-deftest org-shoplist-test/recipe-create-passing-list-of-ings ()
  "Create a recipe with by passing a list of ingredients."
  (should (equal '("Nut Salat"
		   (("Nuts" (* 100 (var g var-g)))
		    ("Salat" (* 200 (var g var-g)))
		    ("Pepper" (* 1 (var tsp var-tsp)))))
		 (org-shoplist-recipe-create "Nut Salat"
				 (list
				  (org-shoplist-ing-create "100g" "Nuts")
				  (org-shoplist-ing-create "200g" "Salat")
				  (org-shoplist-ing-create "1tsp" "Pepper"))))))

(ert-deftest org-shoplist-test/recipe-read-empty-buffer ()
  "Throw an error when buffer is empty."
  (org-shoplist-test-test-in-buffer
   (lambda ()
     (goto-char (point-min)) ;;that buffer don't get terminated
     (should (equal '(error "Not at beginning of recipe")
		    (should-error (org-shoplist-recipe-read))))
     (should (= (point) (point-min))))))

(ert-deftest org-shoplist-test/recipe-read-no-name ()
  "Read a recipe with no ingredients."
  (org-shoplist-test-test-in-buffer
   (lambda ()
     (insert "* ")
     (goto-char (point-min))
     (should (equal '(error "Invalid name for recipe: ’’")
		    (should-error (org-shoplist-recipe-read))))
     (should (= (point) (point-min))))))

(ert-deftest org-shoplist-test/recipe-read-no-ings ()
  "Read a recipe with no ingredients."
  (org-shoplist-test-test-in-buffer
   (lambda ()
     (insert "* Test")
     (goto-char (point-min))
     (should (equal '("Test") (org-shoplist-recipe-read)))
     (should (= (point) (point-min))))))

(ert-deftest org-shoplist-test/-recipe-read-all-ing ()
  "Read all ingredients of recipe."
  (org-shoplist-test-test-in-buffer
   (lambda ()
     (insert "* Test
- (200g Nuts) mahlen")
     (goto-char (point-min))
     (should (equal '(("Nuts" (* 200 (var g var-g))))
		    (org-shoplist--recipe-read-all-ing "*")))
     (should (= 8 (point))))))

(ert-deftest org-shoplist-test/recipe-read-all-two-ing ()
  "Read all ingredients of recipe."
  (org-shoplist-test-test-in-buffer
   (lambda ()
     (insert "* Test
- (200g Nuts) mahlen
- (200g Nuts) mahlen")
     (goto-char (point-min))
     (should (equal '(("Nuts" (* 200 (var g var-g)))
		      ("Nuts" (* 200 (var g var-g))))
		    (org-shoplist--recipe-read-all-ing "*")))
     (should (= 8 (point))))))


(ert-deftest org-shoplist-test/recipe-read-one-ing ()
  "Read a recipe with one full ingredient.
Full means with unit, amount and ing-name."
  (org-shoplist-test-test-in-buffer
   (lambda ()
     (insert "* Test
- (200g Nuts) mahlen")
     (goto-char (point-min))
     (should (equal '("Test" (("Nuts" (* 200 (var g var-g)))))
		    (org-shoplist-recipe-read)))
     (should (= (point) (point-min))))))

(ert-deftest org-shoplist-test/recipe-read-two-ing ()
  "Read a recipe with two full ingredients.
Full means with unit, amount and ing-name."
  (org-shoplist-test-test-in-buffer
   (lambda ()
     (insert "* Test
- (200g Nuts) mahlen
- (200g Nuts) mahlen")
     (goto-char (point-min))
     (should (equal '("Test"
		      (("Nuts" (* 200 (var g var-g)))
		       ("Nuts" (* 200 (var g var-g)))))
		    (org-shoplist-recipe-read)))
     (should (= (point) (point-min))))))

(ert-deftest org-shoplist-test/recipe-read-three-ing ()
  "Read a recipe with three full ingredients.
Full means with unit, amount and ing-name."
  (org-shoplist-test-test-in-buffer
   (lambda ()
     (insert "* Test
- (200g Nuts) mahlen
- (200g Nuts) mahlen
- (200g Nuts) mahlen")
     (goto-char (point-min))
     (should (equal '("Test"
		      (("Nuts" (* 200 (var g var-g)))
		       ("Nuts" (* 200 (var g var-g)))
		       ("Nuts" (* 200 (var g var-g)))))
		    (org-shoplist-recipe-read)))
     (should (= (point) (point-min))))))

(ert-deftest org-shoplist-test/recipe-read-two-ing-with-star-trash-inbetween ()
  "Read a recipe with two full ingredients but trash inbetween.
Full means with unit, amount and ing-name.
Trash means any text that contains no ingredient."
  (org-shoplist-test-test-in-buffer
   (lambda ()
     (insert "* Test
- (200g Nuts) mahlen
*Sauce:*
- (200g Nuts) mahlen")
     (goto-char (point-min))
     (should (equal '("Test"
		      (("Nuts" (* 200 (var g var-g)))
		       ("Nuts" (* 200 (var g var-g)))))
		    (org-shoplist-recipe-read)))
     (should (= (point) (point-min))))))

(ert-deftest org-shoplist-test/recipe-read-two-ing-with-trash-inbetween ()
  "Read a recipe with two full ingredients but trash inbetween.
Full means with unit, amount and ing-name.
Trash means any text that contains no ingredient."
  (org-shoplist-test-test-in-buffer
   (lambda ()
     (insert "* Test
- (200g Nuts) mahlen
Achtung: doppelt!
- (200g Nuts) mahlen")
     (goto-char (point-min))
     (should (equal '("Test"
		      (("Nuts" (* 200 (var g var-g)))
		       ("Nuts" (* 200 (var g var-g)))))
		    (org-shoplist-recipe-read)))
     (should (= (point) (point-min))))))

(ert-deftest org-shoplist-test/recipe-read-all-ing-between-two-headers ()
  "Read a recipe with two full ingredients.
Full means with unit, amount and ing-name."
  (org-shoplist-test-test-in-buffer
   (lambda ()
     (insert "* Rezept 1
Die (200g Nuts) mahlen.
Nuts haben einen hohen Protain gehalt.
Für die Sauce brauchen wir:
- (200g Nuts)
* Rezept 2")
     (goto-char (point-min))
     (should (equal '("Rezept 1"
		      (("Nuts" (* 200 (var g var-g)))
		       ("Nuts" (* 200 (var g var-g)))))
		    (org-shoplist-recipe-read)))
     (should (= (point) (point-min))))))

(ert-deftest org-shoplist-test/recipe-read-all-ing-between-two-nested-headers ()
  "Read a recipe with two full ingredients.
Full means with unit, amount and ing-name."
  (org-shoplist-test-test-in-buffer
   (lambda ()
     (insert "* Rezept 1
Die (200g Nuts) mahlen.
** Nuts information
Nuts haben einen hohen Protain gehalt.
Für die Sauce brauchen wir:
- (200g Nuts)
* Rezept 2")
     (goto-char (point-min))
     (should (equal '("Rezept 1"
		      (("Nuts" (* 200 (var g var-g)))
		       ("Nuts" (* 200 (var g var-g)))))
		    (org-shoplist-recipe-read)))
     (should (= (point) (point-min))))))


;;; org-shoplist-recipe-test.el ends here
