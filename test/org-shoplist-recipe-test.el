;;; org-shoplist-test.el --- Tests for org-shoplist
;;; Commentary:
;; Tests the data structures and functions of org-shoplist
;;; Code:
(ert-deftest org-shoplist-test/feeling-better? ()
    "Checks if it's a good day to program."
  (should (= 1 1)))

(ert-deftest org-shoplist-test/recipe-create-nil ()
  "Should error when passing no name for recipe."
  (should-error (org-shoplist-recipe-create nil) :type '(error "Invalid name for recipe")))

(ert-deftest org-shoplist-test/recipe-create-normal-name-nil ()
  "Should error when passing normal name and nil ingredients."
  (should (equal '("Nut Salat") (org-shoplist-recipe-create "Nut Salat"))))

(ert-deftest org-shoplist-test/recipe-create-normal-name-one-ing ()
  "Should error when passing normal name and one ingredient."
  (should (equal '("Nut Salat" ("Nuts" (* 100 (var g var-g))))
		 (org-shoplist-recipe-create "Nut Salat" (org-shoplist-ing-create "100g" "Nuts")))))

(ert-deftest org-shoplist-test/recipe-create-normal-name-two-ing ()
  "Should error when passing normal name and two ingredients."
  (should (equal '("Nut Salat"
		   ("Nuts" (* 100 (var g var-g)))
		   ("Nuts" (* 100 (var g var-g))))
		 (org-shoplist-recipe-create "Nut Salat"
				 (org-shoplist-ing-create "100g" "Nuts")
				 (org-shoplist-ing-create "100g" "Nuts")))))

(ert-deftest org-shoplist-test/recipe-create-normal-name-two-diff-ing ()
  "Should error when passing normal name and two different ingredients."
  (should (equal '("Nut Salat"
		   ("Nuts" (* 100 (var g var-g)))
		   ("Salat" (* 200 (var g var-g))))
		 (org-shoplist-recipe-create "Nut Salat"
				 (org-shoplist-ing-create "100g" "Nuts")
				 (org-shoplist-ing-create "200g" "Salat")))))

(ert-deftest org-shoplist-test/recipe-create-normal-name-three-diff-ing ()
  "Should error when passing normal name and three different ingredients."
  (should (equal '("Nut Salat"
		   ("Nuts" (* 100 (var g var-g)))
		   ("Salat" (* 200 (var g var-g)))
		   ("Pepper" (* 1 (var tsp var-tsp))))
		 (org-shoplist-recipe-create "Nut Salat"
				 (org-shoplist-ing-create "100g" "Nuts")
				 (org-shoplist-ing-create "200g" "Salat")
				 (org-shoplist-ing-create "1tsp" "Pepper")))))

;;; org-shoplist-test.el ends here
