;;; org-shoplist-test.el --- Tests for org-shoplist
;;; Commentary:
;; Tests the data structures and functions of org-shoplist
;;; Code:
(ert-deftest org-shoplist-test/feeling-better? ()
    "Checks if it's a good day to program."
  (should (= 1 1)))

(ert-deftest org-shoplist-test/recipe-create-nil ()
  "Should error when passing no name for recipe."
  (should-error (org-shoplist-recipe-create nil) :type '(error)))

(ert-deftest org-shoplist-test/recipe-create-normal-name-ing-nil ()
  "Create a recipe without ingredients."
  (should (equal '("Nut Salat") (org-shoplist-recipe-create "Nut Salat"))))

(ert-deftest org-shoplist-test/recipe-create-normal-name-one-ing ()
  "Create a recipe with one ingredient."
  (should (equal '("Nut Salat" ("Nuts" (* 100 (var g var-g))))
		 (org-shoplist-recipe-create "Nut Salat" (org-shoplist-ing-create "100g" "Nuts")))))

(ert-deftest org-shoplist-test/recipe-create-normal-name-two-ing ()
  "Create a recipe with two ingredients."
  (should (equal '("Nut Salat"
		   ("Nuts" (* 100 (var g var-g)))
		   ("Nuts" (* 100 (var g var-g))))
		 (org-shoplist-recipe-create "Nut Salat"
				 (org-shoplist-ing-create "100g" "Nuts")
				 (org-shoplist-ing-create "100g" "Nuts")))))

(ert-deftest org-shoplist-test/recipe-create-normal-name-two-diff-ing ()
  "Create a recipe with two different ingredients."
  (should (equal '("Nut Salat"
		   ("Nuts" (* 100 (var g var-g)))
		   ("Salat" (* 200 (var g var-g))))
		 (org-shoplist-recipe-create "Nut Salat"
				 (org-shoplist-ing-create "100g" "Nuts")
				 (org-shoplist-ing-create "200g" "Salat")))))

(ert-deftest org-shoplist-test/recipe-create-normal-name-three-diff-ing ()
  "Create a recipe with three different ingredients."
  (should (equal '("Nut Salat"
		   ("Nuts" (* 100 (var g var-g)))
		   ("Salat" (* 200 (var g var-g)))
		   ("Pepper" (* 1 (var tsp var-tsp))))
		 (org-shoplist-recipe-create "Nut Salat"
				 (org-shoplist-ing-create "100g" "Nuts")
				 (org-shoplist-ing-create "200g" "Salat")
				 (org-shoplist-ing-create "1tsp" "Pepper")))))

(ert-deftest org-shoplist-test/recipe-read-empty-buffer ()
  "Throw an error when buffer is empty."
  (org-shoplist-test-test-in-buffer
   (lambda ()
     (goto-char 0);;that buffer don't get terminated
     (should-error (org-shoplist-recipe-read) :type '(error)))))


(ert-deftest org-shoplist-test/empty-recipe-read ()
  "Read a recipe with no ingredients."
  (org-shoplist-test-test-in-buffer
   (lambda ()
     (insert "* Test")
     (goto-char (point-min))
     (should (equal '("Test") (org-shoplist-recipe-read))))))

(ert-deftest org-shoplist-test/recipe-read-one-liner ()
  "Read a recipe with one full ingredient.
Full means with unit, amount and ing-name."
  (org-shoplist-test-test-in-buffer
   (lambda ()
     (insert "* Test
- (200g Nuts) mahlen")
     (goto-char (point-min))
     (should (equal '("Test" ("Nuts" (* 200 (var g var-g)))) (org-shoplist-recipe-read))))))
;;; org-shoplist-test.el ends here
