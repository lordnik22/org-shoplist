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
;;; org-shoplist-test.el ends here
