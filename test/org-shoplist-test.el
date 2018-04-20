;;; org-shoplist-test.el --- Tests for org-shoplist
;;; Commentary:
;; Tests the data structures and functions of org-shoplist
;;; Code:
(ert-deftest org-shoplist-test/feeling-better? ()
  "Checks if it's a good day to program."
  (should (= 1 1)))

(ert-deftest org-shoplist-test/shoplist-create-nil ()
  "Return a empty shoplist when passing nils."
  (should (equal '(nil ()) (org-shoplist-shoplist-create nil))))

(ert-deftest org-shoplist-test/shoplist-create-no-recipes-only-date ()
  "Return a shoplist with no recipes when passing a date."
  (should (equal '("01.01.2017" ()) (org-shoplist-shoplist-create "01.01.2017"))))

(ert-deftest org-shoplist-test/shoplist-create-one-recipe-and-date ()
  "Return a shoplist with one recipe when passing one recipe and a date."
  (should (equal '("01.01.2017" (("Nut Salad" (("Nuts" 100) ("Salad" 100))))) (org-shoplist-shoplist-create "01.01.2017"
						      (org-shoplist-recipe-create "Nut Salad"
								      (org-shoplist-ing-create 100 "Nuts")
								      (org-shoplist-ing-create 100 "Salad"))))))

(ert-deftest org-shoplist-test/shoplist-create-two-recipe-and-date ()
  "Return a shoplist with two recipes when two recipes and a date."
  (should (equal '("01.01.2017" (("Nut Salad" (("Nuts" 100) ("Salad" 100)))
				 ("Nut Salad2" (("Nuts" 80) ("Salad" 25)))))
		 (org-shoplist-shoplist-create "01.01.2017"
				   (org-shoplist-recipe-create "Nut Salad"
						   (org-shoplist-ing-create 100 "Nuts")
						   (org-shoplist-ing-create 100 "Salad"))
				   (org-shoplist-recipe-create "Nut Salad2"
						   (org-shoplist-ing-create 80 "Nuts")
						   (org-shoplist-ing-create 25 "Salad"))))))

(ert-deftest org-shoplist-test/shoplist-create-three-recipe-and-date ()
  "Return a shoplist with three recipes and a date."
  (should (equal '("01.01.2017" (("Nut Salad" (("Nuts" 100) ("Salad" 100)))
				 ("Nut Salad2" (("Nuts" 80) ("Salad" 25)))
				 ("Nut Salad3" (("Nuts" 40) ("Salad" 10)))))
		 (org-shoplist-shoplist-create "01.01.2017"
				   (org-shoplist-recipe-create "Nut Salad"
						   (org-shoplist-ing-create 100 "Nuts")
						   (org-shoplist-ing-create 100 "Salad"))
				   (org-shoplist-recipe-create "Nut Salad2"
						   (org-shoplist-ing-create 80 "Nuts")
						   (org-shoplist-ing-create 25 "Salad"))
				   (org-shoplist-recipe-create "Nut Salad3"
						   (org-shoplist-ing-create 40 "Nuts")
						   (org-shoplist-ing-create 10 "Salad"))))))

(ert-deftest org-shoplist-test/shoplist-shopdate-empty-shoplist ()
  "Return nil when getting shopdate of empty shoplist."
  (should (eq nil (org-shoplist-shoplist-shopdate (org-shoplist-shoplist-create nil)))))

(ert-deftest org-shoplist-test/shoplist-shopdate-with-shoplist-shopdate ()
  "Return shopdate when getting shopdate of shoplist with no recipes."
  (should (string= "01.01.2017" (org-shoplist-shoplist-shopdate (org-shoplist-shoplist-create "01.01.2017")))))

(ert-deftest org-shoplist-test/shoplist-recipes-empty-shoplist ()
  "Return nil when getting recipes of empty shoplist."
  (should (eq nil (org-shoplist-shoplist-recipes (org-shoplist-shoplist-create nil)))))

(ert-deftest org-shoplist-test/shoplist-recipes-one-recipe ()
  "Return a with one recipe."
  (should (equal '(("Nut Salad3" (("Nuts" 40) ("Salad" 10))))
		 (org-shoplist-shoplist-recipes
		  (org-shoplist-shoplist-create nil
				    (org-shoplist-recipe-create "Nut Salad3"
						    (org-shoplist-ing-create 40 "Nuts")
						    (org-shoplist-ing-create 10 "Salad")))))))

(ert-deftest org-shoplist-test/shoplist-read-in-empty-buffer ()
  "Get nil when reading a empty buffer."
  (org-shoplist-test-test-in-buffer
   (lambda ()
     (insert "")
     (goto-char (point-min))
     (should (eq nil (org-shoplist-shoplist-read)))
     (should (= (point) (point-min))))))

(ert-deftest org-shoplist-test/shoplist-read-one-recipe ()
  "Get a shoplist with one recipe when reading buffer with one recipe."
  (org-shoplist-test-test-in-buffer
   (lambda ()
     (insert "* Test")
     (goto-char (point-min))
     (should (equal '(nil (("Nut Salat"))) (org-shoplist-shoplist-read)))
     (should (= (point) (point-min))))))

;;; org-shoplist-test.el ends here
