;;; org-shoplist-ing-test.el --- Tests for org-shoplist
;;; Commentary:
;; Tests the data structures and functions of org-shoplist
;;; Code:
(ert-deftest org-shoplist-test/feeling-better? ()
    "Checks if it's a good day to program."
  (should (= 1 1)))


(ert-deftest org-shoplist-test/ing-create-nil-nil ()
  (should (equal '(error "Invalid ‘NAME’ for ingredient")
		 (should-error (org-shoplist-ing-create nil nil)))))

(ert-deftest org-shoplist-test/ing-create-normal ()
  (should (equal '("Nuts" "100 g") (org-shoplist-ing-create "100g" "Nuts"))))

(ert-deftest org-shoplist-test/ing-create-when-amount-nil ()
  (should (equal '(error "Invalid ‘AMOUNT’(asdf) for ingredient")
		 (should-error (org-shoplist-ing-create "asdf" "Nuts")))))

(ert-deftest org-shoplist-test/ing-create-when-amount-invalid-number ()
  (should (equal '(error "Invalid ‘AMOUNT’(asdf) for ingredient")
		 (should-error (org-shoplist-ing-create "asdf" "Nuts")))))

(ert-deftest org-shoplist-test/ing-create-when-amount-true-number ()
  (should (equal '("Nuts" "100") (org-shoplist-ing-create 100 "Nuts"))))

(ert-deftest org-shoplist-test/ing-create-when-name-nil ()
  (should (equal '(error "Invalid ‘NAME’ for ingredient")
		 (should-error (org-shoplist-ing-create "100g" nil)))))

(ert-deftest org-shoplist-test/ing-create-when-custom-unit ()
  (should (equal '("Nuts" "100 foo") (org-shoplist-ing-create "100foo" "Nuts"))))

(ert-deftest org-shoplist-test/ing-create-when-unit-nil ()
  (should (equal '("Nuts" "100") (org-shoplist-ing-create 100 "Nuts"))))

(ert-deftest org-shoplist-test/ing-create-amount-simple-sequence ()
  (should (equal '(error "Invalid ‘AMOUNT’((* 100 (var g var-g))) for ingredient")
		 (should-error (org-shoplist-ing-create '(* 100 (var g var-g)) "Nuts")))))

(ert-deftest org-shoplist-test/ing-create-when-amount-is-calc-expr ()
  (should (equal '(error "Invalid ‘AMOUNT’(100g+200kg) for ingredient")
		 (should-error (org-shoplist-ing-create "100g+200kg" "Nuts")))))

(ert-deftest org-shoplist-test/ing-create-dont-change-match-data ()
  "Match data shouldn't be changed by call."
  (let ((data-before (match-data))
	(data-after nil))
    (org-shoplist-ing-create "100g" "Nuts")
    (setq data-after (match-data))
    (should (equal data-before data-after))))

(ert-deftest org-shoplist-test/ing-amount-amount-with-unit ()
    (should (string= "100g" (org-shoplist-ing-amount '(org-shoplist-ing-create "100g" "Nuts")))))

(ert-deftest org-shoplist-test/ing-amount-when-no-unit ()
  (should (string= "100" (org-shoplist-ing-amount (org-shoplist-ing-create "100" "Nuts")))))

(ert-deftest org-shoplist-test/ing-unit-amount-with-unit ()
  (should (string= "g" (org-shoplist-ing-unit (org-shoplist-ing-create "100g" "Nuts")))))

(ert-deftest org-shoplist-test/ing-unit-amount-number ()
  (should (eq nil (org-shoplist-ing-unit (org-shoplist-ing-create "100" "Nuts")))))

(ert-deftest org-shoplist-test/ing-read-str-nil ()
  "Return nil when nil is passed as `STR'."
  (should (eq nil (org-shoplist-ing-read nil))))

(ert-deftest org-shoplist-test/ing-read-empty-str ()
  "Return nil when empty string is passed."
  (should (eq nil (org-shoplist-ing-read ""))))

(ert-deftest org-shoplist-test/ing-read-str-only-ing ()
  "Return one ingredient-structure when str contains only a ing."
  (should (equal (list (org-shoplist-ing-create "100g" "Nuts")) (org-shoplist-ing-read "(100g Nuts)"))))

(ert-deftest org-shoplist-test/ing-read-str-only-trash ()
  "Return nil when str is not empty-string but contains only trash."
  (should (equal nil (org-shoplist-ing-read "This is trash"))))

(ert-deftest org-shoplist-test/ing-read-str-trash-with-one-ing ()
  "Return one ingredient-structure when str contains one ingredient"
  (should (equal (list (org-shoplist-ing-create "100g" "Nuts")) (org-shoplist-ing-read "This (100g Nuts) is trash"))))

(ert-deftest org-shoplist-test/ing-read-str-two-ings ()
  "Return a list with two ingredient-structures when str contains two ingredient"
  (should (equal (list
		  (org-shoplist-ing-create "100g" "Nuts")
		  (org-shoplist-ing-create "100ml" "Milk"))
		 (org-shoplist-ing-read "(100g Nuts)(100ml Milk)"))))

(ert-deftest org-shoplist-test/ing-read-str-two-ings-with-trash ()
  "Return a list with two ingredient-structures when str contains two ingredient"
  (should (equal (list
		  (org-shoplist-ing-create "100g" "Nuts")
		  (org-shoplist-ing-create "100ml" "Milk"))
		 (org-shoplist-ing-read "This is (100g Nuts) much (100ml Milk) trash."))))

(ert-deftest org-shoplist-test/ing-read-str-three-ings ()
  "Return a list with three ingredient-structures when str contains thee ingredient"
  (should (equal (list
		  (org-shoplist-ing-create "100g" "Nuts")
		  (org-shoplist-ing-create "100ml" "Milk")
		  (org-shoplist-ing-create "100kg" "Flour"))
		 (org-shoplist-ing-read "(100g Nuts)(100ml Milk)(100kg Flour)"))))

(ert-deftest org-shoplist-test/ing-read-str-four-ings ()
  "Return a list with four ingredient-structures when str contains four ingredient"
  (should (equal (list
		  (org-shoplist-ing-create "100g" "Nuts")
		  (org-shoplist-ing-create "100ml" "Milk")
		  (org-shoplist-ing-create "100kg" "Flour")
		  (org-shoplist-ing-create "1" "Egg"))
		 (org-shoplist-ing-read "(100g Nuts)(100ml Milk)(100kg Flour)(1 Egg)"))))

(ert-deftest org-shoplist-test/ing-read-no-pars-read-at-point ()
  "Parse line where point is at when no parameters passed"
  (org-shoplist-test-test-in-buffer
   (lambda ()
     (insert "(100g Nuts)")
     (should (equal '(("Nuts" "100 g"))
		    (org-shoplist-ing-read))))))

(ert-deftest org-shoplist-test/ing-read-when-previous-search-no-ings ()
  "Parse line where point is at when no parameters passed"
  (org-shoplist-test-test-in-buffer
   (lambda ()
     (insert "Nuts")
     (goto-char (point-min))
     (search-forward-regexp "s" nil t 1)
     (should (eq nil
		 (org-shoplist-ing-read))))))

(ert-deftest org-shoplist-test/ing-read-when-previous-search-with-ings ()
  "Parse line where point is at when no parameters passed"
  (org-shoplist-test-test-in-buffer
   (lambda ()
     (insert "(100g Nuts)")
     (goto-char (point-min))
     (search-forward-regexp "s" nil t 1)
     (should (equal (list (org-shoplist-ing-create "100g" "Nuts"))
		    (org-shoplist-ing-read))))))

(ert-deftest org-shoplist-test/ing-multiply-ing-nil ()
  "Return error when passing invalid ingredients."
  (should (equal '(error "Invalid ‘NAME’ for ingredient")
		 (should-error (org-shoplist-ing-* nil 2)))))

(ert-deftest org-shoplist-test/ing-multiply-by-0-with-out-unit ()
  "Return nil when amount is 0."
  (should (equal nil
		 (org-shoplist-ing-* (org-shoplist-ing-create 100 "Nuts") 0))))

(ert-deftest org-shoplist-test/ing-multiply-by-0-with-unit ()
  "Return ing with amount 0g when multiplying by 0."
  (should (equal nil
		 (org-shoplist-ing-* (org-shoplist-ing-create "100g" "Nuts") 0))))

(ert-deftest org-shoplist-test/ing-multiply-by-1 ()
  "Return same ing when multiplying by 1."
  (should (equal (org-shoplist-ing-create "100g" "Nuts")
		 (org-shoplist-ing-* (org-shoplist-ing-create "100g" "Nuts") 1))))

(ert-deftest org-shoplist-test/ing-multiply-by-2 ()
  "Return ing with amount dobbelt multiplying by 2."
  (should (equal (org-shoplist-ing-create "200g" "Nuts")
		 (org-shoplist-ing-* (org-shoplist-ing-create "100g" "Nuts") 2))))

(ert-deftest org-shoplist-test/ing-+-nil ()
  "Add ing with nil return ing."
  (let ((ing (org-shoplist-ing-amount (org-shoplist-ing-create 100 "Nuts"))))
    (should (equal ing (org-shoplist-ing-+ ing nil)))))

(ert-deftest org-shoplist-test/ing-+-0 ()
  "Add ing with 0 return ing."
  (let ((ing (org-shoplist-ing-create 100 "Nuts")))
    (should (equal (org-shoplist-ing-amount ing) (org-shoplist-ing-+ ing 0)))))

(ert-deftest org-shoplist-test/ing-+-0g-unit-uncompatible ()
  "Add ing with 0 return ing."
  (let ((ing (org-shoplist-ing-create 100 "Nuts")))
    (should (equal (org-shoplist-ing-amount ing) (org-shoplist-ing-+ ing "0g")))))

(ert-deftest org-shoplist-test/ing-+-0g-compatible ()
  "Add ing with 0g return ing."
  (let ((ing (org-shoplist-ing-create "100g" "Nuts")))
    (should (equal (org-shoplist-ing-amount ing) (org-shoplist-ing-+ ing "0g")))))

(ert-deftest org-shoplist-test/ing-+-200g-200g ()
  "Add ing multiple amounts return ing with modified amount."
  (let ((ing (org-shoplist-ing-create "100g" "Nuts")))
    (should (equal (org-shoplist-ing-amount (org-shoplist-ing-create "500g" "Nuts"))
		   (org-shoplist-ing-+ ing "200g" "200g")))))

(ert-deftest org-shoplist-test/ing-+-200g+200g ()
  "Add ing with multiple amounts in one string return ing with modified amount."
  (let ((ing (org-shoplist-ing-create "100g" "Nuts")))
    (should (equal (org-shoplist-ing-amount (org-shoplist-ing-create "500g" "Nuts"))
		   (org-shoplist-ing-+ ing "200g+200g")))))

(ert-deftest org-shoplist-test/ing-+-999g-1kg ()
  "Add ing with multiple amounts in one string return ing with modified amount."
  (let ((ing (org-shoplist-ing-create "999g" "Nuts")))
    (should (equal (org-shoplist-ing-amount (org-shoplist-ing-create "1999g" "Nuts"))
		   (org-shoplist-ing-+ ing "1kg")))))

(ert-deftest org-shoplist-test/ing-+-999g-1ml ()
  "Trow error when trying to add grams with mililiters."
  (let ((ing (org-shoplist-ing-create "999g" "Nuts")))
    (should (equal '(error "Invalid ‘AMOUNT’(999 g + ml) for ingredient")
		   (should-error (org-shoplist-ing-+ ing "1ml"))))))

(ert-deftest org-shoplist-test/ing-+-ing ()
  "Add the amount of two ings togehter."
  (let ((ing (org-shoplist-ing-create "100g" "Nuts")))
    (should (equal (org-shoplist-ing-amount (org-shoplist-ing-create "200g" "Nuts"))
		   (org-shoplist-ing-+ ing ing)))))

(ert-deftest org-shoplist-test/ing-+-random ()
  "Trow error when trying to add something random."
  (should (equal '(error "Given ‘AMOUNT’(asdf) can’t be converted")
		 (should-error (org-shoplist-ing-+ "100g" 'asdf)))))

(ert-deftest org-shoplist-test/ing-+-random-str ()
  "Trow error when trying to add something random."
  (should (equal '(error "Invalid ‘AMOUNT’(100 g + asdf) for ingredient")
		 (should-error (org-shoplist-ing-+ "100g" "asdf")))))
;;; org-shoplist-ing-test.el ends here
