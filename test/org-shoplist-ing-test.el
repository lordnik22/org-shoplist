;;; org-shoplist-ing-test.el --- Tests for org-shoplist
;;; Commentary:
;; Tests the data structures and functions of org-shoplist
;;; Code:
(ert-deftest org-shoplist-test/feeling-better? ()
    "Checks if it's a good day to program."
  (should (= 1 1)))

(ert-deftest org-shoplist-test/ing-create-nil-nil-nil ()
  (should (equal '(user-error "Invalid ‘NAME’(nil) for ingredient")
		 (should-error (org-shoplist-ing-create nil nil nil)))))

(ert-deftest org-shoplist-test/ing-create-100g-nil ()
  (should (equal '(user-error "Invalid ‘NAME’(nil) for ingredient")
		 (should-error (org-shoplist-ing-create "100g" nil)))))

(ert-deftest org-shoplist-test/ing-create-100g-Nuts-nil ()
  (should (string= org-shoplist-ing-default-separator (org-shoplist-ing-separator (org-shoplist-ing-create "100g" "Nuts" nil)))))

(ert-deftest org-shoplist-test/ing-create-100g-Nuts-newline ()
  (should (string= "\n" (org-shoplist-ing-separator (org-shoplist-ing-create "100g" "Nuts" "\n")))))

(ert-deftest org-shoplist-test/transform-amount-valid-test ()
  "Test a weide range of diff amounts"
  (should (string= "1" (org-shoplist--ing-transform-amount nil)))
  (should (string= "1" (org-shoplist--ing-transform-amount "0")))
  (should (string= "0g" (org-shoplist--ing-transform-amount "0g")))
  (should (string= "1" (org-shoplist--ing-transform-amount "1")))
  (should (string= "2" (org-shoplist--ing-transform-amount "2.0")))
  (should (string= "100g" (org-shoplist--ing-transform-amount "100g")))
  (should (string= "99999g" (org-shoplist--ing-transform-amount "99999g")))
  (should (string= "9tsp" (org-shoplist--ing-transform-amount "9tsp")))
  (should (string= "400g" (org-shoplist--ing-transform-amount "400g")))
  (should (string= "1ml" (org-shoplist--ing-transform-amount "1.0ml")))
  (should (string= "100gal" (org-shoplist--ing-transform-amount "100gal")))
  (should (string= "100gal" (org-shoplist--ing-transform-amount "100 gal")))
  (should (string= "2390m^3" (org-shoplist--ing-transform-amount "2390 m^3")))
  (should (string= "2.39m^3" (org-shoplist--ing-transform-amount "2390e-3 m^3")))
  (should (string= "0.0239m^3" (org-shoplist--ing-transform-amount "23.90e-3 m^3")))
  (should (string= "35" (org-shoplist--ing-transform-amount "12+23")))
  (should (string= "24" (org-shoplist--ing-transform-amount "12*2")))
  (should (string= "24mg" (org-shoplist--ing-transform-amount "12mg*2")))
  (should (string= "21pwerg" (org-shoplist--ing-transform-amount "21pwerg"))))

(ert-deftest org-shoplist-test/transform-amount-error-test ()
  "Test if error is thown when passing invalid amounts"
  (should (equal '(user-error "Invalid ‘AMOUNT’(df.df) for ingredient")
		  (should-error (org-shoplist--ing-transform-amount "df.df"))))
  (should (equal '(user-error "Invalid ‘AMOUNT’(1ml+1999g) for ingredient")
		 (should-error (org-shoplist--ing-transform-amount "1ml+1999g"))))
  (should (equal '(user-error "Invalid ‘AMOUNT’(1ml+1g) for ingredient")
		 (should-error (org-shoplist--ing-transform-amount "1ml+1g"))))
  (should (equal '(user-error "Invalid ‘AMOUNT’(df.df) for ingredient")
		 (should-error (org-shoplist-ing-create "df.df" "Nuts"))))
  (should (equal '(user-error "Invalid ‘AMOUNT’(-234) for ingredient")
		 (should-error (org-shoplist-ing-create "-234" "Nuts"))))
  (should (equal '(user-error "Invalid ‘AMOUNT’(a>1234) for ingredient")
		 (should-error (org-shoplist-ing-create "a>1234" "Nuts"))))
  (should (equal '(user-error "Invalid ‘AMOUNT’(..02 gal) for ingredient")
		 (should-error (org-shoplist-ing-create "..02 gal" "Nuts")))))

(ert-deftest org-shoplist-test/ing-create-test-diff-wrong-amount ()
  (should (equal '(user-error "Invalid ‘AMOUNT’(df.df) for ingredient")
		 (should-error (org-shoplist-ing-create "df.df" "Nuts"))))
  (should (equal '(user-error "Invalid ‘AMOUNT’(-234) for ingredient")
		 (should-error (org-shoplist-ing-create "-234" "Nuts"))))
  (should (equal '(user-error "Invalid ‘AMOUNT’(a>1234) for ingredient")
		 (should-error (org-shoplist-ing-create "a>1234" "Nuts"))))
  (should (equal '(user-error "Invalid ‘AMOUNT’(..02 gal) for ingredient")
		 (should-error (org-shoplist-ing-create "..02 gal" "Nuts")))))

(ert-deftest org-shoplist-test/ing-create-when-amount-1-something ()
  (should (equal (list "Nuts" "1ml" "m^3" org-shoplist-ing-default-separator) (org-shoplist-ing-create "1ml" "Nuts"))))

(ert-deftest org-shoplist-test/ing-create-when-amount-true-number ()
  (should (equal (list "Nuts" "100" "1" org-shoplist-ing-default-separator)
		 (org-shoplist-ing-create 100 "Nuts"))))

(ert-deftest org-shoplist-test/ing-create-when-custom-unit ()
  (org-shoplist-test-test-in-org-buffer
   (lambda ()
     (org-shoplist-test-set-additioanl-units '((foo nil "*foo")))
     (should (equal (list "Nuts" "2foo" "foo" org-shoplist-ing-default-separator)
		    (org-shoplist-ing-create "2foo" "Nuts"))))))

(ert-deftest org-shoplist-test/ing-create-when-custom-unit-where-first-char-is-in-prefix-table ()
  (org-shoplist-test-test-in-org-buffer
   (lambda ()
     (org-shoplist-test-set-additioanl-units (list (list 'Tl nil "Teelöffel")))
     (should (equal (list "Zucker" "2Tl" "Tl" org-shoplist-ing-default-separator)
		    (org-shoplist-ing-create "2Tl" "Zucker"))))))

(ert-deftest org-shoplist-test/ing-create-when-custom-unit-with-point ()
  (org-shoplist-test-test-in-org-buffer
   (lambda ()
     (org-shoplist-test-set-additioanl-units (list (list 'foo nil "foo.")))
     (should (equal '(user-error "Invalid ‘AMOUNT’(2foo.) for ingredient")
		    (should-error (org-shoplist-ing-create "2foo." "Nuts")))))))

(ert-deftest org-shoplist-test/ing-create-when-custom-unit-with-prefix ()
  (org-shoplist-test-test-in-org-buffer
   (lambda ()
     (should (equal (list "Nuts" "100kg" "g" org-shoplist-ing-default-separator)
		    (org-shoplist-ing-create "100kg" "Nuts"))))))

(ert-deftest org-shoplist-test/ing-create-when-unit-nil ()
  (should (equal (list "Nuts" "100" "1" org-shoplist-ing-default-separator)
		 (org-shoplist-ing-create 100 "Nuts"))))

(ert-deftest org-shoplist-test/ing-create-amount-simple-sequence ()
  (should (equal (list "Nuts" "100g" "g" org-shoplist-ing-default-separator)
		 (org-shoplist-ing-create '(* 100 (var g var-g)) "Nuts"))))

(ert-deftest org-shoplist-test/ing-create-when-amount-is-calc-expr ()
  (should (equal (list "Nuts" "200100g" "g" org-shoplist-ing-default-separator) (org-shoplist-ing-create "100g+200kg" "Nuts"))))

(ert-deftest org-shoplist-test/ing-create-dont-change-match-data ()
  "Match data shouldn't be changed by call."
  (let ((data-before (match-data))
	(data-after nil))
    (org-shoplist-ing-create "100g" "Nuts")
    (setq data-after (match-data))
    (should (equal data-before data-after))))

(ert-deftest org-shoplist-test/ing-create-when-unit-in-unit-group-but-not-prefix ()
  (should (equal (list "Nuts" "100tsp" "m^3" org-shoplist-ing-default-separator)
		 (org-shoplist-ing-create "100tsp" "Nuts"))))
(ert-deftest org-shoplist-test/ing-create-when-unit-in-unit-group-but-not-prefix-deeper ()
  (should (equal (list "Nuts" "100tbsp" "m^3" org-shoplist-ing-default-separator)
		 (org-shoplist-ing-create "100tbsp" "Nuts"))))

(ert-deftest org-shoplist-test/ing-amount-amount-with-unit ()
    (should (string= "100g" (org-shoplist-ing-amount (org-shoplist-ing-create "100g" "Nuts")))))

(ert-deftest org-shoplist-test/ing-amount-when-no-unit ()
  (should (string= "100" (org-shoplist-ing-amount (org-shoplist-ing-create "100" "Nuts")))))

(ert-deftest org-shoplist-test/ing-unit-amount-with-unit ()
  (should (string= "g" (org-shoplist-ing-unit (org-shoplist-ing-create "100g" "Nuts")))))

(ert-deftest org-shoplist-test/ing-unit-amount-number ()
  (should (eq nil (org-shoplist-ing-unit (org-shoplist-ing-create "100" "Nuts")))))

(ert-deftest org-shoplist-test/ing-return-full-unit ()
  (should (string= "mg" (org-shoplist-ing-unit (org-shoplist-ing-create "100mg" "Nuts")))))

(ert-deftest org-shoplist-test/ing-read-str-nil ()
  "Return nil when nil is passed as ‘STR’."
  (should (eq nil (org-shoplist-ing-read))))

(ert-deftest org-shoplist-test/ing-read-empty-str ()
  "Return nil when empty string is passed."
  (should (eq nil (org-shoplist-ing-read nil ""))))

(ert-deftest org-shoplist-test/ing-read-str-only-ing ()
  "Return one ingredient-structure when str contains only a ing."
  (should (equal (list (org-shoplist-ing-create "100g" "Nuts")) (org-shoplist-ing-read nil "(100g Nuts)"))))

(ert-deftest org-shoplist-test/ing-read-str-only-trash ()
  "Return nil when str is not empty-string but contains only trash."
  (should (equal nil (org-shoplist-ing-read nil "This is trash"))))

(ert-deftest org-shoplist-test/ing-read-str-trash-with-one-ing ()
  "Return one ingredient-structure when str contains one ingredient"
  (should (equal (list (org-shoplist-ing-create "100g" "Nuts")) (org-shoplist-ing-read nil "This (100g Nuts) is trash"))))

(ert-deftest org-shoplist-test/ing-read-str-two-ings ()
  "Return a list with two ingredient-structures when str contains two ingredient"
  (should (equal (list
		  (org-shoplist-ing-create "100g" "Nuts")
		  (org-shoplist-ing-create "100ml" "Milk"))
		 (org-shoplist-ing-read nil "(100g Nuts)(100ml Milk)"))))

(ert-deftest org-shoplist-test/ing-read-str-two-ings-no-unit ()
  "Return a list with two ingredient-structures when str contains two ingredient"
  (should (equal (list
		  (org-shoplist-ing-create "1" "Tomato")
		  (org-shoplist-ing-create "1" "Onion"))
		 (org-shoplist-ing-read nil "(1 Tomato)(1 Onion)"))))

(ert-deftest org-shoplist-test/ing-read-str-two-ings-with-trash ()
  "Return a list with two ingredient-structures when str contains two ingredient"
  (should (equal (list
		  (org-shoplist-ing-create "100g" "Nuts")
		  (org-shoplist-ing-create "100ml" "Milk"))
		 (org-shoplist-ing-read nil "This is (100g Nuts) much (100ml Milk) trash."))))

(ert-deftest org-shoplist-test/ing-read-str-three-ings ()
  "Return a list with three ingredient-structures when str contains thee ingredient"
  (should (equal (list
		  (org-shoplist-ing-create "100g" "Nuts")
		  (org-shoplist-ing-create "100ml" "Milk")
		  (org-shoplist-ing-create "100kg" "Flour"))
		 (org-shoplist-ing-read nil "(100g Nuts)(100ml Milk)(100kg Flour)"))))

(ert-deftest org-shoplist-test/ing-read-str-four-ings ()
  "Return a list with four ingredient-structures when str contains four ingredient"
  (should (equal (list
		  (org-shoplist-ing-create "100g" "Nuts")
		  (org-shoplist-ing-create "100ml" "Milk")
		  (org-shoplist-ing-create "100kg" "Flour")
		  (org-shoplist-ing-create "1" "Egg"))
		 (org-shoplist-ing-read nil "(100g Nuts)(100ml Milk)(100kg Flour)(1 Egg)"))))

(ert-deftest org-shoplist-test/ing-read-str-with-line-break ()
  "When there is a line-break keep up reading the ingredient."
  (should (equal (list (org-shoplist-ing-create "100g" "Nuts" "\n"))
		 (org-shoplist-ing-read nil "(100g
Nuts)")))
  (should (equal (list (org-shoplist-ing-create "100g" "red Berries" "\n"))
		 (org-shoplist-ing-read nil "(100g
red Berries)"))))

(ert-deftest org-shoplist-test/ing-read-line-with-line-break ()
  "Read line in buffer, but read the whole ing which ends on next line."
  (org-shoplist-test-test-in-org-buffer
   (lambda ()
     (insert "(100g
Nuts)")
     (goto-char (point-min))
     (should (equal (list (org-shoplist-ing-create "100g" "Nuts" "\n"))
		    (org-shoplist-ing-read nil nil))))))

(ert-deftest org-shoplist-test/ing-read-two-ings-one-broken ()
  "Read line in buffer, but read the whole ing which ends on next line."
  (org-shoplist-test-test-in-org-buffer
   (lambda ()
     (insert "(100ml Milk) (100g
Nuts)")
     (goto-char (point-min))
     (should (equal (list (org-shoplist-ing-create "100ml" "Milk" " ") (org-shoplist-ing-create "100g" "Nuts" "\n"))
		    (org-shoplist-ing-read nil nil))))))

(ert-deftest org-shoplist-test/ing-read-line-with-custom-start-and-end-char ()
  "Read ing as normal with custom chars."
  (org-shoplist-test-test-in-org-buffer
   (lambda ()
     (setq org-shoplist-ing-start-char "/")
     (setq org-shoplist-ing-end-char "\\")
     (insert "/100g Nuts\\")
     (should (equal (list (org-shoplist-ing-create "100g" "Nuts"))
		    (org-shoplist-ing-read nil nil))))))

(ert-deftest org-shoplist-test/ing-read-no-param-read-at-point ()
  "Parse line where point is at when no parameters passed"
  (org-shoplist-test-test-in-org-buffer
   (lambda ()
     (insert "(100g Nuts)")
     (should (equal (list (org-shoplist-ing-create "100g" "Nuts"))
		    (org-shoplist-ing-read))))))

(ert-deftest org-shoplist-test/ing-read-when-previous-search-no-ings ()
  "Parse line where point is at when no parameters passed"
  (org-shoplist-test-test-in-org-buffer
   (lambda ()
     (insert "Nuts")
     (goto-char (point-min))
     (search-forward-regexp "s" nil t 1)
     (should (eq nil (org-shoplist-ing-read))))))

(ert-deftest org-shoplist-test/ing-read-when-previous-search-with-ings ()
  "Parse line where point is at when no parameters passed"
  (org-shoplist-test-test-in-org-buffer
   (lambda ()
     (insert "(100g Nuts)")
     (goto-char (point-min))
     (search-forward-regexp "s" nil t 1)
     (should (equal (list (org-shoplist-ing-create "100g" "Nuts"))
		    (org-shoplist-ing-read))))))

(ert-deftest org-shoplist-test/ing-read-twice-same-ing ()
  "Create two ings even if they are the same."
  (should (equal (list (org-shoplist-ing-create "100g" "Nuts") (org-shoplist-ing-create "100g" "Nuts"))
		 (org-shoplist-ing-read nil "(100g Nuts)(100g Nuts)"))))

(ert-deftest org-shoplist-test/ing-read-twice-same-ing-aggregate ()
  "Aggregate two ings when they are exatcly the same."
  (should (equal (list (org-shoplist-ing-create "200g" "Nuts"))
		 (org-shoplist-ing-read t "(100g Nuts)(100g Nuts)"))))

(ert-deftest org-shoplist-test/ing-read-two-ing-with-compatible-unit-aggregate ()
  "Aggregate two ings when they are exatcly the same."
  (should (equal (list (org-shoplist-ing-create "1.1kg" "Nuts"))
		 (org-shoplist-ing-read t "(100g Nuts)(1kg Nuts)"))))

(ert-deftest org-shoplist-test/ing-read-two-same-ing-with-incompatible-unit-dont-aggregate ()
  "Don't aggregate two ings when they have incompatible units."
  (should (equal (list (org-shoplist-ing-create "100g" "Nuts")
		       (org-shoplist-ing-create "1cl" "Nuts"))
		 (org-shoplist-ing-read t "(100g Nuts)(1cl Nuts)"))))

(ert-deftest org-shoplist-test/ing-double-enclosed-ing ()
  "Get ing when it’s double enclosed."
  (should (equal (list (org-shoplist-ing-create "200g" "Nuts"))
		 (org-shoplist-ing-read nil "((200g Nuts))"))))

(ert-deftest org-shoplist-test/ing-double-enclosed-ing-with-text-in-outer-pars ()
  "Get ing when it’s double enclosed."
  (should (equal (list (org-shoplist-ing-create "200g" "Nuts"))
		 (org-shoplist-ing-read nil "(asd (200g Nuts) asd)"))))

(ert-deftest org-shoplist-test/ing-double-enclosed-ing-with-ing-like-text-in-outer-pars ()
  "Get ing when it’s double enclosed."
  (should (equal (list (org-shoplist-ing-create "200g" "Nuts"))
		 (org-shoplist-ing-read nil "(100g Nuts (200g Nuts))"))))

(ert-deftest org-shoplist-test/two-ing-double-enclosed ()
  "Get ing when it’s double enclosed."
  (should (equal (list (org-shoplist-ing-create "100g" "Nuts")
		       (org-shoplist-ing-create "200g" "Nuts"))
		 (org-shoplist-ing-read nil "((100g Nuts) (200g Nuts))"))))

(ert-deftest org-shoplist-test/ing-multiply-ing-nil ()
  "Return error when passing invalid ingredients."
  (should (equal '(user-error "Invalid ‘NAME’(nil) for ingredient")
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
  (should (equal '(user-error "Incompatible units while aggregating(((\"Nuts\" \"999g\" \"g\" \" \") \"1ml\"))")
		 (should-error (org-shoplist-ing-+ (org-shoplist-ing-create "999g" "Nuts") "1ml")))))

(ert-deftest org-shoplist-test/ing-+-ing ()
  "Add the amount of two ings togehter."
  (let ((ing (org-shoplist-ing-create "100g" "Nuts")))
    (should (equal (org-shoplist-ing-amount (org-shoplist-ing-create "200g" "Nuts"))
		   (org-shoplist-ing-+ ing ing)))))

(ert-deftest org-shoplist-test/ing-+-random ()
  "Trow error when trying to add something random."
  (should (equal '(user-error "Given ‘AMOUNT’(asdf) can’t be converted")
		 (should-error (org-shoplist-ing-+ "100g" 'asdf)))))

(ert-deftest org-shoplist-test/ing-+50g+50g+100g-Nuts ()
  (should (equal (org-shoplist-ing-amount (org-shoplist-ing-create "200g" "Nuts"))
		 (org-shoplist-ing-+ (org-shoplist-ing-create "50g" "Nuts") (org-shoplist-ing-create "50g" "Nuts") (org-shoplist-ing-create "100g" "Nuts")))))

(ert-deftest org-shoplist-test/ing-persitend-unit-output-undependent-on-input-order ()
  (should (equal (org-shoplist-ing-amount (org-shoplist-ing-create "1.2kg" "Nuts"))
		 (org-shoplist-ing-+ (org-shoplist-ing-create "1kg" "Nuts") (org-shoplist-ing-create "50g" "Nuts")
		     (org-shoplist-ing-create "50g" "Nuts") (org-shoplist-ing-create "100g" "Nuts")))))

(ert-deftest org-shoplist-test/ing-+99999g+99999kg-Nuts ()
  (should (equal (org-shoplist-ing-amount (org-shoplist-ing-create "10098999 g" "Nuts"))
		 (org-shoplist-ing-+ (org-shoplist-ing-create "99999g" "Nuts") (org-shoplist-ing-create "9999kg" "Nuts")))))

(ert-deftest org-shoplist-test/ing-+9tsp+1tbsp+20ml+0.5l-Milk ()
  (should (equal (org-shoplist-ing-amount (org-shoplist-ing-create "117.49975083 tsp" "Milk"))
		 (org-shoplist-ing-+ (org-shoplist-ing-create "9tsp" "Milk") (org-shoplist-ing-create "1tbsp" "Milk")
		     (org-shoplist-ing-create "20ml" "Milk") (org-shoplist-ing-create "0.5l" "Milk")))))

(ert-deftest org-shoplist-test/ing-*2-20ml-Milk ()
  (should (equal (org-shoplist-ing-create "40ml" "Milk")
		 (org-shoplist-ing-* (org-shoplist-ing-create "20ml" "Milk") 2))))

(ert-deftest org-shoplist-test/ing-*2.4-20ml-Milk ()
  (should (equal (org-shoplist-ing-create "48. ml" "Milk")
		 (org-shoplist-ing-* (org-shoplist-ing-create "20ml" "Milk") 2.4))))

;;; org-shoplist-ing-test.el ends here
