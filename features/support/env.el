(require 'f)

(defvar org-shoplist-support-path
  (f-dirname load-file-name))

(defvar org-shoplist-features-path
  (f-parent org-shoplist-support-path))

(defvar org-shoplist-root-path
  (f-parent org-shoplist-features-path))

(add-to-list 'load-path org-shoplist-root-path)

;; Ensure that we don't load old byte-compiled versions
(let ((load-prefer-newer t))
  (require 'org-shoplist)
  (require 'espuds)
  (require 'ert))

(Setup
 ;; Before anything has run
 )

(Before
 ;; Before each scenario is run
 )

(After
 ;; After each scenario is run
 )

(Teardown
 ;; After when everything has been run
 )
