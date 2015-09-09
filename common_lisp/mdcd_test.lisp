#! /usr/local/bin/clisp

#-quicklisp
(let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname))))
	(when (probe-file quicklisp-init)
		(load quicklisp-init)))

(ql:quickload "clunit")
(use-package :clunit)

(ql:quickload "cl-utilities")
;(shadowing-import 'cl-utilities:with-gensyms)
;(use-package :cl-utilities)
(load "mdcd-test-package.lisp")
(load "mdcd.lisp")


(defsuite EverythingSuite ())

(defsuite SupportFunctionsSuite (EverythingSuite))
(defsuite PublicFunctionsSuite (EverythingSuite))

(deftest line-matches-section-test (SupportFunctionsSuite)
  (assert-equal t (mdcd:line-matches-section? "## Foo" 0 :description))
  (assert-equal nil (mdcd:line-matches-section? "## Foo" 0 :parameters))
  (assert-equal t (mdcd:line-matches-section? "## Parameters" 5 :parameters))
  (assert-equal t(mdcd:line-matches-section? "## Parameters" 0 :parameters))
)

(deftest capture-good-line-test (SupportFunctionsSuite)
  ; (defun capture-good-line (line header-line section-found line-matches-section)
  (let ((test-line "test-line"))
    ; first matching header
    (assert-equal '("test-line") (mdcd:capture-good-line test-line t nil t))
    ; header after matching header
    (assert-equal nil (mdcd:capture-good-line test-line t t nil))
    ; non-header line, section found
    (assert-equal '("test-line") (mdcd:capture-good-line test-line nil t t))
    ; non-header line before matching section
    (assert-equal nil (mdcd:capture-good-line test-line nil nil t))))

(deftest string-is-md-header-test (SupportFunctionsSuite)
  (let ((yes "## I'm a header")
        (no "I'm not a header")
        (no-b " ## I'm not because of the space"))
    (assert-equal t (mdcd:string-is-md-header? yes))
    (assert-equal nil (mdcd:string-is-md-header? no))
    (assert-equal nil (mdcd:string-is-md-header? no-b))
        ))

(deftest find-header-lines-test (SupportFunctionsSuite)
  (let ((doc-string '("## Public: foo" 
                            "description" 
                            "### Parameters: "
                            "* this - this thing"
                            "* that - that thing"
                            "## Notes:"
                            "notes here")))
    (assert-equal '(0 2 5) (mdcd:find-header-lines doc-string))
                            ))

(deftest extract-section-test (SupportFunctionsSuite)
  (let ((doc-string "## Public: foo 
description 
### Parameters: 
* this - this thing
* that - that thing
## Notes:
notes here")
        (params-string "### Parameters: 
* this - this thing
* that - that thing") ; 56 chars long
)

    (assert-equal params-string (mdcd:extract-section :parameters doc-string))

    (assert-equal "## Notes:
notes here" (mdcd:extract-section :notes doc-string))

    ; an attempt to extract a non-existent section should return nil
    (assert-equal nil (mdcd:extract-section :returns doc-string ))
))

(defun extract-relevant-file-paths (ff-response last-n)
  "This is just a support function"
  (let* ((ff-response-dir (cdr (pathname-directory ff-response)))
         (testable-response 
          (loop for i from (- (length ff-response-dir) last-n) 
                      upto (- (length ff-response-dir) 1) 
                      collect (nth i ff-response-dir))))
      (return-from extract-relevant-file-paths testable-response)))

(deftest mdcd-file-for-test (SupportFunctionsSuite)
  ; (let ((testable-response (extract-relevant-file-paths 
  ;                             (mdcd:mdcd-file-for "foo") 2)))
  ;  (assert-equal '("mdcd" "lisp") testable-response ))
  (let ((testable-response (extract-relevant-file-paths  
                              (mdcd:mdcd-file-for "bar" "foo" "function") 4)))
    (assert-equal '("mdcd" "lisp" "foo" "function") testable-response ))
    ; "bar" is the filename (minus extension)
    ; "foo" is the subdirectory "foo.md" will be stored in.
    ; "function" is another subdirectory based on type of thing that's been
    ; documented.
  (let ((testable-response (extract-relevant-file-paths  
                              (mdcd:mdcd-file-for "bar" "foo") 3)))
    (assert-equal '("mdcd" "lisp" "foo") testable-response ))
)

(deftest path-for-test (SupportFunctionsSuite)
  (let ((testable-response (extract-relevant-file-paths  
                              (mdcd:path-for "foo:bar" 'function) 4)))
    (assert-equal '("mdcd" "lisp" "foo" "function") testable-response ))
    
  (let* ( (path-for-response (mdcd:path-for "foo" :meta))
          (testable-response (extract-relevant-file-paths  
                              path-for-response 3)))
    (assert-equal '("mdcd" "lisp" "foo") testable-response )
    (assert (not (null (pathname-name path-for-response))))
    (assert-equal "index" (pathname-name path-for-response))
    (assert-equal "md" (pathname-type path-for-response)) ))

; RUN THE TEST
(setf clunit:*clunit-report-format* :default)
(print (run-suite 'EverythingSuite))
