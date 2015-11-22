#!/usr/local/bin/csi

; (require-extension test)
(use test); ^^ same effect
(import mdcd)
(use files)
(use directory-utils)
(use srfi-13)
(use filepath)

(define (remove-file file-path)
  (if (file-exists? file-path)
    (system (sprintf "rm ~A" file-path))
  )
)


(let (
      (test-doc-string 
"## Private: test-mdcd-docfun
Totally not a real method

## Returns: 
`#f` because it's bogus

## Example:
Hah!")
      (docfun-fun-path (mdcd-path-for-fun "test-mdcd-docfun" ))
      (docfun-var-path (mdcd-path-for-var "test-mdcd-docfun" ))
      (docfun-syntax-path (mdcd-path-for-syntax "test-mdcd-docfun" ))
)
  (test-group "MDCD"
    (test-group "file paths"
      (test-group "directories"
        ; they functions, variables, and syntax things should 
        ; all be stored in different places
        (test-assert "funtions not stored in same dir as variables"
                     (not (equal? docfun-fun-path docfun-var-path)))
        (test-assert "functions not stored in same dir as syntax"
                     (not (equal? docfun-fun-path docfun-syntax-path)))
        (test-assert "variables not stored in same dir as syntax"
                     (not (equal? docfun-var-path docfun-syntax-path)))
        (let ((manual-fun-path (string-concatenate (list
                         (get-mdcd-home) ; ends with a /
                         "functions/test-mdcd-docfun.md")))
              (manual-var-path (string-concatenate (list
                         (get-mdcd-home) ; ends with a /
                         "variables/test-mdcd-docfun.md")))
              (manual-syntax-path (string-concatenate (list
                         (get-mdcd-home) ; ends with a /
                         "syntax/test-mdcd-docfun.md"))))
        (test "functions stored in correct dir"
              manual-fun-path docfun-fun-path)
        (test "variable stored in correct dir" 
              manual-var-path docfun-var-path)
        (test "syntax stored in correct dir"
              manual-syntax-path docfun-syntax-path)

        )
      )
      (test-group "naming"
        ; doesn't do space removal because you can't name
        ; anything with a space in scheme
        (test "function names converted to safe file names" 
              "horribly_named_fun_with_crap_" 
              (filepath:take-base-name
                (mdcd-path-for-fun "horribly/named:fun|with*crap?" )))
      )
    )
    ; (test-group "file names"
    ; )
    (test-group "creation"
      (test-group "functions"
        ; doc-fun should create a file
        ; so, first delete it.
        (remove-file docfun-fun-path)
        (doc-fun "test-mdcd-docfun" test-doc-string)
        (test-assert "documenting a function creates a file"
                     (file-exists? docfun-fun-path))
        
        (remove-file docfun-fun-path) ; cleanup
      )
      (test-group "variables"
        (remove-file docfun-var-path)
        (doc-var "test-mdcd-docfun" test-doc-string)
        (test-assert "documenting a variable creates a file"
                     (file-exists? docfun-var-path))
        (remove-file docfun-var-path) ; cleanup

      )
      (test-group "syntax"
        (remove-file docfun-syntax-path)
        (doc-syntax "test-mdcd-docfun" test-doc-string)
        (test-assert "documenting syntax creates a file"
                     (file-exists? docfun-syntax-path))
        (remove-file docfun-syntax-path) ; cleanup
     
      )
    )
  )
)


;; IMPORTANT! The following ensures nightly automated tests can
;; distinguish failure from success.  Always end tests/run.scm with this.
(test-exit)
