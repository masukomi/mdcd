#!/usr/local/bin/csi

; (require-extension test)
(use test); ^^ same effect
(import mdcd)
(use files)
(use directory-utils)

;(use filepath)
  ; ^^^ would use it
  ; to test that it's cleaning up names correctly
  ; but it sucks and doesn't compile... i think.

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
      (test-group "dir differences"
        (test-assert (not (equal? docfun-file-path docfun-var-path)))
        (test-assert (not (equal? docfun-file-path docfun-syntax-path)))
        (test-assert (not (equal? docfun-var-path docfun-syntax-path)))
      )
      (test-group "naming"
        (test "horribly_named_fun_with_crap_" 
              (mdcd-path-for-fun "horribly/named:fun|with*crap?" ))
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
        (test-assert (file-exists? docfun-fun-path))
        (remove-file docfun-fun-path) ; cleanup
      )
      ; (test-group "variables"
      ;
      ; )
      ; (test-group "syntax"
      ;
      ; )
    )
  )
)


;; IMPORTANT! The following ensures nightly automated tests can
;; distinguish failure from success.  Always end tests/run.scm with this.
(test-exit)
