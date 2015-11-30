#!/usr/local/bin/csi

; (require-extension test)
(use test); ^^ same effect
(import mdcd-config)
(import mdcd)
(use files)
(use directory-utils)
(use srfi-13)
(use filepath)
(use ports)

(define (remove-file file-path)
  (if (file-exists? file-path)
    (system (sprintf "rm ~A" file-path))
  )
)

(define (get-doc-section section-method docfun-fun-path test-doc-string)
  (remove-file docfun-fun-path)
  (doc-fun "test-mdcd-docfun" test-doc-string)
  (let ((response-string 
          (with-output-to-string
                  (lambda()(section-method "test-mdcd-docfun"))))
                  )
    (remove-file docfun-fun-path)
    response-string)

  )


(set-mdcd-home (mdcd-default-home-dirs))

(let (
      (test-doc-string 
"## Private: test-mdcd-docfun
Totally not a real method

## Returns: 
`#f` because it's bogus

## Examples:
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
              "a-b_c_d_e_f_g_h_i_j_k_l_m_n_o_p_q_r_s" 
              ;^^^ note the hyhpen stays a hyphen
              (filepath:take-base-name
                (mdcd-path-for-fun "a-b$c%d&e*f+g!h.i/j:k<l=m>n?o@p^q~r s" )))
      )
    )
    ; (test-group "file names"
    ; )
    (test-group "creation"
      (test-group "disabled"
        ; nothing should be created when disabled
          (mdcd-disable)
          (remove-file docfun-fun-path)
          (doc-fun "test-mdcd-docfun" test-doc-string)
          (test-assert "documenting a function DOESN'T create a file"
                       (not (file-exists? docfun-fun-path)))
          (remove-file docfun-var-path)
          (doc-var "test-mdcd-docfun" test-doc-string)
          (test-assert "documenting a variable DOESN'T create a file"
                       (not (file-exists? docfun-var-path)))
          (remove-file docfun-syntax-path)
          (doc-syntax "test-mdcd-docfun" test-doc-string)
          (test-assert "documenting a variable DOESN'T create a file"
                       (not (file-exists? docfun-syntax-path)))
      )
      (test-group "enabled"
        (set-mdcd-home (mdcd-default-home-dirs))
        (test-group "sanity check"
          (test-assert (mdcd-enabled?))
          (test "function file path"
                docfun-fun-path 
                (mdcd-path-for-fun "test-mdcd-docfun" )
                    ))
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
        (mdcd-disable)
      )
    )
    (test-group "retrieval"
      (set-mdcd-home (mdcd-default-home-dirs))
      (test-group "show-doc"
        (let ((test-doc-plus-newline (conc test-doc-string #\newline)))
          (test "can display entire doc"
                test-doc-plus-newline
                (get-doc-section show-doc docfun-fun-path test-doc-string)))
        )

      (test-group "show-description"
          (test "can display description alone"
"## Private: test-mdcd-docfun
Totally not a real method

"
                (get-doc-section show-description docfun-fun-path test-doc-string))
      )
      (test-group "show-returns"
          (test "can display returns alone"
"## Returns: 
`#f` because it's bogus

"
                (get-doc-section show-returns docfun-fun-path test-doc-string))
      )
      (test-group "show-examples"
          (test "can display examples alone"
"## Examples:
Hah!

"
                (get-doc-section show-examples docfun-fun-path test-doc-string))
      )
      (test-group "read-doc"
        (mdcd-disable)
        (test "disabled when home is null"
              "MDCD: Disabled"
              (read-doc "test-mdcd-docfun" ))
        (set-mdcd-home old-mdcd-home)
        
      )

      (mdcd-disable)
    )
    
  )
)


;; IMPORTANT! The following ensures nightly automated tests can
;; distinguish failure from success.  Always end tests/run.scm with this.
(test-exit)
