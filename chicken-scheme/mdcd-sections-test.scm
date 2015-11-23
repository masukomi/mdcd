#!/usr/local/bin/csi

; (require-extension test)
(use test); ^^ same effect
(import (prefix mdcd-sections ms:))

(let (
      (test-doc-string 
"## Private: test-mdcd-docfun
Totally not a real method

## Returns: 
`#f` because it's bogus

## Examples:
Hah!"))
  (let ((test-lines (string-split test-doc-string "\n" #t)))
    ; (test-group "basic sanity"
    ;   (test "correct number of test lines"
    ;     8 (length test-lines))
    ; )

    (test-group "MDCD-Sections"
      (test-group "string-is-md-header?"
        (test-assert 
          (ms:string-is-md-header? "# foo"))
        (test-assert
          (not (ms:string-is-md-header? "bar")))
        (test-assert
          (not (ms:string-is-md-header? "")))
          ;^^ because string-contains blows the fuck up if you
          ; ask it if "" contains anything.
      )

     (test-group "header-line-truthyness"
        (test "can determine which lines are headers"
              '((0 . #t) 
                (1 . #f) 
                (2 . #f)
                (3 . #t)
                (4 . #f)
                (5 . #f)
                (6 . #t)
                (7 . #f))
              (ms:header-line-truthyness test-lines))
     )
     (test-group "header-indexes"
        (test "can-find-header-indexes"
            '(
              (0 . "## Private: test-mdcd-docfun")
              (3 . "## Returns: ")
              (6 . "## Examples:"))
            (ms:header-indexes test-lines)
              )
      )
     (test-group "extract-section"
        (test "extracting first section"
          '("## Private: test-mdcd-docfun" "Totally not a real method" "")
            (ms:extract-section 'description test-doc-string)
              )
        (test "extracting midsection"
          '("## Returns: " "`#f` because it's bogus" "")
            (ms:extract-section 'returns test-doc-string)
              )
        (test "extracting final section"
          '("## Examples:" "Hah!")
          (ms:extract-section 'examples test-doc-string)
        )
     )
     (test-group "section-line-numbers"
        (test "can find all the line numbers for start section"
            '(0 1 2)
              (ms:section-line-numbers 
                (ms:header-indexes test-lines) 
                 'description test-lines))
        (test "can find all the line numbers for a mid-section"
            '(3 4 5)
              (ms:section-line-numbers 
                (ms:header-indexes test-lines) 
                 'returns test-lines))
        (test "can find all the line numbers for an end-section"
            '(6 7)
              (ms:section-line-numbers 
                (ms:header-indexes test-lines) 
                 'examples test-lines))

     )
     (test-group "line-matches-section?"
        (test "can find header matching section with crap"
              #t
              (ms:line-matches-section? "# FoO: " 'foO))
              ; ^^^ case insensitive
              ; followed by : and space
        (test "can find header matching section w/o crap"
              #t
              (ms:line-matches-section? "## FoO" 'foO))
        (test "can find section with trailing bits"
              #t
              (ms:line-matches-section? "## Foo: test-mdcd-docfun" 'fOo))
     )
     (test-group "section-start"
        (test "finds correct start of start section"
          '(0 . "## Private: test-mdcd-docfun")
          (ms:section-start (ms:header-indexes test-lines) 'description))
        (test "finds correct start of mid-section"
          '(3 . "## Returns: ")
          (ms:section-start (ms:header-indexes test-lines) 'returns))
        (test "finds correct start of end section"
          '(6 . "## Examples:")
          (ms:section-start (ms:header-indexes test-lines) 'examples))
        (test "returns -1 when no section found"
          #f
          (ms:section-start (ms:header-indexes test-lines) 'bullshit))

      )
    ) 

  ); end inner let
)
(test-exit)

