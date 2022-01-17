(module mdcd-sections
  (

   ; THE FOLLOWING WERE ONLY EXPORTED 
   ; TO FACILITATE TDD
   ; THEY CAN ALL BE COMMENTED OUT
   header-line-truthyness
   header-indexes
   extract-section
   section-line-numbers
   section-start
   line-matches-section?
   string-is-md-header?
   )
  (import chicken.base)
  (import chicken.string)
  (import scheme)
  (import srfi-13)
  (import srfi-1)
  (import listicles)
  (import regex)

  (define (header-indexes lines)
    (let ((line-headerness-pairs (header-line-truthyness lines)))
      (compress
        (map (lambda(x)(cdr x)) ; the truthyness
          line-headerness-pairs)
        (map (lambda(x)
               (cons (car x) (nth (car x) lines)))
                        ; x => (0 . #t)
          line-headerness-pairs))))



  (define (header-line-truthyness lines)
      (map (lambda (i)(cons i (string-is-md-header? (nth i lines))))
           (range 0 (- (length lines) 1))))

  
; (doc-fun "extract-section"
; "## Private: extract-section
; Extracts the specified section of a given doc string (if available).
;
; ### Parameters:
; * section - A symbol: any of the following: 
;   'description, 'parameters, 'returns, 'notes, 'examples. 'description will 
;   extract the first section which hopefully includes the name, visibility,
;   and overview.
; * doc-string - The doc string from which to attempt the extraction 
;   of the specified section
;
; ### Returns: 
; The extracted section of the docs.")
  (define (extract-section section doc-string)
    (let ((lines  (string-split doc-string "\n" #t)))
      ; TODO: OMFG inefficient
      ; there's gotta be a way to slice
       (map (lambda(x)(nth x lines))
          (section-line-numbers
             (header-indexes lines)
             section
             lines))))
  
  (define (section-start header-indexes section)
    (if (eq? section 'description)
        (nth 0 header-indexes) ;the 1st one is always description.
        (let ((result
                      (compress
                        (map (lambda (x) (line-matches-section? (cdr x) section))
                             header-indexes) 
                        header-indexes)))
          (if (and (> (length result) 0) (pair? (car result))) 
            (car result) ; result == '(( 3 . "# FOO"))
            #f)           ; result was empty list
        )
    )
  )

  
  (define (section-line-numbers header-indexes section lines)
    (let ((start (if (eq? section 'description)
                      (nth 0 header-indexes)
                    (section-start header-indexes section)))) ;=> ( 3 . "# FOO")
      (if (eq? #f start)
        '() ; no matching section == no line numbers
          (let ((start-index (list-index (lambda(x)(equal? start x)) header-indexes)))
            (range (first start)
                     (if (not (> (+ 2 start-index) (length header-indexes)))
                       ; ^^ + 2 because: 1 for the NEXT index 
                       ;    & 1 more because length starts at 1 & index at 0
                        (- (first (nth (+ 1 start-index) header-indexes)) 1)
                        (- (length lines) 1)
                     )
            )
          )
      )
    )
  )
  ;
  (define (line-matches-section? line section)
    (not 
      (eq? #f 
           (string-match 
             (regexp 
               (conc "^#+\\s*" (symbol->string section) "\\W*\\s*.*?") #t) 
               line))))

  (define (string-is-md-header? a-string)
    (if (not (eq? 0 (string-length a-string)))
      (equal? 0 (string-contains a-string "#" 0 1))
      #f))
)
