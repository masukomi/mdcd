; # MCDC Mardown Code DoCs
; Allows you to markup your code with Markdown.
; Enables you to: 
; * request docs for any documented function in a REPL
; * generate markdown files for easy searchability 
;   via tools like spotlight
; * generate static html for distributing your documentation
;
; # Dependencies
; * directory-utils
; * utils
; * regex

(module mdcd
  (
    ; configuration

    ; documentating things
    doc-fun
    doc-syntax
    doc-var
    ; displaying docs
    show-doc
    read-doc
    show-description
    show-params
    show-returns
    show-examples
    show-notes
    show-section

    
    ; the following are only exported for 
    ; testing purposes. please delete before creating eggs.
    mdcd-path-for-var
    mdcd-path-for-fun
    mdcd-path-for-syntax

   )
  (import chicken)
  (import scheme)
  (use files)
  (use directory-utils)
  (use data-structures)
  (use utils); for read-all procedure
  (require-extension regex)
  (import mdcd-config)
  (import (prefix mdcd-sections ms:))

  ;(use symbol-utils); for unbound?

  

  ; ## Private: mdcd-name-cleaner
  ; converts method and variable names into something more filesystem friendly
  ; ### Parameters:
  ; * name - the name to be cleaned.
  ; ### Returns:
  ; The "cleaned" name.
  (define (mdcd-name-cleaner name)
    ; "[^\\w_\-]+" seemed like it was too likely to result
    ; in overlap. I could hash the name but I would prefer
    ; to keep the files as something meaningful to humans.
    (string-substitute* name '(("[!%&*+./:<=>?@~ ^$]+" . "_"))))

  ; ## Public: mdcd-file-for
  ; Provides the file-path for a given identifier. 
  ; ### Paramaters:
  ; * identifier - the function/variable/syntax you want documentation for.
  ; ### Returns:
  ; The file-path where the identifiers docs should be written / found.
  
  ; TODO: make subfolder be an array defaulting to an empty one
  (define (mdcd-file-for identifier #!optional (subfolder ""))
    (make-absolute-pathname 
      (list (get-mdcd-home) 
        subfolder)
      (mdcd-name-cleaner identifier)
      "md"))

  ; ## Private: mdcd-write-doc
  ; Writes the specified doc-string to the specified file-path
  ;
  ; ### Paramaters:
  ; * doc-string - the string of documentation
  ; * file-path - the path to the file that should be created / replaced
  ;
  ; ### Returns:
  ; Returns #t if successful
  (define (mdcd-write-doc doc-string file-path)
   (if (not (directory-exists? (pathname? file-path)))
    (create-pathname-directory file-path))
   (with-output-to-file file-path
    (lambda ()
     (display doc-string)
     (newline)))
    #t)


  (define (mdcd-path-for-var name)
    (mdcd-file-for name "variables"))

  (define (mdcd-path-for-fun name)
    (mdcd-file-for name "functions"))

  (define (mdcd-path-for-syntax name)
    (mdcd-file-for name "syntax"))

  (define (write-doc path-function name doc-string)
    (let ((file-path (path-function name)))
      (mdcd-write-doc doc-string file-path)
      file-path))
  ; see below for documentation
  (define (doc-fun name doc-string)
    (if (mdcd-enabled?)
      (write-doc mdcd-path-for-fun name doc-string)))
  ; We now have enough code to start eating our own dog food.
  ; YAY.
  (doc-fun "doc-fun" "## Public: doc-fun
  Generates documentation for a function.

  ### Paramaters:
  * name - a symbol representing the name of the function
  * doc-string - a markdown string documenting the function

  ### Returns:
  Returns the path to the newly written file")


  (doc-fun "doc-syntax" "## Public: doc-syntax
  Generates documentation for a syntax change.

  ### Paramaters:
  * mini-syntax-identifier - a small example of the resulting changes
  * doc-string - a markdown string documenting the function

  ### Returns:
  The path to the file where the docs were written.

  ### Notes:
  Picking a good `mini-syntax-identifier` is tricy because syntax changes
  typically don't have some standard symbol you can point to.
  If, for example you were to add Ruby style array initialization 
  syntax (e.g. [\"a\", \"b\"] ) you might choose `[...]` as your 
  `mini-syntax-identifier`. Just make an attempt to come as close to something
  referencable (like a method name) as possible.")
  (define (doc-syntax mini-syntax-identifier doc-string)
    (write-doc mdcd-path-for-syntax mini-syntax-identifier doc-string))

  (doc-fun "doc-var" "## Public: doc-var
  Generates documentation for a variable.
  Typically you would only use this for a variable of atypical significance
  that others should be made aware of 

  ### Paramaters:
  * name - a symbol representing the name of the variable
  * doc-string - a markdown string documenting the variable

  ### Returns:
  The path to the file where the docs were written.")
  (define (doc-var name doc-string)
    (write-doc mdcd-path-for-var name doc-string))

  (doc-fun "show-doc" "## Public: show-doc
  Displays the documentation for the specified key
  ### Parameters:
  * name - the name of the method/variable/syntax you want 
    documentation for.")
  (define (show-doc name)
    (let ((response-string (read-doc name)))
      (if response-string
        (display response-string)
        (display "Undocumented"))
    )
  )
  (define (show-description name)
    (show-section name 'description))
  (define (show-params name)
    (show-section name 'parameters))
  (define (show-returns name)
    (show-section name 'returns))
  (define (show-examples name)
    (show-section name 'examples))
  (define (show-notes name)
    (show-section name 'notes))

  (define (show-section name section)
    (let ((doc-string (read-doc name)))
      (if doc-string
        (for-each (lambda(x)(print x))
                  (ms:extract-section section doc-string))
        "Undocumented"
        )
      )
    )

  
  (define (read-doc name)
    (if (mdcd-enabled?)
      (call/cc (lambda(k)
                  (let ((paths (list
                                  (mdcd-path-for-fun name)
                                  (mdcd-path-for-var name)
                                  (mdcd-path-for-syntax name)))
                        (reader 
                          (lambda (x) 
                                    (if (file-exists? x)
                                        (k (read-all x))
                                        #f))))
                    (for-each reader paths))))
      "MDCD: Disabled")) 
      ; if get-mdcd-home is null it's been intentionally disabled.



); END module mdcd
