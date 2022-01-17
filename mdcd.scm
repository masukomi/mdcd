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
    mdcd-name-cleaner
    mdcd-forget

    )
  (import scheme)
  (import chicken.base)
  (import chicken.file)
  (import chicken.file.posix)
  (import chicken.format)
  (import chicken.io) ; for read-string
  (import chicken.port)
  (import chicken.pathname)
  (import chicken.string)
  (import directory-utils) ; can NOT manage to install this anymore
  (import regex)
  (import srfi-1)
  (import srfi-13)
  (import srfi-69)
  (import listicles) ; for many things
  (import masufiles) ; for file->string
  (import simple-loops)
  (import mdcd-config)
  (import (prefix mdcd-sections ms:))

;;;  ;(import symbol-utils); for unbound?
  (define **mdcd-memory** (make-hash-table equal?))
  (define **mdcd-memory-types** '("functions" "syntax" "variables"))
  (define (ensure-namespace-in-memory namespace)
    (if (not (hash-table-exists? **mdcd-memory** namespace))
        (let ((sub-hash (make-hash-table equal?)))
          (hash-table-set! sub-hash "functions" (make-hash-table equal?))
          (hash-table-set! sub-hash "syntax"    (make-hash-table equal?))
          (hash-table-set! sub-hash "variables" (make-hash-table equal?))
          (hash-table-set! **mdcd-memory** namespace sub-hash)
          )))
  (define (remember namespace datatype name doc-string)
    (ensure-namespace-in-memory namespace)
    (hash-table-set!
     (hash-table-ref
      (hash-table-ref **mdcd-memory** namespace)
      datatype)
     name
     doc-string))

  (define (recall namespace datatype name)
    (hash-table-ref/default
     (hash-table-ref
      (hash-table-ref **mdcd-memory** namespace)
      datatype)
     name
     '()))

  (define (mdcd-forget namespace datatype name)
    ; Is there a difference between forgetting something,
    ; and remembering nothing?
    (remember namespace datatype name '())
    (if (mdcd-write-enabled?)
        (let ((file-path (mdcd-file-for name datatype namespace)))
          (if (file-exists? file-path)
            (delete-file file-path)))))

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

  (define (mdcd-file-name name)
    (string-concatenate (list (mdcd-name-cleaner name) ".md")))

  (define (mdcd-file-for identifier subfolder #!optional (module-name '()))
    (make-absolute-pathname
      (list (get-mdcd-home) module-name subfolder)
      (mdcd-name-cleaner identifier)
      "md"))

  ; ## Private: mdcd-write-doc
  ; Writes the specified doc-string to the specified file-path
  ;
  ; ### Parameters:
  ; * doc-string - the string of documentation
  ; * file-path - the path to the file that should be created / replaced
  ;
  ; ### Returns:
  ; Returns #t if successful
  (define (mdcd-write-doc doc-string file-path)
    (if (mdcd-write-enabled?)
      (begin
       (if (not (directory-exists? file-path))
        (create-pathname-directory file-path))
       (with-output-to-file file-path
        (lambda ()
         (display doc-string)
         (newline)))

        #t)
      #f)
    )

  (define (write-doc path-function name doc-string #!optional (module-name ""))
    (let ((file-path (path-function name module-name)))
      (mdcd-write-doc doc-string file-path)
      file-path))

  (define (mdcd-path-for-var name #!optional (module-name ""))
    (mdcd-file-for name "variables" module-name))

  (define (mdcd-path-for-fun name #!optional (module-name ""))
    (mdcd-file-for name "functions" module-name))

  (define (mdcd-path-for-syntax name #!optional (module-name ""))
    (mdcd-file-for name "syntax" module-name))


  ; see below for documentation
  (define (doc-fun name doc-string #!optional (module-name ""))
    (remember module-name "functions" name doc-string)
    (write-doc mdcd-path-for-fun name doc-string module-name))
  ; We now have enough code to start eating our own dog food.
  ; YAY.
  (doc-fun "doc-fun" "## Public: doc-fun name doc-string [module-name]
Generates documentation for a function.

### Parameters:
* name - a symbol representing the name of the function
* doc-string - a markdown string documenting the function
* module-name - (optional) name of the module this lives in

### Returns:
Returns the path to the newly written file" "mdcd")


  (doc-fun "doc-syntax"
"## Public: doc-syntax mini-syntax-identifier doc-string [module-name]
Generates documentation for a syntax change.

### Parameters:
* mini-syntax-identifier - a small example of the resulting changes
* doc-string - a markdown string documenting the function
* module-name - (optional) name of the module this lives in

### Returns:
The path to the file where the docs were written.

### Notes:
Picking a good `mini-syntax-identifier` is tricky because syntax changes
typically don't have some standard symbol you can point to.
If, for example you were to add Ruby style array initialization
syntax (e.g. [\"a\", \"b\"] ) you might choose `[...]` as your
`mini-syntax-identifier`. Just make an attempt to come as close to something
referencable (like a method name) as possible." "mdcd")
  (define (doc-syntax mini-syntax-identifier doc-string #!optional (module-name ""))
    (remember module-name "syntax" mini-syntax-identifier doc-string)
    (write-doc
      mdcd-path-for-syntax
      mini-syntax-identifier
      doc-string
      module-name))

  (doc-fun "doc-var" "## Public: doc-var name doc-string [module-name]
Generates documentation for a variable.
Typically you would only use this for a variable of atypical significance
that others should be made aware of

### Parameters:
* name - a symbol representing the name of the variable
* doc-string - a markdown string documenting the variable
* module-name - (optional) name of the module this lives in


### Returns:
The path to the file where the docs were written." "mdcd")
  (define (doc-var name doc-string #!optional (module-name ""))
    (remember module-name "variables" name doc-string)
    (write-doc mdcd-path-for-var name doc-string module-name))

  (doc-fun "show-doc" "## Public: show-doc
Sends the documentation for the specified name to standard out
via the `display` function. Typically only used in the REPL.

### Parameters:
* name - the name of the method/variable/syntax you want
  documentation for.

### Returns:
... whatever `display` returns. Still not sure what that is.

### Examples:
`(show-doc \"my-function\")`
" "mdcd")

  (define (show-doc name )
    (let ((doc-string (extract-doc-from-memory
                       (read-and-choose-memory name))))
      (if (not (null? doc-string))
          doc-string
          "Undocumented")))

    ;; (let ((response-string
    ;;        (read-and-choose-doc (mdcd-file-name name))))
    ;;   (if response-string
    ;;     (display response-string)
    ;;     (display "Undocumented"));<-- can't happen
    ;; ))

  (doc-fun "show-description" "## Public: show-description
Displays the description documentation for the specified name

### Parameters:
* name - the name of the method/variable/syntax you want
  documentation for.

### Returns:
The first section of documentation for the specified name.
Typically this is the `## Public: function-name ...` block.

### Examples:
  `(show-description \"my-function\")`

" "mdcd")
  (define (show-description name)
    (show-section name 'description))

  (doc-fun "show-params" "## Public: show-params
Displays the \"Parameters\" documentation for the specified name

### Parameters:
* name - the name of the method/variable/syntax you want
  documentation for.

### Returns:
The contents of the `### Parameters:...` block (if present).

### Examples:
  `(show-params \"my-function\")`

" "mdcd")
  (define (show-params name)
    (show-section name 'parameters))

  (doc-fun "show-returns" "## Public: show-returns
Displays \"Returns\" documentation of the specified name

### Parameters:
* name - the name of the method/variable/syntax you want
  documentation for.

### Returns:
The contents of the `### Returns:...` block (if present).

### Examples:
  `(show-returns \"my-function\")`

" "mdcd")
  (define (show-returns name)
    (show-section name 'returns))


  (doc-fun "show-examples" "## Public: show-examples
Displays \"Examples\" documentation of the specified name

### Parameters:
* name - the name of the method/variable/syntax you want
  documentation for.

### Returns:
The contents of the `### Examples:...` block (if present).

### Examples:
  `(show-examples \"my-function\")`

" "mdcd")
  (define (show-examples name)
    (show-section name 'examples))


  (doc-fun "show-notes" "## Public: show-notes
Displays \"Returns\" documentation of the specified name

### Parameters:
* name - the name of the method/variable/syntax you want
  documentation for.

### Returns:
The contents of the `### Notes:...` block (if present).

### Examples:
  `(show-notes \"my-function\")`

" "mdcd")

  (define (show-notes name)
    (show-section name 'notes))

  (doc-fun "show-section" "## Public: show-section
Displays \"Returns\" documentation of the specified name

### Parameters:
* name - the name of the method/variable/syntax you want
  documentation for.
* section - a symbol that matches the name of the section
  you want information on. The symbol must match the name of
  the section (case insensitive). E.g. `'paramaters` would be
  specified to match the `### Parameters:` section.

### Returns:
The contents of the specified block (if present).

### Examples:
To return the Parameters section of `my-function` you would:

    (show-section \"my-function\" 'parameters)

If you wanted to do the same for a custom section you would:

    (show-section \"my-function\"  'custom-section-name)

" "mdcd")
  (define (show-section name section)
    (let* (
           (memory (read-and-choose-memory name))
           (doc-string (extract-doc-from-memory memory))
          )
        (if (not (null? doc-string))
            (string-join (ms:extract-section section doc-string) "\n")
            "Undocumented")))

  (define (extract-doc-from-memory memory)
    (if (not (null? memory))
        (nth1 3 memory)
        '()))

    (define (pick-a-number min max)
      (write-line "Which one?")
      (let ((n (read-line)))
        (if n
          (let ((num (string->number n)))
            (if (and num (<= num max) (>= num min))
              num
              (pick-a-number min max))))))


    (define (choose-doc docs)
          (if (eq? 1 (length docs))
            (car docs)
            (begin
              (write-line (sprintf
                            "there were ~A docs matching that name"
                            (length docs)))
              (let ((idx -1))
                (for-each (lambda(x)
                            (begin
                              (set! idx (+ idx 1))
                              (print (sprintf "~A) ~A" idx
                                     (car (string-split x "\n" #t))
                                     ))))
                  docs))
              (nth (pick-a-number 0 (length docs)) docs))))

  (define (read-and-choose-memory name)
    ; iterate over all the namespaces
    (let* (
           ; (((from namespace 1)) ((from namespace 2)))
           (memories (find-memories name))
           (num-memories (length memories))
           )
      (if (> num-memories 1) ; choose one and display it
          (nth1 3 (choose-memory memories))
          (if (> num-memories 0) ; display the only one
            (car memories)
            '()
          )

      )
    )
  )


  (define (choose-memory memories)
    (if (eq? 1 (length memories)) (car memories)
      (begin
        (write-line
          (sprintf "There were ~A docs matching that name" (length memories)))
        (display-options
          memories
          (lambda(index list-item)
            (print (sprintf "~A) ~A ~A"
                    ;index   namespace       datatype
                    ;1) mdcd doc-fun
                    index (car list-item) (car list-item)
                    ))))
        (nth1 (pick-a-number 1 (length memories)) memories))))


  (define (display-options options-list print-lambda count)
    (if (> (length options-list) 0)
      (let ((incremented-count (+ 1 count)))
        (print-lambda incremented-count (car options-list))
        (display-options (cdr options-list) print-lambda incremented-count)
        )))


  ; ## find-memories
  ; Finds all memories for the given name
  ; looks in functions, syntax, and variables
  ;
  (define (find-memories name #!optional (accumulator '()) (namespaces (hash-table-keys **mdcd-memory**)))
    (if (eq? 0 (length namespaces))
        accumulator
        (let ((from-namespace
                (find-memories-in-namespace name (car namespaces))
                ))
          (if (not (null? from-namespace))
              (find-memories name (append accumulator from-namespace) (cdr namespaces) )
              (find-memories name accumulator (cdr namespaces))
              )
          )
    )
  )

  ; ## find-memories-in-namespace
  ; Finds all the memories matching "name" in the given namespace
  ; ### Returns a list of namespace, datatype, docstring tuples
  (define (find-memories-in-namespace name namespace)
    (let* ((memories
            (map
              (lambda(datatype)(list namespace datatype
                                      (recall namespace datatype name)))
              **mdcd-memory-types** )
            )
          (filtered-memories
           (filter
              (lambda(lst)(not (null? (nth1 3 lst))))
              (map
                (lambda(datatype)(list namespace datatype (recall namespace datatype name)))
                  **mdcd-memory-types** )) )
          )

      (if (not (null? filtered-memories))
          (flatten-namespace-memories filtered-memories)
          '()
          )))


  (define (flatten-namespace-memories memories )
    (let ((accumulator '()))
      (do-list memory memories
        (set! accumulator
          (append-list
            accumulator
            memory)))
     accumulator
     ))


    ;; (filter
    ;;   (lambda(lst)(not (null? (nth1 3 lst))))
    ;;    (map
    ;;      (lambda(datatype)(list namespace datatype (recall namespace datatype name)))
    ;;        **mdcd-memory-types** )))

  ;; (define (read-and-choose-doc name)
  ;;   (let ((docs (read-doc name)))
  ;;       (if (> (length docs) 0)
  ;;         (choose-doc docs)
  ;;         (car docs))))

  (define (find-paths-with-name name)
    (find-files (get-mdcd-home)
        test: (lambda(filepath)
          (equal? name (car (reverse (string-split filepath "/")))))))



); END module mdcd
