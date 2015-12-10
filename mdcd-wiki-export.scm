(module mdcd-wiki-export 
  (
    generate-wiki-page

    ; only exported for testing
    ; will dexport ;)
    read-file
    has-x?
    get-the-doc
    append-to-doc
   )
  (import chicken scheme srfi-13)
  (use srfi-1 files utils data-structures posix mdcd-config markdown-svnwiki)

  (define (generate-wiki-page output-file-path #!optional (module '()))
    (if (not (mdcd-enabled?))
      (print "MDCD is disabled. Please set the home dir.")
      (begin
        (display "UNFINISHED...")
        (append-with-divider-if-not-empty (read-readme))
        (add-subfolders-to-doc '("functions" "variables" "syntax") module)
        (append-with-divider-if-not-empty (read-license))
        (export-the-doc output-file-path)
      )
    )
  )
  (define (export-the-doc output-file-path)
    (delete-file* output-file-path)
    (let ((raw-doc (get-the-doc)))
      (call-with-output-file output-file-path
        (lambda (output-port)
          (current-output-port output-port)
          (markdown->svnwiki raw-doc) ) #:append)))

  (define (add-subfolders-to-doc subfolders #!optional (module '()))
    (if (not (null? subfolders))
      (begin
        (let ((subfolder (car subfolders)))
          (if (has-x? subfolder module)
            (begin
            (let ((full-dir (get-full-dir-for-x subfolder module)))
              (let ((files (find-files full-dir)))
              (add-files-to-doc files)))
            )
            (display "\nXXX subfolder not found")
          )
        )
        (add-subfolders-to-doc (cdr subfolders) module)
      )
    )
  )
  (define (add-files-to-doc files)
    (if (not (null? files))
      (begin 
        (let ((file (car files)))
          (display (conc "\nXXX File: " file))
          (append-to-doc (list (read-all file)))
        )
        (add-files-to-doc (cdr files))
      )
    )
  )

  (define *THE_DOC* '())

  (define (append-to-doc . lines)
    (let ((cleaned-lines 
            (filter (lambda(x)(not (equal? "" x))) lines)))
      (if (> (length cleaned-lines) 0)
        (set! *THE_DOC* (append *THE_DOC* (flatten lines)))))
    #t)

  (define (append-with-divider-if-not-empty x)
    (if (not (equal? "" x))
      (append-to-doc x "\n----\n")))

  (define (get-the-doc)
    (string-join (flatten *THE_DOC*) "\n"))

  (define (read-file file)
    (if (file-exists? file)
      (read-all file)
      ""))

  (define (get-full-dir-for-x x #!optional (module '()))
    (string-concatenate 
              (list (get-mdcd-home) 
                    (if (not (null? module)) (conc module "/") "")
                    x  )))

  (define (get-files-for-x x #!optional (module '()))
    (if (has-x? x module)
      (directory (get-full-dir-for-x x module))
      '()))

  (define (has-x? x #!optional (module '()))
    ; does the x dir exist?
    (let ((full-dir 
            (get-full-dir-for-x x module)))
      (if (directory-exists? full-dir)
        (> (length (directory full-dir)) 0)
        #f
          )))
  (define (read-readme)
    (let ((file "README.md"))
      (read-file file)))
  (define (read-license)
    (let ((file "LICENSE.md"))
      (read-file file)))
  
)
