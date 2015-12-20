(module mdcd-config
  (
    set-mdcd-home
    get-mdcd-home
    mdcd-enabled?
    mdcd-disable
    mdcd-use-default-home

    get-mdcd-home-dirs      ; mostly to facilitate testing 
    mdcd-default-home-dirs  ; but maybe someone else will find 
                            ; a use for them
   )
  (import chicken scheme)
  (use files data-structures posix)



  ; we want it to NOT write files by default
  ; because you don't want users of your code
  ; suddenly having your docs spewed into
  ; their systems
  (define *mdcd-home* '())

  ; NOTE THE FOLLOWING ARE NOT COMMENTED WITH MDCD
  ; because it hasn't been loaded yet. :/
  

  ; ## [procedure] set-mcdc-home
  ; Sets the directory where MCDC files are stored
  ; 
  ; ### Parameters:
  ; * directory-list a list of directories.
  ;   E.g.: '("home" "current_username")
  ; 
  ; ### Returns: 
  ; The path to the directory it will save files to.
  ;
  (define (set-mdcd-home directory-list)
    (set! *mdcd-home* directory-list)
    (get-mdcd-home))

  ; ## [procedure] get-mdcd-home
  ; Returns the path to the directory where MDCD files will be stored.
  ;
  ; ### Returns:
  ; The path to the directory where MDCD files will be stored.
  ;
  ; ### Notes:
  ; Defaults to "<current directory>/docs"
  ;
  ; ### Examples:
  ;
  ;     (get-mdcd-home)
  (define (get-mdcd-home)
    (make-absolute-pathname *mdcd-home* ""))

  (define (get-mdcd-home-dirs)
    *mdcd-home*)

  (define (mdcd-enabled?)
    (not (equal? '() *mdcd-home*)))

  (define (mdcd-disable)
    (set-mdcd-home '())
    #t)

  (define (mdcd-default-home-dirs)
    (append (string-split (current-directory) "/") '("docs")))

  (define (mdcd-use-default-home)
    (set-mdcd-home (mdcd-default-home-dirs)))

)
