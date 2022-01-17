(module mdcd-config
  (
    set-mdcd-home
    get-mdcd-home
    mdcd-disable-write
    mdcd-enable-write
    mdcd-write-enabled?
    mdcd-write-with-defaults

    get-mdcd-home-dirs      ; mostly to facilitate testing 
    mdcd-default-home-dirs  ; but maybe someone else will find 
                            ; a use for them
   )
  (import chicken.base)
  (import chicken.file)
  (import chicken.file.posix)
  (import chicken.pathname)
  (import chicken.process-context)
  (import chicken.string)
  (import scheme)


  ; we want it to NOT write files by default
  ; because you don't want users of your code
  ; suddenly having your docs spewed into
  ; their systems
  (define *mdcd-home* '())

  (define *mdcd-write* #f)


  ; ## Public: set-mcdc-home
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

  ; ## Public: get-mdcd-home
  ; Returns the path to the directory where MDCD files will be stored.
  ; 
  ; ### Returns:
  ; The path to the directory where MDCD files will be stored.
  ; 
  ; ### Note:
  ; Defaults to "<current directory>/docs"
  (define (get-mdcd-home)
    (make-absolute-pathname *mdcd-home* ""))

  (define (get-mdcd-home-dirs)
    *mdcd-home*)

  (define (mdcd-write-enabled?)
    *mdcd-write*)

  (define (mdcd-disable-write)
    (set! *mdcd-write* #f)
    #t)

  (define (mdcd-enable-write)
    (set! *mdcd-write* #t)
    #t)
  
  (define (mdcd-default-home-dirs)
    (append (string-split (current-directory) "/") '("docs")))

  (define (mdcd-write-with-defaults)
    (mdcd-enable-write)
    (set-mdcd-home (mdcd-default-home-dirs)))

)
