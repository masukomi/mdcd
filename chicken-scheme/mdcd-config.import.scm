(module mdcd-config
  (
    set-mdcd-home
    get-mdcd-home
    mdcd-enabled?
    mdcd-disable

    get-mdcd-home-dirs      ; mostly to facilitate testing 
    mdcd-default-home-dirs  ; but maybe someone else will find 
                            ; a use for them
   )
  (import chicken)
  (import scheme)
  (use files)




  ; we want it to NOT write files by default
  ; because you don't want users of your code
  ; suddenly having your docs spewed into
  ; their systems
  (define *mdcd-home* '())

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
  ; Defaults to $HOME/mdcd/scheme/
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
    (list (get-environment-variable "HOME" )
          "mdcd" "scheme"))

)
