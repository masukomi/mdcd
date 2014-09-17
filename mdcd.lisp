; # MCDC Mardown Code DoCs
; Allows you to markup your code with Markdown.
; Enables you to: 
; * request docs for any documented function in a REPL
; * generate markdown files for easy searchability 
;   via tools like spotlight
; * generate static html for distributing your documentation
;
; # Dependencies
; * cl-utilities

; two package files for this
; mdcd-package.lisp (the real one)
; mdcd-test-package.lisp (the one for testing with everything exported)

(in-package :mdcd)

(shadowing-import 'cl-utilities:with-gensyms)
(use-package :cl-utilities)

;(require-extension regex)

; ## Private: *mcdc-home*
; We just need to set this because Chicken Scheme
; has no mechanism for testing if a variable has 
; been defined or not. It can be overridden via the
; `set-mcdc-home` method.
(defparameter *mdcd-home* 
  (append (cdr (split-sequence #\/ (ext:getenv "HOME" ))) '("mdcd" "lisp")))

; ## Private *writes-enabled*
; Lets the system know if it's allowed to 
; write to the filesystem. Useful when testing
; or when deploying on a system where you don't want 
; or need the docs written out.
(defparameter *writes-enabled* t)

(defun enable-writes ()
"## Public: enabled-writes

Tells MDCD it is allowed to write to the filesystem.
This is the default state.

## Returns:
T"
  (defparameter *writes-enabled* t))

(defun disable-writes ()
"## Public: disable-writes
)
Tells MDCD it's not allowed towrite to the filesystem. 
Useful when testing or when deploying on a system where you don't want 
or need the docs written out. 

## Returns:
NULL"
  (defparameter *writes-enabled* NULL))

(defun set-mdcd-home (directory-list)
"## Public: set-mcdc-home directory-list
Sets the directory where MCDC files are stored

### Parameters:
* directory-list a list of directories.
  E.g.: '(\"home\" \"current_username\")

### Returns: 
The path to the directory it will save files to."

  (defparameter *mdcd-home* directory-list)
        (get-mdcd-home))
        ; (namestring (get-mdcd-home)) => converted to string

(defun get-mdcd-home ()
"## Public: get-mdcd-home
Returns the path to the directory where MDCD files will be stored.

### Returns:
The path to the directory where MDCD files will be stored.

### Note:
Defaults to $HOME/mdcd/scheme/"
  (make-pathname :directory `(:absolute ,@*mdcd-home*)))

(defun mdcd-name-cleaner (name)
"## Private: mdcd-name-cleaner name
converts method and variable names into something more filesystem friendly
### Parameters:
* name - the name to be cleaned.
### Returns:
The \"cleaned\" name."
  ; "[^\\w_\-]+" seemed like it was too likely to result
  ; in overlap. I could hash the name but I would prefer
  ; to keep the files as something meaningful to humans.
  (let ((cleaned name))
    (loop for bad-char in '(#\/ #\* #\| #\+)
      do (setf cleaned (substitute #\_ bad-char cleaned))
      )
    (setf cleaned (substitute #\/ #\: cleaned))
    (return-from mdcd-name-cleaner cleaned)))


(defun mdcd-file-for (identifier &optional (subfolder "") (item-type ""))
"## Public: mdcd-file-for identifier [subfolder] [item-type]
Provides the file-path for a given identifier. 

### Paramaters:
* identifier - the function/variable/syntax you want documentation for.
* subfolder - This is typically the package name extracted from the name
* item-type - see the \"doc\" function for details on the item-type param

### Returns:
The file-path where the identifiers docs should be written / found."

  (make-pathname :directory `(:absolute 
                              ,@*mdcd-home* 
                              ,subfolder
                              ,(if (equal "meta" item-type ) "" item-type))
                              :name
                              identifier
                              :type  "md"))

(defun mdcd-ensure-dir (a-pathname)
"## Private: mdcd-ensure-dir a-pathname
Internal method that ensures that the directory in the 
specified pathname actually exists so that we can write
a file there.

## Parameters:
* a-pathname - a pathname

## Returns:
The directory"
  (ensure-directories-exist 
      (make-pathname :directory 
        (pathname-directory a-pathname))))


(defun mdcd-write-doc (doc-string a-pathname )
"## Private: mdcd-write-doc doc-string a-pathname
Writes the specified doc-string to the specified file-path

### Paramaters:
* doc-string - the string of documentation
* a-pathname - the pathname where the filename that should be created / replaced

### Returns:
Returns true if successful"
  (mdcd-ensure-dir a-pathname)
  (let ((stream 
                (open a-pathname
                      :direction :output :if-exists :supersede)))
      (write-string doc-string stream)
      (close stream)))


(defun path-for (name item-type)
"## Private: path-for name item-type
Determines the path to where the markdown file should be saved for 
the item specified.

## Parameters:
* name - the name of the thing that's being documented
* item-type - see the \"doc\" function for details on the item-type param

## Returns:
A filepath"
  (let* (
        (name (mdcd-name-cleaner 
                  (string-downcase (if (typep name 'symbol)
                                       (symbol-name name) 
                                       name))))
                  ; ^^ makes the package delimiters look like path delimiters
        (split-name (split-sequence #\/ name))
        (subfolder (car split-name))
        (tail-name (nth 1 split-name))
        (item-type-string (string-downcase (symbol-name item-type)))
          ; ^^ treats the path delimiters like directory indicators
          ; 
          ; name should always look like a variable
          ; or a function and not have something on the end 
          ; that looks like a file extension.
          ; Yes, this may muck with people using 
          ; things like foo.txt as a method name
          ; but i think it should still get them the right docs
          ; in the end.

    ); END let vars
    
    ; subfolder is currently a pathname object
    ; need to convert it to a string
    (if (equal tail-name name)
        (setf subfolder ""))
    (if (and (eq item-type :meta) (null tail-name) )
          (setf tail-name "index"))
    ; name      : foo:bar
    ; tail-name : bar
    ; subfolder : foo
    ; item-type : FUNCTIONF
    (mdcd-file-for 
      tail-name                                 ; identifier
      subfolder                                 ; subfolder
      item-type-string ; item-type
      )))


(defun doc (name doc-string &optional &key (item-type :function) (grouping nil))
"## Public: doc name doc-string [keys: item-type, grouping]
Generates and saves documentation.

### Paramaters:
* name - a symbol or string representing the name of the function. 
* doc-string - a markdown string documenting the function
* Optional Named Parameters
  * item-type - A symbol. Defaults to :function but can be any of the following: 
    :function, :variable, :macro, :class, :meta. :meta is for use in documenting
    less specific things, like notes about the package. 

### Returns:
The doc-string passed in

### Notes:
The item-type and colons in names (packages) all to help segregate 
the stored files. In practice the colon in a function / variable's name 
and item-type should be more than enough.

### Examples:

    (doc 'my-function \"## Public: my-function\")
    (doc \"package:my-packaged-function\" \"## Public: my-packaged-function\")
    (doc 'my-function \"## Public: my-function\" 'function)"
 ; we can't set the documentation directly 
 ; because we want to use this in place of the standard 
 ; docstring , which means the method wouldn't exist yet
 ; to have its documentation set.
 ; instead we'll just use print
 ;(setf (documentation name 'function) doc-string)
 (if *writes-enabled*
  (let ((file-path (path-for name item-type)))
        (mdcd-write-doc doc-string file-path)))
 doc-string)

; We now have enough code to start eating our own dog food.
; YAY.
; Alas, there's nothing left that needs documenting.

; Document the existing things!
(doc "mdcd:set-mdcd-home" (documentation 'set-mdcd-home 'function))
(doc "mdcd:get-mdcd-home" (documentation 'get-mdcd-home 'function))
(doc "mdcd:mdcd-name-cleaner" (documentation 'mdcd-name-cleaner 'function))
(doc "mdcd:mdcd-file-for" (documentation 'mdcd-file-for 'function))
(doc "mdcd:mdcd-ensure-dir" (documentation 'mdcd-ensure-dir 'function))
(doc "mdcd:mdcd-write-doc" (documentation 'mdcd-write-doc 'function))
(doc "mdcd:path-for" (documentation 'path-for 'function))
(doc "mdcd:doc" (documentation 'doc 'function))

(defun show-doc (name &optional &key (item-type :function))
  #.(doc "mdcd:show-doc" "## Public: show-doc name [:item-type]
Loads and displays the documentation for the specified object.

### Parameters:
* name - The name of the function/variable/thing you're seeking docs for.
* Optional *Named* Parameters:
  * item-type - A symbol. Defaults to :function but can be any of the following: 
    :function, :variable, :macro, :class, :meta.

### Returns:
A string containing the documention requested or an indication of 
where it attempted to find it.
")
 (let ((file-path (path-for name item-type)))
      (let ((in (open file-path :if-does-not-exist nil))
            (response '()))
        (when in
          (loop for line = (read-line in nil)
               while line do (setf response (append response `(,line))))
          (close in))
        (if (> (length response) 0)
          (format nil "~{~A~^~% ~}" response)
          (progn
            (if nil
              (documentation name item-type)
                ; TODO fix doc call. those vars won't work
                ; They need to be converted to the appropriate type of symbols
              (format nil "No Docs Found at ~A" file-path)))
          ))))

; unit tested
(defun line-matches-section? (line line-number section)
  (if (and (equal 0 line-number) (equal section :description))
      (return-from line-matches-section? t)
      (and (not (not (search 
                      (symbol-name section)
                      ; upcasing line because symbol-name returns upcase string
                      (string-upcase line)))))))

(defun capture-good-line (line header-line section-found line-matches-section)
  (cond 
    ; line in wrong section
    ((and (not section-found) (not header-line)) 
      (return-from capture-good-line nil)) ; do nothing
    ; line in section we do want
    ((and section-found (not header-line))
      (return-from capture-good-line `(,line)))
    ; header of correct section
    ((and (not section-found) header-line) line-matches-section 
            (return-from capture-good-line `(,line) )
            )
    ; header of NEXT section (we don't want it)
    ((and section-found header-line)
      (return-from capture-good-line nil))
    (t (return-from capture-good-line nil)))) ; can't happen... i hope

(defun string-is-md-header? (a-string)
  (equal 0 (position #\# a-string)))

(defun find-header-lines (lines &optional (line-num -1))
  (map 'list (lambda (y) (car y)) ; get the line numbers from dotted pairs
    (remove-if-not (lambda (x) (cdr x)) ; remove non-dotted pairs
      ; generate a list of (line-num . t) and (line-num) entries
      (cond
        ((> (length lines) 1)
          (loop for i upto (length lines)
                for line = (nth i lines)
                collect (cons i (string-is-md-header? line))))
        (t `(,(cons (+ 1 line-num) (string-is-md-header? (car lines))))))
      )))


(defun extract-section (section doc-string)
  #.(doc "mdcd:extract-section" "## Private: extract-section
Extracts the specified section of a given doc string (if available).

### Parameters:
* section - A symbol: any of the following: 
  :description, :parameters, :returns, :notes, :examples. :description will 
  extract the first section which hopefully includes the name, visibility,
  and overview.
* doc-string - The doc string from which to attempt the extraction 
  of the specified section

### Returns: 
The extracted section of the docs." )
  (let* ((lines (split-sequence #\newline doc-string))
        (response '())
        (header-line-nums (find-header-lines lines))
        ) ;end let vars
    (do ((remaining-header-line-nums header-line-nums (cdr remaining-header-line-nums))
          (header-line-number (car header-line-nums) (car remaining-header-line-nums))
        )
      (nil)                       ;Do forever(ish).
      (if header-line-number
          (let ((line (nth header-line-number lines))
                (next-line (nth (+ 1 header-line-number) lines))
                (more-headers (> (length remaining-header-line-nums) 0))
                )
            (if (line-matches-section? line header-line-number section) 
                 (return-from extract-section
                    (format nil "~{~a~^~%~}" (if (and 
                          next-line ; if this is not the last line
                          more-headers) ; and there's a header after 
                          (loop for i from header-line-number to (- (car remaining-header-line-nums) 1) collect (nth i lines))
                          (loop for i from header-line-number to (- (length
                          lines) 1) collect (nth i lines)))  ))))
          (return nil)))))

(defun show-params (name &optional &key (item-type :function) )
  #.(doc "mdcd:show-params" "## Public: show-params name [keys: item-type]
Shows the Paramaters section of the specified item's docs.

## Parameters
* name - a symbol or string representing the name of the function. 
* Optional Named Parameters
  * item-type - A symbol. Defaults to :function but can be any of the following: 
    :function, :variable, :macro, :class, :meta. :meta is for use in documenting
    less specific things, like notes about the package.

## Returns:
A string or nil if no doc was found.
")
  (extract-section :parameters (show-doc name item-type )))

(defun show-returns (name &optional &key (item-type :function) )
  #.(doc "mdcd:show-returns" "## Public: show-returns name [keys: item-type]
Shows the Returns section of the specified item's docs.

## Parameters
* name - a symbol or string representing the name of the function. 
* Optional Named Param)ters
  * item-type - A symbol. Defaults to :function but can be any of the following: 
    :function, :variable, :macro, :class, :meta. :meta is for use in documenting
    less specific things, like notes about the package.

## Returns:
A string or nil if no doc was found.")
  (extract-section :returns (show-doc name item-type )))

(defun show-notes (name &optional &key (item-type :function) )
  #.(doc "mdcd:show-notes" "## Public: show-notes name [keys: item-type]
Shows the Notes section of the specified item's docs.

## Parameters
* name - a symbol or string representing the name of the function. 
* Optional Named Parameters
  * item-type - A symbol. Defaults to :function but can be any of the following: 
    :function, :variable, :macro, :class, :meta. :meta is for use in documenting
    less specific things, like notes about the package.

## Returns:
A string or nil if no doc was found.")
  (extract-section :returns (show-doc name item-type )))

(defun show-examples (name &optional &key (item-type :function) )
  #.(doc "mdcd:show-examples" "## Public: show-examples name [keys: item-type]
Shows the Examples section of the specified item's docs.

## Parameters
* name - a symbol or string representing the name of the function. 
* Optional Named Parameters
  * item-type - A symbol. Defaults to :function but can be any of the following: 
    :function, :variable, :macro, :class, :meta. :meta is for use in documenting
    less specific things, like notes about the package.

## Returns:
A string or nil if no doc was found.")
  (extract-section :examples (show-doc name item-type )))

(defun show-section (name section-symbol &optional &key (item-type :function))
  #.(doc "mdcd:show-section" "## Public: show-examples name section-symbol [keys: item-type]
Shows the specified section of the specified item's docs.

## Parameters
* name - a symbol or string representing the name of the function. 
* section-symbol - any symbol must be a case-insensitive match for the 
  header you're trying to display the corresponding section of. 
* Optional Named Parameters
  * item-type - A symbol. Defaults to :function but can be any of the following: 
    :function, :variable, :macro, :class, :meta. :meta is for use in documenting
    less specific things, like notes about the package.

## Returns:
A string or nil if no doc was found.

## Notes:
This function is useful if you regularly use a heading in your documentation
that doesn't match any of the conventional headers used by MDCD.

## Examples:

    (show-section \"mdcd:show-doc\" :parameters)

...would show the Parameters section of the mcdc:show-doc function
")
  (extract-section section-symbol (show-doc name item-type)))

(doc "mdcd" 

"# Markdown Code Documentation (mdcd)

A tool for generating and parsing code documentation for Common Lisp. 

MDCD allows you to write your [docstrings](http://en.wikipedia.org/wiki/Docstring#Lisp) (and other documentation) in Markdown, generate a markdown file for each one, and through the use of conventions retrieve just the portions of the documentation you need at the moment. 

## Limitations
MDCD is, itself, thoroughly documented using MDCD, but it does not *yet* convert the generated Markdown docs into static HTML. As a result, its online documentation is somewhat lacking. The irony is not lost. 

In the meantime, the documentation in `mdcd.lisp` is pretty complete, written in Markdown, and will auto-generate `.md` files for its functions in the default dir when you load it (see creating-docs). This is also a great way to see an example of what kind of files a well documented project will generate.

## Creating docs

By default mdcd docs are stored under `~/mdcd/lisp/` This can be changed by calling `set-mdcd-home` with a directory list: 

	(mdcd:set-mdcd-home '(\"home\" \"documentation\"))
	; gets converted to /home/documenation on a unix system. 

You can create MDCD docs in two ways: 

### Continue using standard CL docstrings and generate MDCD docs from them

	(defun greet( &optional (name \"stranger\") )
	  \"## Public: greet [name]
	Generates a greeting string.
	## Parameters:
	* name - (optional) the name of the person to greet
	## Returns:
	a string
	## Example:
	    (greet \"Bob\")
	## Notes: 
	This is *obviously* a contrived example.\"
	  
	  (format nil \"Hello ~A\" name))
	
	; now generate the markdown file 
	(mdcd:doc \"mdcd:doc\" (documentation 'greet 'function))

### Generate docs and docstrings simultaneously

	(defun greet( &optional (name \"stranger\") )
	  #.(mdcd:doc \"## Public: greet [name]
	Generates a greeting string.
	[All the same stuff as the example above...]\")
	  
	  (format nil \"Hello ~A\" name))


## Viewing docs
The standard `documentation` function will, of course still work, and provide you with the documentation written for whatever you specify. MDCD builds on this.

Yes, it has its own function for this, but using it is only necessary if you go beyond the bounds of what lisp's built in docstrings can do.

	(show-doc 'foo) ;=> simplest usage 
	
For example, you can create documentation for a package:

	(show-doc :my-package-name :meta) 
	; \"meta\" documentation is documentation about 
	; the system / package / grouping of things.
	


### Scoped Retrieval
Helpful docs frequently get a bit verbose. It's great to have all the information, but you don't want to have to sift through *all* of it every time you need to just see what paramaters a function takes, or what it returns. That's why MDCD has scoped retrieval helpers.

For example: want to see just the paramaters that the `foo` function accepts?

	(show-params 'foo)
	  ; namespaced function?
	(show-params \"namespace:function-name\")

Just need to know what it returns? 

	(show-returns 'foo)
	
Have some project specific documentation headers that aren't 
part of the defaults listed below? Then check out the `extract-section` function. All the `show-x` functions are just wrappers around it. You could use it straight-up:

	(extract-section :fooberries (show-doc :my-function))
	
Or use it create your own wrapper

	(show-fooberries :my-function)
   


## Conventions
In order to intelligently extract subsections of your documentation for scoped retrieval MDCD uses a convention of Markdown headers to denote
the start of each section.

* \"Parameters\" - starts a section explaining the parameters
  taken by a function
* \"Returns\" - starts a section indicating what a function returns
* \"Notes\" - starts a section of arbitrary notes
* \"Examples\" - starts a section of examples of the usage of the function

It doesn't particularly matter what header you use for the first 
line of a function's docs, but it's recommended that you go with 
\"Public\" or \"Private\" followed by a method signature. See MDCD's 
documentation for examples.

## Contributing
Pull Requests are happily accepted. I only ask that you make sure the unit tests (written with [cl-unit](http://tgutu.github.io/clunit/)) don't fail, and that any new functionality you add has a unit test to confirm it works. 

## About
MDCD was written by [masukomi](http://masukomi.org), because I love Markdown and the only Markdown based documentation system out there for CL ([docudown](http://common-lisp.net/projects/docudown/)) *had no documentation!!!* Never trust a skinny chef, and never trust a documentation tool that isn't documented. Not knowing how it worked, or what it could do, and wanting cool features like scoped retrieval. I set to work. "
  :item-type :meta)

