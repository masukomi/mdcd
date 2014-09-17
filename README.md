# Markdown Code Documentation (mdcd)

A tool for generating and parsing code documentation for Common Lisp. 

MDCD allows you to write your [docstrings](http://en.wikipedia.org/wiki/Docstring#Lisp) (and other documentation) in Markdown, generate a markdown file for each one, and through the use of conventions retrieve just the portions of the documentation you need at the moment. 

## Limitations
MDCD is, itself, thoroughly documented using MDCD, but it does not *yet* convert the generated Markdown docs into static HTML. As a result, its online documentation is somewhat lacking. The irony is not lost. 

In the meantime, the documentation in `mdcd.lisp` is pretty complete, written in Markdown, and will auto-generate `.md` files for its functions in the default dir when you load it (see creating-docs). This is also a great way to see an example of what kind of files a well documented project will generate.

## Creating docs

By default mdcd docs are stored under `~/mdcd/lisp/` This can be changed by calling `set-mdcd-home` with a directory list: 

	(mdcd:set-mdcd-home '("home" "documentation"))
	; gets converted to /home/documenation on a unix system. 

You can create MDCD docs in two ways: 

### Continue using standard CL docstrings and generate MDCD docs from them

	(defun greet( &optional (name "stranger") )
	  "## Public: greet [name]
	Generates a greeting string.
	## Parameters:
	* name - (optional) the name of the person to greet
	## Returns:
	a string
	## Example:
	    (greet "Bob")
	## Notes: 
	This is *obviously* a contrived example."
	  
	  (format nil "Hello ~A" name))
	
	; now generate the markdown file 
	(mdcd:doc "mdcd:doc" (documentation 'greet 'function))

### Generate docs and docstrings simultaneously

	(defun greet( &optional (name "stranger") )
	  #.(mdcd:doc "## Public: greet [name]
	Generates a greeting string.
	[All the same stuff as the example above...]")
	  
	  (format nil "Hello ~A" name))


## Viewing docs
The standard `documentation` function will, of course still work, and provide you with the documentation written for whatever you specify. MDCD builds on this.

Yes, it has its own function for this, but using it is only necessary if you go beyond the bounds of what lisp's built in docstrings can do.

	(show-doc 'foo) ;=> simplest usage 
	
For example, you can create documentation for a package:

	(show-doc :my-package-name :meta) 
	; "meta" documentation is documentation about 
	; the system / package / grouping of things.
	


### Scoped Retrieval
Helpful docs frequently get a bit verbose. It's great to have all the information, but you don't want to have to sift through *all* of it every time you need to just see what paramaters a function takes, or what it returns. That's why MDCD has scoped retrieval helpers.

For example: want to see just the paramaters that the `foo` function accepts?

	(show-params 'foo)
	  ; namespaced function?
	(show-params "namespace:function-name")

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

* "Parameters" - starts a section explaining the parameters
  taken by a function
* "Returns" - starts a section indicating what a function returns
* "Notes" - starts a section of arbitrary notes
* "Examples" - starts a section of examples of the usage of the function

It doesn't particularly matter what header you use for the first 
line of a function's docs, but it's recommended that you go with 
"Public" or "Private" followed by a method signature. See MDCD's 
documentation for examples.

## Contributing
Pull Requests are happily accepted. I only ask that you make sure the unit tests (written with [cl-unit](http://tgutu.github.io/clunit/)) don't fail, and that any new functionality you add has a unit test to confirm it works. 

## About
MDCD was written by [masukomi](http://masukomi.org), because I love Markdown and the only Markdown based documentation system out there for CL ([docudown](http://common-lisp.net/projects/docudown/)) *had no documentation!!!* Never trust a skinny chef, and never trust a documentation tool that isn't documented. Not knowing how it worked, or what it could do, and wanting cool features like scoped retrieval. I set to work. 