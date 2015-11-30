# Markdown Code Documentation (MDCD)

Inline Markdown documentation for your code with REPL support and generated
files for public export.

MDCD takes Common Lisps idea of [docstrings](http://en.wikipedia.org/wiki/Docstring#Lisp) 
and takes it to the next level. 

With MDCD you're not limited to the full docstring for each function. `(show-doc "my-function")` You can extract any section of the docs: 
* the params: `(show-params "my-function")` 
* what it returns: `(show-returns "my-function")`
* example usage: `(show-example "my-function")`
* any notes: `(show-notes "my-function")`
* or some custom section `(show-section "my-function")`

Of course, those presume that you've actually documented 
that functionality.

## Limitations
MDCD is, itself, thoroughly documented using MDCD, but it does not 
*yet* convert the generated Markdown docs into static HTML. As a 
result, its online documentation is somewhat lacking. 

The irony is not lost. 

In the meantime, the documentation in `mdcd.import.scm` is pretty complete, 
written in Markdown, and will auto-generate `.md` files for its functions 
in the default dir when you load it (see creating-docs). This is also a 
great way to see an example of what kind of files a well documented 
project will generate.

## Creating docs

### Enabling File Creation
By default MDCD will _not_ write docs to the filesystem. This is because 
MDCD documentation comes from within your source code, and you
don't want every project that uses your library to have _its_ documentation
poluted with yours. 

So, you need to turn it on by giving it a list of directories that tell it
where to write to. If, for example you want it to write to the "docs" 
directory in the current folder you could say: 

	(set-mdcd-home 
		(append (string-split (current-directory) "/") '("docs")))

Because that's generally what you want, there's also a convenience function for
it.

	(mdcd-use-default-home)

### Documenting your code

Documenting your code is pretty simple. You call one of 3 functions: 

* `doc-fun` ( for documenting functions )
* `doc-var` ( for documenting variables )
* `doc-syntax` ( for documenting syntax )

Each of these take 2 parameters: 

* the name of the function / variable / syntax
* a string containing the documentation.

	(doc-fun "my-function" "some markdown documentation")

The documentation is divided into section by its headers. It is suggested that
you use H2s for the top level description, and H3s for the detailed parts. See
the example below.

In order to intelligently extract subsections of your documentation for scoped 
retrieval MDCD uses a convention of Markdown headers to denotethe start of each 
section. There are 5 offically supported sections: 

* Description - this is whatever comes first in the doc, not a specifically
  named header.
* "Parameters" - starts a section explaining the parameters
  taken by a function
* "Returns" - starts a section indicating what a function returns
* "Examples" - starts a section of examples of the usage of the function
* "Notes" - starts a section of arbitrary notes

It doesn't particularly matter what header you use for the first 
line of a function's docs, but it's recommended that you go with 
"Public" or "Private" followed by a method signature. See MDCD's 
documentation for examples.

Please note that, with the exception of "Description" **these are all plural**.
MDCD is looking for specific strings when trying to find the appropriate
section. Keeping everything plural makes it consistent and easier to find.

	(doc-fun "greet"
	  "## Public: greet [name]
	Generates a greeting string.

	### Parameters:
	* name - (optional) the name of the person to greet

	### Returns:
	a string

	### Examples:
	    (greet "Bob")
	
	### Notes:
	This is *obviously* a contrived example.")
	

Note that in the example above the `## Public: greet [name]` section is the 
"description" because it comes first. It is also what would be returned if 
you called `(show-description "greet")`.


## Viewing Docs
Viewing docs is just a matter of saying `(show-x "identifier")` where x is what
part of the docs you want to see.

* `show-doc` (show all the documentation)
* `show-description` (show the first section)
* `show-params` (show the "Parameters" section)
* `show-returns` (show the "Returns" section)
* `show-examples` (show the "Examples" section)
* `show-notes` (show the "Notes" section)


	(show-doc "my-function") ;=> simplest usage 

### Viewing Custom Sections

Under the covers `(show-returns "my-function") is just calling 
`(show-section "my-function" 'returns)`. The `show-section` function is
exposed so that you can view any non-standard sections that you like to use. 
For example: `(show-section "my-function" 'awesomeness)` would look for the
section with an "Awesomeness" header (case-insensitive) and return whatever's 
in it.


## Contributing
Pull Requests are happily accepted. I only ask that you make sure the unit tests 
(written with [the test egg](http://wiki.call-cc.org/eggref/4/test)) don't fail, 
and that any new functionality you add has a unit test to confirm it works. 

## About
MDCD was written by [masukomi](http://masukomi.org), because I love Markdown,
feel that good documentation is invaluable to the users of any library (and the
creators of that library months after they're written it) and wanted a better
tool for generating those docs.

