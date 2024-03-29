# Markdown Code Documentation (MDCD)

Inline Markdown documentation for your code with REPL support and generated
files for public export.

MDCD takes Common Lisps idea of [docstrings](http://en.wikipedia.org/wiki/Docstring#Lisp) 
and takes it to the next level. 

With MDCD you're not limited to viewing the full documentation for each function. 
You can extract any section of it: 

* everything: `(show-doc "my-function")`
* the params: `(show-params "my-function")` 
* what it returns: `(show-returns "my-function")`
* example usage: `(show-example "my-function")`
* any notes: `(show-notes "my-function")`
* or some custom section `(show-section "my-function")`

Of course, this presumes that someone's actually documented the sections you're
trying to extract.

## Limitations
MDCD is, itself, thoroughly documented using MDCD, but it does not 
*yet* convert the generated Markdown docs into static HTML. As a 
result, its online documentation... could be better.

The irony is not lost. 

In the meantime, you can [view the rendered markdown on github](https://github.com/masukomi/mdcd/tree/master/docs/functions) It also serves as an example of 
the files that mdcd creates, since MDCD uses MDCD for its documentation (of
course).


## Configuration / Setup

```scheme
	(import mdcd-config) 
	; ^^^ must be loaded before mdcd itself.
	(mdcd-write-with-defaults) 
	; ^^^ should only be called when developing locally
	(import mdcd)
```

Because MDCD is primarily used from the REPL (hopefully regularly)
the methods are intentionally named in such a way that you shouldn't 
need to prefix them, thus saving you excess typing.

For more details on `(mdcd-use-default-home)` see Creating Docs below.


## Creating Docs

### Enabling File Creation
By default MDCD will _not_ write docs to the filesystem. This is because 
MDCD documentation comes from within your source code, and you
don't want every project that uses your library to have _its_ documentation
poluted with yours. 

So, you need to turn it on by giving it a list of directories that tell it
where to write to. If, for example you want it to write to the "docs" 
directory in the current folder you could say: 

```scheme
	(set-mdcd-home 
		(append 
			(string-split (current-directory) "/")
			'("docs")))
	; set-mdcd-home is part of mdcd-config
```

Because that's generally what you want, there's also a convenience function for
it:

```scheme
	(mdcd-use-default-home)
```

### Documenting your code

Documenting your code is pretty simple. You call one of 3 functions: 

* `doc-fun` ( for documenting functions )
* `doc-var` ( for documenting variables )
* `doc-syntax` ( for documenting syntax )

Each of these take 2 (or 3) parameters: 

* the name of the function / variable / syntax
* a string containing the documentation.
* (optional) the module name where the thing is

```scheme
	(doc-fun "my-function" "some markdown documentation")
	; or...
	(doc-fun "my-function" "some markdown documentation" "my-module")
```

Note: If the thing you're documenting is contained within a module it's important
to specify that when calling any of the `doc-*` functions. It will help keep the
files from mixing together, and multiple functions with the same name won't
overwrite each other's files.

The documentation is divided into section by its headers. It is suggested that
you use H2s (`## Foo`) for the top level description, and H3s (`### Bar`) for the detailed parts. See
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

Please note that, with the exception of "Description", **all headers are plural**.
MDCD is looking for specific strings when trying to find the appropriate
section. Keeping everything plural makes it consistent and easier to create
without mistakes.

```scheme
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
```

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

Note: When you call `show-*` and MDCD encounters multiple files with the same 
name it will show you the first line of each and ask you to choose which one to 
display the details of.

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
and Common Lisp's "docstrings", and feel that good documentation is invaluable 
to the creators, and users of any codebase. I wanted a better
tool for generating those docs. So, I wrote MDCD.

