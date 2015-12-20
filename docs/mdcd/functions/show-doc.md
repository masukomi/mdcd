## [procedure] (show-doc name)
Sends the documentation for the specified name to standard out
via the `display` function. Typically only used in the REPL.

### Parameters:
* name - the name of the method/variable/syntax you want 
  documentation for.

### Returns:
... whatever `display` returns. Still not sure what that is.

### Examples:
`(show-doc "my-function")`

