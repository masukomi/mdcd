## Public: read-doc
Returns the documentation for the specified name as a string.
Searches for the specified name under functions, vars, and finally
syntax. Returs the first one that is encountered.

### Parameters:
* name - the name of the function/variable/syntax you want
  documentation for.

### Returns:
The complete documentation for the specified item as a string

### Examples:
`(read-doc "my-function")`

