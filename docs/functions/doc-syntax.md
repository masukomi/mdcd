## Public: doc-syntax
Generates documentation for a syntax change.

### Parameters:
* mini-syntax-identifier - a small example of the resulting changes
* doc-string - a markdown string documenting the function

### Returns:
The path to the file where the docs were written.

### Notes:
Picking a good `mini-syntax-identifier` is tricky because syntax changes
typically don't have some standard symbol you can point to.
If, for example you were to add Ruby style array initialization 
syntax (e.g. ["a", "b"] ) you might choose `[...]` as your 
`mini-syntax-identifier`. Just make an attempt to come as close to something
referencable (like a method name) as possible.
