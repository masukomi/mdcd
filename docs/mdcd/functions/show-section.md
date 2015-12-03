## Public: show-section
Displays "Returns" documentation of the specified name

### Parameters:
* name - the name of the method/variable/syntax you want 
  documentation for.
* section - a symbol that matches the name of the section
  you want information on. The symbol must match the name of 
  the section (case insensitive). E.g. `'paramaters` would be 
  specified to match the `### Parameters:` section.

### Returns:
The contents of the specified block (if present).

### Examples:
To return the Parameters section of `my-function` you would:

    (show-section "my-function" 'parameters)

If you wanted to do the same for a custom section you would:

    (show-section "my-function"  'custom-section-name)


