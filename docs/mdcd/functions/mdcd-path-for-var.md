## [procedure] (mdcd-path-for-var name [module-name])
Calculates the path where the documentation for the specified variable will/does live.

### Parameters:
* name - a symbol representing the name of the variable
* module-name - (optional) name of the module this lives in

### Returns:
A string representation of the absolute path to the file.

### Examples:

```scheme
(mdcd-path-for-var "my-var" "my-module")
;=> /path/to/mdcd/home/variables/my-module/my-var.md
```

