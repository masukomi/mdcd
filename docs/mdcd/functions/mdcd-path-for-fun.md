## [procedure] (mdcd-path-for-fun name [module-name])
Calculates the path where the documentation for the specified function will/does live.

### Parameters:
* name - a symbol representing the name of the function
* module-name - (optional) name of the module this lives in

### Returns:
A string representation of the absolute path to the file.

### Examples:

```scheme
(mdcd-path-for-var "my-function" "my-module")
;=> /path/to/mdcd/home/functions/my-module/my-function.md
```

