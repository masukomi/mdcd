## [procedure] (mdcd-path-for-fun mini-syntax-identifier [module-name])
Calculates the path where the documentation for the specified syntax will/does live.

### Parameters:
* mini-syntax-identifier - a small example of the resulting changes
* module-name - (optional) name of the module this lives in

### Returns:
A string representation of the absolute path to the file.

### Examples:

```scheme
(mdcd-path-for-var "my-syntax" "my-module")
;=> /path/to/mdcd/home/syntax/my-module/my-syntax.md
```

