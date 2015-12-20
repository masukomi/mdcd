## [procedure] set-mcdc-home
Sets the directory where MCDC files are stored

### Parameters:
* directory-list a list of directories.
  E.g.: '("home" "current_username")

### Returns: 
The path to the directory it will save files to.

### Examples:

	(set-mdcd-home '("Users" "masukomi" "my_project" "docs"))

