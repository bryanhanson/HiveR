## How to install HiveR

### From CRAN using R:

````r
chooseCRANmirror() # choose a CRAN mirror
install.packages("HiveR")
library("HiveR")
````

### To install from Github using R:

````r
install.packages("remotes")
library("remotes")
install_github(repo = "bryanhanson/HiveR@master")
library("HiveR")
````

If you use `@some_other_branch` you can get other branches that might be available.  They may or may not pass CRAN checks and thus may not install automatically using the method above.  Check the NEWS file to see what's up.

### To see the Vignette:

````r
browseVignette("HiveR")
````
### License Information

HiveR is distributed under the GPL-3 license, as stated in the DESCRIPTION file.  For more info, see the [GPL site.](https://gnu.org/licenses/gpl.html)

Questions?  hanson@depauw.edu
