## How to install HiveR

### To install from Github using R:

````r
install.packages("devtools")
library("devtools")
install_github(repo = "HiveR", username = "bryanhanson", branch = "master")
library("HiveR")
````
If you use `branch = "devel"` you can get the development branch if it is available.  Development branches have new, possibly incompletely tested features.  They may may also not be ready to pass checks and thus may not install automatically using the method above.  Check the news file to see what's up.  If you are interested in a particular feature in the devel branch, you can probably just grab the function of interest and source it into an existing package installation.

### From CRAN using R:

````r
chooseCRANmirror() # choose a CRAN mirror
install.packages("HiveR")	
library("HiveR")
````

### To get to the Vignette:

````r
vignette("HiveR")
````

Questions?  hanson@depauw.edu