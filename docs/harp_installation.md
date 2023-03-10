# harp installation instructions

The instructions for installing harp can be found [here](https://harphub.github.io/harp/)
and in the [2022 tutorial page](https://harphub.github.io/harp-training-2022/about.html).
This should install fairly easily in the new atos supercomputer at ECMWF.

## Setup your environment (OPTIONAL)
As described in section 1.5 of this [tutorial](https://harphub.github.io/harp_tutorial),
it is not a bad idea to isolate your harp installation using the *renv* package. 
This might be a good idea if you want to test different versions of harp. 
The instructions should be as follows:
```
install.packages("renv")
renv::init()
renv::install("remotes")
remotes::install_github("harphub/harp")
remotes::install_github("harphub/Rgrib2")
renv::install("ncdf4")
renv::install("tidyverse")
renv::install("here")
renv::snapshot()
```

Things to note:
When using renv with an R script, add this line at the top of the script
(modify for your own path)
```
renv::load("/home/nhd/R/harpUserScripts")
```
See example in R/verification/point_verif.R

Note that by loading the renv environment and using Rscript inside it you might be missing some 
libraries that are not in the standard R base package loaded by renv.

There is an example `renv.lock` included here for an installation in aa (used nhd).
Rename it as `renv.lock` in your project directory and then use
```
renv::restore()
```
in R to reproduce the environment (untested!)
