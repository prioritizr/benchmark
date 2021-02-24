#### Print traceback on error
if (!interactive()) {
  options(error = function() {
      traceback()
      q("no", 1, FALSE)
  })
}

#### load session management function
source("code/R/functions/session_path.R")

#### -- Packrat Autoloader (version 0.5.0) -- ####
source("packrat/init.R")
#### -- End Packrat Autoloader -- ####

### load gurobi package from system library
packrat::set_opts(external.packages  = "gurobi")
options(keep.source.pkgs = TRUE)

### set CRAN mirror
options(repos = "https://cran.rstudio.com/")
