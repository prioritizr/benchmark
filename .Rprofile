# setup renv
source("renv/activate.R")

# print traceback on error
if (!interactive()) {
  options(error = function() {
      traceback()
      q("no", 1, FALSE)
  })
}

# load session management functions
source("code/R/functions/session.R")

# set CRAN mirror
options(repos = "https://cran.rstudio.com/")
