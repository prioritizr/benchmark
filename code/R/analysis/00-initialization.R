# set seed for reproducibility
set.seed(500)

# parse parameter settings
pars <- commandArgs(TRUE)
if (length(pars) > 0) {
  cat(pars)
  if (grepl("MODE", pars))
    MODE <- strsplit(grep("MODE", pars, value = TRUE),
                     "=", fixed = TRUE)[[1]][[2]]
}

# load packages
library(magrittr)
library(sf)
library(dplyr)

# load parameters
general_parameters <- RcppTOML::parseTOML("code/parameters/general.toml")
general_parameters <- general_parameters[[MODE]]

# load miscellaneous functions
source("code/R/functions/misc.R")

# load spatial data manipulation functions
source("code/R/functions/gdal_functions.R")

# store gurobi version
gurobi_version <- as.character(packageVersion("gurobi"))

# save session
session::save.session(session_path("00"), compress = FALSE)
