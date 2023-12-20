# set seed for reproducibility
set.seed(500)

# parse parameter settings
pars <- commandArgs(TRUE)
if (length(pars) > 0) {
  cat(pars)
  if (grepl("MODE", pars))
    MODE <- strsplit(
      grep("MODE", pars, value = TRUE),
      "=",
      fixed = TRUE
    )[[1]][[2]]
}

# load packages
library(magrittr)
library(sf)
library(dplyr)

# load parameters
general_parameters <- RcppTOML::parseTOML("code/parameters/general.toml")
general_parameters <- general_parameters[[MODE]]

# store gurobi version
gurobi_version <- as.character(packageVersion("gurobi"))

# save session
save_session("00")
