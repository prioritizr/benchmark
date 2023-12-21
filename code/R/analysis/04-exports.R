# restore session
restore_session("03")

# load functions
source("code/R/functions/get_solver_versions.R")

# verify that no SpatRaster objects in session
assertthat::assert_that(
  !any(sapply(ls(), function(x) inherits(get(x), "SpatRaster"))),
  msg = "SpatRaster objects still in session, these should be removed"
)

# save session
save_session("final")

# copy the results file to the results directory
file.copy(
  "data/final/results.rda",
  "results/results.rda",
  overwrite = TRUE
)

# create a zip file in the results directory with the solutions
if (file.exists("data/intermediate/solutions.zip")) {
  file.remove("data/intermediate/solutions.zip")
}
withr::with_dir(
  "data/intermediate",
  zip("solutions.zip", "solutions")
)

# move zip file to results directory
file.copy(
  "data/intermediate/solutions.zip",
  "results/solutions.zip",
  overwrite = TRUE
)
file.remove("data/intermediate/solutions.zip")

# create table with software versions
solver_version_data <- get_solver_versions()

# save solver versions
write.table(
  solver_version_data,
  "results/solver_versions.csv",
  row.names = FALSE,
  sep = ","
)
