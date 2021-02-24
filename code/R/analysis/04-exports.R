# save session
session::save.session("data/final/results.rda")

# copy the results file to the results directory
file.copy("data/final/results.rda", "results/results.rda")

# create a zip file in the results directory with the solutions
withr::with_dir(
  "data/intermediate", zip("solutions.zip", "solutions")

# move zip file to results directory
file.copy("data/intermediate/solutions.zip", "results/solutions.zip")
file.remove("data/intermediate/solutions.zip")
