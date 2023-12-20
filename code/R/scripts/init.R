# restore library
renv::restore()

# install gurobi R package
if (identical(.Platform$OS.type, "unix")) {
  path <-dir(
    paste0(Sys.getenv("GUROBI_HOME"), "/R"), "^.*\\.tar\\.gz",
    full.names = TRUE
  )
} else {
  path <-dir(
    paste0(Sys.getenv("GUROBI_HOME"), "/R"), "^.*\\.zip",
    full.names = TRUE
  )
}
if (nzchar(path)) {
  message("Found gurobi R package here:")
  message(paste0("  "), path)
} else {
  stop("Couldn't find gurobi R package via GUROBI_HOME environmental variable")
}
renv::install(path)

# ensure additional packages are included in renv (available from CRAN/GitHub)

# renv::install("cran/session")
f <- session::restore.session

# renv::install("piggyback")
f <- piggyback::pb_download

# renv::install("Rsymphony")
f <- Rsymphony::Rsymphony_solve_LP

# renv::install("bioc::lpsymphony")
f <- lpsymphony::lpsymphony_solve_LP

# renv::install("dirkschumacher/rcbc")
f <- rcbc::cbc_solve

# renv::install("cran/cplexAPI")
f <- cplexAPI::mipoptCPLEX

# renv::install("highs")
f <- highs::highs_solve

# gurobi R package on system
f <- gurobi::gurobi

# packages for progress bars
f <- doParallel::registerDoParallel
f <- foreach::foreach
f <- iterators::iter

# print success
message("successfully initialized packages!")
