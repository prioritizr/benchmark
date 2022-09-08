# restore library
renv::restore()

# install packages from source (not available online)
renv::install("/opt/gurobi952/linux64/R/gurobi_9.5-2_R_4.2.0.tar.gz")
renv::install(normalizePath("code/sources/highs_0.1-3.tar.gz"))

# ensure additional packages are included in renv (available from CRAN/GitHub)

# renv::install("Rsymphony")
f <- Rsymphony::Rsymphony_solve_LP

# renv::install("rgdal")
f <- rgdal::readOGR

# renv::install("piggyback")
f <- piggyback::pb_download

# renv::install("bioc::lpsymphony")
f <- lpsymphony::Rsymphony_solve_LP

# renv::install("dirkschumacher/rcbc")
f <- rcbc::cbc_solve

# renv::install("cran/cplexAPI")
f <- cplexAPI::mipoptCPLEX

# renv::install("cran/session")
f <- session::restore.session

# print success
message("successfully initialized packages!")
