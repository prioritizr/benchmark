#' Get solver versions
#'
#' Generate a table with the software version numbers for the solvers.
#'
#' @return A `data.frame` object.
#'
#' @export
get_solver_versions <- function() {
  # return results
  tibble::tibble(
    solver = c(
      "SYMPHONY (Rsymphony)",
      "SYMPHONY (lpsymphony)",
      "CBC",
      "HiGHS",
      "IBM CPLEX",
      "Gurobi"
    ),
    version = c(
      get_rsymphony_version(),
      get_lpsymphony_version(),
      get_cbc_version(),
      get_highs_version(),
      get_cplex_version(),
      get_gurobi_version()
    )
  )
}

get_rsymphony_version <- function() {
  if (!requireNamespace("Rsymphony")) return(NA_character_)
  if (!identical(.Platform$OS.type, "unix")) return(NA_character_)
  f <- system("locate SymConfig.h", intern = TRUE)
  if (!nzchar(f) || !file.exists(f)) stop("Couldn't find SYMPHONY!")
  v <- readLines(f)
  v <- v[grepl(" SYMPHONY_VERSION ", v)]
  v <- gsub("\"", "", v, fixed = TRUE)
  v <- strsplit(v, " ", fixed = TRUE)[[1]][[3]]
  v
}

get_lpsymphony_version <- function() {
  if (!requireNamespace("lpsymphony")) return(NA_character_)
  bc_version <- BiocManager::version()
  bc_version <- gsub(".", "_", bc_version, fixed = TRUE)
  url <- paste0(
    "https://code.bioconductor.org/browse/lpsymphony/raw/RELEASE_",
    bc_version, "/src/SYMPHONY/include/coin/SymConfig.h"
  )
  v <- readLines(url)
  v <- v[grepl(" SYMPHONY_VERSION ", v)]
  v <- gsub("\"", "", v, fixed = TRUE)
  v <- strsplit(v, " ", fixed = TRUE)[[1]][[3]]
  v
}

get_highs_version <- function() {
  if (!requireNamespace("highs")) return(NA_character_)
  if (!identical(.Platform$OS.type, "unix")) return(NA_character_)
  pkg_version <- as.character(packageVersion("highs"))
  pkg_version <- strsplit(pkg_version, ".", fixed = TRUE)
  pkg_version <- paste0(
    pkg_version[[1]][[1]], ".",
    pkg_version[[1]][[2]], "-",
    pkg_version[[1]][[3]]
  )
  url <- paste0(
    "https://raw.githubusercontent.com/cran/highs/",
    pkg_version, "/inst/HiGHS/HConfig.h.bazel"
  )
  v <- readLines(url)
  v1 <- v[grepl("HIGHS_VERSION_MAJOR", v)]
  v2 <- v[grepl("HIGHS_VERSION_MINOR", v)]
  v3 <- v[grepl("HIGHS_VERSION_PATCH", v)]
  v1 <- strsplit(v1, " ", fixed = TRUE)[[1]][[3]]
  v2 <- strsplit(v2, " ", fixed = TRUE)[[1]][[3]]
  v3 <- strsplit(v3, " ", fixed = TRUE)[[1]][[3]]
  paste0(v1, ".", v2, ".", v3)
}

get_cplex_version <- function() {
  if (!requireNamespace("cplexAPI")) return(NA_character_)
  cplex_env <- cplexAPI::openEnvCPLEX()
  v <- cplexAPI::getVersionCPLEX(cplex_env)
  cplexAPI::closeEnvCPLEX(cplex_env)
  v
}

get_cbc_version <- function() {
  if (!requireNamespace("cplexAPI")) return(NA_character_)
  r_code <- "rcbc::cbc_solve(obj = c(1, 2), mat = matrix(c(1, 1), ncol = 2, nrow = 1), is_integer = c(TRUE, TRUE), row_lb = -Inf, row_ub = 1, max = TRUE, col_lb = c(0, 0), col_ub = c(1, 1), cbc_args = list('SEC' = '1'))"
  r_exe <- ifelse(
    identical(.Platform$OS.type, "unix"),
    file.path(R.home("bin"), "R"),
    file.path(R.home("bin"), "R.exe")
  )
  withr::with_dir(
    getwd(),
    log <- system(paste0(r_exe, " --slave -e \"", r_code,"\""), intern = TRUE)
  )
  v <- log[startsWith(log, "Version")]
  if (length(v) != 1) stop("Couldn't find CBC version!")
  v <- trimws(v)
  v <- strsplit(v, " ")[[1]][[2]]
  v
}

get_gurobi_version <- function() {
  if (!requireNamespace("gurobi")) return(NA_character_)
  model <- list()
  model$obj <- c(1, 1, 2)
  model$modelsense <- "max"
  model$rhs <- c(4, 1)
  model$sense <- c("<", ">")
  model$vtype <- "B"
  model$A <- matrix(c(1, 2, 3, 1, 1, 0), nrow = 2, ncol = 3, byrow = TRUE)
  f <- tempfile(fileext = ".log")
  g <- gurobi::gurobi(model, params = list(LogFile = f))
  log <- readLines(f)
  v <- log[startsWith(log, "Gurobi")]
  if (length(v) == 0) stop("Couldn't find Gurobi version!")
  v <- v[[1]]
  v <- trimws(v)
  v <- strsplit(v, " ")[[1]][[2]]
  v
}
