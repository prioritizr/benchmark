#' File path for session data files
#'
#' This function generates a file name for an intermediate session
#' file based on its numerical identifier.
#'
#' @param x \code{character} identifier.
#'
#' @return \code{character} file path.
#'
#' @examples
#' session_path("01")
session_path <- function(x) {
  file.path("data/intermediate",
            raster::extension(dir("code/R/analysis",
                              paste0("^", x, ".*$"))[1], ".rda"))
}

#' Restore session
#'
#' This function restores the session at a step in the analysis pipeline.
#'
#' @param x `character` identifier for the session.
#'  Defaults to "final" to load the final results.
#'
#' @return Invisible `TRUE` indicating success.
restore_session <- function(x = "final") {
  # assert valid argument
  assertthat::assert_that(assertthat::is.string(x))
  # determine file path to import
  if (identical(x, "final")) {
    path <- "data/final/results.rda"
    if (!file.exists(path)) stop("Final results have not been computed.")
  } else {
    path <- session_path(x)
  }
  # restore session
  if (is.na(path) || !nzchar(path)) stop("Couldn't determine path.")
  if (!file.exists(path)) stop("Results have not been computed.")
  try(
    withr::with_environment(
      .GlobalEnv,
      session::restore.session(path)
    )
  )
  # return success
  invisible(TRUE)
}

#' Save session
#'
#' This function save the session at a step in the analysis pipeline.
#'
#' @param x `character` identifier for the session.
#'
#' @return Invisible `TRUE` indicating success.
save_session <- function(x) {
  # assert valid argument
  assertthat::assert_that(assertthat::is.string(x))
  # determine file path to import
  if (identical(x, "final")) {
    path <- "data/final/results.rda"
  } else {
    path <- session_path(x)
  }
  # save session
  if (is.na(path) || !nzchar(path)) stop("Couldn't determine path.")
  session::save.session(path, envir = .GlobalEnv, compress = "xz")
  # return success
  invisible(TRUE)
}
