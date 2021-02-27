#' Warp a raster using GDAL
#'
#' This function is a wrapper for \code{\link[gdalUtils]{gdalwarp}}.
#'
#' @param x \code{link[raster]{RasterLayer-class}} object.
#'
#' @param y \code{link[raster]{RasterLayer-class}} object.
#'
#' @param method \code{character} name of interpolation method.
#'
#' @return \code{link[raster]{RasterLayer-class}} object.
gdal_warp_raster <- function(x, y, method = "bilinear") {
  assertthat::assert_that(inherits(x, "Raster"), inherits(y, "Raster"),
                          assertthat::is.string(method))
  f1 <- tempfile(fileext = ".tif")
  f2 <- tempfile(fileext = ".tif")
  raster::writeRaster(x, f1, NAflag = -9999, overwrite = TRUE)
  out <- raster::readAll(gdalUtils::gdalwarp(
      srcfile = f1,
      dstfile = f2,
      s_srs = x@crs@projargs,
      t_srs = y@crs@projargs,
      te = c(raster::xmin(y), raster::ymin(y),
             raster::xmax(y), raster::ymax(y)),
      te_srs = y@crs@projargs,
      tr = raster::res(y),
      r = method,
      srcnodata = -9999,
      dstnodata = -9999,
      of = "GTiff",
      co = "COMPRESS=LZW",
      overwrite = TRUE,
      output_Raster = TRUE,
      verbose = TRUE))
  unlink(f1, force = TRUE)
  unlink(f2, force = TRUE)
  out
}

#' Aggregate a raster using GDAL
#'
#' This function is a wrapper for \code{\link[gdalUtils]{gdalwarp}}.
#'
#' @param x \code{link[raster]{RasterLayer-class}} object.
#'
#' @param fact \code{integer} aggregation factor.
#'
#' @param method \code{character} name of interpolation method.
#'
#' @return \code{link[raster]{RasterLayer-class}} object.
gdal_aggregate_raster <- function(x, fact, method = "average") {
  assertthat::assert_that(inherits(x, "Raster"), assertthat::is.count(fact),
                          assertthat::is.count(fact))
  f1 <- tempfile(fileext = ".tif")
  f2 <- tempfile(fileext = ".tif")
  n_res <- raster::res(x) * fact
  n_ext <- list(
    xmin = raster::xmin(x),
    ymin = raster::ymin(x))
  n_ext$xmax <-
    n_ext$xmin + (ceiling(ncol(x) / fact) * n_res[[1]])
  n_ext$ymax <-
    n_ext$ymin + (ceiling(nrow(x) / fact) * n_res[[2]])
  raster::writeRaster(x, f1, NAflag = -9999, overwrite = TRUE)
  out <- raster::readAll(gdalUtils::gdalwarp(
      srcfile = f1,
      dstfile = f2,
      s_srs = x@crs@projargs,
      t_srs = x@crs@projargs,
      te = c(n_ext$xmin, n_ext$ymin, n_ext$xmax, n_ext$ymax),
      te_srs = x@crs@projargs,
      tr = n_res,
      r = method,
      srcnodata = -9999,
      dstnodata = -9999,
      of = "GTiff",
      co = "COMPRESS=LZW",
      overwrite = TRUE,
      output_Raster = TRUE,
      verbose = TRUE))
  unlink(f1, force = TRUE)
  unlink(f2, force = TRUE)
  out
}

#' Disaggregate a raster using GDAL
#'
#' This function is a wrapper for \code{\link[gdalUtils]{gdalwarp}}.
#'
#' @param x \code{link[raster]{RasterLayer-class}} object.
#'
#' @param fact \code{integer} aggregation factor.
#'
#' @param method \code{character} name of interpolation method.
#'
#' @return \code{link[raster]{RasterLayer-class}} object.
gdal_disaggregate_raster <- function(x, fact, method = "average") {
  assertthat::assert_that(inherits(x, "Raster"), assertthat::is.count(fact),
                          assertthat::is.count(fact))
  f1 <- tempfile(fileext = ".tif")
  f2 <- tempfile(fileext = ".tif")
  raster::writeRaster(x, f1, NAflag = -9999, overwrite = TRUE)
  out <- raster::readAll(gdalUtils::gdalwarp(
      srcfile = f1,
      dstfile = f2,
      s_srs = x@crs@projargs,
      t_srs = x@crs@projargs,
      te = c(raster::xmin(x), raster::ymin(x),
             raster::xmax(x), raster::ymax(x)),
      te_srs = x@crs@projargs,
      tr = raster::res(x) / fact,
      r = method,
      srcnodata = -9999,
      dstnodata = -9999,
      of = "GTiff",
      co = "COMPRESS=LZW",
      overwrite = TRUE,
      output_Raster = TRUE,
      verbose = TRUE))
  unlink(f1, force = TRUE)
  unlink(f2, force = TRUE)
  out
}
