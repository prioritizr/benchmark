# restore session
restore_session("01")

# import parameters
data_parameters <-
  RcppTOML::parseTOML("code/parameters/data.toml")[[MODE]]

# import data
full_pu_data <- readRDS(full_pu_data_path)
full_pu_raster_data <-
  full_pu_raster_data_path %>%
  terra::rast()

# generate different sized planning units for benchmark analysis
## calculate aggregation factors
pu_size_factors <-
  (data_parameters$planning_unit_size) /
  terra::xres(full_pu_raster_data)
pu_size_factors <- round(pu_size_factors) # avoid floating point issues

## validate aggregation factors
assertthat::assert_that(all(pu_size_factors >= 1))

## find indices of planning units in full dataset
idx <- terra::cells(is.na(full_pu_raster_data), 0)[[1]]

## set non-planning unit indices to -1 so that aggregating/disaggregating
## data works
full_pu_raster_data[is.na(full_pu_raster_data)] <- -1

## create a list with the planning unit data at different resolutions
## each list contains a list of containing the planning unit data
## in raster format and also data.frame format
pu_output <-
  seq_along(pu_size_factors) %>%
  plyr::llply(.progress = "text", function(i) {
    ## print resolution for debugging
    message("")
    message("starting resolution = ", data_parameters$planning_unit_size[i])
    message("")
    x <- pu_size_factors[i]
    ## create aggregated dataset
    r <- terra::aggregate(full_pu_raster_data, fact = x, fun = "max")
    ## assign new planning unit ids
    ids <- terra::cells(r > 0, 1)[[1]]
    r[ids] <- ids
    ## disaggregate raster to match resolution original raster
    r2 <- terra::disagg(r, fact = x)
    ## crop raster to match original raster
    r2 <- terra::crop(r2, terra::ext(full_pu_raster_data))
    r2[r2 < 0] <- NA_real_
    ## sanity checks
    terra::compareGeom(r2, full_pu_raster_data, stopOnError = FALSE)
    assertthat::assert_that(all(!is.na(r2[idx])))
    ## create planning unit data for given resolution, including spp data
    d <-
      full_pu_data %>%
      dplyr::mutate(pu = r2[idx][[1]]) %>%
      dplyr::group_by(pu) %>%
      dplyr::summarize_all(sum) %>%
      dplyr::ungroup()
    ## rescale cost values in raster so that maximum value is 10000
    d$cost <- (d$cost / max(d$cost)) * 1e4
    ## validate result
    assertthat::assert_that(
      nrow(d) == length(ids),
      msg = "failed to create dataset with different resolution"
    )
    ## create raster to store planning units
    r <- terra::setValues(r, NA_real_)
    r[ids] <- 1
    ## return result
    list(data = d, raster = r)
  })

## extract raster data for planning units
pu_raster_data <-
  lapply(pu_output, `[[`, "raster")

## extract data.frame data for planning units
pu_data <-
  lapply(pu_output, `[[`, "data")

# calculate number of planning units within each dataset
pu_data_n <- vapply(pu_data, nrow, integer(1))

# prepare boundary length matrix data for benchmark analysis
bd_data <-
  pu_raster_data %>%
  lapply(function(x) {
    ## create boundary matrix
    m <- prioritizr::boundary_matrix(x)
    ## rescale boundary data
    m <- prioritizr::rescale_matrix(m)
    ## find indices of planning units
    idx <- terra::cells(is.na(x), 0)[[1]]
    ## subset matrix to only include planning units
    m[idx, idx]
  })

# save results
## planning unit data
## note that we save these as separate files later to reduce memory
## consumption and during benchmark analysis
pu_data_paths <-
  paste0("data/intermediate/pu-", seq_along(pu_size_factors), ".rds")
invisible(lapply(seq_along(pu_data_paths), function(i) {
  saveRDS(pu_data[[i]], pu_data_paths[[i]], compress = FALSE)
}))

## planning unit raster data
pu_raster_data_paths <-
  paste0("data/intermediate/pu-", seq_along(pu_size_factors), ".tif")
invisible(lapply(seq_along(pu_size_factors), function(i) {
  terra::writeRaster(
    pu_raster_data[[i]],
    pu_raster_data_paths[[i]],
    overwrite = TRUE,
    NAflag =-9999
  )
}))

## boundary data
bd_data_paths <-
  paste0("data/intermediate/bd-", seq_along(bd_data), ".rds")
invisible(lapply(seq_along(bd_data_paths), function(i) {
  saveRDS(bd_data[[i]], bd_data_paths[[i]], compress = FALSE)
}))

# clean up
rm(
  full_pu_data, full_pu_raster_data,
  idx, pu_data, pu_raster_data, pu_output
)

# save session
save_session("02")
