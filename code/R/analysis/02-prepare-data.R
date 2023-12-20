# restore session
restore_session("01")

# import parameters
data_parameters <-
  RcppTOML::parseTOML("code/parameters/data.toml")[[MODE]]

# import data
full_pu_data <- readRDS(full_pu_data_path)
full_pu_raster_data <- terra::rast(full_pu_raster_data_path)

# generate different sized planning units for benchmark analysis
## calculate aggregation factors
pu_size_factors <-
  (data_parameters$planning_unit_size) /
  terra::xres(full_pu_raster_data)
pu_size_factors <- round(pu_size_factors) # avoid floating point issues

## validate aggregation factors
assertthat::assert_that(all(pu_size_factors >= 1))

## find coordinates of planning units in full dataset
pu_xy <- terra::xyFromCell(full_pu_raster_data, full_pu_data$pu)

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
    ## create aggregated dataset
    x <- pu_size_factors[i]
    r <- terra::aggregate(full_pu_raster_data, fact = x, fun = "max")
    ## get indices for planning units
    idx <- terra::cellFromXY(r, pu_xy)
    ## create planning unit ids
    r_idx <- unique(idx)
    r_ids <- seq_along(r_idx)
    r[] <- NA_real_
    r[r_idx] <- r_ids
    ## sanity checks
    assertthat::assert_that(all(!is.na(r[idx][[1]])))
    ## create planning unit data for given resolution, including spp data
    d <-
      full_pu_data %>%
      dplyr::mutate(pu = r[idx][[1]]) %>%
      dplyr::group_by(pu) %>%
      dplyr::summarize_all(sum) %>%
      dplyr::ungroup()
    ## rescale cost values in raster so that maximum value is 10000
    d$cost <- (d$cost / max(d$cost)) * 1e4
    ## validate result
    assertthat::assert_that(
      nrow(d) == length(r_ids),
      msg = "failed to create dataset with different resolution"
    )
    ## create raster to store planning units
    r <- terra::setValues(r, NA_real_)
    r[r_idx] <- 1
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

# calculate total cost of different sized datasets
pu_total_cost <- lapply(pu_data, function(x) sum(x$cost, na.rm = TRUE))

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
