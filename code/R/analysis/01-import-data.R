# restore session
restore_session("00")

# import parameters
import_parameters <-
  RcppTOML::parseTOML("code/parameters/import.toml")[[MODE]]

# species metadata
## import data
spp_data <-
  "data/raw/species/nplcc_species.csv" %>%
  readr::read_csv(col_types = readr::cols(id = readr::col_integer())) %>%
  rename(code = species_code) %>%
  select(-id)

## validate data
assertthat::assert_that(
  assertthat::has_name(spp_data, "code"),
  assertthat::has_name(spp_data, "common_name"),
  assertthat::has_name(spp_data, "scientific_name"),
  identical(anyDuplicated(spp_data$code), 0L)
)

# full planning unit data
## import data
full_pu_data <-
  "data/raw/planning-units/nplcc_cost_occupancy.zip" %>%
  readr::read_csv(
    n_max = as.numeric(import_parameters$n_max_pu),
    col_types = readr::cols(
      .default = readr::col_double(), pu = readr::col_integer())
    )

## validate data
assertthat::assert_that(
  assertthat::has_name(full_pu_data, "pu"),
  assertthat::has_name(full_pu_data, "cost"),
  all(assertthat::has_name(full_pu_data, spp_data$code)),
  identical(anyDuplicated(full_pu_data$pu), 0L),
  nrow(full_pu_data) > 0
)

# planning unit raster data
## import data
full_pu_raster_data <-
  "data/raw/planning-units/nplcc_planning_units.tif" %>%
  terra::rast()

## fix extent for raster so that planning units are actually 100 x 100
## (source dataset has planning units that are slightly different
##  from 100 x 100 meters due to floating point issues)
### create new extent
fixed_res <- round(terra::res(full_pu_raster_data))
fixed_ext <-
  tibble::tibble(
    xmin = terra::xmin(full_pu_raster_data),
    ymin = terra::ymin(full_pu_raster_data)
  ) %>%
  mutate(
    xmax = xmin + (ncol(full_pu_raster_data) * fixed_res[1]),
    ymax = ymin + (nrow(full_pu_raster_data) * fixed_res[2])
  ) %>%
  dplyr::select(xmin, xmax, ymin, ymax) %>%
  as.data.frame() %>%
  as.matrix() %>%
  c() %>%
  terra::ext()

### manually overwrite extent,
### note that this is functionally equivalent to using nearest neighbor
### re-sampling -- given the assumption that the resolution for the
### original dataset is very, very close to 100 x 100 -- but manually
## overwriting is much much faster tan using nearest neighbor resampling
terra::ext(full_pu_raster_data) <- fixed_ext

## verify that
assertthat::assert_that(all(terra::res(full_pu_raster_data) == fixed_res))

## exclude planning units not present in pu data (for debugging)
full_pu_raster_data <-
  full_pu_raster_data %>%
  terra::setValues(NA_real_) %>%
  `[<-`(full_pu_data$pu, value = 1)

## validate data
assertthat::assert_that(
  is.finite(terra::global(full_pu_raster_data, "min", na.rm = TRUE)[[1]]),
  is.finite(terra::global(full_pu_raster_data, "max", na.rm = TRUE)[[1]]),
  abs(
    terra::xres(full_pu_raster_data) - terra::yres(full_pu_raster_data)
  ) <= 1
)

# save results
## planning unit data
full_pu_data_path <- "data/intermediate/full-pu.rds"
saveRDS(full_pu_data, full_pu_data_path, compress = FALSE)

## planning unit raster data
full_pu_raster_data_path <- "data/intermediate/full-pu.tif"
terra::writeRaster(
  full_pu_raster_data,
  full_pu_raster_data_path,
  overwrite = TRUE,
  NAflag = -9999
)

# clean up
rm(full_pu_raster_data, full_pu_data)

# save session
save_session("01")
