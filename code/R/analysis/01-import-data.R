# restore session
session::restore.session(session_path("01"))

# species metadata
spp_data <-
  "data/raw/species/nlpcc_species.csv"
  readr::read_csv(
    col_types = readr::cols(
      .default = readr::col_double(), pu = readr::col_integer()))

# planning unit data
pu_data <-
  "data/raw/planning-units/nplcc_cost_occupancy.zip"
  readr::read_csv(
    col_types = readr::cols(
      .default = readr::col_double(), pu = readr::col_integer()))

# planning unit raster rdata
pu_raster_data <-
  "data/raw/planning-units/nlpcc-planning-units.tif" %>%
  raster::raster()

# save results
## planning unit data
pu_data_path <- "data/intermediate/pu.tif"
saveRDS(pu_data, pu_data_path, compress = "xz")

## planning unit raster data
pu_raster_data_path <- "data/intermediate/pu.tif"
raster::writeRaster(
  pu_raster_data, pu_raster_data_path,
  overwrite = TRUE, NAflag = -9999)


# save session
session::save.session(session_path("00"), compress = "xz")

p
