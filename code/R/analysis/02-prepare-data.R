# restore session
session::restore.session(session_path("01"))

# import parameters
data_parameters <-
  RcppTOML::parseTOML("code/parameters/data.toml")[[MODE]]

# generate different targets for benchmark analysis
{for (i in seq_along()) {
  spp_data  
data_parameters
}}

# generate different sized planning units for benchmark analysis


# save session
session::save.session(session_path("02"), compress = "xz")
