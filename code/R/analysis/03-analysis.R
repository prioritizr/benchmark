# restore session
session::restore.session(session_path("02"))

# import parameters
bm_parameters <-
  RcppTOML::parseTOML("code/parameters/benchmark.toml")[[MODE]]

# perform benchmark analysis

# save session
session::save.session(session_path("03"), compress = "xz")
