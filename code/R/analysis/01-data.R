# restore session
session::restore.session(session_path("01"))

# save session
session::save.session(session_path("00"), compress = "xz")
