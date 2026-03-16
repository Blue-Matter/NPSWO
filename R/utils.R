find_exe <- function() {
  if (.Platform$OS.type != "windows")
    cli::cli_abort("This function is currently only supported on {.val Windows}.", .internal=TRUE)
  system.file("ss3.exe", package = "NPSWO")
}
