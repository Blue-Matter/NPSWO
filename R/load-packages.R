#' Load and Update Required Packages
#'
#' @description
#' `LoadPackages()` ensures that the required GitHub-hosted packages
#' (`MSEtool` and `NPSWO`) are installed. If `update = TRUE`, already-installed
#' packages are also passed to `pak::pkg_install()`, which will upgrade them if
#' the GitHub source has newer changes. `CheckPak()` is a helper that ensures
#' the [`pak`](https://pak.r-lib.org/) package is available, installing it
#' from CRAN if necessary.
#'
#' @param update `logical`. If `TRUE` (default), `pak::pkg_install()` is called
#'   for all required packages regardless of whether they are already installed,
#'   allowing `pak` to upgrade them if the GitHub source has changed. If `FALSE`,
#'   only missing packages are installed.
#'
#' @details
#' Installation sources:
#' - `MSEtool` — `blue-matter/MSEtool@prelease`
#' - `NPSWO`   — `blue-matter/NPSWO`
#'
#' Both functions return `NULL` invisibly and are called for their
#' side effects only.
#'
#' @return `NULL` (invisibly).
#'
#' @examples
#' \dontrun{
#' # Install missing packages and update any that have changed upstream
#' LoadPackages()
#'
#' # Install missing packages only, skip update check
#' LoadPackages(update = FALSE)
#'
#' # Ensure pak is available
#' CheckPak()
#' }
#'
#' @name LoadPackages
#' @export
LoadPackages <- function(update = TRUE) {

  msetool_installed <- requireNamespace("MSEtool", quietly = TRUE)
  npswo_installed   <- requireNamespace("NPSWO",   quietly = TRUE)

  needs_install <- !msetool_installed || !npswo_installed || update

  if (needs_install) {
    CheckPak()
    if (!msetool_installed || update) pak::pkg_install("blue-matter/MSEtool@prelease")
    if (!npswo_installed   || update) pak::pkg_install("blue-matter/NPSWO")
  }

  invisible(NULL)
}

#' @rdname LoadPackages
#' @export
CheckPak <- function() {
  if (!requireNamespace("pak", quietly = TRUE)) install.packages("pak")
  invisible(NULL)
}
