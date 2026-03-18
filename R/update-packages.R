#' Load and Update Required Packages
#'
#' `UpdatePackages()` ensures that the required GitHub-hosted packages
#' (`MSEtool`, `rfishbase`, `FishLife`, and `NPSWO`) are installed. If
#' `update = TRUE`, already-installed packages are also passed to
#' `pak::pkg_install()`, which will upgrade them if the GitHub source has
#' newer changes. `CheckPak()` is a helper that ensures the
#' [`pak`](https://pak.r-lib.org/) package is available, installing it
#' from CRAN if necessary.
#'
#' @param update `logical`. If `TRUE` (default), `pak::pkg_install()` is called
#'   for all required packages regardless of whether they are already installed,
#'   allowing `pak` to upgrade them if the GitHub source has changed. If `FALSE`,
#'   only missing packages are installed.
#'
#' @details
#' Installation sources:
#' - `MSEtool`   — `blue-matter/MSEtool@prelease`
#' - `rfishbase` — `ropensci/rfishbase@fb-21.06` (v3.1.9.99, required by FishLife)
#' - `FishLife`  — `James-Thorson-NOAA/FishLife`
#' - `NPSWO`     — `blue-matter/NPSWO`
#'
#' Both functions return `NULL` invisibly and are called for their
#' side effects only.
#'
#' @return `NULL` (invisibly).
#'
#' @name UpdatePackages
#' @export
UpdatePackages <- function(update = TRUE) {

  msetool_installed   <- requireNamespace("MSEtool",   quietly = TRUE)
  fishlife_installed  <- requireNamespace("FishLife",  quietly = TRUE)
  npswo_installed     <- requireNamespace("NPSWO",     quietly = TRUE)
  npswo.om_installed     <- requireNamespace("NPSWO.OM", quietly = TRUE)

  needs_install <- !msetool_installed || !fishlife_installed || !npswo_installed ||
    !npswo.om_installed || update


  if (needs_install) {
    CheckPak()
    results <- list()

    if (!msetool_installed || update)
      results$msetool <- pak::pkg_install("blue-matter/MSEtool@prelease", ask = FALSE)

    if (!fishlife_installed || update)
      results$fishlife <- pak::pkg_install("James-Thorson-NOAA/FishLife", ask = FALSE)

    if (!npswo_installed || update)
      results$npswo <- pak::pkg_install("blue-matter/NPSWO", ask = FALSE)

    if (!npswo.om_installed || update)
      results$npswo.om <- pak::pkg_install("blue-matter/NPSWO.OM", ask = FALSE)

    was_updated <- results$msetool$type[1] != 'installed' ||
      results$fishlife$type[1] != 'installed' ||
      results$npswo$type[1] != 'installed' ||
      results$npswo.om$type[1] != 'installed'

    if (was_updated) {
      cli::cli_text('')
      cli::cli_alert_warning("Packages updated. Please restart your R session and run `library(NPSWO)`.")

    } else {
      cli::cli_alert_info("All packages are already up to date. No restart needed.")
    }
  }

  invisible(NULL)
}

#' @rdname UpdatePackages
#' @export
CheckPak <- function() {
  if (!requireNamespace("pak", quietly = TRUE)) install.packages("pak")
  invisible(NULL)
}
