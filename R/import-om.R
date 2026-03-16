
#' Import SS3 Outputs
#'
#' Imports Stock Synthesis 3 (SS3) model outputs from simulation subdirectories
#'
#'
#' @param OM_Name Character. Name of the operating model, used to locate the
#'   parent directory containing numbered SS3 simulation subdirectories (as
#'   created by [CreateSSDirectories()]).
#' @param outdir Character. Path to the root conditioning directory where
#'   simulation subdirectories are stored (as created by
#'   [CreateSSDirectories()]). Default: `"Condition"`.
#' @param parallel Logical. Whether to import SS3 reports in parallel using
#'   available cores (as determined by [MSEtool::CheckParallel()]).
#'   Default: `TRUE`.
#' @param nmax Integer or `NULL`. If provided, limits execution to the first
#'   `nmax` subdirectories. Useful for testing. Default: `NULL` (run all).
#'
#' @return A list of SS3 outputs from each directory in `file.path(outdir, OM_Name)`
#'
#' @seealso [CreateSSDirectories()], [MSEtool::ImportSSReport()], [MSEtool::ImportSS()],
#'
#' @export
ImportRepList <- function(OM_Name,
                     outdir = 'Condition',
                     parallel = TRUE,
                     nmax = NULL

) {

  path <- file.path(outdir, OM_Name)
  SSDirs <- list.dirs(path, recursive=FALSE)

  if (!is.null(nmax))
    SSDirs <- SSDirs[seq_len(min(length(SS3Dirs), nmax))]

  parallel <- MSEtool::CheckParallel(parallel)
  MSEtool::ImportSSReport(SSDirs, parallel = parallel)
}
