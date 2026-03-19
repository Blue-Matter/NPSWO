#' Save, List, and Load Hist Objects
#'
#' A matched set of functions for saving and loading [MSEtool::hist-class] objects to
#' disk. `SaveHist()` compresses and writes a `.hist` file locally. `ListHist()`
#' lists available `.hist` files in a directory. `LoadHist()` loads and expands
#' a saved `.hist` file back to full array dimensions.
#'
#' Output files are ignored by git and must be generated locally.
#'
#' @param Hist A [MSEtool::hist-class] object.
#' @param name Character. Name of the object (without extension), used as the
#'   file name for saving and loading.
#' @param path Character. Directory path where `.hist` files are saved and
#'   read from. Created recursively by `SaveHist()` if it does not exist.
#'   Default: `"Objects/Hist"`.
#' @param compress Logical. Whether to call [Compress()] on `Hist` before
#'   saving, reducing object size by dropping redundant array dimensions.
#'   Default: `TRUE`.
#' @param overwrite Logical. Whether to overwrite an existing file of the same
#'   name. Default: `TRUE`.
#' @param silent Logical. Whether to suppress the printed list of available
#'   objects. Default: `FALSE`.
#'
#' @return
#' - `SaveHist()`: Invisibly returns `NULL`. Called for its side-effect of
#'   writing `<path>/<name>.hist` to disk.
#' - `ListHist()`: Invisibly returns a character vector of available file names
#'   (without extension).
#' - `LoadHist()`: Returns an expanded [MSEtool::hist-class] object with array
#'   slots restored to full `nSim x nYear` dimensions via [Expand()].
#'
#' @seealso [Compress()], [Expand()], [MSEtool::Save()]
#'
#' @importFrom cli cli_abort cli_progress_step cli_inform cli_bullets
#' @importFrom tools file_path_sans_ext
#' @importFrom stats setNames
#' @name SaveLoadHist
#' @export
SaveHist <- function(Hist, name, path="Objects/Hist", compress=TRUE, overwrite=TRUE) {

  if (!inherits(Hist, 'hist'))
    cli::cli_abort("Hist must be an {.cls MSEtool::hist} object")

  if (!dir.exists(path))
    dir.create(path, recursive = TRUE)

  if (compress) {
    cli::cli_progress_step("Compressing {.val {name}}")
    Hist <- Compress(Hist)
    cli::cli_process_done()
  }

  path <- file.path(path, paste0(name, '.hist'))
  MSEtool::Save(Hist, path, overwrite)

}

#' @rdname SaveLoadHist
#' @export
ListHist <- function(path="Objects/Hist", silent=FALSE) {
  files <- tools::file_path_sans_ext(list.files(path))
  if (!silent) {
    cli::cli_text("Hist objects available in {.val {path}}:")
    cli::cli_bullets(stats::setNames(files, rep("*", length(files))))
  }

  invisible(files)
}

#' @rdname SaveLoadHist
#' @export
LoadHist <- function(name, path="Objects/Hist") {
  availHist <- ListHist(path, silent=TRUE)
  if (!name %in% availHist)
    cli::cli_abort(c("x"="{.val {name}} not found in {.val {path}}",
                     "i"="Available `Hist` objects are: {.val {availHist}}"))

  full.path <- file.path(path, paste0(name, '.hist'))

  cli::cli_progress_step("Loading {.val {full.path}}")

  Hist <- readRDS(full.path)

  cli::cli_progress_step("Expanding {.val {name}}")

  Expand(Hist)

}


