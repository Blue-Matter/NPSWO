#' Save an OM Object to the NPSWO.OM Data Package
#'
#' Optionally compresses an [MSEtool::om-class] object via [Compress()], then
#' writes it to the `data/` directory of the NPSWO.OM package as a compressed
#' `.rda` file. Optionally also saves a local copy as a `.om` file in
#' `Objects/OM/`. An `OM_` prefix is added to `name` automatically if not
#' already present.
#'
#' @param OM An [MSEtool::OM()] object.
#' @param name Character. Name for the saved object and `.rda` file (e.g.
#'   `"Base"` or `"OM_Base"`). The `OM_` prefix is added automatically if
#'   missing, so both produce `data/OM_Base.rda`.
#' @param path Character. Path to the root directory of the NPSWO.OM package.
#'   Default: `"../NPSWO.OM"`.
#' @param compress Logical. Whether to call [Compress()] on the OM before
#'   saving, reducing object size by dropping redundant array dimensions.
#'   Default: `TRUE`.
#'
#' @param local Logical. Whether to also save a local copy as
#'   `Objects/OM/<name>.om`. The directory is created if it does not exist.
#'   Local files are ignored by git and must be generated locally.
#'   Default: `TRUE`.
#'
#' @param overwrite Logical. Whether to overwrite an existing `.rda` file of
#'   the same name. Default: `FALSE`.
#'
#' @return Invisibly returns `NULL`. Called for its side-effect of writing
#'   `data/OM_<name>.rda` in the NPSWO.OM package, and optionally
#'   `Objects/OM/<name>.om` locally.
#'
#' @seealso [Compress()], [usethis::use_data()], [withr::with_dir()]
#'
#' @importFrom withr with_dir with_options
#' @importFrom usethis use_data
#' @importFrom cli cli_abort
#' @export
SaveOM <- function(OM, name, path="../NPSWO.OM", compress=TRUE, local=TRUE, overwrite = FALSE) {

  if (!inherits(OM, 'om'))
    cli::cli_abort("OM must be an {.cls MSEtool::om} object")

  if (!dir.exists(path))
    cli::cli_abort(c('x'="path {.val {path}} not found",
                     'i'='Make sure {.val path} points to the local copy of the {.val NPSWO.OM} package',
                     'i'= 'Clone from the GitHub repository {.url https://github.com/Blue-Matter/NPSWO.OM}')
    )

  if (is.null(name))
    name <- deparse(substitute(OM))

  if (!grepl("^OM_", name))
    name <- paste("OM", name, sep='_')

  if (compress) {
    cli::cli_progress_step("Compressing {.val {name}}")
    OM <- Compress(OM)
  }

  assign(name, OM)

  cli::cli_progress_step("Saving {.val {name}} to {.path {file.path(path, 'data', paste0(name, '.rda'))}}")
  withr::with_options(
    list(cli.default_handler = function(msg) invisible(NULL)), {
      withr::with_dir(path, {
        do.call(usethis::use_data, list(as.name(name), overwrite = overwrite))
      })
    }
  )


  if (local) {
    localpath <- "Objects/OM"
    cli::cli_progress_step("Saving {.val {name}} to {.path {file.path(localpath, paste0(name, '.om'))}}")
    if (!dir.exists(localpath))
      dir.create(localpath, recursive = TRUE)
    path <- file.path(localpath, paste0(name, '.om'))
    MSEtool::Save(Hist, path, overwrite)
  }

  cli::cli_progress_done()
}


#' List Available OM Objects
#'
#' Lists all OM objects with an `OM_` prefix available in the NPSWO.OM package,
#' either from the local repository copy or the installed package.
#'
#' @param source Character. Where to look for OM objects. `"local"` searches
#'   the local repository copy at `path`; `"installed"` searches the installed
#'   NPSWO.OM package. Default: `"installed"`.
#' @param path Character. Path to the root directory of the local NPSWO.OM
#'   repository. Only used when `source = "local"`.
#'   Default: `"../NPSWO.OM"`.
#'
#' @return A character vector of OM names (without the `OM_` prefix).
#'
#' @seealso [LoadOM()], [SaveOM()]
#'
#' @importFrom cli cli_abort cli_inform
#' @export
ListOMs <- function(source = c("installed", "local"), path = "../NPSWO.OM") {
  source <- match.arg(source)

  if (source == "installed") {
    if (!requireNamespace("NPSWO.OM", quietly = TRUE))
      cli::cli_abort(c(
        "x" = "Package {.pkg NPSWO.OM} is not installed.",
        "i" = "Install it from {.url https://github.com/Blue-Matter/NPSWO.OM}."
      ))
    files <- utils::data(package = "NPSWO.OM")$results[, "Item"]
  } else {
    if (!dir.exists(path))
      cli::cli_abort(c(
        "x" = "Path {.path {path}} not found.",
        "i" = "Make sure {.arg path} points to the local copy of the {.pkg NPSWO.OM} package.",
        "i" = "Clone from the GitHub repository {.url https://github.com/Blue-Matter/NPSWO.OM}."
      ))
    files <- tools::file_path_sans_ext(list.files(file.path(path, "data")))
  }

  files <- files[grepl("^OM_", files)]
  names <- gsub("^OM_", "", files)

  cli::cli_text("OM objects available in {.pkg NPSWO.OM} ({source}):")
  cli::cli_bullets(stats::setNames(names, rep("*", length(names))))

  invisible(names)
}

#' Load an OM Object from the NPSWO.OM Package
#'
#' Loads a named OM object from the installed NPSWO.OM package and restores
#' its full array dimensions via [Expand()]. The `OM_` prefix is added
#' automatically if not already present.
#'
#' @param name Character. Name of the OM to load (e.g. `"Base"` or
#'   `"OM_Base"`). The `OM_` prefix is added automatically if missing.
#'
#' @return An [MSEtool::OM()] object with array slots expanded to full
#'   `nSim x nYear` dimensions.
#'
#' @seealso [SaveOM()], [ListOMs()], [Expand()]
#'
#' @importFrom utils data
#' @export
LoadOM <- function(name) {

  if (!grepl("^OM_", name))
    name <- paste("OM", name, sep = "_")

  cli::cli_progress_step("Loading {.val {name}}")
  env <- new.env(parent = emptyenv())
  utils::data(list = name, package = "NPSWO.OM", envir = env)
  cli::cli_progress_step("Expanding {.val {name}}")
  OM <- Expand(env[[name]])

  cli::cli_progress_done()
  OM

}
