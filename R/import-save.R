#' Import SS3 Output and Save Operating Model
#'
#' Reads SS3 report files for a named Operating Model, combines them into an
#' [MSEtool::OM-class] object via [MSEtool::ImportSS()], optionally drops
#' specified fleets, applies control specifications, and saves the result to
#' disk. All specifications and global variables, including `ControlList`, are
#' sourced from `0. Specifications.R`.
#'
#' @param OM_Name `character(1)`. The name of the Operating Model. Used to
#'   locate the SS3 report files via [ImportRepList()] and as the identifier
#'   passed to [SaveOM()].
#' @param DropFleets `character` or `integer` vector of fleet names or indices
#'   to remove from the OM. Fleet names are matched against
#'   [MSEtool::FleetNames()] and converted to indices before subsetting.
#'   If `NULL` (default), all fleets are retained.
#'
#' @return Called for its side effects. Invisibly returns `NULL`. The
#'   constructed [MSEtool::OM-class] object is saved to disk via [SaveOM()].
#'
#' @details
#' The function proceeds in five steps:
#'
#' 1. **Specifications** — `0. Specifications.R` is sourced to populate
#'    `Name`, `pYear`, `StockName`, `Species`, and `ControlList` in the
#'    local environment.
#' 2. **Import** — [ImportRepList()] reads SS3 report files from all
#'    subdirectories associated with `OM_Name` into a list.
#' 3. **Conversion** — [MSEtool::ImportSS()] combines the report list into
#'    a single [MSEtool::OM-class] object. This step may take several minutes
#'    depending on the number of SS3 models (i.e. the length of `RepList`,
#'    which corresponds to `nSim`).
#' 4. **Drop fleets** — If `DropFleets` is not `NULL`, fleet names or indices
#'    are resolved via [MSEtool::FleetNames()] and the remaining fleets are
#'    retained using [MSEtool::Subset()].
#' 5. **Save** — [MSEtool::Control()] is assigned from `ControlList` and the
#'    OM is written to the package data store via [SaveOM()] with
#'    `overwrite = TRUE`.
#'
#' @seealso [ImportRepList()], [MSEtool::ImportSS()], [MSEtool::FleetNames()],
#'   [MSEtool::Subset()], [SaveOM()], [RunSimulations()]
#' @export
Import_Save <- function(OM_Name, DropFleets = NULL) {
  source("0. Specifications.R")

  # Read the SS3 report files from all subdirectories into a list
  RepList <- ImportRepList(OM_Name)

  # Combine the SS3 outputs into a single MSEtool OM object
  # (this can take a few minutes depending on `length(RepList)`)
  OM <- MSEtool::ImportSS(RepList,
                          Name  = Name,
                          nSim = nSim,
                          pYear = pYear,
                          StockName = StockName,
                          Species = Species)

  MSEtool::Control(OM) <- ControlList

  if (!is.null(DropFleets)) {
    AllFleets <- MSEtool::FleetNames(OM, TRUE)
    if (is.character(DropFleets))
      DropFleets <- match(DropFleets,AllFleets)
    KeepFleets <- AllFleets[-DropFleets]
    OM <- MSEtool::Subset(OM, Fleets=KeepFleets)
  }

  # Save the OM to the NPSWO.OM package
  SaveOM(OM, OM_Name, overwrite = TRUE)
}
