#' Compress and Expand OM Array Slots
#'
#' `Compress()` reduces stochastic fleet and survey selectivity and weight
#' arrays to their core dimensions by dropping redundant simulation and year
#' replicates via [MSEtool::ReduceDims()]. `Expand()` is the inverse operation,
#' broadcasting those compressed arrays back out to the full `nSim x nYear`
#' dimensions via [MSEtool::Extend()].
#'
#' These functions are typically used as a matched pair: `Compress()` before
#' saving or transmitting an OM to reduce object size, and `Expand()` before
#' running an MSE to restore the full array dimensions expected by the
#' simulation engine.
#'
#' The following slots are affected for every stock-fleet combination:
#' - `OM@Fleet[[st]][[fl]]@Selectivity@MeanAtAge`
#' - `OM@Fleet[[st]][[fl]]@WeightFleet`
#'
#' And for every observation model-fleet combination (where non-`NULL`):
#' - `OM@Obs[[i]][[fl]]@Survey@Selectivity[[st]]`
#'
#' @param OM An [MSEtool::OM()] object.
#'
#' @return An [MSEtool::OM()] object with the affected array slots either
#'   compressed or expanded.
#'
#' @seealso [MSEtool::ReduceDims()], [MSEtool::Extend()],
#'
#' @name CompressExpand
#' @aliases Compress Expand
#' @export
Compress <- function(OM) {
  nStock <- MSEtool::nStock(OM)
  nFleet <- MSEtool::nFleet(OM)

  for (st in seq_len(nStock)) {
    for (fl in seq_len(nFleet)) {
      array <- OM@Fleet[[st]][[fl]]@Selectivity@MeanAtAge
      OM@Fleet[[st]][[fl]]@Selectivity@MeanAtAge <- MSEtool::ReduceDims(array, IncYear=TRUE)

      OM@Fleet[[st]][[fl]]@WeightFleet  <- MSEtool::ReduceDims(OM@Fleet[[st]][[fl]]@WeightFleet, IncYear=TRUE)
    }
  }

  for (i in seq_along(OM@Obs)) {
    for (fl in seq_along(OM@Obs[[i]])) {

      Sel <- OM@Obs[[i]][[fl]]@Survey@Selectivity
      if (is.null(Sel)) next

      for (st in seq_along(Sel)) {
        Sel[[st]] <- MSEtool::ReduceDims(Sel[[st]], IncYear=TRUE)
      }

      OM@Obs[[i]][[fl]]@Survey@Selectivity <- Sel
    }
  }

  OM
}

#' @rdname CompressExpand
#' @export
Expand <- function(OM) {
  nStock <- nStock(OM)
  nFleet <- nFleet(OM)
  nSim <- nSim(OM)
  Years <- Years(OM,'H')

  for (st in seq_len(nStock)) {
    for (fl in seq_len(nFleet)) {
      array <- OM@Fleet[[st]][[fl]]@Selectivity@MeanAtAge
      OM@Fleet[[st]][[fl]]@Selectivity@MeanAtAge <- MSEtool::Extend(array,
                                                           nSim=nSim,
                                                           Years=Years,
                                                           maintain_seasonal_pattern = FALSE)

      OM@Fleet[[st]][[fl]]@WeightFleet  <- MSEtool::Extend(OM@Fleet[[st]][[fl]]@WeightFleet,
                                                  nSim=nSim,
                                                  Years=Years,
                                                  maintain_seasonal_pattern = FALSE)
    }
  }


  for (i in seq_along(OM@Obs)) {
    for (fl in seq_along(OM@Obs[[i]])) {

      Sel <- OM@Obs[[i]][[fl]]@Survey@Selectivity
      if (is.null(Sel)) next

      for (st in seq_along(Sel)) {
        Sel[[st]] <- MSEtool::Extend(Sel[[st]],
                            nSim=nSim,
                            Years=Years,
                            maintain_seasonal_pattern = FALSE)
      }

      OM@Obs[[i]][[fl]]@Survey@Selectivity <- Sel
    }
  }
  OM
}
