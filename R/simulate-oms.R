#' Simulate Operating Models
#'
#' Loads and runs [MSEtool::Simulate()] for one or more Operating Models (OMs),
#' saving each result to disk. When multiple OMs are provided and `parallel = TRUE`,
#' simulations are run in parallel using `future.apply`.
#'
#' @param OM_names `character` vector of Operating Model names to simulate.
#' @param DoRefLandings Logical. Passed to [MSEtool::Simulate()]. Default: `FALSE`.
#' @param parallel `Logical.. Should simulations be run in parallel when
#'   `length(OM_names) > 1`? Default `FALSE`. Ignored for single OMs.
#' @param ... Additional arguments passed to [MSEtool::Simulate()].
#'
#' @return Invisibly returns a list of `Hist` objects, one per OM.
#'
#' @details
#' When `length(OM_names) > 1` and `parallel = TRUE`, [MSEtool::SetupParallel()]
#' is called with `workers = 2`. This can use a lot of memory with large OMs
#' so only run with `parallel=TRUE` if you have a lot of memory!
#'
#' When `parallel = FALSE` or only a single OM is provided, simulations run
#' sequentially in the current session.
#'
#' @seealso [MSEtool::Simulate()], [LoadOM()], [SaveHist()], [MSEtool::SetupParallel()]
#'
#'
#' @export
RunSimulations <- function(OM_names, DoRefLandings= FALSE, parallel = FALSE, ...) {
  if (!is.character(OM_names) || length(OM_names) == 0)
    cli::cli_abort("`OM_names` must be a non-empty character vector.")

  n <- length(OM_names)
  t_start <- proc.time()[["elapsed"]]

  OMList <- lapply(OM_names, LoadOM)

  if (n > 1 && parallel) {
    workers <- 2
    cli::cli_alert_info(
      "Running {n} OM{?s} in parallel on {workers} worker{?s}: {.val {OM_names}}"
    )
    MSEtool::SetupParallel(workers = workers)
    HistList <- future.apply::future_mapply(
      FUN           = .simulate_and_save,
      om_obj        = OMList,
      om_name       = OM_names,
      DoRefLandings = DoRefLandings,
      silent        = TRUE,
      MoreArgs      = list(...),
      SIMPLIFY      = FALSE,
      future.seed   = TRUE
    )
  } else {
    mode_msg <- if (n > 1 && !parallel) "sequentially (parallel = FALSE)" else "sequentially"
    cli::cli_alert_info(
      "Running {n} OM{?s} {mode_msg}: {.val {OM_names}}"
    )
    HistList <- lapply(seq_len(n), function(i) {
      cli::cli_alert("Simulating {.val {OM_names[i]}} ({i}/{n})")
      .simulate_and_save(OMList[[i]], OM_names[i], DoRefLandings, silent = FALSE, ...)
    })
  }

  elapsed <- round(proc.time()[["elapsed"]] - t_start, 1)
  cli::cli_alert_success(
    "Finished {n} OM{?s} in {elapsed}s."
  )

  invisible(HistList)
}


.simulate_and_save <- function(om_obj, om_name, DoRefLandings = FALSE, silent = TRUE, ...) {
  Hist <- MSEtool::Simulate(om_obj, DoRefLandings = DoRefLandings, silent = silent, ...)
  SaveHist(Hist, om_name, overwrite=TRUE)
  invisible(Hist)
}
