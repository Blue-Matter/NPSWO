#' Create SS3 Directories with Stochastic Life-History Parameters
#'
#' Reads a base Stock Synthesis 3 (SS3) model, applies stochastic life-history
#' parameters, and writes `nSim` simulation directories.
#'
#' @param OM_Name         Character. Name of the operating model; used as the
#'                        output subdirectory name within `outdir`.
#' @param ssdir           Character. Path to the base SS3 model directory
#'                        containing input files.
#' @param outdir          Character. Path to the root conditioning directory
#'                        where simulation subdirectories will be created.
#' @param StochasticValues Data frame. Each row contains one set of sampled
#'                        life-history parameters (see [Generate_LH_Samples()]).
#' @param datfile         Character. Name of the SS3 data file.
#'                        Default: `"swo2023_v004.dat"`.
#' @param ctlfile         Character. Name of the SS3 control file.
#'                        Default: `"swo2023_v007.ctl"`.
#' @param starterfile     Character. Name of the SS3 starter file.
#'                        Default: `"starter.ss"`.
#' @param forecastfile    Character. Name of the SS3 forecast file.
#'                        Default: `"forecast.ss"`.
#' @param OffFleets       Integer vector or `NULL`. Fleet indices to turn off.
#'                        Default: `NULL`.
#' @param parallel        Logical. If `TRUE`, simulation directories are
#'                        written in parallel.
#'                        Requires a `future` plan to be set by the caller
#'                        (see [MSEtool::SetupParallel()]).
#'                        Default: `FALSE`.
#'
#' @return Invisibly returns the path to the output directory.
#'
#' @examples
#' \dontrun{
#' # Sequential
#' CreateSSDirectories("OM_Base", ssdir="SS3/Base", outdir="Condition", LHSamples)
#'
#' # Parallel
#' MSEtool::SetupParallel()
#' CreateSSDirectories("OM_Base", ssdir="SS3/Base", outdir="Condition", LHSamples,
#'                     parallel = TRUE)
#' MSEtool::DisableParallel()
#' }
#'
#' @seealso [WriteSSFiles()], [RunSS3Models()]
#'
#' @importFrom r4ss SS_readdat_3.30 SS_readctl_3.30 SS_readstarter SS_readforecast
#' @importFrom cli cli_progress_bar cli_progress_update cli_progress_done cli_alert_success
#' @importFrom furrr future_walk furrr_options
#' @export
CreateSSDirectories <- function(OM_Name,
                                ssdir,
                                outdir = 'Condition',
                                StochasticValues,
                                datfile      = 'swo2023_v004.dat',
                                ctlfile      = 'swo2023_v007.ctl',
                                starterfile  = 'starter.ss',
                                forecastfile = 'forecast.ss',
                                OffFleets    = NULL,
                                parallel     = FALSE) {

  if (!dir.exists(ssdir))
    cli::cli_abort("Cannot find `ssdir`: {ssdir}")

  if (!dir.exists(outdir)) {
    cli::cli_alert_info('Creating directory {.val {outdir}}')
    dir.create(outdir)
  }

  if (!is.data.frame(StochasticValues) || nrow(StochasticValues) == 0)
    cli::cli_abort("`StochasticValues` must be a non-empty data frame.")

  parallel <- MSEtool::CheckParallel(parallel)

  # Read base SS3 files
  dat <- r4ss::SS_readdat_3.30(file.path(ssdir, datfile), verbose = FALSE)
  ctl <- r4ss::SS_readctl_3.30(file.path(ssdir, ctlfile), datlist = dat, verbose = FALSE)
  starter  <- r4ss::SS_readstarter(file.path(ssdir, starterfile), verbose = FALSE)
  forecast <- r4ss::SS_readforecast(file.path(ssdir, forecastfile), verbose = FALSE)

  # Prepare output directory
  SS3OutDir <- file.path(outdir, OM_Name)
  if (dir.exists(SS3OutDir)) {
    cli::cli_alert_info('Deleting existing contents of {.val {SS3OutDir}}')
    unlink(SS3OutDir, recursive = TRUE)
  }

  cli::cli_alert_info('Creating directory {.val {SS3OutDir}}')
  dir.create(SS3OutDir, recursive = TRUE)

  nsim    <- nrow(StochasticValues)
  sim_ids <- seq_len(nsim)

  #  Write simulation directories
  if (parallel) {
    cli::cli_alert_info(
      "Writing {nsim} directories in parallel to {.path {SS3OutDir}}"
    )

    furrr::future_walk(
      sim_ids,
      \(i) WriteSSFiles(SS3OutDir,
                        dat, ctl, starter, forecast,
                        i, StochasticValues,
                        OffFleets),
      .options = furrr::furrr_options(seed = TRUE)
    )

  } else {
    cli::cli_progress_bar(
      format = "Writing {cli::pb_current}/{cli::pb_total} directories to {SS3OutDir}",
      total  = nsim
    )

    for (i in sim_ids) {
      WriteSSFiles(SS3OutDir,
                   dat, ctl, starter, forecast,
                   i, StochasticValues, OffFleets)
      cli::cli_progress_update()
    }

    cli::cli_progress_done()
  }

  cli::cli_alert_success("Done. {nsim} directories written to {.path {SS3OutDir}}")
  invisible(SS3OutDir)
}


#' Write SS3 Input Files for a Single Simulation
#'
#' Applies stochastic life-history parameters (natural mortality and steepness)
#' for simulation `i` to a set of SS3 model objects, creates a numbered
#' subdirectory, and writes all four SS3 input files to disk.
#'
#'                         `M_female`, `M_male`, and `h`.
#' @param SS3OutDir        Character. Parent directory in which the numbered
#'                         simulation subdirectory will be created.
#' @param dat              List. SS3 data object from `r4ss::SS_readdat_3.30()`.
#' @param ctl              List. SS3 control object from `r4ss::SS_readctl_3.30()`.
#' @param starter          List. SS3 starter object from `r4ss::SS_readstarter()`.
#' @param forecast         List. SS3 forecast object from `r4ss::SS_readforecast()`.
#' @param i                Integer. Row index into `StochasticValues`;
#'                         also determines the subdirectory name (zero-padded).
#' @param StochasticValues Data frame. Sampled parameter sets with columns
#' @param OffFleets        Integer vector or `NULL`. Fleet indices to
#'                         deactivate by negating their year values in catch,
#'                         length composition, and CPUE data. Default: `NULL`.
#'
#' @return Invisibly returns the path to the simulation subdirectory.
#'
#' @details
#' Natural mortality vectors are rescaled relative to their terminal age value,
#' preserving the age-varying shape while shifting the overall level to the
#' sampled value. Steepness is applied to both the `INIT` and `PRIOR`
#' fields of the `SR_BH_steep` parameter row.
#'
#' The starter is configured to use control file initial values
#' (`init_values_src = 0`), suppress run display output, and disable the
#' cumulative report. A minimum of one forecast year is enforced.
#'
#' @seealso [CreateSSDirectories()]
#'
#' @importFrom r4ss SS_writectl_3.30 SS_writedat_3.30 SS_writestarter SS_writeforecast
#' @export
WriteSSFiles <- function(
                         SS3OutDir, dat, ctl, starter, forecast,
                         i = NULL, StochasticValues = NULL,
                         OffFleets = NULL) {

  if (!is.null(StochasticValues)) {
    nms <- names(StochasticValues) |> strsplit("_")
    nms <- lapply(nms, '[[', 1) |> unlist() |> unique()

    supported <- c('M', 'h')

    if (!any(nms %in% supported))
      cli::cli_abort(c("x" = "Non supported parameters for this function",
                       'i' = "Provided: {.val {nms}}",
                       'i' = "Supported: {.val {supported}}"),
                     .internal=TRUE)


    colnames(StochasticValues) <- tolower(colnames(StochasticValues))

    #  Natural Mortality: preserve age-varying shape, shift overall level
    scale_natM <- function(base_row, new_M) {
      base_vec   <- as.numeric(base_row)
      terminal_M <- base_vec[length(base_vec)]
      new_M * (base_vec / terminal_M)
    }

    ctl$natM[1, ] <- scale_natM(ctl$natM[1, ], StochasticValues$m_female[i])
    ctl$natM[2, ] <- scale_natM(ctl$natM[2, ], StochasticValues$m_male[i])

    # Steepness
    ctl$SR_parms["SR_BH_steep", c("INIT", "PRIOR")] <- StochasticValues$h[i]

    # Create zero-padded simulation subdirectory
    sim_dir <- file.path(SS3OutDir, formatC(i, width = 3, flag = "0"))

    if (dir.exists(sim_dir))
      unlink(sim_dir, recursive = TRUE)
    dir.create(sim_dir)
  } else {
    sim_dir <- SS3OutDir
  }

  # Configure starter
  starter$ctlfile            <- "control.ss"
  starter$datfile            <- "data.ss"
  starter$init_values_src    <- 0
  starter$run_display_detail <- 0
  starter$cumreport          <- 0

  #  Deactivate fleets (negate year values)
  if (!is.null(OffFleets)) {
    negate_fleet_years <- function(df, fleet_col, year_col, fleets) {
      df[[year_col]][df[[fleet_col]] %in% fleets] <-
        -abs(df[[year_col]][df[[fleet_col]] %in% fleets])
      df
    }
    dat$catch   <- negate_fleet_years(dat$catch,   "fleet", "year", OffFleets)
    dat$lencomp <- negate_fleet_years(dat$lencomp, "fleet", "year", OffFleets)
    dat$CPUE    <- negate_fleet_years(dat$CPUE,    "index", "year", OffFleets)
  }

  # Write SS3 input files
  r4ss::SS_writectl_3.30(ctl,
                         file.path(sim_dir, "control.ss"),
                         verbose = FALSE, overwrite = TRUE)

  r4ss::SS_writedat_3.30(dat,
                         file.path(sim_dir, "data.ss"),
                         verbose = FALSE, overwrite = TRUE)

  r4ss::SS_writestarter(starter,
                        sim_dir,
                        verbose = FALSE, overwrite = TRUE)

  forecast$Nforecastyrs <- max(forecast$Nforecastyrs, 1L)

  r4ss::SS_writeforecast(forecast,
                         sim_dir,
                         verbose = FALSE, overwrite = TRUE)

  invisible(sim_dir)
}


#' Run SS3 Models Across Simulation Directories
#'
#' Iterates over all subdirectories in `path` and executes an SS3 model in
#' each, either sequentially with a progress bar or in parallel via `furrr`.
#'
#' @param OM_Name Character. Parent directory containing numbered SS3
#'                  simulation subdirectories (as created by
#'                  [CreateSSDirectories()]).
#' @param outdir  Character. Path to the root conditioning directory
#'                  where simulation subdirectories are stored (as created by
#'                  [CreateSSDirectories()]). Default: "Condition"
#' @param parallel  Logical. If `TRUE`, runs are distributed across workers
#'                  using [furrr::future_map()]. Requires a `future` plan to
#'                  be active (see [MSEtool::SetupParallel()]).
#'                  Default: `TRUE`.
#' @param nmax      Integer or `NULL`. If provided, limits execution to the
#'                  first `nmax` subdirectories. Useful for testing.
#'                  Default: `NULL` (run all).
#'
#' @return Invisibly returns `NULL`. SS3 output is written to each simulation
#'         subdirectory of `file.path(outdir, OM_Name)`.
#'
#' @seealso [CreateSSDirectories()], [RunSS()]
#'
#' @importFrom purrr map
#' @importFrom furrr future_map
#' @importFrom progressr with_progress progressor
#' @importFrom cli cli_inform cli_alert_success
#' @export
RunSS3Models <- function(OM_Name,
                         outdir = 'Condition',
                         parallel = TRUE,
                         nmax = NULL) {

  path <- file.path(outdir, OM_Name)
  SS3Dirs <- list.dirs(path, recursive = FALSE)

  n_dirs <- length(SS3Dirs)

  if (n_dirs<1)
    cli::cli_abort("No sub-directories found in {.val {path}}")

  if (!is.null(nmax))
    SS3Dirs <- SS3Dirs[seq_len(min(length(SS3Dirs), nmax))]

  parallel <- MSEtool::CheckParallel(parallel)

  start_time <- proc.time()[["elapsed"]]

  exe_path <- find_exe()

  if (!parallel) {
    purrr::map(
      SS3Dirs,
      \(SS_path) RunSS(SS_path, exe_path),
      .progress = list(
        format = "Running SS3 {cli::pb_current}/{cli::pb_total} {cli::pb_bar} {cli::pb_percent}"
      )
    )
  } else {
    cli::cli_inform("Starting parallel SS3 runs for {.val {n_dirs}} directories...")
    progressr::with_progress({
      p <- progressr::progressor(steps = n_dirs)
      furrr::future_map(
        SS3Dirs,
        \(SS_path) {
          result <- RunSS(SS_path, exe_path)
          p()
          result
        },
        .options = furrr::furrr_options(seed = TRUE)
      )
    })
  }

  elapsed <- round(proc.time()[["elapsed"]] - start_time, 1)
  mins    <- floor(elapsed / 60)
  secs    <- round(elapsed %% 60, 0)
  cli::cli_alert_success(
    "Completed {.val {n_dirs}} SS3 runs in {mins}m {secs}s \\
    ({round(elapsed / n_dirs, 1)}s per run)"
  )

  invisible(NULL)
}


#' Run a Single SS3 Model
#'
#' Copies the SS3 executable into a simulation directory, runs the model, and
#' removes the executable on exit. The working directory is restored safely via
#' `on.exit()` regardless of success or failure.
#'
#' @param SS_path  Character. Path to the simulation directory containing SS3
#'                 input files (as written by [WriteSSFiles()]).
#' @param exe_path Character. Full path including fill name to the SS3 executable.
#'                 Default: `ss3.exe` included in the `NPSWO` package.
#'
#' @param args     Character. Command-line arguments passed to the executable.
#'                 Default: `"-nohess"`. Set to `character(0)` to run with
#'                 Hessian estimation.
#' @param stdout   Logical or character. Passed to `system2()`. `FALSE`
#'                 discards stdout; `TRUE` captures it as a character vector;
#'                 a file path redirects output to disk. Default: `FALSE`.
#' @param stderr   Logical or character. As `stdout`, but for stderr.
#'                 Default: `FALSE`.
#'
#' @return Invisibly returns the integer exit code from `system2()`.
#'   A non-zero value indicates the model run failed.
#'
#' @seealso [RunSS3Models()], [WriteSSFiles()]
#' @export
RunSS <- function(SS_path,
                  exe_path = NULL,
                  exe      = "ss3.exe",
                  args     = "-nohess",
                  stdout   = FALSE,
                  stderr   = FALSE) {

  if (is.null(exe_path))
    exe_path <- find_exe()

  exe <- basename(exe_path)
  exe_src  <- file.path(exe_path)
  exe_dest <- file.path(SS_path,  exe)

  if (!file.exists(exe_src))
    cli::cli_abort("SS3 executable not found: {exe_src}")

  if (!dir.exists(SS_path))
    cli::cli_abort("Simulation directory not found: {SS_path}")

  old_wd <- getwd()

  on.exit({
    if (file.exists(exe_dest)) file.remove(exe_dest)
    setwd(old_wd)
  }, add = TRUE)

  file.copy(exe_src, exe_dest, overwrite = TRUE)
  setwd(SS_path)

  exit_code <- system2(exe, args = args, stdout = stdout, stderr = stderr)

  invisible(exit_code)
}



#' Run a Single SS3 Model and Save Output
#'
#' Reads a set of SS3 input files from a base directory, writes them to a
#' named output directory, and executes the SS3 model.
#'
#' @param OM_Name Name of the Operating Model. Used as the
#'   name of the subdirectory created inside `outdir` to store SS3 output.
#' @param ssdir Path to the directory containing the base SS3
#'   input files. An error is thrown if this directory does not exist.
#' @param outdir Path to the parent output directory. Created
#'   if it does not already exist. Defaults to `"Condition"`.
#' @param datfile  Name of the SS3 data file inside `ssdir`.
#'   Defaults to `"swo2023_v004.dat"`.
#' @param ctlfile  Name of the SS3 control file inside `ssdir`.
#'   Defaults to `"swo2023_v007.ctl"`.
#' @param starterfile Name of the SS3 starter file inside
#'   `ssdir`. Defaults to `"starter.ss"`.
#' @param forecastfile Name of the SS3 forecast file inside
#'   `ssdir`. Defaults to `"forecast.ss"`.
#' @param OffFleets `integer` or `character` vector of fleets to turn off
#'   before running the model. If `NULL` (default), all fleets are retained.
#'
#' @return Called for its side effects. Invisibly returns `NULL`. SS3 output
#'   files are written to `file.path(outdir, OM_Name)`.
#'
#' @details
#' The function proceeds in four steps:
#'
#' 1. **Validation** — checks that `ssdir` exists, creating `outdir` if
#'    needed.
#' 2. **Read** — reads all four SS3 input files from `ssdir` using
#'    [r4ss::SS_readdat_3.30()], [r4ss::SS_readctl_3.30()],
#'    [r4ss::SS_readstarter()], and [r4ss::SS_readforecast()].
#' 3. **Write** — prepares a clean output directory at
#'    `file.path(outdir, OM_Name)`, deleting any existing contents, then
#'    writes the input files via [WriteSSFiles()].
#' 4. **Run** — executes SS3 in the output directory via [RunSS()].
#'
#' @section Warning:
#' Any existing contents of `file.path(outdir, OM_Name)` are deleted before
#' the model is run. Ensure any previous results have been saved elsewhere
#' before calling this function.
#'
#' @seealso [WriteSSFiles()], [RunSS()], [r4ss::SS_readdat_3.30()],
#'   [r4ss::SS_readctl_3.30()]
#'
#' @examples
#' \dontrun{
#' # Run a single SS3 model using default file names
#' RunSingleSS(
#'   OM_Name = "OM_Base",
#'   ssdir   = "SS3/Base"
#' )
#'
#' # Specify a custom output directory and control file
#' RunSingleSS(
#'   OM_Name = "OM_LowM",
#'   ssdir   = "SS3/LowM",
#'   outdir  = "Condition/Sensitivities",
#'   ctlfile = "swo2023_lowM.ctl"
#' )
#' }
#'
#' @export
RunSingleSS <- function(OM_Name,
                        ssdir,
                        outdir = 'Condition',
                        datfile      = 'swo2023_v004.dat',
                        ctlfile      = 'swo2023_v007.ctl',
                        starterfile  = 'starter.ss',
                        forecastfile = 'forecast.ss',
                        OffFleets    = NULL) {

  if (!dir.exists(ssdir))
    cli::cli_abort("Cannot find `ssdir`: {ssdir}")

  if (!dir.exists(outdir)) {
    cli::cli_alert_info('Creating directory {.val {outdir}}')
    dir.create(outdir)
  }

  # Read base SS3 files
  dat <- r4ss::SS_readdat_3.30(file.path(ssdir, datfile), verbose = FALSE)
  ctl <- r4ss::SS_readctl_3.30(file.path(ssdir, ctlfile), datlist = dat, verbose = FALSE)
  starter  <- r4ss::SS_readstarter(file.path(ssdir, starterfile), verbose = FALSE)
  forecast <- r4ss::SS_readforecast(file.path(ssdir, forecastfile), verbose = FALSE)

  # Prepare output directory
  SS3OutDir <- file.path(outdir, OM_Name)
  if (dir.exists(SS3OutDir)) {
    cli::cli_alert_info('Deleting existing contents of {.val {SS3OutDir}}')
    unlink(SS3OutDir, recursive = TRUE)
  }

  cli::cli_alert_info('Creating directory {.val {SS3OutDir}}')
  dir.create(SS3OutDir, recursive = TRUE)

  WriteSSFiles(SS3OutDir, dat, ctl, starter, forecast, OffFleets=OffFleets)

  cli::cli_progress_step("Running SS3 for {.val {OM_Name}}")
  RunSS(SS3OutDir)
  cli::cli_progress_done()
}
