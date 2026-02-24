# Helper and Internal Functions


# Create SS3 directories with stochastic life-history parameters
CreateSSDirectories <- function(OM_Name,
                                SSDirBase,
                                ConditionDir,
                                StochasticValues,
                                DataFileName='swo2023_v004.dat',
                                CrtFileName='swo2023_v007.ctl',
                                OffFleets=NULL) {

  if (!dir.exists(SSDirBase))
    stop("Can't find SSDirBase")

  if (!dir.exists(ConditionDir))
    stop("Can't find ConditionDir")

  # Load Base Data and Ctl Files
  dat <- r4ss::SS_readdat_3.30(file.path(SSDirBase, DataFileName),
                                verbose = FALSE)

  ctl <- r4ss::SS_readctl_3.30(file.path(SSDirBase, CrtFileName),
                               datlist=dat,
                               verbose = FALSE)

  starter <- r4ss::SS_readstarter(file.path(SSDirBase, 'starter.ss'),verbose = FALSE)

  forecast <- SS_readforecast(file.path(SSDirBase, 'forecast.ss'), verbose=FALSE)

  SS3OutDir <- file.path(ConditionDir, OM_Name)

  if (!dir.exists(SS3OutDir))
    dir.create(SS3OutDir)

  nsim <- nrow(StochasticValues)

 cli::cli_progress_bar(
    format="Writing {cli::pb_current}/{cli::pb_total} directories to {SS3OutDir}",
    total = nsim
  )

  for (i in seq_len(nsim)) {

    WriteSSFiles(i, StochasticValues, SS3OutDir, dat, ctl, starter, forecast, OffFleets)
    cli::cli_progress_update()
  }

}

# Write new SS3 files with stochastic life-history parameters
WriteSSFiles <- function(i, StochasticValues, SS3OutDir, dat, ctl, starter, forecast, OffFleets) {

  # Natural Mortality
  baseM_female <- as.numeric(ctl$natM[1,])
  baseM_male <- as.numeric(ctl$natM[2,])

  M_female_rel <- baseM_female/baseM_female[length(baseM_female)]
  M_male_rel <- baseM_male/baseM_male[length(baseM_male)]

  # new values
  ctl$natM[1,] <- StochasticValues$M_female[i] * M_female_rel
  ctl$natM[2,] <- StochasticValues$M_male[i] * M_male_rel

  # Steepness
  ctl$SR_parms["SR_BH_steep",c('INIT', 'PRIOR')] <- StochasticValues$h[i]

  # create directory
  i_char <- as.character(i)
  if (nchar(i_char)==1) i_char <- paste0('00', i_char)
  if (nchar(i_char)==2) i_char <- paste0('0', i_char)

  dir <- file.path(SS3OutDir, i_char)
  if (dir.exists(dir))
    unlink(dir, recursive=TRUE)

  if (!dir.exists(dir))
    dir.create(dir)

  # modify starter.ss
  starter$ctlfile <- 'control.ss'
  starter$datfile <- 'data.ss'
  starter$init_values_src <- 0 # use control file for initial values
  starter$run_display_detail <- 0
  starter$cumreport <- 0

  # turn off fleets (if applicable)
  if (!is.null(OffFleets)) {

    # modifiy dat
    dat$catch$year[dat$catch$fleet %in% OffFleets] <-
      -dat$catch$year[dat$catch$fleet %in% OffFleets]

    dat$lencomp$year[dat$lencomp$fleet %in% OffFleets] <-
      -dat$lencomp$year[dat$lencomp$fleet %in% OffFleets]

    dat$CPUE$year[dat$CPUE$index %in% OffFleets] <-
      -dat$CPUE$year[dat$CPUE$index %in% OffFleets]

  }

  # write the modified files
  SS_writectl_3.30(ctl, file.path(dir, 'control.ss'), verbose=F, overwrite = TRUE)
  SS_writedat_3.30(dat, file.path(dir, 'data.ss'), verbose=F, overwrite = TRUE)
  SS_writestarter(starter, dir, verbose=F, overwrite = TRUE)

  # requires at least 1 forecast year
  forecast$Nforecastyrs <- 1
  SS_writeforecast(forecast, dir, verbose=FALSE, overwrite=TRUE)



}

CheckParallel <- function(parallel) {
  if (!parallel)
    return(FALSE)
  current_plan <- future::plan()

  if (inherits(current_plan, "sequential")) {
    cli::cli_alert_warning(
      "{.val parallel = TRUE} requested, but no parallel future plan detected."
    )
    cli::cli_text("Initialize a parallel plan first using:")
    cli::cli_ul()
    cli::cli_li("e.g: `SetupParallel(workers = 4)`")
    cli::cli_text("Running sequentially instead (`parallel = FALSE`).")
    return(FALSE)
  }
  parallel
}

RunSS3Models <- function(path, Exe_path='Condition', parallel=TRUE) {
  SS3Dirs <- list.dirs(path, recursive = FALSE)

  parallel <- CheckParallel(parallel)

  if (!parallel) {
    purrr::map(SS3Dirs, \(SS_path) {
      RunSS(SS_path, Exe_path)
    },
    .progress = list(
      caller = environment(),
      format = "Running SS3 for {.val {length(SS3Dirs)}} directories {cli::pb_bar} {cli::pb_percent}"
    )
    )

  } else {
    # Parallel processing
    cli::cli_inform("Starting parallel SS3 runs for {.val {length(SS3Dirs)}} directories...")

    furrr::future_map(SS3Dirs, function(SS_path) {
      RunSS(SS_path, Exe_path)
    })
  }

}


RunSS <- function(SS_path,
                  Exe_path='Condition',
                  nohess = TRUE,
                  stdout = FALSE,
                  stderr=FALSE) {

  old_wd <- getwd()

  on.exit(setwd(old_wd))

  file.copy(file.path(Exe_path, "ss3.exe"),
            file.path(SS_path, 'ss3.exe'))

  setwd(SS_path)
  if (nohess) {
    system2('ss3.exe -nohess',
            stdout = stdout,
            stderr = stderr)
  } else {
    system2('ss3.exe',
            stdout = stdout,
            stderr = stderr)
  }



  if (file.exists('ss3.exe'))
    file.remove('ss3.exe')

}
