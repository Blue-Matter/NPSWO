
library(MSEtool)
library(r4ss)

if (!dir.exists('Condition'))
  dir.create('Condition')

# ---- Base SS3 Files that will be modified ----
# File path to the SS3 files in WCNPOSWO-2023
SSDirBase <- "../WCNPOSWO-2023/Final Base-case"
DataFileName <- 'swo2023_v004.dat'
CrtFileName <- 'swo2023_v007.ctl'

BaseDataFile <- r4ss::SS_readdat_3.30(file.path(SSDirBase, DataFileName),
                                  verbose = FALSE)

BaseCtlFile <- r4ss::SS_readctl_3.30(file.path(SSDirBase, CrtFileName),
                                 datlist=BaseDataFile,
                                 verbose = FALSE)


# ---- Import Stochastic Life-History Parameters ----

OM_Name <- 'Base'
SS3OutDir <- file.path('Condition', OM_Name)

if (!dir.exists(SS3OutDir))
  dir.create(SS3OutDir)

# StochasticValues_Female <- read.csv('Condition/Base_LHSamples_Female.csv')
# StochasticValues_Male <- read.csv('Condition/Base_LHSamples_Male.csv')

DummyValues <- data.frame(M=c(0.2, 0.3, 0.4),
                          h=c(0.7, 0.8, 0.9))

StochasticValues_Female <- DummyValues
StochasticValues_Male <- DummyValues



if (nrow(StochasticValues_Female) != nrow(StochasticValues_Male))
  stop("Female and Male life-history parameters must be the same length")


# ---- Loop over life-history samples ----

nsim <- nrow(StochasticValues_Female)
for (i in seq_len(nsim)) {
  message(i, '/', nsim)

  dat <- BaseDataFile
  ctl <- BaseCtlFile

  # Natural Mortality
  baseM_female <- as.numeric(ctl$natM[1,])
  baseM_male <- as.numeric(ctl$natM[2,])

  M_female_rel <- baseM_female/baseM_female[length(baseM_female)]
  M_male_rel <- baseM_male/baseM_male[length(baseM_male)]

  # new values
  ctl$natM[1,] <- StochasticValues_Female$M[i] * M_female_rel
  ctl$natM[2,] <- StochasticValues_Male$M[i] * M_male_rel

  # Steepness
  ctl$SR_parms["SR_BH_steep",c('INIT', 'PRIOR')] <- StochasticValues_Female$h[i]

  # create directory
  i_char <- as.character(i)
  if (nchar(i_char)==1) i_char <- paste0('00', i_char)
  if (nchar(i_char)==2) i_char <- paste0('0', i_char)


  dir <- file.path('Condition', OM_Name, i_char)
  if (dir.exists(dir))
    unlink(dir, recursive=TRUE)

  if (!dir.exists(dir))
    dir.create(dir)

  # modify starter.ss
  starter <- r4ss::SS_readstarter(file.path(SSDirBase, 'starter.ss'),verbose = FALSE)
  starter$ctlfile <- 'control.ss'
  starter$datfile <- DataFileName
  starter$init_values_src <- 0 # use control file for initial values
  starter$run_display_detail <- 0
  starter$cumreport <- 0

  # write the modified files
  SS_writectl_3.30(ctl, file.path(dir, 'control.ss'), verbose=F, overwrite = TRUE)
  SS_writedat_3.30(dat, file.path(dir, DataFileName), verbose=F, overwrite = TRUE)
  SS_writestarter(starter, file.path(dir), verbose=F, overwrite = TRUE)

  # requires at least 1 forecast year
  forecast <- SS_readforecast(file.path(SSDirBase, 'forecast.ss'), verbose=FALSE)
  forecast$Nforecastyrs <- 1
  SS_writeforecast(forecast, dir,
                   verbose=FALSE, overwrite=TRUE)


}

# Run the SS3 models
# Note: can run this in parallel if it takes too long
SS3Dirs <- list.dirs(SS3OutDir, recursive = FALSE)

WD <- getwd()
for (i in seq_along(SS3Dirs)) {
  setwd(WD)
  # copy SS3 executable
  file.copy("Condition/SS3.exe", file.path(SS3Dirs[i], 'SS3.exe'))

  setwd(SS3Dirs[i])
  system2('SS3.exe', stdout = FALSE, stderr = FALSE)

  # delete SS3 executable
  if (file.exists('SS3.exe'))
    file.remove('SS3.exe')
}


setwd(WD)

OM <- ImportSS(SS3Dirs[i])
Hist <- Simulate(OM)






