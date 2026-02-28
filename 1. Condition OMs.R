
library(MSEtool)
library(r4ss)

source('999. Functions.R')

SSDirBase <- "../WCNPOSWO-2023/Final Base-case"

ConditionDir <- "../NPSWO_SS3"

# Combine Fleet Info
Names <- list("JPN_WCNPO_OSDWCOLL_Area1",
              "TWN_WCNPO_DWLL",
              "US_WCNPO_LL_shallow",
              "JPN_WCNPO_Other")

Fleets <- list(
  c(1,6),
  c(2, 13),
  c(3, 7),
  c(11, 12)
)



n <- 50

# ---- Import Stochastic Life-History Parameters ----

StochasticValues <- read.csv('LHSamples.csv')


pairs(StochasticValues, pch=16)


# ---- Base OM ----

CreateSSDirectories(OM_Name = 'Base',
                    SSDirBase,
                    ConditionDir,
                    StochasticValues,
                    DataFileName = 'swo2023_v004.dat',
                    CrtFileName = 'swo2023_v007.ctl'
)

SetupParallel()

path <- file.path(ConditionDir, 'Base')

RunSS3Models(path)

SetupParallel()

SSDirs <- list.dirs(path, recursive=FALSE)
RepList <- ImportSSReport(SSDirs[1:n], parallel = TRUE)

OM_Base <- ImportSS(RepList) |> CombineFleets(Names, Fleets)

Save(OM_Base, "Objects_OM/Base.om", overwrite = TRUE)


# ---- Base OM - WCPO fleets only ----

CreateSSDirectories(OM_Name = 'WCPO_only',
                    SSDirBase,
                    ConditionDir,
                    StochasticValues,
                    DataFileName = 'swo2023_v004.dat',
                    CrtFileName = 'swo2023_v007.ctl',
                    OffFleets = c(4,5) #EPO fleets; F04_IATTC, F05_JPN_EPO_OSDWLL
)

SetupParallel()

path <- file.path(ConditionDir, 'WCPO_only')

RunSS3Models(path)

SetupParallel()
SSDirs <- list.dirs(path, recursive=FALSE)
RepList <- ImportSSReport(SSDirs[1:n], parallel = TRUE)

WCPO_only <- ImportSS(RepList) |> CombineFleets(Names, Fleets)
Save(WCPO_only, "Objects_OM/WCPO_only.om")

