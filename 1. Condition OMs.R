
library(MSEtool)
library(r4ss)

source('999. Functions.R')

SSDirBase <- "../WCNPOSWO-2023/Final Base-case"

ConditionDir <- "../NPSWO_SS3"

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


