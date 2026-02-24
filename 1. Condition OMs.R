
library(MSEtool)
library(r4ss)

source('999. Functions.R')

SSDirBase <- "../WCNPOSWO-2023/Final Base-case"

ConditionDir <- "../NPSWO_SS3"

# ---- Import Stochastic Life-History Parameters ----

StochasticValues_Female <- read.csv('LHSamples_Female.csv')
StochasticValues_Male <- read.csv('LHSamples_Male.csv')

# Note: maintain consistent M-male >> M-female?

StochasticValues <- data.frame(M_female = StochasticValues_Female$M,
                               M_male = StochasticValues_Male$M,
                               h = StochasticValues_Female$h)

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
















