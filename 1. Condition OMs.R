
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


# ---- Base OM - WCPO fleets only ----

CreateSSDirectories(OM_Name = 'WCPO_only',
                    SSDirBase,
                    ConditionDir,
                    StochasticValues,
                    DataFileName = 'swo2023_v004.dat',
                    CrtFileName = 'swo2023_v007.ctl',
                    OffFleets = c(4,5) #EPO fleets; F04_IATTC, F05_JPN_EPO_OSDWLL
)




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






