# TODO
# - OM diagnostics - identify and drop simulations that didn't converge or outliers
# - do parallel for ImportSS
# - confirm reloaded OMs have identical structure to saved
# - fix issue with importing WCPO_only - need to drop those fleets from OM
# - Combine fleets
# - Run Historical and Save
# - Develop CMPs
# - Run MSEs and Save
# - Develop PM functions
# - write readme for packages
# - Update TS Doc & Project homepage


# Names <- list("JPN_WCNPO_OSDWCOLL_Area1",
#               "TWN_WCNPO_DWLL",
#               "US_WCNPO_LL_shallow",
#               "JPN_WCNPO_Other")
#
# Fleets <- list(
#   c(1,6),
#   c(2, 13),
#   c(3, 7),
#   c(11, 12)
# )



# This script conditions operating models (OMs) for the North Pacific Swordfish MSE.
# Each OM represents a different hypothesis about stock dynamics, built by running
# an ensemble of SS3 models with stochastically sampled life-history parameters.

# ---- Initial Setup ----

# Install the NPSWO package following the directions in README before running this script
library(NPSWO)

# Check GitHub for package updates and reinstall if the remote has changed
UpdatePackages()


################################################################################
######                        1. Base Case OM                             ######
################################################################################

# The base-case OM represents the central stock hypothesis. An ensemble of nSim
# SS3 models is run, each with a unique draw of life-history parameters sampled
# from the distributions defined in LifeHistory/Base.csv. The resulting runs are
# then combined into a single MSEtool OM object.

# Read in the central (mean, natural-space) and uncertainty (SD, log-space) values
# for each stochastic life-history parameter. See ?Generate_LH_Samples for valid
# parameter names.
LH_Mean_SD_Base <- read.csv("LifeHistory/Base.csv")

# Draw nSim correlated parameter sets from the specified distributions and plot
# the resulting samples to verify coverage of the parameter space
LH_Samples <- Generate_LH_Samples(Parameters = LH_Mean_SD_Base,
                                  nSim = nSim,
                                  plot = TRUE)

# Save samples to disk
Save(LH_Samples, "LifeHistory/Base.rds", overwrite = TRUE)

OM_Name <- "Base"
source("0. Specifications.R")

################################################################################
#                                                                              #
# ONLY RUN THIS BLOCK TO REBUILD SS3 DIRECTORIES AND RE-RUN THE SS3 MODELS     #
#                                                                              #
################################################################################

# Create a numbered subdirectory for each of the nSim ensemble members,        #
# copying the base SS3 model into each and substituting the sampled            #
# life-history parameters

CreateSSDirectories(OM_Name,
                    ssdir = ssdir_base,
                    StochasticValues = LH_Samples,
                    parallel = TRUE)

# Execute all nSim SS3 models in parallel (this step is slow)
RunSS3Models(OM_Name, parallel = TRUE)

################################################################################
#                                                                              #
# END SS3 MODEL BLOCK                                                          #
#                                                                              #
################################################################################


# Read the SS3 report files from all ensemble subdirectories into a list
RepList <- ImportRepList(OM_Name)

# Combine the ensemble of SS3 outputs into a single MSEtool OM object
# (this can take a few minutes depending on `nSim`)
Base <- MSEtool::ImportSS(RepList,
                             Name  = Name,
                             pYear = pYear,
                             StockName = StockName,
                             Species = Species)

# Save the OM to the NPSWO.OM package
SaveOM(Base, overwrite = TRUE)


################################################################################
######                        2. WCPO Fleets Only                         ######
################################################################################

# This OM is identical to Base Case OM, except that it does not include the
# East Pacific Ocean (EPO) fleets in the OM Conditioning; i.e. it is restricted
# to the Western Central Pacific Fleet only

OM_Name <- 'WCPO_only'
source("0. Specifications.R")


################################################################################
#                                                                              #
# ONLY RUN THIS BLOCK TO REBUILD SS3 DIRECTORIES AND RE-RUN THE SS3 MODELS     #
#                                                                              #
################################################################################

LH_Samples <- readRDS("LifeHistory/Base.rds")

CreateSSDirectories(OM_Name,
                    ssdir = ssdir_base,
                    StochasticValues = LH_Samples,
                    parallel = TRUE)

# Execute all nSim SS3 models in parallel (this step is slow)
RunSS3Models(OM_Name, parallel = TRUE)

################################################################################
#                                                                              #
# END SS3 MODEL BLOCK                                                          #
#                                                                              #
################################################################################

# Read the SS3 report files from all ensemble subdirectories into a list
RepList <- ImportRepList(OM_Name)

# Combine the ensemble of SS3 outputs into a single MSEtool OM object
# (this can take a few minutes depending on `nSim`)
WCPO_only <- MSEtool::ImportSS(RepList,
                          Name  = Name,
                          pYear = pYear,
                          StockName = StockName,
                          Species = Species)

# Save the OM to the NPSWO.OM package
SaveOM(WCPO_only, overwrite = TRUE)



##################### DEV ######################################################



ListOMs()
Loaded <- LoadOM("Base")

Hist <- Simulate(Base, DoRefLandings = FALSE)
Hist2 <- Simulate(Loaded, DoRefLandings = FALSE)

sim <- 200
plot(Hist@SBiomass[sim,1,], type='l')
lines(Hist2@SBiomass[sim,1,], col='blue')



t = PopulateOM(OM2)

OM2 <- CombineFleets(OM_Base, Names, Fleets)
OM2 <- Populate(OM2)


Hist <- Simulate(OM_Base, nSim=5, DoRefLandings = FALSE)
Hist2 <- Simulate(OM2, nSim=5, DoRefLandings = FALSE)




Hist@Biomass[1,1,]
Hist2@Biomass[1,1,]






Save(WCPO_only, "Objects_OM/WCPO_only.om")









SetupParallel()
SSDirs <- list.dirs(path, recursive=FALSE)
RepList <- ImportSSReport(SSDirs[1:n], parallel = TRUE)

WCPO_only <- ImportSS(RepList) |> CombineFleets(Names, Fleets)
Save(WCPO_only, "Objects_OM/WCPO_only.om")





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




pairs(StochasticValues, pch=16)





SetupParallel()

SSDirs <- list.dirs(path, recursive=FALSE)
RepList <- ImportSSReport(SSDirs[1:n], parallel = TRUE)

OM_Base <- ImportSS(RepList) |> CombineFleets(Names, Fleets)

Save(OM_Base, "Objects_OM/Base.om", overwrite = TRUE)




