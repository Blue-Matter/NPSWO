# TODO

# High Priority
# - OM diagnostics - identify and drop simulations that didn't converge or outliers
# - Develop CMPs
# - Run MSEs and Save
# - Develop PM functions
# - write readme for packages
# - Update TS Doc & Project homepage


# Lower Priority
# - Combine fleets - TODO later

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

# Install the NPSWO package following the directions in README before running
# this script
library(NPSWO)

# Check GitHub for package updates and reinstall if the remote has changed
UpdatePackages()

source("0. Specifications.R")

################################################################################
######                        0. Reference OM                             ######
################################################################################

# This is the Base Case Assessment without any additional uncertainty. It is
# used for a reference OM and to compare the estimated stock dynamics with those
# from the stochastic OMs developed below.

OM_Name <- "RefOM"

RunSingleSS(OM_Name, ssdir = ssdir_base)



##################### - START Stochastic Sampling Approach - ###########################


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



################################################################################
#                                                                              #
# ONLY RUN THIS BLOCK TO REBUILD SS3 DIRECTORIES AND RE-RUN THE SS3 MODELS     #
#                                                                              #
################################################################################

OM_Name <- "Base"

# Create a numbered subdirectory for each of the nSim ensemble members,
# copying the base SS3 model into each and substituting the sampled
# life-history parameters

CreateSSDirectories(OM_Name,
                    ssdir = ssdir_base,
                    StochasticValues = LH_Samples,
                    parallel = TRUE)

# Execute all nSim SS3 models in parallel (this step is slow)
RunSS3Models(OM_Name, parallel = TRUE)

# Import SS3 models into an OM object and save to NPSWO.OM package
Import_Save(OM_Name)


################################################################################
#                                                                              #
# END SS3 MODEL BLOCK                                                          #
#                                                                              #
################################################################################



################################################################################
######                        2. WCPO Fleets Only                         ######
################################################################################

# This OM is identical to Base Case OM, except that it does not include the
# East Pacific Ocean (EPO) fleets in the OM Conditioning; i.e. it is restricted
# to the Western Central Pacific Fleet only


################################################################################
#                                                                              #
# ONLY RUN THIS BLOCK TO REBUILD SS3 DIRECTORIES AND RE-RUN THE SS3 MODELS     #
#                                                                              #
################################################################################

OM_Name <- 'WCPO_only'

OffFleets <- 4:5 #EPO fleets; F4_IATTC, F5_JPN_EPO_OSDWL

CreateSSDirectories(OM_Name,
                    ssdir = ssdir_base,
                    StochasticValues = LH_Samples,
                    OffFleets = OffFleets,
                    parallel = TRUE)

# Execute all nSim SS3 models in parallel (this step is slow)
RunSS3Models(OM_Name, parallel = TRUE)


# Import SS3 models into an OM object and save to NPSWO.OM package
Import_Save(OM_Name, DropFleets=OffFleets)

################################################################################
#                                                                              #
# END SS3 MODEL BLOCK                                                          #
#                                                                              #
################################################################################

###################### - END Stochastic Sampling Approach - ############################




############ - START OMs from 2023 Assessment (Base and Sens. Runs) - #################

################################################################################
######                      1. Reference Set OMs                          ######
################################################################################
# The reference set of OMs represents the final accepted base case model of the
# 2023 ISC NPSWO stock assessment and the associated 24 sensitivity runs (25 total)
library(gtools)

# direct to local folder containing all SS model outputs
con_dir = file.path(getwd(), "Condition", "WCNPOSWO-2023")

# get lists of (correctly ordered) model file paths, shortened names
# the base case OM is stored as OM_0
sim_dirs = mixedsort(list.dirs(con_dir, recursive=FALSE))
sim_names = mixedsort(list.dirs(con_dir, full.names = FALSE, recursive=FALSE))
sim_nums = mixedsort(sub("_.*", "", sim_names))

# looping through all model files and saving as individual OMs
for(i in 1:length(sim_dirs)){
  OM_Name <- sim_names[i]
  RepList <- ImportSSReport(sim_dirs[i])
  assign(paste0("OM_", sim_nums[i]), MSEtool::ImportSS(RepList,
                                                       Name  = OM_Name,
                                                       nSim = nSim,
                                                       pYear = pYear,
                                                       StockName = StockName,
                                                       Species = Species))
  # saving newly created OM to NPSWO.OM package
  SaveOM(get(paste0("OM_", sim_nums[i])), OM_Name, overwrite = TRUE)

}
############# - END OMs from 2023 Assessment (Base and Sens. Runs) - ###################
