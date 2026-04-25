# This script calculates performance metrics for the North Pacific Swordfish MSE.

# ---- Initial Setup ----
library(NPSWO)

#UpdatePackages()

source("0. Specifications.R")
source("Functions_RS.R")
source("PM_Functions.R")


########### Example MOs for demonstration purposes ###################
### MO1: Avoid SSB falling below LRP:
###      Probability of SSB < SBMSY OR SSB < 17%SSB0 (NPSWO is a Level 1 stock at ISC; MSY is considered
###      an appropriate LRP)

### MO2: Maintain SSB above TRP:
###      Probability of SSB > 25%SSB0 (just picked a value above SBMSY)

### MO3: Maximize catch:
###      Average annual catch over all years of the projection period

### MO4: Maximize catch in the short term:
###      Average annual catch in the first five years of the projection period
##################################################################################################################
## DEBUGGING LINES
#Yrs = chk_yrs(Yrs = NULL, base)
#MSEobj = base

# List of available MSEs
All_MSE <- ListMSE()
Use_MSE <- All_MSE # subset if desired

base  <- LoadMSE("0_Final_base_case")
lowM  <- LoadMSE("2_base_case_lowM")
highM <- LoadMSE("1_base_case_highM")
lowh  <- LoadMSE("4_Sensitivity_h081")
highh <- LoadMSE("5_Sensitivity_h099")

#Probability of SBiomass < SBMSY across any year of projection period
#all 0
Safety_MSY(base)
Safety_MSY(lowM)
Safety_MSY(highM)
Safety_MSY(lowh)
Safety_MSY(highh)

#Probability of SBiomass < 17%SSB0 (BMSY proxy) across any year of projection period
#all 0
Safety_17SSB0(base)
Safety_17SSB0(lowM)
Safety_17SSB0(highM)
Safety_17SSB0(lowh)
Safety_17SSB0(highh)


#Probability of SBiomass > 25%SSB0 across any year of projection period
#Based on assumption that LRP is BMSY (or 17%SSB0 proxy) - TRP would need to be higher
#all 1
Status_25SSB0(base)
Status_25SSB0(lowM)
Status_25SSB0(highM)
Status_25SSB0(lowh)
Status_25SSB0(highh)


#Probability of SBiomass > 50%SSB0 across any year of projection period (just to make sure it's working)
Status_50SSB0(base) #mean 0.07
Status_50SSB0(lowM) #mean 0.32
Status_50SSB0(highM) #mean 0.01
Status_50SSB0(lowh) #mean 0.03
Status_50SSB0(highh) #mean 1


#Average annual catch (males and females) over entire projection period
# Running into issues with this - saying 'Incomplete PMobj', so manually extracting slots
base_catch = AvgAnn_Catch(base)
base_catch@Stat #avg annual catch values by sim
base_catch@Prob #avg annual catch values (not actually Prob)
base_catch@Mean #avg catch value

lowM_catch = AvgAnn_Catch(lowM)
lowM_catch@Stat #avg annual catch values by sim
lowM_catch@Prob #avg annual catch values (not actually Prob)
lowM_catch@Mean #avg catch value

highM_catch = AvgAnn_Catch(highM)
highM_catch@Stat #avg annual catch values by sim
highM_catch@Prob #avg annual catch values (not actually Prob)
highM_catch@Mean #avg catch value

lowh_catch = AvgAnn_Catch(lowh)
lowh_catch@Stat #avg annual catch values by sim
lowh_catch@Prob #avg annual catch values (not actually Prob)
lowh_catch@Mean #avg catch value

highh_catch = AvgAnn_Catch(highh)
highh_catch@Stat #avg annual catch values by sim
highh_catch@Prob #avg annual catch values (not actually Prob)
highh_catch@Mean #avg catch value



#Average annual catch (males and females) over first 10 years of projection period
# Running into issues with this - saying 'Incomplete PMobj', so manually extracting slots
base_STcatch = ST_Catch(base)
base_STcatch@Stat #avg annual catch values by sim
base_STcatch@Prob #avg annual catch values (not actually Prob)
base_STcatch@Mean #avg catch value

lowM_STcatch = ST_Catch(lowM)
lowM_STcatch@Stat #avg annual catch values by sim
lowM_STcatch@Prob #avg annual catch values (not actually Prob)
lowM_STcatch@Mean #avg catch value

highM_STcatch = ST_Catch(highM)
highM_STcatch@Stat #avg annual catch values by sim
highM_STcatch@Prob #avg annual catch values (not actually Prob)
highM_STcatch@Mean #avg catch value

lowh_STcatch = ST_Catch(lowh)
lowh_STcatch@Stat #avg annual catch values by sim
lowh_STcatch@Prob #avg annual catch values (not actually Prob)
lowh_STcatch@Mean #avg catch value

highh_STcatch = ST_Catch(highh)
highh_STcatch@Stat #avg annual catch values by sim
highh_STcatch@Prob #avg annual catch values (not actually Prob)
highh_STcatch@Mean #avg catch value







#######################################################################################
############################### Old Code Parking Lot ##################################
#######################################################################################

###################### Pulling Ref Points from SS model files #########################
# TO DO: make this into GetRPs() function
#GetRPs()

library(gtools)
#Define OMs of interest
All_OMs = ListOMs()
Use_OMs = All_OMs[c(1, 17, 18, 20, 21)]

#Define RPs of interest
RPs = c("SSB0", "SBMSY", "FMSY")

#Set up blank dataframe
OM_RPs = array(dim = c(length(Use_OMs), length(RPs)))
rownames(OM_RPs) = Use_OMs
colnames(OM_RPs) = RPs


# direct to local folder containing all SS model outputs
con_dir <- file.path(getwd(), "Condition", "WCNPOSWO-2023")
sim_dirs <- mixedsort(list.dirs(con_dir, recursive=FALSE))

# subset directories to be only those associated with OMs we want RPs from
use_dirs <- sim_dirs[grepl(paste(Use_OMs, collapse = "|"), sim_dirs)]


for (i in 1:length(use_dirs)){
  RepList   <- ImportSSReport(use_dirs[i])
  DerQuants <- RepList$`1`$derived_quants

  # pull SSB0; average 5 year SSB0 estimate
  SSB0 <- DerQuants[DerQuants$Label == "SSB_unfished", ]
  OM_RPs[i,1] <- SSB0$Value

  # pull SSBMSY - note that SSB is based on female spawning stock biomass
  SBMSY <- DerQuants[DerQuants$Label == "SSB_MSY", ]
  OM_RPs[i,2] <- SBMSY$Value

  # pull annual FMSY (age 1-10)
  FMSY <- DerQuants[DerQuants$Label == "annF_MSY", ]
  OM_RPs[i,3] <- FMSY$Value

}

OM_RPs
#######################################################################################

