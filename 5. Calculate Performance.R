# DEV #################################

# Using NPALB PMs as starting point:
# See Table ES1: https://isc.fra.go.jp/pdf/ISC21/ISC21_ANNEX11_Report_of_the_North_Pacific_ALBACORE_MSE.pdf

########### Examples based on NPALB,PBFT, and US Stakeholder call (4/22) ###################
### MO1: Status: Maintain stock size at levels capable of producing maximum sustainable yield
#prob SSB > SSBMSY

MSE@Unfished@Dynamic@SBiomass
MSE@Unfished@Equilibrium@SBiomass
MSE@SBiomass

Status_pgk(MSE)

### MO2: Safety: probability of breaching an LRP should be very low
#prob SSB > 7.7%SSB0 #IATTC LRP
#prob SSB > 0.5BMSY (US domestic min stock size threshold)

### MO3: Yield: Maintain catches above historical average (SWO ref period in other CMM is 2008-2010)
# prob mean annual catch > average of catches from 2008-2010

### MO4: Yield: Maximize yield
# avg catch in short, med, and long term

### M05: Stability: limit changes in overall catch (effort?) limits between management periods to no more than 25%
##################################################################################################################
source("Functions_RS.R")
source("PM_Functions.R")
Catch_ST(MSE)


