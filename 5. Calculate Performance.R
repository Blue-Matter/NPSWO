# DEV #################################

# Using NPALB PMs as starting point:
# See Table ES1: https://isc.fra.go.jp/pdf/ISC21/ISC21_ANNEX11_Report_of_the_North_Pacific_ALBACORE_MSE.pdf

############################ NPALB ###########################################################
###MO1: Maintain historical spawning biomass
#prob SSB > LRP (LRP is F value, varies between HCRs) - skip for now
#prob SSB > 20%SSB0
#prob SSB > 7.7%SSB0
#prob SSB > Equilibrium 7.7%SSB0

MSE@Unfished@Dynamic@SBiomass
MSE@Unfished@Equilibrium@SBiomass
MSE@SBiomass


###MO2: Maintain historical total biomass (2006-2015 - last 10 years of assessment)
#prob depletion > minimum historical



###MO3: Maintain historical harvest ratios of each fishery (1999-2015)
###  agreed that harvest ratios of each fishery be maintained at historical average
###  in the MSE simulation, to not have allocation rules specific to each fishery.
###  Therefore didn't differ among the candidate HCRs and was not evaluated

###MO4: Maintain catches above historical average (1981 -2010)
#prob mean annual catch > historical catch
#prob mean medium term catch > historical catch (years 7-13 of simulation)
#prob mean long term catch > historical catch (years 20-30 of simulation)

###MO5: Minimize changes in management over time
#prob of no management change

###MO6: Maintain fishing impact around target historical level
#Ftarget/F (Ftarget = either F40%SPR, F50%SPR depending on HCR)
##############################################################################################

#source("PM_Functions.R")

#Probability that SSB in any given year of the MSE simulation is above 20% dynamic unfished SSB
#PP20_SSB0(MSE)
