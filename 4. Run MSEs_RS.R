# TBD
# Quick Test to get an MSE object

source("Functions_RS.R")

#Running MSEs
All_Hist <- ListHist()
Use_Hist <- All_Hist[c(1,2,10,11,12)]
#Use_Hist <- All_Hist

# choosing MPs to test
mps = c('ConstantEffort')

# define number of simulations (keeping it small for testing)
nsim = 10

# Running projections and creating MSE objects for each Hist object
for(i in 1:length(Use_Hist)){
  Hist_Name <- Use_Hist[i]
  Curr_Hist <- LoadHist(Use_Hist[i])
  assign(paste0("MSE_", Hist_Name), MSEtool::Project(Curr_Hist, MPs = mps, nSim = nsim))

  # saving MSE objects
  SaveMSE(get(paste0("MSE_", Hist_Name)), Hist_Name, overwrite = TRUE)

}
LoadHist()

