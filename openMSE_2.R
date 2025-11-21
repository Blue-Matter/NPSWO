packageVersion('MSEtool') # should be v4+

library(MSEtool)
library(DLMtool)
library(ggplot2)
# install.packages('tictoc')

SSDir <- 'TempFiles/Base'

nsim <- 10

# Legacy version
tictoc::tic()
MOM <- SS2MOM(SSDir, nsim=nsim, proyears = 30)
tictoc::toc()

# 2.0
tictoc::tic()
OM <- ImportSS(SSDir, nSim = nsim, StockName = 'Albacore')

tictoc::toc()

# Compare object size
object.size(MOM) |> format('Mb')
object.size(OM) |> format('Mb')

# Simulate Historical Fishery
tictoc::tic()
multiHist <- Simulate(MOM)
tictoc::toc()

tictoc::tic()
Hist <- Simulate(OM)
tictoc::toc()


# Projections
# data is by stock and fleet
Data_old_fleet1 <- multiHist$Female$Fleet_01@Data
Data_old_fleet2 <- multiHist$Female$Fleet_02@Data

# data object contains all fleets
Data_new <- Hist@Data$`1`$Albacore

object.size(Data_old_fleet1) |> format('Mb') # only one fleet
object.size(Data_new) |> format('Mb')  # all fleets

# Catch data
cbind(Data_old_fleet1@Cat[1,], Data_old_fleet2@Cat[1,]) # fleets 1 and 2

Data_new@Landings@Name
Data_new@Landings@Units
Data_new@Landings@Value


## Average Catch MP
DLMtool::AvC

Data <- Data_new

AvC_new <- function(Data) {
  advice <- Advice()
  YearLH_Ind <- GetYearLH(Data)
  average_hist_catch <- apply(Data@Landings@Value[1:YearLH_Ind ,], 'Year', sum, na.rm=TRUE) |>
    mean()
  advice@TAC <- average_hist_catch
  advice
}
class(AvC_new) <- 'mp'

tictoc::tic()
multiMSE <- Project(multiHist, MPs='AvC')
tictoc::toc()

tictoc::tic()
MSE <- Project(Hist, MPs='AvC_new')
tictoc::toc()

# Compare object size
object.size(multiMSE) |> format('Mb')
object.size(MSE) |> format('Mb')

# Accessing catch
dim(multiMSE@Catch) # sim, stock, fleet, mp, projection year
multiMSE@Catch[1,1,,1,]  # catch by fleet and projection year

dim(MSE@Landings$Albacore) # sim, age, projection year
Landings(MSE) |> dplyr::filter(Period=='Projection')



# cpars vs hierarchical objects

names(MOM@cpars$Female$Fleet_01)

OM@Stock$Albacore@Ages
OM@Stock$Albacore@Length

OM@Stock$Albacore |> Length() |> MeanAtAge()
MOM@cpars$Female$Fleet_01$Len_age

OM@Stock$Albacore |> NaturalMortality() |> MeanAtAge()
MOM@cpars$Female$Fleet_01$M_ageArray

OM@Stock$Albacore |> Fecundity() |> MeanAtAge()
MOM@cpars$Female$Fleet_01$Fec_age

OM@Fleet$Albacore$Fleet_01 |> Selectivity() |> MeanAtAge()
MOM@cpars$Female$Fleet_01$V

OM@Fleet$Albacore$Fleet_01 |> Selectivity() |> MeanAtAge() |> Array2DF()


