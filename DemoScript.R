
# Requires the latest development version of MSEtool
# Always update as it is under constant development
pak::pkg_install('blue-matter/MSEtool@dev')

packageVersion('MSEtool') # should be v4+

library(MSEtool)
library(ggplot2)

# ----- NPSWO Initial Demo Code ----

# TODO:
# - fix OM landings < SS3 landings - weight-at-age?
# - calculate MSY ref points in seasonal model
# - develop method to distribute TAC over seasons
# - check historical and projected indices
# - check fleet allocation - ConstantCatch vs ConstantCatchFleet
# - aggregate fleets?
# - aggregate seasons?

# Questions for Group:
# - how should TAC be distributed over seasons?
# - which indices should be used for MPs?
# - data lags

# File path to the SS3 files in WCNPOSWO-2023
SSDir <- '../WCNPOSWO-2023/Final Base-case'

# Import the SS3 Output
RepList <- ImportSSReport(SSDir)

# Correct M-at-Age for initial age class
# for some reason, M for age-0 is exactly half the actual value
RepList[[1]]$M_at_age[,4] <- RepList[[1]]$M_at_age[,4] * 2

# some catches are reported in numbers but this appears to be an error
RepList$`1`$catch_units[] <- 1


# Meta-data
Name <- 'North Pacific Swordfish'
StockName <- c("Female", 'Male')
Species <- "Xiphias gladius"
Region <- 'North Pacific'

# OM Settings - to be updated
Interval <- 3 # Management Interval
DataLag <- 0 #
nSim <- 2 # small for demo
pYear <- 30 # number of projection years

# Generate OM from SS3 output
OM <- ImportSS(RepList,
               Name,
               nSim,
               pYear,
               Region=Region,
               StockName=StockName,
               Species=Species,
               Interval=Interval,
               DataLag=DataLag)

# Simulate Historical Fishery
Hist <- Simulate(OM)

# Compare OM Dynamics with SS3 Output
CompareSS_Number(RepList, Hist)
CompareSS_Biomass(RepList, Hist)
CompareSS_Landings(RepList, Hist) # ~ 5 - 10% lower in OM


# Simple Example MPs
ConstantEffort <- function(Data) {
  advice <- Advice()
  advice@Effort <- 1
  advice
}
class(ConstantEffort) <- 'mp'

ConstantCatchFleet <- function(Data) {
  advice <- Advice()
  YearLH_Ind <- GetYearLH(Data) # index for last historical year (season)
  LastHistoricalCatch <- Data@Landings@Value[YearLH_Ind ,]
  advice@TAC <- LastHistoricalCatch # set TAC by Fleet
  advice
}
class(ConstantCatchFleet) <- 'mp'

ConstantCatch <- function(Data) {
  advice <- Advice()
  YearLH_Ind <- GetYearLH(Data) # index for last historical year (season)
  LastHistoricalCatch <- Data@Landings@Value[YearLH_Ind ,]
  advice@TAC <- sum(LastHistoricalCatch) # overall TAC
  advice
}
class(ConstantCatch) <- 'mp'

# Projections
MSE <- Project(Hist, MPs=c('ConstantEffort',
                           'ConstantCatchFleet',
                           'ConstantCatch'))


# Plots
Landings <- Landings(MSE, byFleet=TRUE) |>
  dplyr::filter(Sim==1) |>
  dplyr::group_by(Year, Fleet, Period, MP) |>
  dplyr::summarise(Value=sum(Value))

ggplot(Landings, aes(x=Year, y=Value, linetype=Period, color=MP)) +
  facet_wrap(~Fleet, scales='free_y') +
  geom_line() +
  expand_limits(y=0) +
  theme_bw()

Biomass <- Biomass(MSE) |>
  dplyr::group_by(Year, Stock, MP, Period) |>
  dplyr::summarise(Value=mean(Value))

ggplot(Biomass, aes(x=Year, y=Value, linetype=Period, color=MP)) +
  facet_grid(~Stock) +
  geom_line() +
  expand_limits(y=0) +
  theme_bw()






