
# Requires the latest development version of MSEtool
# Always update as it is under constant development
# pak::pkg_install('blue-matter/MSEtool@prerelease')

packageVersion('MSEtool') # should be v4+

library(MSEtool)
library(ggplot2)

# ---- TODO List ----
# TODO:
# - fix OM landings < SS3 landings - uses weight-at-age calculated internally in model - ie not empirical

#   closest is endgrowth$Wt_Mid but not perfect

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


# ----- Import Base Case OM ----
# File path to the SS3 files in WCNPOSWO-2023
SSDir <- '../WCNPOSWO-2023/Final Base-case'

# Import the SS3 Output
RepList <- ImportSSReport(SSDir)

# Age-0 is half the actual value, presumably because SS3 has 2 seasons for Age-0
# while OM has 4 seasons for all age-classes
RepList[[1]]$M_at_age[,4] <- RepList[[1]]$M_at_age[,4] * 2


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
df <- CompareSS_Number(RepList, Hist)

df |> dplyr::filter(Stock=='Female') |>
  tidyr::pivot_wider(values_from = Value, names_from = Model)

# Need to fix recruitment ...
# was good but now not!!!

# SP0 and SP_t are different now ...


# ---------------------- DEBUG ----------------------

sim <- 1
replist <- RepList[[sim]]

HistYears <- Years(Hist@OM, 'H')

OM_Value <- Number(Hist, byAge=TRUE) |> dplyr::mutate(Model='OM') |>
  dplyr::filter(Sim==sim, Stock=='Female') |>
  dplyr::select(Year, Age, Value)

AgeClasses <- GetSSAgeClasses(replist)
SS_Value <- replist$natage |>
  dplyr::filter(`Beg/Mid`=='B') |>
  dplyr::rename(Year=Yr, Stock=Sex) |>
  tidyr::pivot_longer(cols=as.character(AgeClasses)) |>
  dplyr::filter(Stock==1, Year>=1975) |>
  dplyr::select(Year, Seas, Time, name, value)

ss_time <- SS_Value$Time |> unique()
om_time <- OM_Value$Year |> unique()

i <- 1
SS_Value |> dplyr::filter(Time==ss_time[i])
OM_Value |> dplyr::filter(Year==om_time[i]) |> print(n=62)


i <- 3
SS_Value |> dplyr::filter(Time==ss_time[i])
OM_Value |> dplyr::filter(Year==om_time[i]) |> print(n=62)

i <- 15
SS_Value |> dplyr::filter(Time==ss_time[i], name==min(name))
OM_Value |> dplyr::filter(Year==om_time[i], Age<1)


replist$recruit |> dplyr::filter(Yr==1976)
853.701 *0.5* exp(-0.106429 )

# SS Recruitment ...
S0_ss <- 99807.8
S_ss <- 28425.6   # ss_sb$SpawnBio[2]

replist$timeseries |> dplyr::filter(Yr==1976) |>
  dplyr::select(Year=Yr, Seas, SpawnBio, Recruit_0)

mod(S_ss, S0_ss, R0*2, h)
replist$recruit |> dplyr::filter(Yr==1976)


# Why is SS SpawnBio different than OM?
replist$timeseries |> dplyr::filter(Yr==1973) |>
  dplyr::select(Year=Yr, Seas, SpawnBio, Recruit_0)

N_unfished <-  CalcUnfishedNumber_seasonal(OM, SP=TRUE)
sum(N_unfished$Female[1,,1] * OM@Stock$Female@Fecundity@MeanAtAge[1,,])

Hist@Unfished@Equilibrium@SProduction[1,1,1:4]
Hist@SProduction[1,1,1:4]

n_unfished <- replist$natage_annual_1_no_fishery |> dplyr::filter(Sex==1, Yr==1975)
n_unfished[1,4:ncol(n_unfished)]


endgrowth <- replist$endgrowth |> dplyr::filter(Sex==1)
endgrowth |> dplyr::select(Real_Age, Age_Beg, `Mat*Fecund`, Seas) |>
  dplyr::arrange(Real_Age)

OM@Stock$Female@Fecundity@MeanAtAge[1,,]




# OM Recruitment Calc
S <- Hist@SProduction[1,1,5]
S0 <- Hist@Unfished@Equilibrium@SProduction[1,1,5]

h <- Hist@OM@Stock$Female@SRR@Pars$h[1,1]
R0 <- Hist@OM@Stock$Female@SRR@R0[1,3]
mod <- Hist@OM@Stock$Female@SRR@Model
mod(S, S0, R0*2, h)


## SS Rec Calc
ss_sb <- replist$timeseries |> dplyr::filter(Yr>=1975) |>
  dplyr::select(Year=Yr, Seas, SpawnBio)

replist$timeseries |> dplyr::filter(Yr==min(Yr)) |>
  dplyr::select(Year=Yr, Seas, SpawnBio, Recruit_0)

replist$timeseries |> dplyr::filter(Yr==1976) |>
  dplyr::select(Year=Yr, Seas, SpawnBio, Recruit_0)


## Compare OM and SS SP0

replist$timeseries |> dplyr::filter(Yr==1975) |>
  dplyr::select(Year=Yr, Seas, SpawnBio, Recruit_0)


replist$natage |> dplyr::filter(Sex==1, Yr==1975)











853.701/2 * exp(-0.106429)

Rec_main$pred_recr/Rec_main$exp_recr

OM@Stock$Female@SRR@RecDevHist[1,]

# Compare spawning output


plot(ss_sb$SpawnBio, ylim=c(0, 40000))
lines(Hist@SProduction[1,1,], col='blue')




OM_Value |> dplyr::filter(Age==0.5, Value>0.001)


SS_Value |> dplyr::filter(name==0, Seas==3)
OM_Value |> dplyr::filter(Age==0.5, Value>0.001)


OM_Value |> dplyr::filter(Year==1975)




# -------------------- END DEBUG --------------------


# CompareSS_Biomass(RepList, Hist)
CompareSS_Landings(RepList, Hist) # ~ 5 - 10% lower

# initial n

LoadArgs(Simulate_om)


Hist@Unfished@Equilibrium@Number$Female[1,,1:4]


Hist@Number$Female[1,,1:8,1] |> round(2)

replist <- RepList$`1`
SSn <- replist$natage |> dplyr::filter(Yr==1975, `Beg/Mid`=='B', Sex==1)

sum(SSn[,28])
sum(Hist@Number$Female[1,62,1:4,1] )


slotNames(Hist)

Number(Hist)
Biomass(Hist)

L <- Landings(Hist, byAge=TRUE, byFleet=TRUE, byArea=TRUE)
array <- DF2Array(L)


Landings_Number <- function(Hist) {
  Hist@LandingsAtAge
}

Landings_Weight <- function(Hist) {
  NumberList <- Hist@LandingsAtAge
  purrr::map2(Hist@OM@Fleet, NumberList, \(fleet, numbers) {
    fleetWeight <- fleet@WeightFleet |> AddDimension('Area')
    ArrayMultiply(fleetWeight, numbers)
  })
}




yrs <- 1:
Hist@Number$Female[1,,yrs,1] |> round(2)

Hist@Number$Female[1,62,,1] |> plot(type='l')

LoadArgs('Simulate_om')





# ---------------------- DEBUG ----------------------
# Fix for equilibrium unfished ...
yr <- 1976
ind <- match(yr, OM@Years)
SS_n <- GetSSNatAge(replist, OM, yrs=yr)
colnames(SS_n) <- OM@Years[ind:(ind+3)]
SS_n[is.na(SS_n)] <- 0

df1 <- Array2DF(SS_n |> round(2)) |> dplyr::mutate(Model='SS3')
df2 <- Array2DF(Hist@Number[[1]][1,,ind:(ind+3),1] |> round(2)) |> dplyr::mutate(Model='OM')

df <- dplyr::bind_rows(df1, df2)

ggplot(df, aes(x=Year, y=Value, color=Model)) +
  facet_wrap(~Age, scales='free') +
  geom_line()


ggplot(df |> dplyr::group_by(Year, Model) |>
         dplyr::mutate(Value=sum(Value)),
       aes(x=Year, y=Value, color=Model)) +
  geom_line()


replist
AgeClasses <- GetSSAgeClasses(replist)
SS_Value <- replist$natage |>
  dplyr::filter(Yr%in%HistYears, `Beg/Mid`=='B') |>
  dplyr::rename(Year=Yr, Stock=Sex) |>
  tidyr::pivot_longer(cols=as.character(AgeClasses))


om <- testOM |> tinyErr() |> Convert()
UnfishedNumberAtAge <- CalcUnfishedNumber(om)
UnfishedNumberAtAge[[1]][1,,1:12]

om@Stock$Albacore@NaturalMortality@MeanAtAge[1,,1]


UnfishedNumberAtAge <- CalcUnfishedNumber(OM)

UnfishedNumberAtAge$Female[1,,1:12]


years <- df$Year |> unique()

df |> dplyr::filter(Year==years[1]) |>
  tidyr::pivot_wider(names_from = Model, values_from = Value) |>
  dplyr::mutate(Rel=SS3/OM) |>
  print(n=70)

df |> dplyr::filter(Year==years[1]) |>
  dplyr::group_by(Model) |>
  dplyr::summarise(sum(Value))

# -------------------- END DEBUG --------------------


# ---------------------- DEBUG ----------------------
# Issue: OM number and biomass match SS3 output
#        but


Data <- OM@Data$`Female Male`
Data@Landings@Value[188,]


#  - need to replace the empirical fleet weight stuff i added with this ...
replist <- RepList[[1]]
Landings <- Landings(Hist, byFleet=TRUE)

year <- 1980
flname <- 'F10_JPN_WCNPO_OSDF'
yearind <- match(year, OM@Years)
fleetind <- match(flname, FleetNames(OM))
replist$catch |> dplyr::filter(Yr==year, Seas==1, Fleet_Name==flname)
Landings |> dplyr::filter(Year==year) |>
  dplyr::filter(Sim==1, Fleet==flname) |>
  dplyr::group_by(Year, Fleet) |>
  dplyr::summarise(Value=sum(Value))

SSAgeClasses <- GetSSAgeClasses(replist)

NAA <- replist$natage |> dplyr::filter(Yr==year, Seas==1,
                                `Beg/Mid`=='M') |>
  dplyr::select(as.character(SSAgeClasses))

ageind <- seq(1, by=4, to=62)
NAL1 <- as.numeric(NAA[1,]) %*% Hist@OM@Stock$Female@Length@ASK[1,ageind,,1]
NAL2 <- as.numeric(NAA[2,]) %*% Hist@OM@Stock$Male@Length@ASK[1,ageind,,1]

sel1 <- OM@Fleet$Female$F10_JPN_WCNPO_OSDF@Selectivity@MeanAtLength[1,,1]
sel2 <- OM@Fleet$Male$F10_JPN_WCNPO_OSDF@Selectivity@MeanAtLength[1,,1]

overallF <- apply(Hist@FDead$Female[1,, yearind, ], 'Age', sum)
apicalF1 <- max(overallF)
F1 <- max(Hist@FDead$Female[1,, yearind, fl])

overallF <- apply(Hist@FDead$Male[1,, yearind, ], 'Age', sum)
apicalF2 <- max(overallF)
F2 <- max(Hist@FDead$Male[1,, yearind, fl])

t = replist$timeseries |> dplyr::filter(Yr==year, Seas==1)
t$`F:_10`

Z1 <- apicalF1 + 0.0550
Z2 <- apicalF2 + 0.0900
CAAL1 <- NAL1 * sel1 * F1/Z1 * (1-exp(-Z1))
CAAL2 <- NAL2 * sel2 * F2/Z2 * (1-exp(-Z2))

sum(CAAL1 * replist$biology$Wt_F) +
sum(CAAL2 * replist$biology$Wt_M)


replist$ALK[,,1]
replist$ALK[,,2]

# TODO
# Calculate Fleet-Specific Weight-at-Age
st <- 1
fl <- 1
Classes <- OM@Stock[[st]]@Length@Classes

replist$sizeselex |> dplyr::filter(Sex==st, Fleet==fl)

replist$biology$Len_mean |> length()
replist$sizeselex |> dim()
dimnames(replist$ALK)
replist$ALK
matplot(replist$ALK[,1], type='l')

OM@Fleet$Female$F1_JPN_WCNPO_OSDWCOLL_late_Area1@Selectivity@MeanAtLength[1,,3]
Hist@Number$Female |> dim()




# -------------------- END DEBUG --------------------






Data <- OM@Data$`Female Male`

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

OM@Complexes$`Female Male`

Data <- OM@Data$`Female Male`

Data@CPUE@Name

Data@Survey@Name

Data@Landings@Value |> apply('Year', sum) |> plot(type='l')
df <- Data@Survey@Value |> Array2DF()

ggplot(df, aes(x=Year, y=Value)) +
  geom_point() +
  facet_wrap(~Fleet, scales='free') +
  theme_bw()


