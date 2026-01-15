# Requires the latest development version of MSEtool
# Always update as it is under constant development
# pak::pkg_install('blue-matter/MSEtool@prerelease')


packageVersion("MSEtool") # should be v4+

library(MSEtool)
library(ggplot2)
library(patchwork)

# ----- Import Base Case OM ----
# File path to the SS3 files in WCNPOSWO-2023
SSDir <- "../WCNPOSWO-2023/Final Base-case"

# Import the SS3 Output
RepList <- ImportSSReport(SSDir)

# Age-0 is half the actual value, presumably because SS3 has 2 seasons for Age-0
# while OM has 4 seasons for all age-classes
RepList[[1]]$M_at_age[, 4] <- RepList[[1]]$M_at_age[, 4] * 2

# Meta-data
Name <- "North Pacific Swordfish"
StockName <- c("Female", "Male")
Species <- "Xiphias gladius"
Region <- "North Pacific"

# OM Settings - to be updated
Interval <- 1 # Management Interval
DataLag <- 0 #
nSim <- 2 # small for demo
pYear <- 10 # number of projection years

# Generate OM from SS3 output
OM <- ImportSS(RepList,
  Name,
  nSim,
  pYear,
  Region = Region,
  StockName = StockName,
  Species = Species,
  Interval = Interval,
  DataLag = DataLag,
  UpdateRecDevs=TRUE
)

# TODO - make historical effort same across stocks for a given fleet

# ---- Simulate Historical Fishery ----
Hist <- Simulate(OM)

B <- Biomass(Hist) |>
  dplyr::filter(Sim==1) |>
  dplyr::group_by(Year, Variable) |>
  dplyr::summarise(Value=sum(Value))

L <- Landings(Hist) |>
  dplyr::filter(Sim==1) |>
  dplyr::group_by(Year, Variable) |>
  dplyr::summarise(Value=sum(Value))

df <- dplyr::bind_rows(B, L)

p1 <- ggplot(df, aes(x=Year, y=Value)) +
  facet_wrap(~Variable, scales='free_y') +
  geom_line(linewidth=0.5) +
  expand_limits(y=0) +
  theme_classic() +
  labs(y='')

ggsave("Figures/Historical.png", width=7, height=3)


# ---- Example MPs -----
Data <- Data(Hist)[[1]][[1]]

SeasonalEffort <- Hist@Effort[1,1,185:188,]

CurrentEffort <- function(Data) {
  # Seasonal Effort
  nSeason <- 4
  seasonIndex <- length(Data@Years) %% nSeason + 1
  advice <- Advice()
  advice@Effort <- SeasonalEffort[seasonIndex,]
  advice
}
class(CurrentEffort) <- 'mp'

L <- Landings(Hist, byFleet=TRUE) |>
  dplyr::filter(Sim==1) |>
  dplyr::group_by(Year, Fleet) |>
  dplyr::summarise(Value=sum(Value))

LastYear <- L$Year |> unique() |> tail(4)

SeasonalCatch <- L |>
  dplyr::filter(Year%in%LastYear) |>
  dplyr::pull(Value) |>
  matrix(4, 19, byrow=TRUE)

CurrentCatch <- function(Data) {
  # Total Seasonal Catch
  nSeason <- 4
  seasonIndex <- length(Data@Years) %% nSeason + 1
  advice <- Advice()
  advice@TAC <- SeasonalCatch[seasonIndex,]
  advice
}
class(CurrentCatch) <- 'mp'

MPs <- c('CurrentEffort', 'CurrentCatch')

MSE <- Project(Hist, MPs=MPs)


B <- B_B0(MSE) |>
  dplyr::filter(Sim==1) |>
  dplyr::ungroup() |>
  dplyr::group_by(Year, Period, Variable, MP) |>
  dplyr::summarise(Value=sum(Value), .groups='drop')

ggplot(B, aes(x=Year, y=Value, color=MP)) +
  geom_hline(yintercept = 1, linetype=2) +
  geom_line() +
  expand_limits(y=0) +
  theme_classic() +
  labs(y='B/B0')

HistLandings <- Hist@Landings |>
  MSEtool:::ExtendSims(nSim(Hist)) |>
  MSEtool:::AddDimension('MP', 'Historical') |> Array2DF()

ProjLandings <- MSE@Landings |> Array2DF()

L <- dplyr::bind_rows(HistLandings, ProjLandings) |>
  dplyr::filter(Sim==1) |>
  dplyr::ungroup() |>
  dplyr::group_by(Year, MP) |>
  dplyr::summarise(Value=sum(Value), .groups='drop')

ggplot(L, aes(x=Year, y=Value, color=MP)) +
  geom_hline(yintercept = 1, linetype=2) +
  geom_line() +
  expand_limits(y=0) +
  theme_classic() +
  labs(y='Landings')


st <- 1
fl <- 1
Hist@Effort[1,st,185:188,fl]
MSE@Effort[1,st,1:4,fl,1]


Hist@Landings[1,st,185:188,fl]
MSE@Landings[1,st,1:4,fl,2]

SeasonalCatch[,fl]
MSE@Landings[1,1,1:4,fl,2] + MSE@Landings[1,2,1:4,fl,2]











