
# TODO
# - develop diagnostics for the stochastic OMs - probably R0 and F - some have F ~ 0
# - compare stock dynamics from RefOM and stochastic OMs - initial


library(NPSWO)

# UpdatePackages()

# ---- OM Diagnostics -----

All_OMs <- ListOMs()

OMList <- purrr::map(All_OMs, LoadOM)
names(OMList) <- All_OMs

base_df <- GetMaxApicalF(OMList$Base, 'Base')
refom_df <- GetMaxApicalF(OMList$RefOM, 'RefOM')
wcpo_df <- GetMaxApicalF(OMList$WCPO_only, 'WCPO_only')


# Model is super unstable!!


# Identify Sims where maximum F is ~ 0

OM =OMList$RefOM
model = 'RefOM'

GetMaxApicalF <- function(OM, model) {
  M_Female <- OM@Stock$Female@NaturalMortality@MeanAtAge[,62, , drop=FALSE] |>
    ReduceDims(IncYear=TRUE) |> Array2DF() |> dplyr::select(Sim, Value) |>
    dplyr::rename(M_Female=Value)

  M_Male <- OM@Stock$Male@NaturalMortality@MeanAtAge[,62,, drop=FALSE] |>
    ReduceDims(IncYear=TRUE) |> Array2DF()  |> dplyr::select(Sim, Value) |>
    dplyr::rename(M_Male=Value)

  h <- OM@Stock$Male@SRR@Pars$h |> Array2DF()  |> dplyr::select(Sim, Value) |>
    dplyr::rename(h=Value)

  LHdf <- dplyr::left_join(M_Female, M_Male, by = dplyr::join_by(Sim)) |>
    dplyr::left_join(h, by = dplyr::join_by(Sim))

  purrr::map(OM@Fleet, \(StockFleet) {
    purrr::map(StockFleet, \(Fleet) {
      apicalF <- ArrayMultiply(Fleet@Effort@Effort, Fleet@Catchability@Efficiency)
      apply(apicalF, "Sim", max) * Seasons(OM)
    }) |> List2Array()
  }) |> List2Array('Stock') |>
    aperm(c('Sim', 'Stock', 'Fleet')) |>
    Array2DF() |>
    dplyr::mutate(Value=round(Value,4), Model=model) |>
    dplyr::left_join(LHdf, by = dplyr::join_by(Sim)) |>
    dplyr::arrange(Sim, Stock, Fleet)

}


OMList$Base@Fleet$Female$F1_JPN_WCNPO_OSDWCOLL_late_Area1@Effort@Effort
OMList$Base@Fleet$Female$F1_JPN_WCNPO_OSDWCOLL_late_Area1@Catchability@Efficiency

OMList$RefOM@Stock$Female@SRR@R0 |> apply('Sim', mean) |> range()
OMList$Base@Stock$Female@SRR@R0 |> apply('Sim', mean) |> range()
OMList$WCPO_only@Stock$Female@SRR@R0 |> apply('Sim', mean) |> range()

# Re-save OMs
# Rebuild NPSWO.OM Package
# Push NPSWO.OM to GitHub


# ---- Run Historical Simulations and Save  ----

# List of available OMs in the NPSWO.OM package
All_OMs <- ListOMs()



# Select the OMs to run Simulate on:
Run_OMs <- All_OMs

# Runs `Simulate` for all OMs in Run_OMs and saves to disk using SaveHist
# This will overwrite any existing .hist files for the same OMs
RunSimulations(Run_OMs)


# ---- Explore Diagnostics -----

ListHist()

RefOM_hist     <- LoadHist("RefOM")
Base_hist      <- LoadHist("Base")
WCPO_only_hist <- LoadHist("WCPO_only")


#### Becky 4/24:#################################################################
# List of available OMs in the NPSWO.OM package
All_OMs <- ListOMs()

# Select the OMs to run Simulate on:
Run_OMs <- All_OMs[c(17, 18, 20, 21)]
Run_OMs
# Runs `Simulate` for all OMs in Run_OMs and saves to disk using SaveHist
# This will overwrite any existing .hist files for the same OMs
RunSimulations(Run_OMs)


# ---- Explore Diagnostics -----

ListHist()

base_hist  <- LoadHist("0_Final_base_case")
lowM_hist  <- LoadHist("2_base_case_lowM")
highM_hist <- LoadHist("1_base_case_highM")
lowh_hist  <- LoadHist("4_Sensitivity_h081")
highh_hist <- LoadHist("5_Sensitivity_h099")
#################################################################################

# TODO: develop plotting functions

Ref_df <- Array2DF(RefOM@Biomass) |> dplyr::mutate(Model='Ref')
Base_df <- Array2DF(Base@Biomass) |> dplyr::mutate(Model='Base')
WCPO_only_df <- Array2DF(WCPO_only@Biomass) |> dplyr::mutate(Model='WCPO_only')

ggplot2::ggplot(Ref_df, ggplot2::aes(x=Year, y=Value, group=Sim)) +
  ggplot2::facet_grid(~Stock) +
  ggplot2::geom_line()

ggplot2::ggplot(Base_df, ggplot2::aes(x=Year, y=Value, group=Sim)) +
  ggplot2::facet_grid(~Stock) +
  ggplot2::geom_line()

ggplot2::ggplot(WCPO_only_df, ggplot2::aes(x=Year, y=Value, group=Sim)) +
  ggplot2::facet_grid(~Stock) +
  ggplot2::geom_line()


# explore diagnostics, subset to nSim2


# Subset and Re-Save Hist objects






