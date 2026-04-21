library(NPSWO)


# UpdatePackages() # if needed

# List of available Hist objects saved to disk

ListHist()

Hist <- LoadHist("Base")


# Historical Data from `Base` OM for testing MPs
Data <- Data(Hist)[[1]][[1]]

# ------ Indices ----
#
# `Late` surveys:
#  - "S2_JPN_WCNPO_OSDWCOLL_late_Area1" "
#  - "S4_JPN_WCNPO_OSDWLL_late_Area2"
#  - "S5_TWN_WCNPO_DWLL_late"
#  - "S6_US_WCNPO_LL_deep"
#  - "S8_US_WCNPO_LL_shallow_late"

# S5 and S8 not included in assessment (see Abundance Indices section in BILLWG_SAR.docx)

# selecting S2 and S4 - both have similar trends


# ---- Iterative Effort Control ----
# Calculates the multiplicative effort adjustment (deltaEffort) for the
# Itarget-style HCR. Combines multiple survey indices using inverse-variance
# weighting, computes a historical reference level and a current level (each
# as `nYear` means), and returns the ratio of current to target index,
# capped within [1 - MaxChange, 1 + MaxChange].
CalcDeltaEffort <- function(Data,
                            survey_fleets,
                            nYear,
                            relTarget,
                            tunepar,
                            MaxChange) {

  survey_value <- Array2DF(Data@Survey@Value[,survey_fleets]) |>
    dplyr::filter(!is.na(Value)) |>
    dplyr::mutate(Year=round(Year,0))

  survey_cv <- Array2DF(Data@Survey@CV[,survey_fleets]) |>
    dplyr::rename(CV=Value) |>
    dplyr::filter(!is.na(CV)) |>
    dplyr::mutate(Year=round(Year,0))

  df <- dplyr::left_join(survey_value, survey_cv, by=dplyr::join_by('Year', 'Fleet'))

  # weighted mean - inverse variance
  df <- df |>
    dplyr::group_by(Year) |>
    dplyr::summarise(
      Value = stats::weighted.mean(Value, w = 1 / CV^2, na.rm = TRUE),
      CV    = sqrt(1 / sum(1 / CV^2, na.rm = TRUE))  # combined CV from inverse-variance weights
    )

  # Historical Index
  # Mean over last `nYear` years
  Last_nYears <- tail(df$Year[df$Year <= Data@YearLH],nYear)
  Historical <- df |> dplyr::filter(Year%in% Last_nYears) |>
    dplyr::summarise(Mean=mean(Value, na.rm=TRUE)) |>
    dplyr::pull(Mean)

  Historical <- Historical * tunepar  # adjust by `tunepar` for tuning

  # Target index level
  Target <- relTarget * Historical

  # Current Index
  Last_nYears <- tail(df$Year, nYear)
  Current <- df |> dplyr::filter(Year%in% Last_nYears) |>
    dplyr::summarise(Mean=mean(Value, na.rm=TRUE)) |>
    dplyr::pull(Mean)

  deltaEffort <- Current/Target

  if (deltaEffort>(1+MaxChange))
    deltaEffort <- 1+MaxChange

  if (deltaEffort<(1-MaxChange))
    deltaEffort <- 1-MaxChange

  deltaEffort
}
class(CalcDeltaEffort) <- 'mp'


# Management procedure (MP) implementing a seasonal Itarget HCR based on
# effort controls. At the start of each year (Season 1), calls CalcDeltaEffort()
# to compute a new effort adjustment and stores it in the Misc slot so it can
# be retrieved unchanged in subsequent seasons. Each season, the effort advice
# is set by scaling the same-season effort from the previous year by
# deltaEffort. Returns an Advice object with absolute effort regulations.
IterEff <- function(Data,
                    survey_fleets = c(2,4),
                    nYear = 3,
                    relTarget = 0.8,
                    tunepar = 1,
                    MaxChange = 0.1) {

  # Create a new `advice` object specifying Absolute effort regulations
  advice <- Advice(EffType = 'Abs')

  # Determine Current Season
  nSeason <- Seasons(Data)
  nYear   <- length(Data@Years)
  ThisSeason <- nYear %% nSeason + 1  # current season (1 to nSeason)

  # Get real effort from OM
  OM_Effort <- Data@Misc$DataOM@Effort[1,,]

  # Last time step
  lastTS <- length(Data@Years)

  # Index for the same season in the previous year:
  # go back nSeason steps from the same-season slot of last complete year
  SameSeasonLastYear <- lastTS - nSeason + 1
  LastYearEffort <- OM_Effort[SameSeasonLastYear, ]

  if (ThisSeason==1) {
    # beginning of the year - set new management advice
    deltaEffort <- CalcDeltaEffort(Data, survey_fleets,
                                   nYear, relTarget,
                                   tunepar, MaxChange)

  } else {
    deltaEffort <- Misc(Data)$deltaEffort
  }

  # assign to Misc slot so it can be retrieved next time step
  Misc(advice) <- list(deltaEffort=deltaEffort)

  # relative effort adjustment
  NewEffort <- LastYearEffort * deltaEffort

  Effort(advice) <- NewEffort

  advice
}
class(IterEff) <- 'mp'


###### TEST ###################################

MSE <- Project(Hist, MPs='IterEff', nSim=3)

b <- c(MSE@Hist@SBiomass[sim,1,], MSE@SBiomass[sim,1,,1])
b0 <- mean(MSE@Unfished@Equilibrium@SBiomass[sim,1,])
b_bo <- b/b0
plot(b_bo, type='l', ylim=c(0, max(b_bo)))

MSE@Misc$Advice$IterEff$`2022`$`1`$`Female Male`

MSE@Effort[1,,,1]


# need to fix - such that it only adjust effort every year not every time step

sim <- 2
fl <- 1
eff <- c(MSE@Hist@Effort[sim,,fl],MSE@Effort[sim,,fl,1])

b0 <- mean(MSE@Unfished@Equilibrium@Biomass[sim,1,])
b <- c(MSE@Hist@Biomass[sim,1,], MSE@Biomass[sim,1,, 1]) / b0

f <- c(MSE@Hist@FDead[sim,1,,fl], MSE@FDead[sim,1,,fl,1])


par(mfrow=c(2,2))
plot(b, type='l', ylim=c(0,max(b)))
plot(eff, type='l',  ylim=c(0,max(eff)))
plot(f, type='l',  ylim=c(0,max(f)))

# need to keep Hist@Data in the MSE object

# Write GetPPD function - Sim, Stock - automatically join Hist data to it ...

MSE@PPD$IterEff[[sim]]$`Female Male`@Survey




cbind(MSE@Misc$Advice$IterEff[[1]]$`1`$`Female Male`@Effort,
MSE@Misc$Advice$IterEff[[2]]$`1`$`Female Male`@Effort)

###############################################



## ----- Variants ----

IterEff1

IterEff2







# Quick Test to get an MSE object

MSE <- Project(Hist, MPs="StatusQuo")

Years <- MSE@PPD$StatusQuo$`1`$`Female Male`@Years |> round(0)
Years[Years>Data@YearLH] |> length()

length(MSE@PPD$StatusQuo$`1`$`Female Male`@Years)
MSE@PPD$StatusQuo$`1`$`Female Male`@Advice@Effort[2,,1]

dimnames(MSE@PPD$StatusQuo$`1`$`Female Male`@Advice@Effort)

## DEV #############################


b <- Hist@Biomass |> SumOverStock() |> Array2DF()

# scaling issue
ggplot(b, aes(x=Year, y=Value, color=as.factor(Sim))) +
  geom_line() + guides(color='none')


# Example 1 - TODO
DynamicEffort1 <- function(Data) {

  # Calculate weighted mean index over `survey_fleets`
  # four `Late` surveys: S5 and S8 not included in assessment (see Abundance
  # Indices section in BILLWG_SAR.docx)

  # S2_JPN_WCNPO_OSDWCOLL_late_Area1 & S2_JPN_WCNPO_OSDWCOLL_late_Area1
  survey_fleets <- c(2,4)

  survey_value <- Array2DF(Data@Survey@Value[,survey_fleets]) |>
    dplyr::filter(!is.na(Value)) |>
    dplyr::mutate(Year=round(Year,0))

  survey_cv <- Array2DF(Data@Survey@CV[,survey_fleets]) |>
    dplyr::rename(CV=Value) |>
    dplyr::filter(!is.na(CV)) |>
    dplyr::mutate(Year=round(Year,0))

  df <- dplyr::left_join(survey_value, survey_cv, by=dplyr::join_by('Year', 'Fleet'))

  # weighted mean - inverse variance
  df <- df |>
    dplyr::group_by(Year) |>
    dplyr::summarise(
      Value = stats::weighted.mean(Value, w = 1 / CV^2, na.rm = TRUE),
      CV    = sqrt(1 / sum(1 / CV^2, na.rm = TRUE))  # combined CV from inverse-variance weights
    )


  # Last Historical Index
  LastHist <- df |> dplyr::filter(Year==Data@YearLH)




}
class(DynamicEffort1) <- 'mp'

MPs <- "DynamicEffort1"
MSE <- Project(Hist, MPs=MPs)

e <- rbind(MSE@Hist@Effort[3,,], MSE@Effort[3,,,1])
matplot(e[,5], type='l')
plot(c(MSE@Hist@Biomass[3,1,], MSE@Biomass[3,1,,1]), type='l')

Data@Landings@Units
Data@Survey@Units



replist <- ImportSSReport()
replist$survey_units


indices_data <- Data@Survey

plot_timeseries_data <- function(timeseries, title=NULL) {

  if (MSEtool:::isNewObject(timeseries))
    return(invisible(NULL))

  units_df <- data.frame(Fleet = timeseries@Name,
                         Units = timeseries@Units)

  value_df <- Array2DF(timeseries@Value)

  if (all(value_df$Value==0))
    return(invisible(NULL))

  df <- dplyr::left_join(value_df, units_df, by=dplyr::join_by("Fleet")) |>
    dplyr::filter(!is.na(Value)) |>
    dplyr::mutate(
      Year = as.Date(lubridate::date_decimal(Year)),
      Fleet_label = paste0(Fleet, "\n(", Units, ")")
    )

  df$Fleet_label <- factor(df$Fleet_label,
                           levels=unique(df$Fleet_label),
                           ordered = TRUE)

  ggplot2::ggplot(df, ggplot2::aes(x = Year, y = Value)) +
    ggplot2::facet_wrap(~Fleet_label, scales = "free_y") +
    ggplot2::geom_line(na.rm = TRUE) +
    ggplot2::expand_limits(y = 0) +
    ggplot2::scale_x_date(date_labels = "%Y", date_breaks = "5 years") +
    ggplot2::labs(title = title, x = NULL, y = NULL) +
    ggplot2::theme_bw()

}

plot_timeseries_data(Data@Survey, "Survey")
plot_timeseries_data(Data@Landings, "Landings")

