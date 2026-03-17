library(NPSWO)

# UpdatePackages() # if needed

# List of available Hist objects saved to disk

ListHist()

Hist <- LoadHist("Base")

# Quick Test to get an MSE object

MSE <- Project(Hist, MPs="StatusQuo")



## DEV #############################

# Historical Data from `Base` OM for testing MPs
Data <- Data(Hist)[[1]][[1]]

b <- Hist@Biomass |> SumOverStock() |> Array2DF()

# scaling issue
ggplot(b, aes(x=Year, y=Value, color=as.factor(Sim))) +
  geom_line() + guides(color='none')


# Example 1 - TODO
DynamicEffort1 <- function(Data) {

  # Calculate weighted mean index over `survey_fleets`
  survey_fleets <- c(2,4,5)

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

