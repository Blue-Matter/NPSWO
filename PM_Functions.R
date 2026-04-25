
#prob that SSB < SBMSY
Safety_MSY <- function(MSEobj, Ref=1, Yrs=NULL) {
  Yrs <- chk_yrs(Yrs, MSEobj)
  PMobj <- new("PMobj")
  PMobj@Name <- "Spawning Biomass relative to SBMSY"
  if (Ref != 1) {
    PMobj@Caption <- paste0("Prob. SB < ", Ref, " SBMSY (Proj. Years ",
                            Yrs[1], " - ", Yrs[2]/4, ")")
  }
  else {
    PMobj@Caption <- paste0("Prob. SB < SBMSY (Proj. Years ", Yrs[1],
                            " - ", Yrs[2]/4, ")")
  }
  PMobj@Ref <- Ref

  ###############################################################################
  #calling RP from OM because not yet included in mse object - REMOVE WHEN FIXED
  #importing OM with the same name as the current MSEobj
  dir_list  <- mixedsort(list.dirs(ssdir_all, recursive=FALSE))
  RepList   <- ImportSSReport(dir_list[grepl(MSEobj@OM@Name, dir_list, ignore.case = TRUE)])
  #Pulling out SBMSY
  DerQuants <- RepList$`1`$derived_quants
  SBMSY <- DerQuants[DerQuants$Label == "SSB_MSY", ]
  SBMSY <- SBMSY$Value
  ################################################################################

  #calculating annual values of SBiomass (female only)
  SBiomass <- MSEobj@SBiomass[,1,Yrs[1]:Yrs[2],]
  Annual_SBiomass <- as.data.frame(SBiomass) |>   #averaging across seasons to get annual values
    tibble::rownames_to_column("Sim") |>
    tidyr::pivot_longer(-Sim, names_to = "Year", values_to = "Value") |>
    dplyr::mutate(Year = substr(Year, 1, 4)) |>
    dplyr::summarise(Value = mean(Value), .by = c(Sim, Year)) |>
    tidyr::pivot_wider(names_from = Year, values_from = Value)

  Annual_SBiomass <- as.data.frame(Annual_SBiomass)
  PMobj@Stat <- as.array(as.matrix((Annual_SBiomass |>
      dplyr::mutate(dplyr::across(-Sim, ~ round(.x / SBMSY,2))))))
  #dropping Sim column and making numeric otherwise calcPrb doesn't work correctly
  PMobj@Prob <- calcProb(apply(PMobj@Stat[,2:ncol(PMobj@Stat)], 2, as.numeric) < PMobj@Ref)
  #PMobj@Mean <- calcMean(PMobj@Prob, MSEobj) # TO DO: circle back to calcMean() function - not working
  PMobj@Mean <- mean(PMobj@Prob)
  PMobj@MPs  <- names(MSEobj@MPs)
  PMobj
}
class(Safety_MSY) <- 'pm'


#########################################################################################################################

#prob that SSB < 17%SSB0
Safety_17SSB0 <- function(MSEobj, Ref=.17, Yrs=NULL) {
  Yrs <- chk_yrs(Yrs, MSEobj)
  PMobj <- new("PMobj")
  PMobj@Name <- "Spawning Biomass relative to 17%SSB0 (BMSY)"
  if (Ref != 1) {
    PMobj@Caption <- paste0("Prob. SB < ", Ref, " SSB0 (Proj. Years ",
                            Yrs[1], " - ", Yrs[2]/4, ")")
  }
  else {
    PMobj@Caption <- paste0("Prob. SB < SSB0 (Proj. Years ", Yrs[1],
                            " - ", Yrs[2]/4, ")")
  }
  PMobj@Ref <- Ref

  ###############################################################################
  #calling RP from OM because not yet included in mse object - REMOVE WHEN FIXED
  #importing OM with the same name as the current MSEobj
  dir_list  <- mixedsort(list.dirs(ssdir_all, recursive=FALSE))
  RepList   <- ImportSSReport(dir_list[grepl(MSEobj@OM@Name, dir_list, ignore.case = TRUE)])
  #Pulling out SSB0
  DerQuants <- RepList$`1`$derived_quants
  SSB0 <- DerQuants[DerQuants$Label == "SSB_unfished", ]
  SSB0 <- SSB0$Value
  ################################################################################

  #calculating annual values of SBiomass (female only)
  SBiomass <- MSEobj@SBiomass[,1,Yrs[1]:Yrs[2],]
  Annual_SBiomass <- as.data.frame(SBiomass) |>   #averaging across seasons to get annual values
    tibble::rownames_to_column("Sim") |>
    tidyr::pivot_longer(-Sim, names_to = "Year", values_to = "Value") |>
    dplyr::mutate(Year = substr(Year, 1, 4)) |>
    dplyr::summarise(Value = mean(Value), .by = c(Sim, Year)) |>
    tidyr::pivot_wider(names_from = Year, values_from = Value)

  Annual_SBiomass <- as.data.frame(Annual_SBiomass)
  PMobj@Stat <- as.array(as.matrix((Annual_SBiomass |>
                                      dplyr::mutate(dplyr::across(-Sim, ~ round(.x / SSB0,2))))))
  #dropping Sim column and making numeric otherwise calcPrb doesn't work correctly
  PMobj@Prob <- calcProb(apply(PMobj@Stat[,2:ncol(PMobj@Stat)], 2, as.numeric) < PMobj@Ref)
  #PMobj@Mean <- calcMean(PMobj@Prob, MSEobj) # TO DO: circle back to calcMean() function - not working
  PMobj@Mean <- mean(PMobj@Prob)
  PMobj@MPs  <- names(MSEobj@MPs)
  PMobj
}
class(Safety_17SSB0) <- 'pm'


#########################################################################################################################


#prob that SSB > 25%SSB0
Status_25SSB0 <- function(MSEobj, Ref=0.25, Yrs=NULL) {
  Yrs <- chk_yrs(Yrs, MSEobj)
  PMobj <- new("PMobj")
  PMobj@Name <- "Spawning Biomass relative to 25% SSB0"
  if (Ref != 1) {
    PMobj@Caption <- paste0("Prob. SB > ", Ref, " SSB0 (Proj. Years ",
                            Yrs[1], " - ", Yrs[2]/4, ")")
  }
  else {
    PMobj@Caption <- paste0("Prob. SB > SSB0 (Proj. Years ", Yrs[1],
                            " - ", Yrs[2]/4, ")")
  }
  PMobj@Ref <- Ref

  ###############################################################################
  #calling RP from OM because not yet included in mse object - REMOVE WHEN FIXED
  #importing OM with the same name as the current MSEobj
  dir_list  <- gtools::mixedsort(list.dirs(ssdir_all, recursive=FALSE))
  RepList   <- ImportSSReport(dir_list[grepl(MSEobj@OM@Name, dir_list, ignore.case = TRUE)])
  #Pulling out SSB0
  DerQuants <- RepList$`1`$derived_quants
  SSB0 <- DerQuants[DerQuants$Label == "SSB_unfished", ]
  SSB0 <- SSB0$Value
  ################################################################################

  #calculating annual values of SBiomass (female only)
  SBiomass <- MSEobj@SBiomass[,1,Yrs[1]:Yrs[2],]
  Annual_SBiomass <- as.data.frame(SBiomass) |>   #averaging across seasons to get annual values
    tibble::rownames_to_column("Sim") |>
    tidyr::pivot_longer(-Sim, names_to = "Year", values_to = "Value") |>
    dplyr::mutate(Year = substr(Year, 1, 4)) |>
    dplyr::summarise(Value = mean(Value), .by = c(Sim, Year)) |>
    tidyr::pivot_wider(names_from = Year, values_from = Value)

  Annual_SBiomass <- as.data.frame(Annual_SBiomass)
  PMobj@Stat <- as.array(as.matrix((Annual_SBiomass |>
                                      dplyr::mutate(dplyr::across(-Sim, ~ round(.x / SSB0,2))))))
  #dropping Sim column and making numeric otherwise calcPrb doesn't work correctly
  PMobj@Prob <- calcProb(apply(PMobj@Stat[,2:ncol(PMobj@Stat)], 2, as.numeric) > PMobj@Ref)
  #PMobj@Mean <- calcMean(PMobj@Prob, MSEobj) # TO DO: circle back to calcMean() function - not working
  PMobj@Mean <- mean(PMobj@Prob)
  PMobj@MPs  <- names(MSEobj@MPs)
  PMobj
}
class(Status_25SSB0) <- 'pm'



#########################################################################################################################


#prob that SSB > 50%SSB0
Status_50SSB0 <- function(MSEobj, Ref=0.5, Yrs=NULL) {
  Yrs <- chk_yrs(Yrs, MSEobj)
  PMobj <- new("PMobj")
  PMobj@Name <- "Spawning Biomass relative to 50%SSB0"
  if (Ref != 1) {
    PMobj@Caption <- paste0("Prob. SB > ", Ref, " SSB0 (Proj. Years ",
                            Yrs[1], " - ", Yrs[2]/4, ")")
  }
  else {
    PMobj@Caption <- paste0("Prob. SB > SSB0 (Proj. Years ", Yrs[1],
                            " - ", Yrs[2]/4, ")")
  }
  PMobj@Ref <- Ref

  ###############################################################################
  #calling RP from OM because not yet included in mse object - REMOVE WHEN FIXED
  #importing OM with the same name as the current MSEobj
  dir_list  <- gtools::mixedsort(list.dirs(ssdir_all, recursive=FALSE))
  RepList   <- ImportSSReport(dir_list[grepl(MSEobj@OM@Name, dir_list, ignore.case = TRUE)])
  #Pulling out SSB0
  DerQuants <- RepList$`1`$derived_quants
  SSB0 <- DerQuants[DerQuants$Label == "SSB_unfished", ]
  SSB0 <- SSB0$Value
  ################################################################################

  #calculating annual values of SBiomass (female only)
  SBiomass <- MSEobj@SBiomass[,1,Yrs[1]:Yrs[2],]
  Annual_SBiomass <- as.data.frame(SBiomass) |>   #averaging across seasons to get annual values
    tibble::rownames_to_column("Sim") |>
    tidyr::pivot_longer(-Sim, names_to = "Year", values_to = "Value") |>
    dplyr::mutate(Year = substr(Year, 1, 4)) |>
    dplyr::summarise(Value = mean(Value), .by = c(Sim, Year)) |>
    tidyr::pivot_wider(names_from = Year, values_from = Value)

  Annual_SBiomass <- as.data.frame(Annual_SBiomass)
  PMobj@Stat <- as.array(as.matrix((Annual_SBiomass |>
                                      dplyr::mutate(dplyr::across(-Sim, ~ round(.x / SSB0,2))))))
  #dropping Sim column and making numeric otherwise calcPrb doesn't work correctly
  PMobj@Prob <- calcProb(apply(PMobj@Stat[,2:ncol(PMobj@Stat)], 2, as.numeric) > PMobj@Ref)
  #PMobj@Mean <- calcMean(PMobj@Prob, MSEobj) # TO DO: circle back to calcMean() function - not working
  PMobj@Mean <- mean(PMobj@Prob)
  PMobj@MPs  <- names(MSEobj@MPs)
  PMobj
}
class(Status_50SSB0) <- 'pm'


#########################################################################################################################


#Average Annual Catch (Males and Females) across all Fleets for all years of the projection period
AvgAnn_Catch <- function(MSEobj, Yrs=NULL) { #years argument will have to be whatever you want *4 because it's a seasonal model
  Yrs <- chk_yrs(Yrs, MSEobj)
  PMobj <- new("PMobj")
  PMobj@Name <- "Average Annual Catch (all fleets)"
  PMobj@Caption <- paste0('Average Catch (Years ', Yrs[1], ' - ', Yrs[2]/4, ')')
  Catch <- MSEobj@Landings[,1,Yrs[1]:Yrs[2],,] + MSEobj@Landings[,2,Yrs[1]:Yrs[2],,]
  Catch_allFleets <- apply(Catch, c(1,2), sum) #summing over fleets
  Catch_Annual <- as.data.frame(Catch_allFleets) |>   #averaging across seasons to get annual values
    tibble::rownames_to_column("Sim") |>
    tidyr::pivot_longer(-Sim, names_to = "Year", values_to = "Value") |>
    dplyr::mutate(Year = substr(Year, 1, 4)) |>
    dplyr::summarise(Value = mean(Value), .by = c(Sim, Year)) |>
    tidyr::pivot_wider(names_from = Year, values_from = Value)

  PMobj@Stat <- as.array(as.matrix(Catch_Annual))
  PMobj@Prob <- as.matrix(colMeans(apply(PMobj@Stat[, -1], 2, as.numeric)))
  #TO-DO: figure out how to change PMobj slots
  #PMobj@Mean <- calcMean(PMobj@Prob, MSEobj) |> round() # TO DO: circle back to calcMean() function - not working
  PMobj@Mean <- mean(PMobj@Prob)
  PMobj@MPs  <- names(MSEobj@MPs)
  PMobj
}
class(AvgAnn_Catch) <- 'pm'


#########################################################################################################################


#Short-term Catch (Males and Females) across all Fleets for first 5 years of projection period
ST_Catch <- function(MSEobj, Yrs=20) { #years argument will have to be whatever you want *4 because it's a seasonal model
  Yrs <- chk_yrs(Yrs, MSEobj)
  PMobj <- new("PMobj")
  PMobj@Name <- "Average Short-Term Catch (all fleets)"
  PMobj@Caption <- paste0('Average Catch (Years ', Yrs[1], ' - ', Yrs[2]/4, ')')
  Catch <- MSEobj@Landings[,1,Yrs[1]:Yrs[2],,] + MSEobj@Landings[,2,Yrs[1]:Yrs[2],,]
  Catch_allFleets <- apply(Catch, c(1,2), sum) #summing over fleets
  Catch_Annual <- as.data.frame(Catch_allFleets) |>   #averaging across seasons to get annual values
    tibble::rownames_to_column("Sim") |>
    tidyr::pivot_longer(-Sim, names_to = "Year", values_to = "Value") |>
    dplyr::mutate(Year = substr(Year, 1, 4)) |>
    dplyr::summarise(Value = mean(Value), .by = c(Sim, Year)) |>
    tidyr::pivot_wider(names_from = Year, values_from = Value)

  PMobj@Stat <- as.array(as.matrix(Catch_Annual))
  PMobj@Prob <- as.matrix(colMeans(apply(PMobj@Stat[, -1], 2, as.numeric)))
  #TO-DO: figure out how to change PMobj slots
  #PMobj@Mean <- calcMean(PMobj@Prob, MSEobj) |> round() # TO DO: circle back to calcMean() function - not working
  PMobj@Mean <- mean(PMobj@Prob)
  PMobj@MPs  <- names(MSEobj@MPs)
  PMobj
}
class(ST_Catch) <- 'pm'

