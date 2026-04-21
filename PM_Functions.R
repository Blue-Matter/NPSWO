
#prob that stock is in green kobe
Status_pgk <- function(MSE, Yrs=NULL) {
  Yrs <- chk_yrs(Yrs, MSE)

  PMobj <- new("PMobj")
  PMobj@Name <- "Probability Stock is in Green Kobe"
  PMobj@Caption <- paste0('Prob. Green Kobe (Years ', Yrs[1], ' - ', Yrs[2], ')')

  #2-sex model, so have to sum female and male biomass before dividing by ref point
  PMobj@Stat <- round((MSE@SBiomass[,1,Yrs[1]:Yrs[2],] + MSE@SBiomass[,2,Yrs[1]:Yrs[2],])/MSE@Reference@MSY@SBMSY,2) >= 1 & #no ref points calculated
     round(MSE@F_FMSY[,,Yrs[1]:Yrs[2]],2) <= 1 #MSY ref points calculated for entire stock (male/female?)
  PMobj@Ref <- 1
  PMobj@Prob <- calcProb(PMobj@Stat, MSE) #function calcProb() looks like it's calculating mean
  PMobj@Mean <- calcMean(PMobj@Prob,) #function calcMean() looks like it's calculating prob
  PMobj@MPs <- MSE@MPs
  PMobj
}
class(Status_pgk) <- 'pm'


#prob that SSB/SSBMSY > 0.5
Safety_05MSY <- function(MSE, Ref=0.5, Yrs=NULL) {
  Yrs <- chk_yrs(Yrs, MSE)
  PMobj <- new("PMobj")
  PMobj@Name <- "Spawning Biomass relative to SBMSY"
  if (Ref != 1) {
    PMobj@Caption <- paste0("Prob. SB > ", Ref, " SBMSY (Years ",
                            Yrs[1], " - ", Yrs[2], ")")
  }
  else {
    PMobj@Caption <- paste0("Prob. SB > SBMSY (Years ", Yrs[1],
                            " - ", Yrs[2], ")")
  }
  PMobj@Ref <- Ref
  PMobj@Stat <- (MSE@SBiomass[,1,Yrs[1]:Yrs[2],] + MSE@SBiomass[,2,Yrs[1]:Yrs[2],])/MSE@Reference@MSY@SBMSY
  PMobj@Prob <- calcProb(PMobj@Stat > PMobj@Ref, MSE)
  PMobj@Mean <- calcMean(PMobj@Prob)
  PMobj@MPs <- MSE@MPs
  PMobj
}
class(Safety_05MSY) <- 'pm'



Catch_ST <- function(MSE, Yrs=20) { #adjust timeframe as necessary, keep in mind that seasonal model mult. by 4 (so this is 1st 5 years)
  Yrs <- chk_yrs(Yrs, MSE)

  PMobj <- new("PMobj")
  PMobj@Name <- "Average Short-Term Catch"

  PMobj@Caption <- paste0('Average Catch (Years ', Yrs[1], ' - ', Yrs[2], ')')
  PMobj@Stat <- MSE@Landings[,1,Yrs[1]:Yrs[2],,] + MSE@Landings[,2,Yrs[1]:Yrs[2],,]
  PMobj@Ref <- 1
  PMobj@Prob <- calcProb(PMobj@Stat)
  PMobj@Mean <- calcMean(PMobj@Prob, MSE) |> round()
  PMobj@MPs <- MSE@MPs
  PMobj
}
class(Catch_ST) <- 'pm'

Catch_LT <- Catch_ST
formals(Catch_LT)$Yrs <- -10
class(Catch_LT) <- 'pm'
