#updated function for getting projection years
chk_yrs <- function (Yrs, MSEobj)
{
  if (!methods::is(MSEobj, "mse"))
    stop("Require object of class `mse`",
         call. = FALSE)
  if (is.null(Yrs)) {
    y.st <- 1
    y.end <- MSEobj@OM@pYear*4 #need to decide how to handle this because it's seasonal
  }
  else {
    if (length(Yrs) == 1) {
      if (Yrs == 0)
        stop("Yrs must be postive or negative", call. = FALSE)
      if (Yrs < 0) {
        y.st <- MSEobj@OM@pYear*4 + Yrs[1] + 1
        y.end <- MSEobj@OM@pYear*4
      }
      else {
        y.st <- 1
        y.end <- y.st + Yrs[1] - 1
      }
    }
    else {
      if (length(Yrs) > 2)
        stop("Yrs must be numeric vector of length 1 or 2",
             call. = FALSE)
      y.st <- Yrs[1]
      y.end <- Yrs[2]
      if (Yrs[1] > Yrs[2])
        stop("Yrs[1] is > Yrs[2]", call. = FALSE)
      if (any(Yrs < 1))
        stop("Yrs must be positive", call. = FALSE)
      if (Yrs[2] >MSEobj@OM@pYear*4) {
        message("Yrs[2] is greater than MSEobj@OM@pYear. Setting Yrs[2] = MSEobj@OM@pYear")
        y.end <- MSEobj@OM@pYear*4
      }
      if (Yrs[1] > MSEobj@OM@pYear*4) {
        message("Yrs[1] is greater than MSEobj@OM@pYear. Setting Yrs[1] = Yrs[2] - Yrs[1]")
        y.st <- max(1, y.end - (Yrs[2] - Yrs[1]))
      }
    }
  }
  return(c(y.st, y.end))
}


calcMean <- function (PM, MSEobj)
{
  if (length(MSEobj@MPs) > 1) {
    mar <- 2
  }
  else mar <- 1
  mar <- 1:mar
  apply(PM, mar, mean, na.rm = TRUE)
}


calcProb <- function (Prob)
{
  if ("matrix" %in% class(Prob))
    return(apply(Prob, 2, mean, na.rm = TRUE))
  if ("numeric" %in% class(Prob))
    return(mean(Prob, na.rm = TRUE))
}


