#chk_yrs()
chk_yrs <- function (Yrs, MSEobj)
{
  if (!methods::is(MSEobj, "mse"))
    stop("Require object of class `mse`",
         call. = FALSE)
  if (is.null(Yrs)) {
    y.st <- 1
    y.end <- MSEobj@OM@pYear*4 #multiplied by 4 because this is a seasonal model
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



#calcMean() - confused about how it's a function of PM...throwing errors so leaving out for now
#calcMean <- function (PM, MSEobj)
#{
 # if (length(MSEobj@MPs) > 1) {
#    mar <- 2
#  }
#  else mar <- 1
#  mar <- 1:mar
#  apply(PM, mar, mean, na.rm = TRUE)
#}


#calcProb()
calcProb <- function (Prob)
{
  if ("matrix" %in% class(Prob))
    return(apply(Prob, 2, mean, na.rm = TRUE))
  if ("numeric" %in% class(Prob))
    return(mean(Prob, na.rm = TRUE))
}



#SaveMSE()
SaveMSE <- function(MSE, name, path="Objects/MSE", compress=TRUE, overwrite=TRUE) {

  if (!inherits(MSE, 'mse'))
    cli::cli_abort("MSE must be an {.cls MSEtool::mse} object")

  if (!dir.exists(path))
    dir.create(path, recursive = TRUE)

  if (compress) {
    cli::cli_progress_step("Compressing {.val {name}}")
    MSE <- Compress(MSE)
    cli::cli_process_done()
  }

  path <- file.path(path, paste0(name, '.mse'))
  MSEtool::Save(MSE, path, overwrite)

}


#ListMSE()
ListMSE <- function(path="Objects/MSE", silent=FALSE) {
  files <- tools::file_path_sans_ext(list.files(path))
  if (!silent) {
    cli::cli_text("MSE objects available in {.val {path}}:")
    cli::cli_bullets(stats::setNames(files, rep("*", length(files))))
  }

  invisible(files)
}


#LoadMSE()
LoadMSE <- function(name, path="Objects/MSE") {
  availMSE <- ListMSE(path, silent=TRUE)
  if (!name %in% availMSE)
    cli::cli_abort(c("x"="{.val {name}} not found in {.val {path}}",
                     "i"="Available `MSE` objects are: {.val {availMSE}}"))

  full.path <- file.path(path, paste0(name, '.mse'))

  cli::cli_progress_step("Loading {.val {full.path}}")

  MSE <- readRDS(full.path)

  cli::cli_progress_step("Expanding {.val {name}}")

  Expand(MSE)

}
