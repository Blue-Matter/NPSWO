



#' Sample Life-History Parameters Using FishLife Priors
#'
#' Generates correlated samples of life-history parameters by combining
#' user-specified means and standard deviations with the correlation structure
#' from the [FishLife](https://github.com/James-Thorson-NOAA/FishLife) database.
#' Parameters may be stock-specific (e.g. sex-specific natural mortality) or
#' shared across stocks. Sampling is performed on the log scale using a
#' truncated multivariate normal distribution.
#'
#' The compensation ratio (`CR`) is accepted as an alias for steepness (`h`)
#' and is converted internally via [MSEtool::h2CR()]; returned values are
#' back-converted to `h`.
#'
#' @param Parameters A `data.frame` with the following columns:
#'   - `Par` — parameter name; one of `"Linf"`, `"K"`, `"M"`, `"Lm"`, `"CR"`
#'   - `Mean` — mean value on the natural scale
#'   - `SD` — standard deviation on the log scale
#'   - `Stock` *(optional)* — stock or sex label (e.g. `"Female"`, `"Male"`).
#'     Rows without a `Stock` value (or `NA`) are treated as applying to all
#'     stocks. Each unique `Par`–`Stock` combination becomes one column in the
#'     output.
#' @param Genus `character`. Genus name passed to the FishLife database lookup
#'   (default `"Xiphias"`).
#' @param Species `character`. Species name passed to the FishLife database
#'   lookup (default `"gladius"`).
#' @param same_par_cor `numeric` in \[-1, 1\]. Correlation applied between the
#'   same parameter across stocks (e.g. `M_Female` and `M_Male`). Default
#'   `0.5`.
#' @param nSim `integer`. Number of samples to draw. Default `200`.
#' @param truncSD `numeric`. Truncation point in standard deviations applied
#'   symmetrically around the log-scale mean. Default `1.96`.
#' @param seed `integer`. Random seed for reproducibility. Default `101`.
#' @param plot `logical`. If `TRUE`, a pairs plot of the sampled values is
#'   displayed. Default `FALSE`.
#'
#' ## Correlation structure
#' The full parameter correlation matrix is constructed in two steps:
#' 1. **Cross-parameter correlations** (e.g. `M` vs `h`) are taken directly
#'    from the FishLife covariance matrix for the target species.
#' 2. **Within-parameter, cross-stock correlations** (e.g. `M_Female` vs
#'    `M_Male`) are set to `same_par_cor`, which the user specifies to reflect
#'    their prior belief about how tightly the same trait co-varies across
#'    stocks or sexes.
#'
#' ## Log-scale sampling
#' All parameters are sampled on the log scale and back-transformed. Means and
#' standard deviations in `Parameters` should therefore be on the natural
#' scale for `Mean` and the log scale for `SD`.
#'
#' ## Attribution
#' This methodology is adapted from the Southern Swordfish MSE developed by
#' Nathan Taylor (ICCAT).
#'
#' @return A `data.frame` with `nSim` rows and one column per `Par`–`Stock`
#'   combination. `CR` columns are returned as steepness (`h`).
#'
#' @examples
#' \dontrun{
#' # Sex-specific natural mortality
#' Par_M <- data.frame(
#'   Par   = "M",
#'   Mean  = c(0.22, 0.36),
#'   SD    = 0.1,
#'   Stock = c("Female", "Male")
#' )
#'
#' # Compensation ratio (steepness), shared across stocks
#' Par_CR <- data.frame(
#'   Par  = "CR",
#'   Mean = MSEtool::h2CR(0.9),
#'   SD   = 0.25
#' )
#'
#' Parameters <- dplyr::bind_rows(Par_M, Par_CR)
#'
#' Samples <- Generate_LH_Samples(Parameters, nSim = 500, plot = TRUE)
#' }
#'
#' @export
Generate_LH_Samples <- function(Parameters,
                                Genus        = "Xiphias",
                                Species      = "gladius",
                                same_par_cor = 0.5,
                                nSim         = 200,
                                truncSD      = 1.96,
                                seed         = 101,
                                plot         = FALSE) {
  set.seed(seed)

  # Validate parameter names
  validPars <- c("Linf", "K", "M", "Lm", "CR")
  pars <- unique(Parameters$Par)
  if (!any(pars %in% validPars))
    cli::cli_abort(c(
      "x" = "Invalid parameter names.",
      "i" = "Valid names are {.val {validPars}}"
    ))

  # Convert CR -> h internally
  pars <- gsub("CR", "h", pars)
  Parameters$Par <- gsub("CR", "h", Parameters$Par)

  # Retrieve FishLife mean and covariance for target species
  Mean_Cov <- get_fishlife_mean_cov(Genus, Species)
  fl_ind   <- match(pars, names(Mean_Cov$Mean))
  CovVar   <- Mean_Cov$Cov[fl_ind, fl_ind]

  # Build parameter-level correlation matrix from FishLife
  cor.mat      <- stats::cov2cor(CovVar)
  colnames(cor.mat) <- rownames(cor.mat) <- pars

  # Build Par_Stock labels (drop "_NA" suffix for shared parameters)
  Par_Stock <- paste(Parameters$Par, Parameters$Stock, sep = "_")
  Par_Stock <- gsub("_$", "", Par_Stock)

  # Build stock-level correlation matrix
  l <- length(Par_Stock)
  cor.mat.stock <- matrix(NA_real_, l, l)
  diag(cor.mat.stock) <- 1
  colnames(cor.mat.stock) <- rownames(cor.mat.stock) <- Par_Stock

  for (i in seq_len(l)) {
    par_i <- strsplit(Par_Stock[i], "_")[[1]][1]
    for (j in seq_len(l)) {
      if (i == j) next
      par_j <- strsplit(Par_Stock[j], "_")[[1]][1]
      cor.mat.stock[i, j] <- if (par_i == par_j) {
        same_par_cor                                         # within-parameter, cross-stock
      } else {
        cor.mat[par_i, par_j]                               # cross-parameter from FishLife
      }
    }
  }

  # Sample from truncated multivariate normal on log scale
  means <- log(Parameters$Mean)
  sds   <- Parameters$SD
  covar <- cor.mat.stock * (as.matrix(sds) %*% t(as.matrix(sds)))

  log_samples <- tmvtnorm::rtmvnorm(
    nSim,
    mean  = means,
    sigma = covar,
    lower = means - sds * truncSD,
    upper = means + sds * truncSD
  )

  # Back-transform and convert h columns
  Vals <- exp(log_samples)
  colnames(Vals) <- Par_Stock

  h_cols <- grep("^h", Par_Stock)
  Vals[, h_cols] <- MSEtool::CR2h(Vals[, h_cols])

  Vals <- as.data.frame(Vals)

  # Optional pairs plot
  if (plot) {
    panel.hist <- function(x, ...) {
      usr <- par("usr")
      on.exit(par(usr=usr))
      par(usr = c(usr[1:2], 0, 1.5))
      h  <- hist(x, plot = FALSE)
      y  <- h$counts / max(h$counts)
      rect(h$breaks[-length(h$breaks)], 0, h$breaks[-1], y, col = "darkgray", ...)
    }
    pairs(Vals, pch = 16, cex = 0.5, diag.panel = panel.hist)
  }

  Vals
}



get_fishlife_mean_cov <- function(Genus, Species) {
  L        <- search_fishlife_species(Genus, Species)
  Database <- FishLife::FishBase_and_RAM

  Mean_Cov <- list(
    Mean = Database$beta_gv[L$GroupNum, ],
    Cov  = Database$Cov_gvv[L$GroupNum, , ]
  )

  for (nm in c("Mean", "Cov")) {
    names_fn <- if (nm == "Mean") `names` else `colnames`
    names(Mean_Cov[[nm]])    <- gsub("Loo", "Linf", names(Mean_Cov[[nm]]))
  }

  Mean_Cov
}


search_fishlife_species <- function(Genus,Species) {


  ParentChild_gz <-FishLife::FishBase_and_RAM$ParentChild_gz

  # Normalise species name to epithet-only form to match FishLife conventions
  full_name  <- paste(Genus, Species)
  match_rows <- which(fishbase$Species == full_name)
  fishbase[match_rows, "Species"] <- trimws(gsub(Genus, "", fishbase[match_rows, "Species"]))

  group <- which(grepl(paste(Genus, Species, sep = "_"), ParentChild_gz$ChildName))

  list(
    GroupNum       = group,
    match_taxonomy = ParentChild_gz$ChildName[group]
  )
}

