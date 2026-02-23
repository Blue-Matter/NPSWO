source('Condition/Specifications.R')
source('LifeHistoryParameters.R')

pak::pkg_install('tmvtnorm')
pak::pkg_install('James-Thorson-NOAA/FishLife')

# Assumptions:
# - natural mortality (M) is log-normally distributed with mean M_mu and SD M_sd specified in LifeHistoryParameters.R
# - steepess (h) is log-normally distributed with mean h_mu and SD h_sd specified in LifeHistoryParameters.R
# - log-normal distributions are truncated at 1.96 SD
# - correlation between M & h taken from FishLife estimates for Thunnus alalunga

# Note: this methodology is adapted from the Southern Swordfish MSE work developed
#       by Nathan Taylor (ICCAT)

truncSD <- 1.96

# Correlation Matrix from FishLife

FL <- FishLife::Plot_taxa(FishLife::Search_species(
  Genus='Xiphias',Species='gladius')$match_taxonomy, mfrow=c(3,2))[[2]]


ind = c(6,13) #M and h
cor <- stats::cov2cor(FL$Cov_pred[ind, ind])
diag(cor) <- NA
cor <- cor |>  dplyr::as_tibble() |>
  dplyr::mutate(term=colnames(cor)) |>
  dplyr::relocate(term)
class(cor) <-c("cor_df", "tbl_df", "tbl", "data.frame")

cor.mat <- cor |>
  corrr::focus(c('M', 'h'), mirror = TRUE) |>
  dplyr::arrange(match(term, c('M', 'h'))) |>
  dplyr::select(-term) |>
  as.matrix()
diag(cor.mat) <- 1



#-------------- Female --------------#
means_f <- c(M_mu_f, h_mu) |> log()
sds_f <- c(M_sd_f, h_sd)


# weak correlation
covar_f <- cor.mat *as.matrix(sds_f) %*% t(as.matrix(sds_f))
colnames(covar_f) <- NULL
lower_f <- means_f - sds_f*truncSD
upper_f <- means_f + sds_f*truncSD

Vals_f <- tmvtnorm::rtmvnorm(nsim,
                           mean = means_f,
                           sigma = covar_f,
                           lower=lower_f,
                           upper=upper_f) |>
  exp()

ValsDF_f <- data.frame(M=Vals_f[,1], h=Vals_f[,2])

panel.hist_f <- function(x, ...) {
  usr <- par("usr")
  par(usr = c(usr[1:2], 0, 1.5) )
  h <- hist(x, plot = FALSE)
  breaks <- h$breaks; nB <- length(breaks)
  y <- h$counts; y <- y/max(y)
  rect(breaks[-nB], 0, breaks[-1], y, col = "darkgray", ...)
}

pairs(ValsDF_f, pch=16, diag.panel=panel.hist_f)

write.csv(ValsDF_f, 'Condition/LHSamples_Female.csv', row.names = FALSE)



#-------------- Male --------------#
means_m <- c(M_mu_m, h_mu) |> log()
sds_m <- c(M_sd_m, h_sd)


# weak correlation
covar_m <- cor.mat *as.matrix(sds_m) %*% t(as.matrix(sds_m))
colnames(covar_m) <- NULL
lower_m <- means_m - sds_m*truncSD
upper_m <- means_m + sds_m*truncSD

Vals_m <- tmvtnorm::rtmvnorm(nsim,
                             mean = means_m,
                             sigma = covar_m,
                             lower=lower_m,
                             upper=upper_m) |>
  exp()

ValsDF_m <- data.frame(M=Vals_m[,1], h=Vals_m[,2])

panel.hist_m <- function(x, ...) {
  usr <- par("usr")
  par(usr = c(usr[1:2], 0, 1.5) )
  h <- hist(x, plot = FALSE)
  breaks <- h$breaks; nB <- length(breaks)
  y <- h$counts; y <- y/max(y)
  rect(breaks[-nB], 0, breaks[-1], y, col = "darkgray", ...)
}

pairs(ValsDF_m, pch=16, diag.panel=panel.hist_m)

write.csv(ValsDF_m, 'Condition/LHSamples_Male.csv', row.names = FALSE)

