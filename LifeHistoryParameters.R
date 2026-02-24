################################################
#                                              #
#  Assumed Life History Parameters for NPSWO   #
#                                              #
################################################

# See TS Document (TS/TS.Rmd) for justifications for these parameters
# Update TS document with any changes made here

# February 2026: Currently assuming uncertainty in natural mortality (M) and steepness (h)



# ---- Natural Mortality (F) ----

#M_range <- c(0.3, 0.4)

M_mu_f <- 0.22
M_sd_f <- 0.1

# ---- Natural Mortality (M) ----

#M_range <- c(0.3, 0.4)

M_mu_m <- 0.36
M_sd_m <- 0.1

# ---- Length-at-Age (F)----

L_Amax_mu_f <- 226.3
K_mu_f <- 0.246
L_Amin_mu_f <- 80.1

# ---- Length-at-Age (M)----

L_Amax_mu_m <- 206.4
K_mu_m <- 0.271
L_Amin_mu_m <- 83.2

# ---- Maturity ----

L50_mu <- 143.68
#L95_mu <- 94

# ---- Stock Recruit ----

#h_range <- c(0.7, 0.9)

h_mu <- 0.9

CR2h <- function(CR) {
  CR/(CR+4)
}

h2CR <- function(h) {
  (4*h)/(1-h)
}

CR_mu <- h2CR(h_mu)
CR_sd <- 0.25

PE_mu <- 0.42

# ---- Length-Weight ----

#Wa_mu <- 1.3718E-5
#Wb_mu <-  3.09773

#
# h2CR <- function(h) {
#   (4*h)/(1-h)
# }
#
# CR2h <- function(CR) {
#   CR/(CR+4)
# }
#
# CR_sd <- 4
