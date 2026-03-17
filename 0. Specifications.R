nSim <- 200 # number of stochastic SS3 runs per OM

# Stock meta-data passed to the OM object
Name      <- "North Pacific Swordfish"
StockName <- c("Female", "Male")
Species   <- "Xiphias gladius"
Region    <- "North Pacific"

# MSE settings applied to all OMs
Interval <- 1  # number of years between management updates
DataLag  <- 0  # years of lag between data collection and management action (to be updated)
pYear    <- 25 # number of years in the MSE projection period

# Path to the SS3 base-case model directory
ssdir_base <- "../WCNPOSWO-2023/Final Base-case"

# Detect available cores and register a parallel backend for downstream functions
SetupParallel()
