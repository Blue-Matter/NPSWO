
# TODO
# - develop diagnostics for the stochastic OMs - probably R0 and F - some have F ~ 0
# - compare stock dynamics from RefOM and stochastic OMs - initial


library(NPSWO)

# UpdatePackages()


# ---- Run Historical Simulations and Save  ----

# List of available OMs in the NPSWO.OM package
All_OMs <- ListOMs()

# Select the OMs to run Simulate on:
Run_OMs <- All_OMs

# Runs `Simulate` for all OMs in Run_OMs and saves to disk using SaveHist
# This will overwrite any existing .hist files for the same OMs
RunSimulations(Run_OMs)


# ---- Explore Diagnostics -----


# explore diagnostics, subset to nSim2


# Subset and Re-Save Hist objects






