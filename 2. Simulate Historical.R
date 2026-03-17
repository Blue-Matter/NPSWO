library(NPSWO)

# UpdatePackages() #

# List of available OMs in the NPSWO.OM package
ListOMs()

# ---- Base OM -----

Base <- LoadOM("Base")

Hist <- Simulate(Base, DoRefLandings = FALSE)

# Saved locally to Objects/Hist/Base.hist
SaveHist(Hist, 'Base')




