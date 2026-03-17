library(NPSWO)

ListOMs()

Base <- LoadOM("Base")

Hist <- Simulate(Base, DoRefLandings = FALSE)

# inspect Hist

object.size(Hist)/1E6
