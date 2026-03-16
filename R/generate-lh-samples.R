
Generate_LH_Samples <- function(Genus = "Xiphias",
                                Species = "gladius",
                                nSim=200,
                                truncSD=1.96,
                                seed = 101

                                ) {

  set.seed(seed)

  # Correlation Matrix from FishLife

  FL <- FishLife::Plot_taxa(FishLife::Search_species(
    Genus = Genus,Species=Species)$match_taxonomy, mfrow=c(3,2))[[2]]

}
