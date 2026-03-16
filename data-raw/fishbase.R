## code to prepare `fishbase` dataset goes here

fishbase <- rfishbase::load_taxa()

usethis::use_data(fishbase, overwrite = TRUE)
