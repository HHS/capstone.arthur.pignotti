#Placeholder for loading html files
mycorpus <- Corpus(DirSource(comLoc, pattern = "html"))
myhtmlfiles <- list.files(path = comLoc, pattern = "html",  full.names = TRUE) #get list of PDFs


#fix tm install issue
slam_url <- "https://cran.r-project.org/src/contrib/Archive/slam/slam_0.1-37.tar.gz"
install_url(slam_url)
