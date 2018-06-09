.libPaths( c("C:/R/Packages", .libPaths()) ) #add extra library location
setwd("C:/Users/P6BQ/Desktop/capstone.arthur.pignotti") #local location of github repo

#fix tm install issue
slam_url <- "https://cran.r-project.org/src/contrib/Archive/slam/slam_0.1-37.tar.gz"
install_url(slam_url)

#libraries
library(tm)
library(readxl)
library(rvest)
library(scrapeR)

comLoc <- "C:/Data/Comments/CMS-2017-0163/FDMS/files"
myfiles <- list.files(path = comLoc, pattern = "pdf",  full.names = TRUE) #get list of PDFs
comReport <- read_excel("CMS-2017-0163/report.xlsx", sheet = "Nonrulemaking-Public Submission")
attReport <- read_excel("CMS-2017-0163/report.xlsx", sheet = "Attachments")


#Placeholder for loading html files
mycorpus <- Corpus(DirSource(comLoc, pattern = "html"))
myhtmlfiles <- list.files(path = comLoc, pattern = "html",  full.names = TRUE) #get list of PDFs

#html test
webpage <- read_html(myhtmlfiles[1])
test <- html_table(html_nodes(webpage, "table"))

