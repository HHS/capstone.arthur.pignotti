#####################
# Initial Setup     #
#####################
.libPaths( c("C:/R/Packages", .libPaths()) ) #add extra library location
setwd("C:/Users/P6BQ/Desktop/capstone.arthur.pignotti") #local location of github repo

dktid = "CMS-2017-0163"

#libraries
library(tm)
library(readxl)
library(tidyverse)


########################
# Convert PDFs         #
########################

attachLoc <- "C:/Data/Comments/CMS-2017-0163/FDMS/files"
pdfCnt <- length(list.files(path = "files/", pattern = "pdf")) #number of PDFs
docCnt <- length(list.files(path = "files/", pattern = "doc")) #number of Docs




pdfCnt <- length(list.files(path = "files/", pattern = "pdf",  full.names = TRUE)) #get list of PDFs
mycorpus <- Corpus(DirSource(attachLoc, pattern = "pdf"))


########################
# Load Raw Datasets    #
########################
comLoc <- "C:/Data/Comments/Testing"

#Comment Report from FDMS
comReport <- read_excel(paste0(comLoc, "/report.xlsx"), sheet = 1)
colnames(comReport) <- make.names(colnames(comReport))

#Comment Report from FDMS
attReport <- read_excel(paste0(comLoc, "/report.xlsx"), sheet = 2)
colnames(attReport) <- make.names(colnames(attReport))

#Comment Dataset from Data_Download.R
comments <- read_csv("Data/comments.csv")

#Map of Comments, Commenters, Commenter Categories
map <- read_excel(paste0(comLoc, "/map.xlsx"))
colnames(map) <- make.names(colnames(map))

#Text Extract of Comment Attachments
attachExtract <- read_excel("Data/CMS-2017-0156-TExtract.xlsx")
colnames(attachExtract) <- make.names(colnames(attachExtract))

comReport$Site_Key <- substring(comReport$Email.Address, regexpr("@", comReport$Email.Address) + 1)


########################
# Load Text Extract    #
########################
attachExtract <- attachExtract %>%
    mutate(Comment.ID = substr(File.Name,1,18),
           Document.ID = substr(File.Name, 1, regexpr("\\.", File.Name) - 1)) %>%
    select(Comment.ID, Document.ID, Text, Page.Number) %>%
    left_join(comReport, by = c("Comment.ID" = "Document.ID")) %>%
    left_join(map, by = c("Comment.ID" = "Document"))

commentsDf <- comments %>%
    mutate(Document.ID = paste0(documentId, "-0"),
           Comment.ID = documentId,
           Text = commentText,
           Page.Number = 1) %>%
    select(Comment.ID, Document.ID, Text, Page.Number) %>%
    left_join(comReport, by = c("Comment.ID" = "Document.ID")) %>%
    left_join(map, by = c("Comment.ID" = "Document")) %>%
    union(attachExtract)


write.csv(commentsDf, file = "Data/commentsDf.csv", row.names = FALSE)
