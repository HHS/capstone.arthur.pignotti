#### Initial Setup ####
.libPaths( c("C:/R/Packages", .libPaths()) ) #add extra library location
setwd("C:/Users/P6BQ/Desktop/capstone.arthur.pignotti") #local location of github repo

# Load libraries
library(readxl)
library(tidyverse)

source("helper_functions.R")

#### Load Raw Datasets ####
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
attachExtract <- read_excel("Data/CMS-2017-0163-TExtract.xlsx")
colnames(attachExtract) <- make.names(colnames(attachExtract))

comReport$Site_Key <- substring(comReport$Email.Address, regexpr("@", comReport$Email.Address) + 1)

#### Load Text Extract ####
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


#### Filter Out Message Comments Only Referencing Attachments Comments ####
# Create test file to review and find word count cutoff
testComment <- commentsDf %>%
    filter(!(word(Document.ID, -1, sep = "-") == "0" & str_detect(tolower(Text), "attached")))

write.csv(testComment, file = "Data/testAttacted.csv")

# Apply word filter with word count cutoff
commentsDf <- commentsDf %>%
    filter(!((word(Document.ID, -1, sep = "-") == "0" & str_detect(tolower(Text), "attached")) & wordcount(Text) < 150))

#### Export Cleaned File for Text Mining
write.csv(commentsDf, file = "Data/commentsDf.csv", row.names = FALSE)
