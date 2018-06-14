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
# Load FDMS Reports    #
########################
library(readxl)
comLoc <- "C:/Data/Comments/CMS-2017-0163/FDMS"
comReport <- read_excel(paste(comLoc, "/report.xlsx", sep=""), sheet = "Nonrulemaking-Public Submission")
colnames(comReport) <- make.names(colnames(comReport))
comReport$Site_Key <- substring(comReport$Email.Address, regexpr("@", comReport$Email.Address) + 1)
attReport <- read_excel(paste(comLoc, "/report.xlsx", sep=""), sheet = "Attachments")
colnames(attReport) <- make.names(colnames(attReport))


########################
# Load Text Extract    #
########################
comments <- read_csv("Data/comments.csv")
attachExtract <- read_excel("Data/CMS-2017-0163-TExtract.xlsx")
colnames(attachExtract) <- make.names(colnames(attachExtract))
attachExtract <- attachExtract %>%
    mutate(Comment.ID = substr(File.Name,1,18)) %>%
    select(-Document.ID) %>%
    mutate(Document.ID = substr(File.Name, 1, regexpr("\\.", File.Name) - 1))
test <- left_join(attachExtract, comReport)

##########################
# Create Corpus Using TM #
##########################
mycorpus = Corpus(VectorSource(comments$commentText)) 
commentsDf <- data.frame(paste(comments$documentId,"-0",sep=""), comments$commentText, stringsAsFactors = FALSE)
colnames(commentsDf) <- c("doc_id","text")
mycorpus = Corpus(DataframeSource(commentsDf)) 




auths <- paste0('Author',seq(nrow(commentsDf)))
i <- 0
mycorpus = tm_map(mycorpus, function(x) {
    i <<- i +1
    meta(x, "Author") <- m[i,2]
    x
})


########################
# Clean Corpus         #
########################
mycorpus <- tm_map(mycorpus, stripWhitespace)
mycorpus <- tm_map(mycorpus, content_transformer(tolower))
mycorpus <- tm_map(mycorpus, removeWords, stopwords("english"))


#html test
webpage <- read_html(myhtmlfiles[1])
test <- html_table(html_nodes(webpage, "table"))


############################
# Create Corpus Using Tidy #
############################

library(dplyr)
library(stringr)
library(ggplot2)
library(tidytext)
commentsDf <- data.frame(doc_id = paste0(comments$documentId,"-0"), text = comments$commentText, stringsAsFactors = FALSE)

data(stop_words)
test <- commentsDf %>%
    group_by(doc_id) %>%
    mutate(linenumber = row_number()) %>%
    ungroup()

test1 <- test %>%
    unnest_tokens(word, text)

test1 <- test1 %>%
    anti_join(stop_words)

test1 %>%
    count(word, sort = TRUE) %>%
    filter(n > 600) %>%
    mutate(word = reorder(word, n)) %>%
    ggplot(aes(word, n)) +
    geom_col() +
    xlab(NULL) +
    coord_flip()
