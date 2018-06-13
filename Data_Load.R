#####################
# Initial Setup     #
#####################
.libPaths( c("C:/R/Packages", .libPaths()) ) #add extra library location
setwd("C:/Users/P6BQ/Desktop/capstone.arthur.pignotti") #local location of github repo

dktid = "CMS-2017-0163"


#libraries
library(tm)
library(readxl)
library(rvest)
library(scrapeR)
library(jsonlite)
library(httr)
library(tidyverse)


#####################
# API Data Call     #
#####################
api_key = "bbPnmY2FqvoazRuHseN0liEsWh0qI255CgJTsPAo"

#API call to get the number of record calls needs. API limits pull to 1000 records
countUrl = paste0("https://api.data.gov:443/regulations/v3/documents.json?api_key=",api_key,"&countsOnly=1&encoded=1&dct=PS&dktid=", dktid)
recCount <- fromJSON(countUrl)
pageCount <- ceiling(recCount$totalNumRecords/100)

for (i in 1:pageCount){
    pageUrl = paste0("https://api.data.gov:443/regulations/v3/documents.json?api_key=", api_key, "&rpp=100&dct=PS&encoded=1&dktid=", dktid, "&po=", (i-1)*100)
    dataPull <- fromJSON(pageUrl)
    if (i==1){
        comments <- data.frame(dataPull$documents)
    } else {
        tmp <- data.frame(dataPull$documents)
        comments <- rbind(comments, tmp)
    }
}


########################
# Download Attachments #
########################
attachList <- subset(comments, attachmentCount > 0)
errflag = 1
for (comment in 1:nrow(attachList)){
    for (doc in 1:attachList[comment, "attachmentCount"]){
        attachUrl = paste("https://www.regulations.gov/contentStreamer?documentId=", attachList[comment, "documentId"], "&disposition=attachment&attachmentNumber=", doc, sep ="")
        #testUrl = paste("https://www.regulations.gov/contentStreamer?documentId=", attachList[1, "documentId"], "&disposition=attachment&attachmentNumber=", 1, sep ="")
        #test <- getURI(testUrl,header=TRUE,verbose=TRUE)
        test <- HEAD(attachUrl)
        if (test$status_code==200){
            tmp <- test$headers$`content-disposition`
            file <- substr(tmp, nchar(tmp)-1, nchar(tmp)-1)
            file <- str_extract(tmp, "\\.[A-Za-z]{3,4}")
            download.file(attachUrl, paste0("files/",  attachList[comment, "documentId"], "-", doc, file), mode="wb")
            
        } else if (errflag==1){
            errorLog = data.frame(docId = attachList[comment, "documentId"], error = test$status_code)
            errflag = errflag+1
        } else {
            errorLog[errflag, 1] = attachList[comment, "documentId"]
            errorLog[errflag, 2] = error = test$status_code
            errflag = errflag+1
        }
    }
}
nrow(attachList)
write.csv(attachList, file="test.csv")
attachUrl = "https://www.regulations.gov/contentStreamer?documentId=CMS-2017-0163-1203&disposition=attachment&attachmentNumber=1"
test <- HEAD(attachUrl)
file <- substr(tmp, 23, nchar(tmp)-1)
download.file(attachUrl, paste("files/",  attachList[comment, "documentId"], "-", doc, " - " , file, sep = ""), mode="wb")

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
comReport$Site_Key <- substring(comReport$`Email Address`, regexpr("@", comReport$`Email Address`) + 1)
attReport <- read_excel(paste(comLoc, "/report.xlsx", sep=""), sheet = "Attachments")


########################
# Load FDMS Reports    #
########################



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
