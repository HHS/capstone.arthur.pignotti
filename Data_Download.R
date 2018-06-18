#####################
# Initial Setup     #
#####################
.libPaths( c("C:/R/Packages", .libPaths()) ) #add extra library location
setwd("C:/Users/P6BQ/Desktop/capstone.arthur.pignotti") #local location of github repo


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
dktid = "CMS-2017-0156"
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

write.csv(comments, file = "Data/comments.csv", row.names = FALSE)


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


# Clean up?
nrow(attachList)
write.csv(attachList, file="test.csv")
attachUrl = "https://www.regulations.gov/contentStreamer?documentId=CMS-2017-0163-1203&disposition=attachment&attachmentNumber=1"
test <- HEAD(attachUrl)
file <- substr(tmp, 23, nchar(tmp)-1)
download.file(attachUrl, paste("files/",  attachList[comment, "documentId"], "-", doc, " - " , file, sep = ""), mode="wb")
