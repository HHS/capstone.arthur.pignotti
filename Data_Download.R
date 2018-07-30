#### Initial Setup ####
.libPaths( c("C:/R/Packages", .libPaths()) ) #add extra library location
setwd("C:/Users/P6BQ/Desktop/capstone.arthur.pignotti") #local location of github repo

# API parameters
dktid = "CMS-2017-0163"
api_key = "bbPnmY2FqvoazRuHseN0liEsWh0qI255CgJTsPAo"

# Load libraries
library(jsonlite)
library(httr)
library(stringr)

#### API Data Call ####
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


#### Download Attachments ####
attachList <- subset(comments, attachmentCount > 0)
errflag = 1
for (comment in 1:nrow(attachList)){
    for (doc in 1:attachList[comment, "attachmentCount"]){
        attachUrl = paste("https://www.regulations.gov/contentStreamer?documentId=", attachList[comment, "documentId"], "&disposition=attachment&attachmentNumber=", doc, sep ="")
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
            errorLog[errflag, 2] = test$status_code
            errflag = errflag+1
        }
    }
}

write.csv(errorLog, file = "Data/attachmentDownloadErrorLog.csv", row.names = FALSE)


pathofvbscript = "Office_Macros/PDFConvert.vbs"
shell(shQuote(normalizePath(pathofvbscript)), "cscript", flag = "//nologo")
