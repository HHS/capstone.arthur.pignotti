.libPaths( c("C:/R/Packages", .libPaths()) )


########################
# Load FDMS Reports    #
########################
library(tidyverse)
library(readxl)
comLoc <- "C:/Data/Comments/CMS-2017-0163/FDMS"
comReport <- read_excel(paste(comLoc, "/report.xlsx", sep=""), sheet = "Nonrulemaking-Public Submission")
comReport$Site_Key <- substring(comReport$`Email Address`, regexpr("@", comReport$`Email Address`) + 1)

pubSites <- c(NA,
              "aim.com",
              "aol.com",
              "google.com",
              "gmail.com",
              'comcast.com',
              'cox.net',
              "cox.com",
              'hotmail.com',
              'icloud.com',
              'yahoo.com',
              'mail.com',
              'att.net',
              'bellsouth.net',
              'charter.net',
              'comcast.net',
              "msn.com",
              'gmail.co',
              'outlook.com',
              'verizon.net',
              'ymail.com',
              'aeneas.net')
siteDf <- subset(comReport, !(tolower(Site_Key) %in% pubSites))["Site_Key"]
siteDf <- distinct(siteDf)
    
library(rvest)

for (i in 11:20){
    url <- paste('http://www.', siteDf[i, 'Site_Key'], sep='')
    webpage <- read_html(url)
    siteDf[i, 'htitle'] <- webpage %>%
        html_node("title") %>%
        html_text()
}

