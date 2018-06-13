.libPaths( c("C:/R/Packages", .libPaths()) )


########################
# Load FDMS Reports    #
########################
library(tidyverse)
library(readxl)
comLoc <- "C:/Data/Comments/CMS-2017-0163/FDMS"
comReport <- read_excel(paste(comLoc, "/report.xlsx", sep=""), sheet = "Nonrulemaking-Public Submission")
comReport$Site_Key <- substring(comReport$`Email Address`, regexpr("@", comReport$`Email Address`) + 1)


########################
# Getting Info         #
########################
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
              'me.com',
              'aeneas.net')
siteDf <- subset(comReport, !(tolower(Site_Key) %in% pubSites))[c("Site_Key","Organization Name")]
siteDf <- distinct(siteDf)


########################
# Web Scraping         #
######################## 
library(rvest)
library(httr)
library(RCurl)

for (i in 1:nrow(siteDf)){
    url <- paste('http://www.', siteDf[i, 'Site_Key'], sep='')
    test <- url.exists(url)
    if (test == TRUE){
        test <- HEAD(url)
        if (test$status_code==200){
            webpage <- read_html(url, verbose(info=TRUE))
            siteDf[i, 'htitle'] <- webpage %>%
                html_node("title") %>%
                html_text()
            siteDf[i, 'body'] <- webpage %>%
                html_node("body") %>%
                html_text()
            }
        }
}

########################
# Testing              #
########################
url <- paste('http://www.', siteDf[5, 'Site_Key'], sep='')
test <- url.exists(url)
webpage <- read_html(url)
webpage %>%
    html_node("body") %>%
    html_text()
