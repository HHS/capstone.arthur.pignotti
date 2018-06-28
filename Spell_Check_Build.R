#### Initial Setup ####

.libPaths( c("C:/R/Packages", .libPaths()) ) #add extra library location
setwd("C:/Users/P6BQ/Desktop/capstone.arthur.pignotti") #local location of github repo

#Load libraries
library(dplyr)
library(stringr)
library(ggplot2)
library(tidytext)
library(tidyverse)
library(hunspell)

#Load comment data
commentsDf <- read.csv("Data/commentsDf.csv",
                       stringsAsFactors = FALSE)



#### Setup Corpus for Spell Check ####

#Load stop words
data(stop_words)

#Load CMS-specific stop words
cms_stop <- data.frame(word = c("CMS","Medicare","MA", "plan", "care", "beneficiaries", "Advantage", "proposed", "rule", "health", "plans", "MD", "Baltimore", "Seema", "Verma", "Washington", "DC", "boulevardbaltimore"), stringsAsFactors = FALSE)

#Unnest tokens and removd stop words
comment.words <- commentsDf %>%
    unnest_tokens(word, Text, to_lower = FALSE) %>%
    anti_join(stop_words) %>%
    anti_join(cms_stop) %>%
    filter(!(str_detect(word, regex("^http"))),
           !(str_detect(word, regex("^www"))))



#### Spell Check Testing ####

spell.find <- hunspell_find(comment.words$word)

spell.count <- as.data.frame(matrix(unlist(spell.find)),
                             byrow = TRUE, 
                             stringsAsFactors = FALSE)

spell.count.unique <- as.data.frame(matrix(unique(unlist(spell.find)),
                                           byrow = TRUE), 
                                    stringsAsFactors = FALSE)

spell.suggest <- hunspell_suggest(spell.count.unique$V1)

#Function to find the most amount of elements in a list
elementMax <- function(list){
    maxEl = 0
    for (i in 1:length(list)){
        if (length(list[[i]]) > maxEl) {
            maxEl <- length(list[[i]])
        }
    }
    return(maxEl)
}

#Create blank data.frame to input spelling suggestions into
spell.suggest.df <- data.frame(word = matrix(unlist(spell.count.unique), byrow = TRUE),
                               matrix(nrow = length(spell.suggest),
                                      ncol = elementMax(spell.suggest)))

#Put spelling suggestions into a data frame
for (i in 1:14652){
    if (length(spell.suggest[[i]]) != 0){
        for (j in 1:length(spell.suggest[[i]])){
            spell.suggest.df[i, j+1] = spell.suggest[[i]][j]
        }
    }
}


#Create blank data.frame to input spelling find results into
spell.find.df <- data.frame(Document.ID = comment.words$Document.ID,
                            word = comment.words$word,
                            misspell = matrix(nrow = dim(comment.words)[1], ncol = 1))

#Put spelling find results into a data frame

for (i in 1:length(spell.find)){
    if (length(spell.find[[i]]) != 0){
        spell.find.df[i, 3] = spell.find[[i]][1]
    }
}

spell.count.overall <- spell.find.df %>%
    group_by(Document.ID) %>%
    count()

spell.count.find <- spell.find.df %>%
    filter(!is.na(misspell)) %>%
    group_by(Document.ID) %>%
    count()

spell.count.overall <- spell.count.overall %>%
    left_join(spell.count.find, by = c("Document.ID" = "Document.ID"))

spell.count.overall$n.y <- spell.count.overall$n.y %>%
    replace_na(0)

spell.count.overall <- spell.count.overall %>%
    mutate(percent.misspelled = n.y/n.x) %>%
    arrange(desc(percent.misspelled))

write.csv(spell.count.overall,
          file = "Data/spellCheckMetric.csv", 
          row.names = FALSE)

spell.count.overall %>%
    ggplot(aes(x = percent.misspelled)) +
    labs(x = "Percent Misspelled", y = "Count") +
    geom_histogram()

spell.count.overall %>%
    filter(n.x < 1500) %>%
    ggplot(aes(x = n.x)) +
    labs(x = "Number of Words", y = "Count") +
    geom_histogram(bins = 50)

spell.count.overall %>%
    ggplot(aes(x = n.x)) +
    geom_density()

spell.count.hist <- spell.count.overall %>%
    group_by(n.x) %>%
    count() %>%
    ungroup()

median(spell.count.overall$n.x)
mean(spell.count.overall$n.x)

test <- comment.words %>%
    group_by(Document.ID) %>%
    count()

#### Check for word cutoff ####
count.check <- commentsDf %>%
    inner_join(spell.count.overall) %>%
    arrange(n.x) %>%
    select(Document.ID, Text, n.x, n.y, percent.misspelled)

write.csv(count.check, file = "Data/countCheck.csv", row.names = FALSE)    

filter(word(Document.ID, -1, sep = "-"))

#Visualize counts of misspelling
spell.count %>%
    count(V1, sort = TRUE) %>%
    filter(n <= 30 & n >= 25) %>%
    mutate(V1 = reorder(V1, n)) %>%
    ggplot(aes(V1, n)) +
    geom_col() +
    xlab(NULL) +
    coord_flip()

spell.count %>%
    count(V1, sort = TRUE) %>%
    filter(n <= 1000 & n > 10) %>%
    ggplot(aes(n)) +
    geom_histogram(binwidth = 10)

spell.count.tf <- spell.count %>%
    count(V1, sort = TRUE) %>%
    mutate(V1 = reorder(V1, n))

spell.final <- spell.count.tf %>%
    inner_join(test, by = c("V1" = "word"))

misspell.find <- comment.words %>%
    filter(str_detect(word, regex("commentsrxhistoryrequestrxhistoryresponsepainitiationrequestpainitiationresponseparequestparesponsepaappealrequestpaappealresponsepacancelrequestpacancelresponse")))

comment.words$word <- str_replace_all(comment.words$word, "ahcancal", "ahca/ncal")
comment.words$word <- str_replace_all(comment.words$word, regex("[a-zA-Z].[a-zA-Z]"), "measure level")

write.csv(misspelled, file = "Data/misspelled.csv", row.names = FALSE)
write.csv(misspell.find, file = "Data/misspellTest.csv", row.names = FALSE)
write.csv(spell.final, file = "Data/misspellSuggest.csv", row.names = FALSE)
