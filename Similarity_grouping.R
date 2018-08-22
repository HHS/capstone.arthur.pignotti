#### Initial Setup ####
.libPaths( c("C:/R/Packages", .libPaths()) ) # add extra library location
setwd("C:/Users/P6BQ/Desktop/capstone.arthur.pignotti") # local location of github repo
source("helper_functions.R")

# Load libraries
library(dplyr)
library(stringr)
library(ggplot2)
library(tidytext)
library(tidyverse)
library(SnowballC)
library(tm)
library(topicmodels)
library(quanteda)
library(visNetwork)


# Load comment data
commentsDf <- read.csv("Data/commentsDf.csv", stringsAsFactors = FALSE)
bigram.list <- read.csv("Data/bigram_list2.csv", stringsAsFactors = FALSE)

baselineDoc <- read_excel("Data/AN_Part2 - V4.xlsx", sheet = 1)
colnames(baselineDoc) <- make.names(colnames(baselineDoc))


#### Create Corpus Using Tidy ####
# Load stop words
data(stop_words)

# Load CMS-specific stop words
cms_stop <- read_csv("Data/cms_stop_words.csv")


# Remove non-alpha-numeric characters
#commentsDf$Text <- gsub("[^A-z0-9 ]", "", commentsDf$Text)

#Unnest tokens and remove stop words
docs <- commentsDf %>%
    select(Document.ID, Text) %>%
    union_all(select(baselineDoc,
                     Document.ID,
                     Text))

baselineName <- unique(baselineDoc$Document.ID)

comment.words <- docs %>%
    unnest_tokens(word, Text) %>%
#    filter(!(str_detect(word, regex("^http"))),
#           !(str_detect(word, regex("^www")))) %>%
#    anti_join(stop_words) %>%
#    anti_join(cms_stop) %>%
#    mutate(word = wordStem(word))

#### Create TF-IDF ####
comment.count <- comment.words %>%
    count(Document.ID,
          word,
          sort = TRUE) %>%
    ungroup()

comment.total <- comment.count %>% 
    group_by(Document.ID) %>% 
    summarize(total = sum(n))

comment <- left_join(comment.count,
                     comment.total)

comment.tf_idf <- comment %>%
    bind_tf_idf(word,
                Document.ID,
                n)

#### Convert to DFM and Calculate Similarity Matrices ####
words_dfm <- cast_dfm(comment.count,
                      document = Document.ID,
                      term = word,
                      value = n)

words_dfm_tf_idf <- cast_dfm(comment.tf_idf,
                             document = Document.ID,
                             term = word,
                             value = tf_idf)

words_dtm <- cast_dtm(comment.tf_idf,
                      Document.ID,
                      word,
                      n)

# Calculate similarity matrix and tidy it up
similarity <- textstat_simil(words_dfm,
                             margin = "documents",
                             method = "cosine") %>%
    as.dist() %>%
    tidy() %>%
    arrange(desc(distance))

names(similarity) <- c("doc1", "doc2", "distance")
head(similarity)

#### Separate out baseline similarities ####
baseSim <- similarity %>%
    filter(doc1 == baselineName | doc2 == baselineName) %>%
    arrange(-distance)

baseSim1 <- filter(baseSim, doc1 == baselineName)

baseSim2 <- filter(baseSim, doc2 == baselineName) %>%
    rename(doc1 = doc2,
           doc2 = doc1)

baseSim <- baseSim1 %>%
    union_all(baseSim2) %>%
    arrange(-distance)

unique(baseSim$doc1)

write.csv(baseSim,
          file = "Data/Similarity_to_Baseline.csv",
          row.names = FALSE)

similarity <- similarity %>%
    filter(!(doc1 == baselineName | doc2 == baselineName))

#### Investigate similar comments ####
hist(filter(similarity, distance > .85)$distance)

count <- similarity %>%
    filter(distance > .75) %>%
    arrange(doc1)

sim.doc.list <- union_all(count$doc1,
                          count$doc2) %>%
    as.data.frame() %>%
    distinct()

colnames(sim.doc.list) <- "id"

my_ggtheme = theme_bw() +                     
    theme(axis.title = element_text(size = 18),
          axis.text = element_text(size = 16),
          legend.text = element_text(size = 16),              
          legend.title = element_text(size = 18),             
          plot.title = element_text(size = 22),               
          plot.subtitle = element_text(size = 18)) 

similarity_nodes = data.frame(id = sim.doc.list, #<- node data frame must an `id` column
                             stringsAsFactors = FALSE)

similarity_edges = similarity[similarity$distance > 0.75, ]
colnames(similarity_edges) = c("from", "to", "value")


sim_network = visNetwork(similarity_nodes,
                         similarity_edges) %>%       #<- set edges    
    visOptions(highlightNearest = TRUE,  #<- highlight nearest when clicking on a node 
               nodesIdSelection = TRUE)  #<- add an id node selection menu 


sim_network


cnt = 0
for (x in c(.95, .9, .85, .8, .75, .7, .65, .6)){
    cnt = cnt + 1
    count <- similarity %>%
        filter(distance >= x) %>%
        arrange(doc1)
    
    # Build cluster
    for (i in 1:dim(sim.doc.list)[1]){
        if (i == 1) {
            clus.df <- data.frame(doc = sim.doc.list$id,
                                  cluster = 0,
                                  checked = "N",
                                  stringsAsFactors = FALSE)
            cluster = 1
            tmp.list <- union_all(filter(count, doc1 == clus.df$doc[i])$doc2,
                                  filter(count, doc2 == clus.df$doc[i])$doc1)
            clus.df$checked[clus.df$doc == clus.df$doc[i]] = "Y"
            clus.df$cluster[clus.df$doc == clus.df$doc[i]] = cluster
            clus.df$checked[clus.df$doc %in% tmp.list] = "Y"
            clus.df$cluster[clus.df$doc %in% tmp.list] = cluster
            for (j in 1:length(tmp.list)) {
                tmp.list1 <- union_all(filter(count, doc1 == tmp.list[j])$doc2,
                                       filter(count, doc2 == tmp.list[j])$doc1)
                clus.df$checked[clus.df$doc %in% tmp.list1] = "Y"
                clus.df$cluster[clus.df$doc %in% tmp.list1] = cluster
                for (k in 1:length(tmp.list1)) {
                    tmp.list2 <- union_all(filter(count, doc1 == tmp.list1[j])$doc2,
                                           filter(count, doc2 == tmp.list1[j])$doc1)
                    clus.df$checked[clus.df$doc %in% tmp.list1] = "Y"
                    clus.df$cluster[clus.df$doc %in% tmp.list1] = cluster
                }
            }
            cluster = cluster + 1
        } else {
            if (clus.df$checked[i] == "N") {
                tmp.list <- union_all(filter(count, doc1 == clus.df$doc[i])$doc2,
                                      filter(count, doc2 == clus.df$doc[i])$doc1)
                clus.df$checked[clus.df$doc == clus.df$doc[i]] = "Y"
                clus.df$cluster[clus.df$doc == clus.df$doc[i]] = cluster
                clus.df$checked[clus.df$doc %in% tmp.list] = "Y"
                clus.df$cluster[clus.df$doc %in% tmp.list] = cluster
                for (j in 1:length(tmp.list)) {
                    tmp.list1 <- union_all(filter(count, doc1 == tmp.list[j])$doc2,
                                           filter(count, doc2 == tmp.list[j])$doc1)
                    clus.df$checked[clus.df$doc %in% tmp.list1] = "Y"
                    clus.df$cluster[clus.df$doc %in% tmp.list1] = cluster
                    for (k in 1:length(tmp.list1)) {
                        tmp.list2 <- union_all(filter(count, doc1 == tmp.list1[j])$doc2,
                                               filter(count, doc2 == tmp.list1[j])$doc1)
                        clus.df$checked[clus.df$doc %in% tmp.list1] = "Y"
                        clus.df$cluster[clus.df$doc %in% tmp.list1] = cluster
                    }
                }
                cluster = cluster + 1
            }
        }
    }
    if (cnt == 1) {
        clus.count <- data.frame(threshold = x,
                                 k = max(clus.df$cluster))
    } else {
        clus.tmp <- data.frame(threshold = x,
                               k = max(clus.df$cluster))
        clus.count <- union_all(clus.count, clus.tmp)
    }
}




# Cluster testing
simtest <- textstat_simil(words_dfm_tf_idf,
                          margin = "documents",
                          method = "cosine") %>%
    as.dist()

hc <- hclust(simtest, "ward.D")
plot(hc, main = "testing",
     ylab = "", xlab = "", yaxt = "n")


