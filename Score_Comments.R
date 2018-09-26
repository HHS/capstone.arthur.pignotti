#### Initial Setup ####
.libPaths( c("C:/R/Packages", .libPaths()) ) # add library location - library workaround
setwd("C:/Users/P6BQ/Desktop/capstone.arthur.pignotti") # local location of github repo
source("helper_functions.R") # load helper functions

# Load libraries
library(dplyr)
library(stringr)
library(ggplot2)
library(tidytext)
library(tidyverse)
library(SnowballC)
library(tm)
library(quanteda)
library(topicmodels)

# Similarity to Baseline Document Threshold
baseSimThres = .95

# Load comment data
commentsDf <- read.csv("Data/commentsDf.csv",
                       stringsAsFactors = FALSE) # dataset of comments
bigram.list <- read.csv("Data/bigram_list2.csv",
                        stringsAsFactors = FALSE) # load list of relevant bigrams

commentsDf$Text <- gsub("[^A-z0-9 ]", " ", commentsDf$Text)

#### Remove Comment Attachment that are Uploads of the Baseline Document ####
baseSim <- read.csv("Data/Similarity_to_Baseline.csv")
simRemoves <- baseSim %>%
    filter(distance > baseSimThres) %>%
    select(doc2) %>%
    rename(doc = doc2)

commentsDf <- commentsDf %>%
    filter(!(Document.ID %in% simRemoves$doc))

#### Create Corpus Using Tidy ####
data(stop_words) # load common English stop words
cms_stop <- read_csv("Data/cms_stop_words.csv") # load CMS-specific stop words

## Unnest tokens, integrate bigrams, and remove stop words ##
# Unnnest bigram tokens
bigram.index <- commentsDf %>%
    unnest_tokens(bigram,
                  Text,
                  token = "ngrams",
                  n = 2) %>% # unnest bigram tokens
    rowid_to_column("index") %>% # add index column
    separate(bigram,
             c("word1", "word2"),
             sep = " ") %>% # separate bigram into two words for stemming 
    mutate(word1 = wordStem(word1),
           word2 = wordStem(word2)) # stem separated bigrams

# Fix bigram unnesting so it matches word unnesting. Bigram unnesting will not unnest the final word of a cell into the first word of the next cell or by itself so the indexing between bigrams and words will not naturally match. So, we are unnesting the second word from the final bigram from each Document.ID-Page.Number combo to create a separate row with the final word by itself 
bigram.last.row <- bigram.index %>%
    group_by(Document.ID,
             Page.Number) %>% # group by Document.ID and Page.Number
    filter(row_number() == n()) %>% # filters for the last row of each group
    ungroup() %>%
    mutate(word = word2, # transform the bigram into the final word
           index = index + 1) %>% # increment the index so it arrange at the end of the Document.ID-Page.Number group when reintegrated
    filter(!is.na(word)) %>% # remove NA that show up when unnesting text with less than two words
    select(-c(word1, word2)) # 

bigram.index <- bigram.index %>%
    unite(word,
          word1,
          word2,
          sep = " ") %>% # recombine stemmed bigrams
    union_all(bigram.last.row) %>% # combine the original unnested bigram and the single word fix
    arrange(Document.ID,
            Page.Number,
            index) %>% # re
    rowid_to_column("index1") %>%
    select(-index) %>%
    inner_join(bigram.list,
               by = c("word" = "word")) %>%
    mutate(index2 = index1 + 1,
           index = index1)

comment.words <- commentsDf %>%
    unnest_tokens(word,
                  Text) %>%
    rowid_to_column("index") %>%
    filter(!(str_detect(word, regex("^http"))),
           !(str_detect(word, regex("^www")))) %>% 
    anti_join(stop_words) %>%
    anti_join(cms_stop) %>%
    anti_join(bigram.index,
              by = c("index" = "index1")) %>%
    anti_join(bigram.index,
              by = c("index" = "index2")) %>%
    mutate(word = wordStem(word))

comments <- comment.words %>%
    union_all(bigram.index) %>%
    select(-c(index1, index2))

#### Create TF-IDF ####
comment.count <- comments %>%
    mutate(Document.ID = paste0(Document.ID,"-",Page.Number)) %>%
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

words_dtm_tf_idf <- cast_dtm(comment.tf_idf,
                             document = Document.ID,
                             term = word,
                             value = tf_idf)

words_dtm <- cast_dtm(comment.count,
                      document = Document.ID,
                      term = word,
                      value = n)


#### Apply Base Model ####
# Load model created in Setup_Baseline_for_Model.R
load("Models/lda_test.rda") 

# Run model against comments
words_dtm.topics <- posterior(base_lda,
                              words_dtm,
                              control = list(seed = 1234))

save(words_dtm.topics, file = "Models/comment_scores.rda")

# Model parameters to map to document sections and calculate final scores
model.topic.term <- tidy(base_lda, matrix = "beta")
model.doc.topic <- tidy(base_lda, matrix = "gamma")
model.doc.term <- model.topic.term %>%
    inner_join(model.doc.topic,
               by = c("topic" = "topic")) %>%
    mutate(score = beta * gamma) %>%
    select(document, term, score, gamma) %>%
    group_by(document,
             term) %>%
    summarise(score = sum(score)/sum(gamma)) %>%
    arrange(desc(score))

# Transform results into tidy dataframe and map scores to document sections
words_scoring <- as.data.frame(cbind(Document.ID = rownames(words_dtm.topics$topics),
                                     words_dtm.topics$topics)) %>%
    gather(key = "Topic",
           value = "Score",
           2:87) %>%
    mutate(Topic = as.numeric(Topic)) %>%
    right_join(model.doc.topic,
               by = c("Topic" = "topic")) %>%
    mutate(final_score = as.numeric(Score) * gamma) %>%
    group_by(Document.ID,
             document) %>%
    summarise(sum = sum(final_score)/sum(gamma)) %>%
    arrange(-sum) %>%
    ungroup()

# Calculate min and max score of each section to normalize scores
minmax.base <- words_scoring %>%
    group_by(document) %>%
    summarise(min = min(sum, na.rm = TRUE),
              max = max(sum, na.rm = TRUE))

# Mix-Max normalize scores
words_scoring.norm <- words_scoring %>%
    inner_join(minmax.base,
               by = c("document" = "document")) %>%
    mutate(score = (sum-min)/(max-min))

# Transform dataframe for final use
words_scoring.matrix <- words_scoring.norm %>%
    mutate(score = if_else(score < .001,
                           0,
                           score)) %>% # Scores below .1% are changed to zero to remove scientific notation
    select(-c(sum, min, max)) %>% # Remove unneeded scoring components
    spread(document,
           score) # Transform tidy dataframe into wide format

# Write out results
write.csv(words_scoring.matrix, file = "Data/results.csv", row.names = FALSE)

# Create histograms of scores for each section to look at distribution
for (i in 1:length(unique(words_scoring.norm$document))) {
    doc <- unique(words_scoring.norm$document)[i]

    subset <- words_scoring.norm %>%
        filter(document == doc) %>%
        filter(score > .01)

    png(paste0("score_hists/", doc,".png"),
        width=1280,
        height=800)

    hist(subset$score,
         main = doc)

    dev.off()
}


#### Find common words in comments, but not in model ####
model.terms <- base_lda@terms

comment.term.count.missing <- comment.count %>%
    group_by(word) %>%
    summarise(count = sum(n)) %>%
    arrange(desc(count)) %>%
    filter(!word %in% model.terms) %>%
    filter(count > 50)

hist(filter(comment.term.count.missing, count < 500)$count)
