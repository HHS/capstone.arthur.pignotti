load("Models/lda_test.rda")

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

test <- model.doc.term %>%
    mutate(score = log(score)) %>%
    arrange(document, term) %>%
    spread(term, score)

docs <- sort(unique(model.doc.term$document))

doc_map <- data.frame(Section = docs) %>%
    rowid_to_column("Topic")

test_lda@beta <- as.matrix(test)
test_lda@terms <- colnames(test)
test_lda@k <- as.integer(79)
test_lda@documents <- docs

words_dtm.topics <- posterior(test_lda, words_dtm)

# Transform results into tidy dataframe and map scores to document sections
words_scoring <- as.data.frame(cbind(Document.ID = rownames(words_dtm.topics$topics),
                                     words_dtm.topics$topics)) %>%
    gather(key = "document",
           value = "Score",
           2:80) %>%
    mutate(Score = as.numeric(Score),
           document = as.numeric(document)) %>%
    inner_join(doc_map, by = c("document" = "Topic"))


# Calculate min and max score of each section to normalize scores
minmax.base <- words_scoring %>%
    group_by(Section) %>%
    summarise(min = min(Score, na.rm = TRUE),
              max = max(Score, na.rm = TRUE))

# Mix-Max normalize scores
words_scoring.norm <- words_scoring %>%
    inner_join(minmax.base,
               by = c("Section" = "Section")) %>%
    mutate(Final_score = (Score-min)/(max-min))

# Transform dataframe for final use
words_scoring.matrix <- words_scoring.norm %>%
    mutate(Final_score = if_else(Final_score < .001,
                           0,
                           Final_score)) %>% # Scores below .1% are changed to zero to remove scientific notation
    select(-c(Score, min, max, document)) %>% # Remove unneeded scoring components
    spread(Section,
           Final_score) # Transform tidy dataframe into wide format

# Write out results
write.csv(words_scoring.matrix, file = "Data/results_test.csv", row.names = FALSE)
