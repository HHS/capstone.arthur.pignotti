# installing tm
install.packages('devtools', lib="C:/R/Packages")
library(devtools)

slam_url <- "https://cran.r-project.org/src/contrib/Archive/slam/slam_0.1-37.tar.gz"
install_url(slam_url)

dest <- "C:/Data/Test Folder"

mytxtfiles <- list.files(path = dest, pattern = "txt",  full.names = TRUE)

library(tm)
mycorpus <- Corpus(DirSource(dest, pattern = "txt"))
# warnings may appear after you run the previous line, they
# can be ignored
mycorpus <- tm_map(mycorpus,  removeNumbers)
mycorpus <- tm_map(mycorpus,  removePunctuation)
mycorpus <- tm_map(mycorpus,  stripWhitespace)
mydtm <- DocumentTermMatrix(mycorpus)
# remove some OCR weirdness
# words with more than 2 consecutive characters
mydtm <- mydtm[,!grepl("(.)\\1{2,}", mydtm$dimnames$Terms)]

# get each doc as a csv with words and counts
for(i in 1:nrow(mydtm)){
    # get word counts
    counts <- as.vector(as.matrix(mydtm[1,]))
    # get words
    words <- mydtm$dimnames$Terms
    # combine into data frame
    df <- data.frame(word = words, count = counts,stringsAsFactors = FALSE)
    # exclude words with count of zero
    df <- df[df$count != 0,]
    # write to CSV with original txt filename
    write.csv(df, paste0(mydtm$dimnames$Docs[i],".csv"), row.names = FALSE) 
}

# and now you're ready to work with the csv files

############### PDF to TXT (all text between two words) ####

## Below is about splitting the text files at certain characters
## can be skipped...

# if you just want the abstracts, we can use regex to extract that part of
# each txt file, Assumes that the abstract is always between the words 'Abstract'
# and 'Introduction'

abstracts <- lapply(mytxtfiles, function(i) {
    j <- paste0(scan(i, what = character()), collapse = " ")
    regmatches(j, gregexpr("(?<=Abstract).*?(?=Introduction)", j, perl=TRUE))
})
# Write abstracts into separate txt files...

# write abstracts as txt files 
# (or use them in the list for whatever you want to do next)
lapply(1:length(abstracts),  function(i) write.table(abstracts[i], file=paste(mytxtfiles[i], "abstract", "txt", sep="."), quote = FALSE, row.names = FALSE, col.names = FALSE, eol = " " ))

# And now you're ready to do some text mining on the txt