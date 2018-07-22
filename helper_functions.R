#### wordcount: Function to count words ####
wordcount <- function(str) {
    sapply(gregexpr("\\b\\W+\\b", str, perl=TRUE), function(x) sum(x>0) ) + 1 
}
