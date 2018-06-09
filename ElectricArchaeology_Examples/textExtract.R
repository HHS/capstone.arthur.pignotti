###Note: there's some preprocessing that I (sg) haven't shown here: go see the original gist

################# Wait! ####################################
# Before proceeding, make sure you have a copy of pdf2text
# on your computer! Details: https://en.wikipedia.org/wiki/Pdftotext
# Download: http://www.foolabs.com/xpdf/download.html

# Tell R what folder contains your 1000s of PDFs
dest <- "C:/Data/Test Folder"

# make a vector of PDF file names
myfiles <- list.files(path = dest, pattern = "pdf",  full.names = TRUE)

# now there are a few options...

############### PDF to TXT #################################
# convert each PDF file that is named in the vector into a text file
# text file is created in the same directory as the PDFs
# note that my pdftotext.exe is in a different location to yours
lapply(myfiles, function(i) system(paste("C:/Data/xpdf/bin64/pdftotext.exe", paste0('"', i, '"')), wait = FALSE) )

# where are the txt files you just made?
dest # in this folder
