##### Wait! #####
# Before proceeding, make sure you have a copy of Tesseract
# on your computer! Details & download:
# https://code.google.com/p/tesseract-ocr/
# and a copy of ImageMagick: http://www.imagemagick.org/
# and a copy of pdftoppm on your computer!
# Download: http://www.foolabs.com/xpdf/download.html
# And then after installing those three, restart to
# ensure R can find them on your path.
# And note that this process can be quite slow...

# PDF filenames can't have spaces in them for these operations
# so let's get rid of the spaces in the filenames

sapply(myfiles, FUN = function(i){
    file.rename(from = i, to =  paste0(dirname(i), "/", gsub(" ", "", basename(i))))
})

# get the PDF file names without spaces
myfiles <- list.files(path = dest, pattern = "pdf",  full.names = TRUE)

# Now we can do the OCR to the renamed PDF files. Don't worry
# if you get messages like 'Config Error: No display
# font for...' it's nothing to worry about

lapply(myfiles, function(i){
    # convert pdf to ppm (an image format), using
    shell(shQuote(paste0("pdftoppm ", i, " -f 1 -l 10 -r 600 ocrbook")))
    # convert ppm to tif ready for tesseract
    shell(shQuote(paste0("convert *.ppm ", i, ".tif")))
    # convert tif to text file
    shell(shQuote(paste0("tesseract ", i, ".tif ", i, " -l eng")))
    # delete tif file
    file.remove(paste0(i, ".tif" ))
})

# where are the txt files you just made?
dest # in this folder