# Title: combine_no_duplicates.R
# Author: Kelly Heilman
# Date: 10/10/19

# The purpose of this script is to read in csv files from the literature review searches, combine them, check for & remove duplicated citations, and output one master copy.

# read in libraries
library(tidyr)
#library(googlesheets)
suppressMessages(library(dplyr))

library(googledrive)

# 1. Read in the most recent (all) of the searches listed in the projecte googledrive
# 2. Combine all these together into one big data frame
# 3. Check for and remove duplcates
# 4. Output the most recent master file with the current date 


# Read in the most recent (all) of the searches listed in the projecte googledrive
drive_find(n=30)
saved.recs  <- drive_find(team_drive = "Frontiers", type = "csv", pattern = "savedrecs") 


# download files from google drive and save to Data/savedrecs
setwd("/Users/kah/Documents/MSS_Frontiers/Data/savedrecs/")
for(i in 1:length(saved.recs$name)){
  drive_download(saved.recs[i,], overwrite = TRUE)
}
setwd("/Users/kah/Documents/MSS_Frontiers/")


# read in all the .csv files in Data/savedrecs folder:
files.to.read <- paste0("/Users/kah/Documents/MSS_Frontiers/Data/savedrecs/", list.files("/Users/kah/Documents/MSS_Frontiers/Data/savedrecs/"))

read.csv(skip = 27)

# create a custom function that figures out which row contains the header for each file, then read in the csv with that row as the header
find.header.read.csv <- function(x){
  test.csv <- read.csv(x) # read in csv without skipping lines
  header.row <- which(test.csv[,1] %in% "Title") # find the row where the first column has "Title" in it. This is the header row
  nskip <- as.numeric(header.row) 
  read.csv(x, skip = nskip, header = TRUE) # read the csv in for year & skip the nskip
}

test.csv <- find.header.read.csv(files.to.read[[1]])

listed.refs <- lapply(files.to.read, find.header.read.csv) # use lapply to apply our function over the list of files in Data/savedrecs
colnames(listed.refs[[2]])
colnames(listed.refs[[1]])

# we are really only interested in columns 1-21 ,so extract all those

refs.subset <- lapply(listed.refs, function(x){x[1:21]}) # should make this cleaner, but this is fine for now

# use do.call & rbind to combine all together:
all.refs <- do.call(rbind, refs.subset)

# check for duplicates & remove:
non.duplicated <- all.refs[!duplicated(all.refs),]



