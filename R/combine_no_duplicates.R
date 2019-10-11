# Title: combine_no_duplicates.R
# Author: Kelly Heilman
# Date: 10/10/19

# The purpose of this script is to read in csv files from the literature review searches, combine them, check for & remove duplicated citations, and output one master copy.

# read in libraries
library(tidyr)
#library(googlesheets)
library(dplyr)
library(googledrive)




# Within the Data folder, there should be another folder titled "savedrecs"
# The only thing you should need to do is to specify your working directory:

workingdir <- "/Users/kah/Documents/MSS_Frontiers/"

#---------------------------- Function Outline ----------------------------------------
# 1. Read in the most recent (all) of the searches listed in the projecte googledrive
# 2. Combine all these together into one big data frame
# 3. Check for and remove duplcates
# 4. Output the most recent master file with the current date 


clean.literature.searches <- function(workingdir = workingdir){
      # Before starting, you should have a folder called "Data" and one called "Outputs" created within the MSS_Frontiers folder.
      # This code will create these if they do not exist
      if(!dir.exists(paste0(workingdir, "Data/savedrecs/"))){
        print("creating Data/savedrecs/ directory")
        dir.create(file.path(paste0(workingdir, "Data/savedrecs/")))
      }
      
      if(!dir.exists(paste0(workingdir, "Outputs/"))){
        print("creating Outputs directory")
        dir.create(file.path(paste0(workingdir, "Outputs/")))
      }
      
      
      #------------------------------------------------------------------------------------
      # 1. Read in the most recent (all) of the searches listed in the project googledrive
      #------------------------------------------------------------------------------------
      #this line will bring you to the browser and ask you to sign into your googledrive
      saved.recs  <- drive_find(team_drive = "Frontiers", type = "csv", pattern = "savedrecs") 
      
      
      # download files from google drive and save to Data/savedrecs
      setwd( paste0(workingdir, "Data/savedrecs/") )
      for(i in 1:length(saved.recs$name)){
        drive_download(saved.recs[i,], overwrite = TRUE)
      }
      setwd( workingdir )
      
      
      # read in all the .csv files in Data/savedrecs folder:
      # use list.files to create a list of all files in the savedrecs folder
      files.to.read <- paste0(workingdir,"Data/savedrecs/", list.files(paste0(workingdir,"/Data/savedrecs/")))
      
      
      
      # create a custom function that figures out which row contains the header for each file, then read in the csv with that row as the header
      find.header.read.csv <- function(x){
        test.csv <- read.csv(x) # read in csv without skipping lines
        header.row <- which(test.csv[,1] %in% "Title") # find the row where the first column has "Title" in it. This is the header row
        nskip <- as.numeric(header.row) 
        read.csv(x, skip = nskip, header = TRUE) # read the csv in for year & skip the nskip
      }
      
      # create a list that contains the contents of all csv files
      listed.refs <- lapply(files.to.read, find.header.read.csv) # use lapply to apply our function over the list of files in Data/savedrecs
      #colnames(listed.refs[[2]]) # check that the colnames make sense
      #colnames(listed.refs[[1]])
      
      # we are really only interested in columns 1-21, so extract only those columns
      
      refs.subset <- lapply(listed.refs, function(x){x[1:21]}) # should make this cleaner, but this is fine for now
      
      #------------------------------------------------------------------------------------
      #2. Combine all these together into one big data frame
      #------------------------------------------------------------------------------------
      
      # use do.call & rbind to combine all together:
      all.refs <- do.call(rbind, refs.subset)
      
      #------------------------------------------------------------------------------------
      # 3. Check for and remove duplcates
      #------------------------------------------------------------------------------------
      
      # check for duplicates & remove one of the duplicate pairs:
      non.duplicated <- all.refs[!duplicated(all.refs$Title),] # note: I am checking for duplicates based on title
      
      # check that duplicated is only removing all but 1 of the duplicated entries:
      # the Staver paper is listed at least twice here:
      #nrow(all.refs[all.refs$Title %in% "The Global Extent and Determinants of Savanna and Forest as Alternative Biome States",])
      # but this is reduced to 1 after removing duplicated items
      #nrow(non.duplicated[non.duplicated$Title %in% "The Global Extent and Determinants of Savanna and Forest as Alternative Biome States",])
      
      # check that there are no more duplicates:
      if(nrow(non.duplicated[duplicated(non.duplicated),]) <1 ){
        print("No more duplicated entries!!")
      }
      
      
      # create a new csv of the non-duplicated dataframe that includes the date in the filename
      updated.csv <- paste0("lit.list.no.dups.", Sys.Date(), ".csv") # create the filename with today's date
      write.csv(non.duplicated, paste0("Outputs/", updated.csv )) # save
      
      #------------------------------------------------------------------------------------
      # 4. Output the most recent master file with the current date 
      #------------------------------------------------------------------------------------
      
      # we need to find the right filepath id for the folder that we want to upload the cleaned file to:
      teamdrive.file.path <- drive_find(team_drive = "Frontiers", type = "folder", pattern = "Compiled_Searches") 
      
      # drive_upload will upload files to the team drive
      (uploaded.file <- drive_upload(
        media =  paste0("Outputs/", updated.csv ), # the file we want to upload
        path = teamdrive.file.path, # a "dribble" that tells googledrive where to upload the file to
        name = updated.csv, # the name of the file, as we want it to appear in google drive
        type = "spreadsheet", # file type
        overwrite = TRUE # overwrite the file if it already exists in google drive (this may not be working properly, so we need to be careful)
      ))

}

clean.literature.searches(workingdir = workingdir)
