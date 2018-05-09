#################################
# 4/25/2016                     #
# Sven Nelson                   #
# function: mergeCSVs           #
#################################

#### Function to combine multiple csv files and output a combined file ####

# make folders: day0 WW, day0 WS, day1 WW, day1 WS, day2 WW, day2 WS
#'
#' @export
#'
mergeCSVs <- function(csvFile = file.choose(), returnFile = F, outputName = NULL) { # select a file in the target folder
  # All csv files in the target folder will be combined into one file.
  directory <- dirname(csvFile)
  tempWD <- getwd()
  setwd(directory)

  # List the files in the directory
  file_list <- list.files()

  for (file in file_list){
    # if the merged dataset doesn't exist, create it
    if (!exists("mergedCSVfiles")){
      mergedCSVfiles <- utils::read.csv(file, header = T, stringsAsFactors = F)
    } else if (exists("mergedCSVfiles")){ # if the merged dataset does exist, append to it
      temp_dataset <- utils::read.csv(file, header = T, stringsAsFactors = F)
      mergedCSVfiles <- rbind(mergedCSVfiles, temp_dataset)
      rm(temp_dataset)
    }
  }

  # Get filepath and filename:
  csvFile.name <- basename(csvFile)
  csvFile.orig <- csvFile.name # Keep track of the original name
  #csvFile.path <- dirname(csvFile) # Looks like we won't need this

  if (is.null(outputName)) {
    # move up 1 folder and save the output .csv file
    #setwd("../") # not working
    # Parse the name so that:
    # 20160321 Diva wt WW1 day2 s1.csv --> 20160321 Diva wt WW day2 (combined).csv
    # 20160319 Diva wt WS1 day0.csv --> 20160319 Diva wt WS day0 (combined).csv
    # find-and-replace WW1 with WW, WS1 with WS, and s* with ""
    # append " (combined)" before .csv
    if (grepl("WW[1-3]", csvFile.name)) { # convert WW# to WW
      csvFile.name <- sub("WW[1-3]", "WW", csvFile.name)
    } else if (grepl("WS[1-3]", csvFile.name)) { # WS1-3 --> WS
      csvFile.name <- sub("WS[1-3]", "WS", csvFile.name)
    }
    if (grepl(" s[1-8]", csvFile.name)) { # get rid of s1 - s8
      csvFile.name <- sub(" s[1-8]", "", csvFile.name)
    }
    if (grepl(".csv", csvFile.name)) {
      csvFile.name <- sub(".csv", " (combined).csv", csvFile.name)
    }
    csvFile <- sub(csvFile.orig, csvFile.name, csvFile)
  } else {
    csvFile <- sub(csvFile.orig, outputName, csvFile)
  }

  # output a file
  utils::write.csv(mergedCSVfiles, file = csvFile, row.names = F)
  # change back to the original working directory
  setwd(tempWD)
  writeLines("File output complete.")
  if (returnFile) {
    return(mergedCSVfiles)
  }
}
