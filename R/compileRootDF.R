#################################
# 3/22/2016                     #
# original: 12/3/2015           #
# Sven Nelson                   #
# function: compileRootDF       #
#################################

#'
#' @export
#'
compileRootDF <- function (day0csv=file.choose(), day1csv=file.choose(), day2csv=file.choose(), printFilePaths = TRUE, rtSixMinimum = 3) {
  # require(stringi)
  # Import data from csv
  day0.df <- utils::read.csv(file=day0csv,header = TRUE,stringsAsFactors = FALSE)
  if (printFilePaths) {
    writeLines(paste("DAY0: The file you have loaded is located at:\n\t",day0csv,"\n"))
  }
  day1.df <- utils::read.csv(file=day1csv,header = TRUE,stringsAsFactors = FALSE)
  if (printFilePaths) {
    writeLines(paste("DAY1: The file you have loaded is located at:\n\t",day1csv,"\n"))
  }
  day2.df <- utils::read.csv(file=day2csv,header = TRUE,stringsAsFactors = FALSE)
  if (printFilePaths) {
    writeLines(paste("DAY2: The file you have loaded is located at:\n\t",day2csv,"\n"))
  }
  #writeLines(paste('For easy copying:\n\t"',day0csv,'","', day1csv,'" ,"', day2csv,'"')) # extra spaces

  # Get genotype based on the word after the first space in the filename
  # ex: "20151125 Hollis wt second time root lengths (day 1).csv" => Hollis
  filename1 <- utils::tail(strsplit(day1csv, split="/")[[1]],1)
  FromFileName1 <- utils::head(strsplit(filename1, split=" ")[[1]],2)[2]

  for (i in 0:2) {
    if (i == 0) {
      #attach(day0.df)
      day <- rep(i, length(day0.df$length))
      rootData <- data.frame(day0.df$root_name, day0.df$length, day)
      rootData <- stats::setNames(object = rootData, nm = c("root_name", "length", "day"))
      rm(day)
      #detach(day0.df)
    } else if (i == 1) {
      #attach(day1.df)
      day <- rep(i, length(day1.df$length))
      rootData <- data.frame(day1.df$root_name, day1.df$length, day)
      rootData <- stats::setNames(object = rootData, nm = c("root_name", "length", "day"))
      rm(day)
      #detach(day1.df)
    } else if (i == 2) {
      #attach(day2.df)
      day <- rep(i, length(day2.df$length))
      rootData <- data.frame(day2.df$root_name, day2.df$length, day)
      rootData <- stats::setNames(object = rootData, nm = c("root_name", "length", "day"))
      rm(day)
      #detach(day2.df)
    } else {
      writeLines("ERROR: check loop.")
    }

    rootData$seed <- NA # number from 1 to 20 (seedling number indicates position on the plate)
    rootData$structure <- NA # (coleoptile) shoot, r1, r2, r3, r4, r5

    rootData$structure[grepl("sht", rootData$root_name)] <- "shoot" # allow sht1 or sht
    rootData$structure[grepl("st", rootData$root_name)] <- "shoot" # allow st1 or st also
    rootData$structure[grepl("rt0", rootData$root_name)] <- "shoot" # allow rt0 also
    rootData$structure[grepl("rt1", rootData$root_name)] <- "r1"
    rootData$structure[grepl("r1", rootData$root_name)] <- "r1" # to allow seed1r1 naming
    rootData$structure[grepl("rt2", rootData$root_name)] <- "r2"
    rootData$structure[grepl("r2", rootData$root_name)] <- "r2" # to allow seed1r2 naming
    rootData$structure[grepl("rt3", rootData$root_name)] <- "r3"
    rootData$structure[grepl("r3", rootData$root_name)] <- "r3" # to allow seed1r3 naming
    rootData$structure[grepl("rt4", rootData$root_name)] <- "r4"
    rootData$structure[grepl("r4", rootData$root_name)] <- "r4" # to allow seed1r4 naming
    rootData$structure[grepl("rt5", rootData$root_name)] <- "r5"
    rootData$structure[grepl("r5", rootData$root_name)] <- "r5" # to allow seed1r5 naming
    rootData$structure[grepl("rt6", rootData$root_name)] <- "r6"
    rootData$structure[grepl("r6", rootData$root_name)] <- "r6" # to allow seed1r6 naming

    # For r4 or r5 where root is present, but not visible (name with seed1r4_NA)
    rootData$length[grepl("_NA", rootData$root_name)] <- NA # changes any with _NA to NA
    # Intended for r4 and r5, since other structures will be NA if not present,
    # however, this code will change the length to NA for any structure with _NA in the
    # root_name.

    rootData$genotype <-rep(FromFileName1, length(rootData$length))

    for (j in 1:length(rootData$structure)) {
      if (is.na(rootData$structure[j])) {
        writeLines("ERROR: one of the root traces is improperly named.  Check for a sh1 instead of sht1\n")
        writeLines(paste("\t-LOCATION OF ERROR-\n\tTable from day: ", i,""))
        writeLines(paste("\tRow number: ", j+1,"\n"))
      }
      # indices 6 and 7 of the string (not 5 and 6) because SmartRoot adds an extra space
      # at the beginning of seed1rt4 => " seed1rt4".  If this ever changes, the indices
      # will also need to be changed.
      if (!grepl("^[[:digit:]]+$", stringi::stri_sub(rootData$root_name, 6, 7))[j]) { # if seed## are not both numbers
        rootData$seed[j] <- as.numeric(stringi::stri_sub(rootData$root_name, 6, 6)[j]) # set number from seed#
      } else {
        rootData$seed[j] <- as.numeric(stringi::stri_sub(rootData$root_name, 6, 7)[j]) # set numbers from seed##
      }
    }

    # require(stringi)

    #stri_sub("abcde", -2, -1)
    #cat <- c("seed2rt2", "seed12rt11", "seed20sht1")
    #stri_sub(cat, 5, 6) # This is vectorized.

    #     for (k in 1:length(rootData$root_name)) { # for some reason number in stri_sub +1 for data frame
    #       if (!grepl("^[[:digit:]]+$", stri_sub(rootData$root_name, 6, 7))[k]) { # if seed## are not both numbers
    #         rootData$seed[i] <- as.numeric(stri_sub(rootData$root_name, 6, 6)[k]) # set number from seed#
    #       } else {
    #         rootData$seed[i] <- as.numeric(stri_sub(rootData$root_name, 6, 7)[k]) # set numbers from seed##
    #       }
    #     }
    if (i == 0) {
      rootDay0 <- rootData[,2:length(rootData[1,])] # subset to only columns 2 through 5
    } else if (i == 1) {
      rootDay1 <- rootData[,2:length(rootData[1,])] # subset to only columns 2 through 5
    } else if (i == 2) {
      rootDay2 <- rootData[,2:length(rootData[1,])] # subset to only columns 2 through 5
    } else {
      writeLines("ERROR: Days exceed 3 or unusual day number indicated.")
      writeLines(paste("\nThe value of i is: ", i,"\n"))
    }
    rm(rootData)
  }

  # require("dplyr")
  structList <- c("r4", "r5", "r6") # only do this for r4 and r5
  for (i in 1:max(rootDay0$seed)) {
    if (i %in% rootDay0$seed) { # if the seed column contains i
      for (j in 1:length(structList)) {
        #         print(i)
        #         print(structList[j])
        #         print("position 1")
        # if r4 or r5 or r6 does not exists in day 0, add at length = 0
        if (!(structList[j] %in% dplyr::filter(rootDay0, seed==i)$structure)) {
          newRow <- list(length = 0, day = 0, seed = i, structure = structList[j], genotype = FromFileName1)
          rootDay0 <- rbind(rootDay0, newRow)
          rm(newRow)
        }
        # if r4 or r5 does not exists in day 1, add at length = 0
        if (!(structList[j] %in% dplyr::filter(rootDay1, seed==i)$structure)) {
          newRow <- list(length = 0, day = 1, seed = i, structure = structList[j], genotype = FromFileName1)
          rootDay1 <- rbind(rootDay1, newRow)
          rm(newRow)
        }
        # if r4 or r5 does not exists in day 2, add at length = 0
        if (!(structList[j] %in% dplyr::filter(rootDay2, seed==i)$structure)) {
          newRow <- list(length = 0, day = 2, seed = i, structure = structList[j], genotype = FromFileName1)
          rootDay2 <- rbind(rootDay2, newRow)
          rm(newRow)
        }
      }
    }
  }


  # combine two data frames with rbind
  rootLengthsDataFrame <- rbind(rootDay0, rootDay1)
  rootLengthsDataFrame <- rbind(rootLengthsDataFrame, rootDay2)

  # Convert lengths from centimeters to millimeters
  rootLengthsDataFrame$length <- round(rootLengthsDataFrame$length * 10,2)

  #   # Insert zero values for shoots that sprouted after day 1
  #   for (i in 1:max(rootLengthsDataFrame$seed)) {
  #     if (i %in% rootLengthsDataFrame$seed) { # if the seed column contains i
  #       # if length of subset day 3 seed i == day 2
  #       # if not,
  #     }
  #   }
  ### Skip this part and attempt to do it in the plotting section: seed 2 length - seed 1, if seed 1 exists, else minus 0.

  # another way: add zeros for absolutely everything that is missing, then remove seed/structure combinations if day 3 is equal 0.

  # Sort? By day, then seed, then structure?
  rootLengthsDataFrame <- dplyr::filter(rootLengthsDataFrame, !is.na(length))

  ## Remove r6 if less than 3 seedlings with r6 present ##
  numWithRootSix <- length(dplyr::filter(rootLengthsDataFrame, structure == "r6" & day == 2 & length > 0)$length)

  if (numWithRootSix < rtSixMinimum) {
    rootLengthsDataFrame <- dplyr::filter(rootLengthsDataFrame,structure != "r6")
  }

  return(rootLengthsDataFrame)
}
