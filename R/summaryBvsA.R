#################################
# 4/25/2016                     #
# Sven Nelson                   #
# function: summaryBvsA         #
#################################

# Write a function to create a summary between WW and WS
# summaryWWvsWS (A = WW, B = WS)

pkg.env <- new.env(parent = emptyenv())

#'
#' @export
#'
summaryBvsA <- function(A, B, measurevar = "length", pCuttoff = 0.05, Aname = "WW", Bname = "WS") {

  B$seed <- B$seed + max(A$seed)
  # Add a treatment column to each dataframe with their treatment (WW or WS)
  A$treatment <- Aname
  B$treatment <- Bname
  # Then combine the two dataframes into one.
  combinedData <- rbind(A, B)
  if (length(dplyr::filter(combinedData, structure == "r6")$structure) > 0) {
    structList <- c("shoot", "r1", "r2", "r3", "r4", "r5", "r6")
  } else {
    r4AllZero <- nrow(dplyr::filter(combinedData, structure == "r4" & length != 0)) == 0
    r5AllZero <- nrow(dplyr::filter(combinedData, structure == "r5" & length != 0)) == 0
    if (!r4AllZero & !r5AllZero) {
      structList <- c("shoot", "r1", "r2", "r3", "r4", "r5")
    } else if (r4AllZero & r5AllZero) {
      structList <- c("shoot", "r1", "r2", "r3")
    } else if (r4AllZero & !r5AllZero) {
      structList <- c("shoot", "r1", "r2", "r3", "r5")
    } else if (!r4AllZero & r5AllZero) {
      structList <- c("shoot", "r1", "r2", "r3", "r4")
    }

  }
  # combinedData$treatment <- factor(combinedData$treatment, levels = c("WW", "WS"))
  # combinedData$seed <- factor(combinedData$seed, levels = c(1:max(combinedData$seed)))
  # combinedData$structure <- factor(combinedData$structure, levels = structList)
  # combinedData$day <- factor(combinedData$day, levels = c(0:2))
  combinedData <- combinedData[,c(1,2,3,4,6)] # Remove genotype row (otherwise, need as factor)

  #### Create new dataframe with differences ####
  summA <- rootPlot(rootDF = A, returnSummary = T)
  summB <- rootPlot(rootDF = B, returnSummary = T)

  # create dataframe
  # columns: diff (WW$length - WS$length), day (keep), structure (keep),
  BvsAdf <- summA[,1:2]
  BvsAdf$diff <- summB$length - summA$length
  BvsAdf$pval <- NA
  BvsAdf$rowNM <- row.names(BvsAdf)
  #### Now to do some statistics on this ####
  for (struct in structList) {
    #combData_trim <<- data.frame()
    if (length(dplyr::filter(combinedData, structure == struct)$structure) > 0) {
      # trim to 1 structure at a time for processing
      combData_trim <- dplyr::filter(combinedData, structure == struct)
      combData_trim$treatment <- factor(combData_trim$treatment, levels = c("WW", "WS"))
      combData_trim$seed <- factor(combData_trim$seed, levels = c(1:max(combData_trim$seed)))
      combData_trim$structure <- factor(combData_trim$structure, levels = struct)
      combData_trim$day <- factor(combData_trim$day, levels = c(0:max(unique(combData_trim$day))))
      # lsmeans has an env issue and cannot always access the local variables, so use pkg.env
      pkg.env$combData_tr <- combData_trim
      # Mixed Design Anova with post hoc lsmeans analysis
      # Independent Variable between:  treatment
      # Independent Variable within:   day
      # Dependent Variable:            length
      # require(lsmeans)
      # require(afex)
      # Mixed effects modelling
      utils::capture.output( # Capture printing from mixed function to console (don't dispay)
        utils::capture.output( # Capture messages from mixed function to console (don't display)
          fit_mixed <- afex::mixed(length ~ treatment*day + (1|seed), data = pkg.env$combData_tr),
        type = "message")
        )
      ## Pairwise comparisons
      ref3 <- lsmeans::lsmeans(fit_mixed, ~ treatment|day, data = pkg.env$combData_tr) # | is same as "by"
      comps <- lsmeans::contrast(ref3, method="pairwise")
      # adjusting for each level
      outputLSM <- summary(comps)
      rm(comps)
      outputPvals <- outputLSM$p.value
      rowNums <- c()
      rowNums[1] <- dplyr::filter(BvsAdf, structure == struct & day == 0)$rowNM
      rowNums[2] <- dplyr::filter(BvsAdf, structure == struct & day == 1)$rowNM
      rowNums[3] <- dplyr::filter(BvsAdf, structure == struct & day == 2)$rowNM
      # BvsAdf$pval[row1] <- outputPvals[1] # day 0
      # BvsAdf$pval[row2] <- outputPvals[2] # day 1
      # BvsAdf$pval[row3] <- outputPvals[3] # day 2

      for (i in 1:length(rowNums)) {
        pvalue <- outputPvals[i] # day i
        if (!is.na(pvalue) & pvalue < pCuttoff) {
          BvsAdf[rowNums[i],]$pval <- pvalue
        } else {
          BvsAdf[rowNums[i],]$pval <- NA
        }
      }
      rm(pvalue)
    }
    rm(combData_trim)
    rm(combData_tr, envir = pkg.env)
  }
  BvsAdf <- BvsAdf[,1:4] # remove the rowNums column
  return(BvsAdf)
}
