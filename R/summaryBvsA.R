#################################
# 4/25/2016                     #
# Sven Nelson                   #
# function: summaryBvsA         #
#################################

# Write a function to create a summary between WW and WS
# summaryWWvsWS
#'
#' @export
#'
summaryBvsA <- function(A, B, measurevar = "length", pCuttoff = 0.05) {

  summA <- rootPlot(rootDF = A, returnSummary = T)
  summB <- rootPlot(rootDF = B, returnSummary = T)

  # create dataframe
  # columns: diff (WW$length - WS$length), day (keep), structure (keep),

  BvsAdf <- summA[,1:2]
  BvsAdf$diff <- summB$length - summA$length
  BvsAdf$pval <- NA
  for (i in 1:length(BvsAdf$pval)) {
    #repsA <- A$length[A$day==BvsAdf$day[i] & A$structure==BvsAdf$structure[i]]
    repsA <- dplyr::filter(A, day == BvsAdf$day[i], structure == BvsAdf$structure[i])$length
    #repsB <- B$length[B$day==BvsAdf$day[i] & B$structure==BvsAdf$structure[i]]
    repsB <- dplyr::filter(B, day == BvsAdf$day[i], structure == BvsAdf$structure[i])$length
    pvalue <- stats::t.test(repsB, repsA)$p.value
    if (!is.na(pvalue) & pvalue < pCuttoff) {
      BvsAdf$pval[i] <- pvalue
    } else {
      BvsAdf$pval[i] <- NA
    }
    rm(repsA)
    rm(repsB)
    rm(pvalue)
  }
  return(BvsAdf)
}
