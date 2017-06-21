#################################
# 4/25/2016                     #
# Sven Nelson                   #
# function: summaryWSvsWW       #
#################################

# Write a function to create a summary between WW and WS
# summaryWWvsWS (A = WW, B = WS)
#'
#' @export
#'
summaryWSvsWW <- function(dataWW, dataWS, measurevar = "length", pCuttoff = 0.05) {
  summaryBvsA(dataWW, dataWS, measurevar, pCuttoff)
}
