#################################
# 10/28/2016                    #
# Sven Nelson                   #
# function: plotDifferences     #
#################################

#'
#' @export
#'
plotDifferences <- function(summaryBvsA, ymin = -20.5) {

  summaryBvsA$structure <- factor(summaryBvsA$structure, levels=c("shoot","r1", "r2","r3","r4","r5", "r6"))
  ## Plot the difference
  diffPlot <- ggplot2::ggplot(summaryBvsA, ggplot2::aes(x = day, y = diff)) +
    ggplot2::geom_hline(yintercept = 0) +
    ggplot2::geom_line(ggplot2::aes(group = structure), size=0.8) +
    ggplot2::geom_point(data = summaryBvsA[is.na(summaryBvsA$pval),], colour = "grey", size=3) +
    ggplot2::geom_point(data = summaryBvsA[!is.na(summaryBvsA$pval),], colour = "black", size=3) +
    #geom_point(aes(colour = -pval), size=3) +
    ggplot2::xlab("Time after water stress (hours)") + # Set x-axis label
    ggplot2::ylab("Difference (WS - WW)") + # Set y-axis label
    ggplot2::scale_x_continuous(breaks=c(0,1,2), labels = c(" WS","24h","48h ")) +
    ggplot2::ylim(ymin,10) + # default ylim(-20,10)
    ggplot2::theme_bw() +
    ggplot2::facet_wrap(~structure, nrow = 2, ncol = 3, dir = "v") +
    ggplot2::theme(#strip.text.y = element_blank(), #element_text(face = "bold",size=12),
      strip.text.x = ggplot2::element_text(size = 14),
      #strip.background = element_rect(colour="NA", fill="NA"),
      axis.text.x = ggplot2::element_text(size = 11),
      axis.text.y = ggplot2::element_text(size = 12),
      axis.title.y = ggplot2::element_text(size = 15),
      axis.title.x = ggplot2::element_text(size = 15))

  if (nrow(dplyr::filter(summaryBvsA, day == 2 & structure == "r6")) > 0) {
    diffPlot <- diffPlot + ggplot2::facet_wrap(~structure, nrow = 2, ncol = 4, dir = "v")
  }

  return(diffPlot)
}


