#################################
# 3/22/2016                     #
# Sven Nelson                   #
# function: rootPlot            #
#################################

## sumarySE function must exist --> try: Rmisc::summarySE() instead.
#'
#' @export
#'
rootPlot <- function (rootDF, title = "", max = 40, by = 5, returnSummary = FALSE) {
  ## Already in long format, next summarize data
  rootDFsumm <- Rmisc::summarySE(rootDF, measurevar = "length", groupvars=c("day","structure"))

  if (returnSummary) {
    return (rootDFsumm)
  }

  ## Plot the results for DF
  rootDFsumm$day <- factor(rootDFsumm$day, levels = c(0,1,2)) # necessary?
  #levels(rootDFsumm$day)

  rootDFsumm.d0 <- dplyr::filter(rootDFsumm, day==0, structure!="shoot")
  rootDFsumm.d1 <- dplyr::filter(rootDFsumm, day==1, structure!="shoot")
  rootDFsumm.d2 <- dplyr::filter(rootDFsumm, day==2, structure!="shoot")

  rootDFsumm.d0sht <- dplyr::filter(rootDFsumm, day==0, structure=="shoot")
  rootDFsumm.d1sht <- dplyr::filter(rootDFsumm, day==1, structure=="shoot")
  rootDFsumm.d2sht <- dplyr::filter(rootDFsumm, day==2, structure=="shoot")

  # to make it plot on top of r1, change the structure to r1
  rootDFsumm.d0sht$structure <- "r1"
  rootDFsumm.d1sht$structure <- "r1"
  rootDFsumm.d2sht$structure <- "r1"

  # make a function to just solve this problem of bar reordering a simpler way
  orderByNum <- function (dataframe, order=c("r4","r2","r1","r3","r5", "r6")) {
    for (i in 1:nrow(dataframe)) {
      if (dataframe[i,]$structure == "r1") {
        dataframe[i,]$structure <- 3
      } else if (dataframe[i,]$structure == "r2") {
        dataframe[i,]$structure <- 2
      } else if (dataframe[i,]$structure == "r3") {
        dataframe[i,]$structure <- 4
      } else if (dataframe[i,]$structure == "r4") {
        dataframe[i,]$structure <- 1
      } else if (dataframe[i,]$structure == "r5") {
        dataframe[i,]$structure <- 5
      } else if (dataframe[i,]$structure == "r6") {
        dataframe[i,]$structure <- 6
      }
    }
    return(dataframe)
  }


  rootDFsumm.d0 <- orderByNum(rootDFsumm.d0)
  rootDFsumm.d1 <- orderByNum(rootDFsumm.d1)
  rootDFsumm.d2 <- orderByNum(rootDFsumm.d2)
  rootDFsumm.d0sht <- orderByNum(rootDFsumm.d0sht)
  rootDFsumm.d1sht <- orderByNum(rootDFsumm.d1sht)
  rootDFsumm.d2sht <- orderByNum(rootDFsumm.d2sht)

  # require(ggplot2)
  # require(RColorBrewer)
  plotDF <- ggplot2::ggplot(data=rootDFsumm, ggplot2::aes(x = structure, y = length, group = structure, fill = day)) +
    #geom_bar(stat="identity") +
    ggplot2::geom_bar(stat="identity", position="identity", data = rootDFsumm.d2sht, color = "gray20", width=0.7) + # Default is: position="stack", which is why they are stacked, this allows overlay
    ggplot2::geom_bar(stat="identity", position="identity", data = rootDFsumm.d1sht, color = "gray20", width=0.7) + # Default is: position="stack", which is why they are stacked, this allows overlay
    ggplot2::geom_bar(stat="identity", position="identity", data = rootDFsumm.d0sht, color = "gray20", width=0.7) + # Default is: position="stack", which is why they are stacked, this allows overlay
    ggplot2::geom_bar(stat="identity", position="identity", data = rootDFsumm.d2, color = "gray20", width=0.7, ggplot2::aes(y = length *(-1))) + # Default is: position="stack", which is why they are stacked, this allows overlay
    ggplot2::geom_bar(stat="identity", position="identity", data = rootDFsumm.d1, color = "gray20", width=0.7, ggplot2::aes(y = length *(-1))) + # Default is: position="stack", which is why they are stacked, this allows overlay
    ggplot2::geom_bar(stat="identity", position="identity", data = rootDFsumm.d0, color = "gray20", width=0.7, ggplot2::aes(y = length *(-1))) + # Default is: position="stack", which is why they are stacked, this allows overlay
    ggplot2::geom_abline(ggplot2::aes(slope=0,intercept=0), color="black") +
    #geom_errorbar(data = rootDFsumm, aes(ymin=length-se, ymax=length+se), width=0.2) + # Add/adjust errbars (use sd for standard deviation or se for standard error)
    ggplot2::geom_errorbar(data = rootDFsumm.d0sht, ggplot2::aes(ymin=length-se, ymax=length+se), width=0.2) + # Add/adjust errbars (use sd for standard deviation or se for standard error)
    ggplot2::geom_errorbar(data = rootDFsumm.d1sht, ggplot2::aes(ymin=length-se, ymax=length+se), width=0.2) + # Add/adjust errbars (use sd for standard deviation or se for standard error)
    ggplot2::geom_errorbar(data = rootDFsumm.d2sht, ggplot2::aes(ymin=length-se, ymax=length+se), width=0.2) + # Add/adjust errbars (use sd for standard deviation or se for standard error)
    ggplot2::geom_errorbar(data = rootDFsumm.d0, ggplot2::aes(ymin=(length-se)*-1, ymax=(length+se)*-1), width=0.2) + # Add/adjust errbars (use sd for standard deviation or se for standard error)
    ggplot2::geom_errorbar(data = rootDFsumm.d1, ggplot2::aes(ymin=(length-se)*-1, ymax=(length+se)*-1), width=0.2) + # Add/adjust errbars (use sd for standard deviation or se for standard error)
    ggplot2::geom_errorbar(data = rootDFsumm.d2, ggplot2::aes(ymin=(length-se)*-1, ymax=(length+se)*-1), width=0.2) + # Add/adjust errbars (use sd for standard deviation or se for standard error)
    ggplot2::scale_fill_brewer(palette = "Accent") + # for colors
    #scale_fill_brewer(palette = "Dark2") + # for colors
    #scale_fill_brewer(palette = 6) + # black and white
    #scale_fill_manual(name="treatment", values=c("brown","orange","blue")) +
    # scale_y_continuous(breaks = seq(-35, 30, 5), labels = abs(seq(-35, 30, 5))) + # Adjusts y axis breaks and max/min values
    ggplot2::scale_y_continuous(breaks = seq(-(max), max, by), labels = abs(seq(-(max), max, by))) + # Adjusts y axis breaks and max/min values
    ggplot2::xlab("Structure") + # Set x-axis label
    ggplot2::ylab("Length (mm)") + # Set y-axis label
    ggplot2::theme_bw() +
    ggplot2::ggtitle(title) +
    # geom_text(data=rootDFsumm.d1, aes(x=4.5, y=30, label="shoot"), size = 10, color="black", inherit.aes=FALSE, parse=FALSE) + # Adds amino acid names as text in the bottom right corner of each facet
    ggplot2::geom_text(data=rootDFsumm.d0, ggplot2::aes(x=4.25, y= 60, label="shoot"), size = 10, color="black", inherit.aes=FALSE, parse=FALSE) + # Adds amino acid names as text in the bottom right corner of each facet
    # geom_text(data=rootDFsumm.d1, aes(x=4.5, y= 35, label="shoot"), size = 10, color="black", inherit.aes=FALSE, parse=FALSE) + # Adds amino acid names as text in the bottom right corner of each facet
    # geom_text(data=rootDFsumm.d1, aes(x=4.5, y=-33, label="roots"), size = 10, color="black", inherit.aes=FALSE, parse=FALSE) + # Adds amino acid names as text in the center right of each facet
    ggplot2::geom_text(data=rootDFsumm.d0, ggplot2::aes(x=4.4, y= -63, label="roots"), size = 10, color="black", inherit.aes=FALSE, parse=FALSE) + # Adds amino acid names as text in the center right of each facet
  # geom_text(data=rootDFsumm.d1, aes(x=4.5, y= -38, label="roots"), size = 10, color="black", inherit.aes=FALSE, parse=FALSE) + # Adds amino acid names as text in the center right of each facet
    #guides(fill = guide_legend(reverse=TRUE)) +
    ggplot2::scale_x_discrete(breaks = c(1,2,3,4,5,6), labels=c("r4","r2","r1","r3","r5","r6")) +
    ggplot2::theme(#strip.text.y = element_blank(), #element_text(face = "bold",size=12),
      #strip.text.x = element_text(face = "bold", size = 14),
      strip.background = ggplot2::element_rect(colour="NA", fill="NA"),
      axis.text.x = ggplot2::element_text(size = 14),
      axis.text.y = ggplot2::element_text(size = 14),
      axis.title.y = ggplot2::element_text(size = 16),
      axis.title.x = ggplot2::element_text(size = 16),
      legend.position = c(.900, .750), legend.background = ggplot2::element_rect(fill = "transparent"), legend.text.align=0)

  return(plotDF)
}
# rootLengthsApp(
#   wwday0csv = "~/CloudStation/000 Lab Notebook - Oliver Lab/Data collection/Root elongation WW vs WS (soil plates)/20160319 Chinese_Spring wt WW vs WS (3 plates each)/WW combined files/20160320 Chinese_Spring wt WW day0 (combined).csv",
#   wwday1csv = "~/CloudStation/000 Lab Notebook - Oliver Lab/Data collection/Root elongation WW vs WS (soil plates)/20160319 Chinese_Spring wt WW vs WS (3 plates each)/WW combined files/20160321 Chinese_Spring wt WW day1 (combined).csv",
#   wwday2csv = "~/CloudStation/000 Lab Notebook - Oliver Lab/Data collection/Root elongation WW vs WS (soil plates)/20160319 Chinese_Spring wt WW vs WS (3 plates each)/WW combined files/20160322 Chinese_Spring wt WW day2 (combined).csv",
#   wsday0csv = "~/CloudStation/000 Lab Notebook - Oliver Lab/Data collection/Root elongation WW vs WS (soil plates)/20160319 Chinese_Spring wt WW vs WS (3 plates each)/WS combined files/20160320 Chinese_Spring wt WS day0 (combined).csv",
#   wsday1csv = "~/CloudStation/000 Lab Notebook - Oliver Lab/Data collection/Root elongation WW vs WS (soil plates)/20160319 Chinese_Spring wt WW vs WS (3 plates each)/WS combined files/20160321 Chinese_Spring wt WS day1 (combined).csv",
#   wsday2csv = "~/CloudStation/000 Lab Notebook - Oliver Lab/Data collection/Root elongation WW vs WS (soil plates)/20160319 Chinese_Spring wt WW vs WS (3 plates each)/WS combined files/20160322 Chinese_Spring wt WS day2 (combined).csv", )

# summTest <- rootPlot(compileRootDF(day0csv = "~/CloudStation/000 Lab Notebook - Oliver Lab/Data collection/Root elongation WW vs WS (soil plates)/20160319 Chinese_Spring wt WW vs WS (3 plates each)/WW combined files/20160320 Chinese_Spring wt WW day0 (combined).csv", day1csv = "~/CloudStation/000 Lab Notebook - Oliver Lab/Data collection/Root elongation WW vs WS (soil plates)/20160319 Chinese_Spring wt WW vs WS (3 plates each)/WW combined files/20160321 Chinese_Spring wt WW day1 (combined).csv", day2csv = "~/CloudStation/000 Lab Notebook - Oliver Lab/Data collection/Root elongation WW vs WS (soil plates)/20160319 Chinese_Spring wt WW vs WS (3 plates each)/WW combined files/20160322 Chinese_Spring wt WW day2 (combined).csv"), returnSummary = T)
