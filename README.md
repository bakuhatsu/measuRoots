# measuRoots
A package for automated data analysis of wheat root lengths exported from SmartRoot.  This package includes the following functions:

```r
mergeCSVs() #merges multiple .csv files exported from SmartRoot into one .csv file (This is also an RStudio addin)
compileRootDF() #processes +0h, +24h, and +48h .csv files & outputs dataframe for plotting
rootPlot() #to plot the data
summaryWSvsWW() #to summarize data for plotting significant differences
plotDifferences() #to plot the differences between WW and WS at each timepoint with significance indicated
rootLengthsApp() #graphical app (and RStudio addin) runs as an RStudio gadget or in web browser (displays, outputs, return plots & summaries)
```

Most of the data analysis pipeline can be run from the `rootLengthsApp` graphical interface.  `rootLengthsApp` is a neat shiny gadget and Rstudio addin that can run in an RStudio window or in a local web browser.


### Installing and loading the `measuRoots` package:
Installing through GitHub requires the `devtools` package, so instructions for installing and loading that package are provided below.
```r
## Install and load devtools for loading packages from GitHub
install.packages("devtools") # to allow us to install packages from GitHub
library(devtools)
```
Since GitHub packaged are compiled on your machine to run, you may be prompted to "install build tools" or something similar.  Follow the instructions, which will install the tools needed to compile this package.
  
Now you should be ready to install and then load the `measuRoots` package
```r
install_github("bakuhatsu/measuRoots") # syntax for installing from GitHub: username/library
library(measuRoots) # To load the package (not necessary to directly use the addin version)
```
Run directly from the command line with:
```r  
rootLengthsApp()
# or if the package isn't loaded:
measuRoots::rootLengthsApp()
``` 
The app does not require RStudio, but works nicely in this IDE.  If run outside of RStudio, will load in the default web browser.  
