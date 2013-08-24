# Install commonly used packages
# Useful to run when updating R

install.packages(c("Amelia",
                   "blockrand",
                   "ggplot2",
                   "reshape",
                   "reshape2",
                   "Hmisc",
                   "pander",
                   "rapport",
                   "geepack",
                   "MASS",
                   "mediation",
                   "mi",
                   "psych",
                   "psychometric",
                   "plyr",
                   "WhatIf",
                   "Zelig",
                   "ProjectTemplate",
                   "plink",
                   "ltm",
                   "lordif",
                   "rgm",
                   "devtools",
                   "xlsx"
                   )
               )

# Install Shiny
options(repos=c(RStudio='http://rstudio.org/_packages', getOption('repos')))
install.packages('shiny')

# Install Slidify
library(devtools)
install_github("slidify", "ramnathv")
install_github("slidifyLibraries", "ramnathv")
