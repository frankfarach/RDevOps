# Frank's helper functions for a variety of R tasks

helper.function <- function()
{
   return(1)
}

first_line <- function(df, split_by){
  # Given a data frame and a single factor on which to split it
  # return the first line.
  # Useful for extracting constant but repeated data
  # in conjunction with ddply
  
  require(plyr)
  
  if (class(df) != "data.frame") stop("df must be a data.frame")
  ddply(df, split_by, function(piece) piece[1, ])
}

same_dims <- function(df){
  # check if all data frames have the same number of rows and columns
  # if not, abort with error msg; otherwise continue with concatenation
  # df must be a list of data frames
  dims <- lapply(df, dim)
  if (length(Reduce(intersect, dims)) != 2) stop("Files don't all have the same number of rows and columns")
  else print("Same rows and same columns: **** PASSED ****")
}

same_cols <- function(df){
  # Check if all data frames have the same column names
  # df must be a list of data frames
  # Requires the plyr package
  require(plyr)
  
  cols <- lapply(df, names) # list of names for each data frame
  cols_length <- ldply(lapply(cols, length)) # Number of columns in each data frame
  
  # Check if the intersection of all column names has as many names as each data frame
  # If true, this logically entails that the names are the same
  if ((max(cols_length) == min(cols_length)) && (any(cols_length == 0) == FALSE)){
    if (length(Reduce(intersect, cols)) != max(cols_length)) stop("Column names aren't the same across files")
  }
  print("Same column names in the same order: **** PASSED ****")
}

csv_to_df_list <- function(file_pattern, path=".", sep=",", header=TRUE, full.names=TRUE, ...){
  # Get a list of all files ending in a particular extension,
  # read them all into a list of data frames.
  #
  # Arguments:
  # file_pattern: regular expression used to match file names
  # path: path to search for matching files. Default is current working directory.
  # sep: separator for csv file. Default is a comma. Use sep="\t" for tab-separated values.
  # header: include a header row. Change to FALSE if there isn't a header.
  # full.names: generate full path + filename for list.files.
  
  listfiles <- list.files(pattern = file_pattern, full.names = full.names, path = path)
  df <- lapply(listfiles, function(file) read.csv(file, header = header, sep = sep, ...))
  df
}

list_to_df <- function(df.list){
  # Given a list of data frames, check whether all have identical column names.
  # Requires that all rows have identical column names. Extra arguments
  # are passed through to the read.csv function applied to each file.
  # Requires: same_dims(), same_cols(), and csv_to_df_list (~/Dropbox/R/myHelpers.R)
  #
  # Arguments:
  # file_pattern: regular expression used to match file names
  # path: path to search for matching files. Default is current working directory.
  # sep: separator for csv file. Default is a comma. Use sep="\t" for tab-separated values.
  # header: include a header row. Change to FALSE if there isn't a header.
  # full.names: generate full path + filename for list.files.
  
  same_dims(df.list) # check dimensions of files
  same_cols(df.list) # check if all data frames have the same column names
  df <- do.call(rbind, df.list) # concatenate files
  df
}

csv_to_df <- function(file_pattern, ...){
  # Turn a list of CSV files matching file_pattern into a master data frame
  # with all CSV contents concatenated vertically, with checks that all data
  # frames have the same dimensions and column names.
  # Depends on list_to_df, csv_to_df_list, same_dims, and same_cols.
  
  list_of_dfs <- csv_to_df_list(file_pattern, sep = "\t", ...)
  df <- list_to_df(list_of_dfs)
  df
}

load_xlsx <- function(file_pattern, path = ".", sheetIndex = 1, full.names = TRUE, rowIndex, colIndex, ...){
  # Get a list of all files ending in a particular extension,
  # read them all into a list of data frames. Then check that they have the same number
  # of rows and columns and have the same column names.
  #
  # Arguments:
  # file_pattern: regular expression used to match file names
  # path: path to search for matching files. Default is current working directory.
  # sheetIndex: Numeric worksheet index; 1 would be the first worksheet in a workbook.
  # full.names: generate full path + filename for list.files.
  # rowIndex: which rows to include from each worksheet
  # colIndex: which columns to include from each worksheet
  
  require(xlsx)
  require(plyr)
  listfiles <- list.files(pattern = file_pattern, full.names = full.names, path = path)
  print(paste("Found", length(listfiles), "to import."))
  print(listfiles) # What files are we about to read in?
  reader <- function(file) read.xlsx(file, 
                                     sheetIndex = sheetIndex, 
                                     ...)[rowIndex, colIndex]
  df_list <- lapply(listfiles, reader) # Read each worksheet into a list of data frames
  if (length(df_list) == 0) stop("No files to import. Are there files you expected to load? Are they named properly?")
  print(paste(length(df_list), "files successfully imported."))
  same_dims(df_list) # Check that each data frame has the same number of rows and columns
  same_cols(df_list) # Check that each data frame has the same column names
  df <- do.call(rbind, df_list) # concatenate files into one large data frame
  df
}

se <- function(x, ...) {
   # calculate standard error of the mean
   N <- length(x)
   s <- var(x)
   s/N
}


mnctr <- function(x, ...) {
   # mean-center convenience function
   scale(x, scale=FALSE, ...)
}

prop2pc <- function(x, dig = 2) { 
  # Define function to convert proportion into percentage
  # with optionally specified number of rounding digits
   round((mean(x)*100), dig) 
}

binom.thresh <- function(qtiles, size, prob = 0.5) {
   # Calculate the binomial threshold in proportion to size
   # given mean, size of sample, and quantile(s).
   thresh <- qbinom(qtiles, size, prob) / size
   names(thresh) <- paste(round(qtiles*100, 4), "%", sep='')
   thresh
}


merge_csv <- function(file_pattern, sep, header = TRUE, ...){
   # Get a list of all files ending in a particular extension,
   # read them all into a list of data frames, then concatenate them row-wise.
   # Requires that all rows have identical column names.
   listfiles <- list.files(pattern = file_pattern, full.names = TRUE)
   df <- do.call(rbind, lapply(listfiles, function(x) read.csv(x, sep = sep, header = header)))
   df
}



summarySE <- function(data=NULL, measurevar, groupvars=NULL, na.rm=FALSE, conf.interval=.95, .drop=TRUE) {
  # Data summary functions
  # Source: http://www.cookbook-r.com/Graphs/Plotting_means_and_error_bars_(ggplot2)/
  
  ## Summarizes data.
  ## Gives count, mean, standard deviation, standard error of the mean, and confidence interval (default 95%).
  ##   data: a data frame.
  ##   measurevar: the name of a column that contains the variable to be summariezed
  ##   groupvars: a vector containing names of columns that contain grouping variables
  ##   na.rm: a boolean that indicates whether to ignore NA's
  ##   conf.interval: the percent range of the confidence interval (default is 95%)
   require(plyr)
   
   # New version of length which can handle NA's: if na.rm==T, don't count them
   length2 <- function (x, na.rm=FALSE) {
      if (na.rm) sum(!is.na(x))
      else       length(x)
   }
   
   # This is the summary; it's not easy to understand...
   datac <- ddply(data, groupvars, .drop=.drop,
                  .fun= function(xx, col, na.rm) {
                     c( N    = length2(xx[,col], na.rm=na.rm),
                        mean = mean   (xx[,col], na.rm=na.rm),
                        sd   = sd     (xx[,col], na.rm=na.rm)
                     )
                  },
                  measurevar,
                  na.rm
   )
   
   # Rename the "mean" column    
   datac <- rename(datac, c("mean"=measurevar))
   
   datac$se <- datac$sd / sqrt(datac$N)  # Calculate standard error of the mean
   
   # Confidence interval multiplier for standard error
   # Calculate t-statistic for confidence interval: 
   # e.g., if conf.interval is .95, use .975 (above/below), and use df=N-1
   ciMult <- qt(conf.interval/2 + .5, datac$N-1)
   datac$ci <- datac$se * ciMult
   
   return(datac)
}


normDataWithin <- function(data=NULL, idvar, measurevar, betweenvars=NULL,
                           na.rm=FALSE, .drop=TRUE) {
  ## Norms the data within specified groups in a data frame; it normalizes each
  ## subject (identified by idvar) so that they have the same mean, within each group
  ## specified by betweenvars.
  ##   data: a data frame.
  ##   idvar: the name of a column that identifies each subject (or matched subjects)
  ##   measurevar: the name of a column that contains the variable to be summariezed
  ##   betweenvars: a vector containing names of columns that are between-subjects variables
  ##   na.rm: a boolean that indicates whether to ignore NA's
   require(plyr)
   
   # Measure var on left, idvar + between vars on right of formula.
   data.subjMean <- ddply(data, c(idvar, betweenvars), .drop=.drop,
                          .fun = function(xx, col, na.rm) {
                             c(subjMean = mean(xx[,col], na.rm=na.rm))
                          },
                          measurevar,
                          na.rm
   )
   
   # Put the subject means with original data
   data <- merge(data, data.subjMean)
   
   # Get the normalized data in a new column
   measureNormedVar <- paste(measurevar, "_norm", sep="")
   data[,measureNormedVar] <- data[,measurevar] - data[,"subjMean"] +
      mean(data[,measurevar], na.rm=na.rm)
   
   # Remove this subject mean column
   data$subjMean <- NULL
   
   return(data)
}


summarySEwithin <- function(data=NULL, measurevar, betweenvars=NULL, withinvars=NULL,
                            idvar=NULL, na.rm=FALSE, conf.interval=.95, .drop=TRUE) {
   
  ## Summarizes data, handling within-subjects variables by removing inter-subject variability.
  ## It will still work if there are no within-S variables.
  ## Gives count, un-normed mean, normed mean (with same between-group mean),
  ##   standard deviation, standard error of the mean, and confidence interval.
  ## If there are within-subject variables, calculate adjusted values using method from Morey (2008).
  ##   data: a data frame.
  ##   measurevar: the name of a column that contains the variable to be summariezed
  ##   betweenvars: a vector containing names of columns that are between-subjects variables
  ##   withinvars: a vector containing names of columns that are within-subjects variables
  ##   idvar: the name of a column that identifies each subject (or matched subjects)
  ##   na.rm: a boolean that indicates whether to ignore NA's
  ##   conf.interval: the percent range of the confidence interval (default is 95%)
  
   # Ensure that the betweenvars and withinvars are factors
   factorvars <- vapply(data[, c(betweenvars, withinvars), drop=FALSE],
                        FUN=is.factor, FUN.VALUE=logical(1))
   
   if (!all(factorvars)) {
      nonfactorvars <- names(factorvars)[!factorvars]
      message("Automatically converting the following non-factors to factors: ",
              paste(nonfactorvars, collapse = ", "))
      data[nonfactorvars] <- lapply(data[nonfactorvars], factor)
   }
   
   # Get the means from the un-normed data
   datac <- summarySE(data, measurevar, groupvars=c(betweenvars, withinvars),
                      na.rm=na.rm, conf.interval=conf.interval, .drop=.drop)
   
   # Drop all the unused columns (these will be calculated with normed data)
   datac$sd <- NULL
   datac$se <- NULL
   datac$ci <- NULL
   
   # Norm each subject's data
   ndata <- normDataWithin(data, idvar, measurevar, betweenvars, na.rm, .drop=.drop)
   
   # This is the name of the new column
   measurevar_n <- paste(measurevar, "_norm", sep="")
   
   # Collapse the normed data - now we can treat between and within vars the same
   ndatac <- summarySE(ndata, measurevar_n, groupvars=c(betweenvars, withinvars),
                       na.rm=na.rm, conf.interval=conf.interval, .drop=.drop)
   
   # Apply correction from Morey (2008) to the standard error and confidence interval
   #  Get the product of the number of conditions of within-S variables
   nWithinGroups    <- prod(vapply(ndatac[,withinvars, drop=FALSE], FUN=nlevels,
                                   FUN.VALUE=numeric(1)))
   correctionFactor <- sqrt( nWithinGroups / (nWithinGroups-1) )
   
   # Apply the correction factor
   ndatac$sd <- ndatac$sd * correctionFactor
   ndatac$se <- ndatac$se * correctionFactor
   ndatac$ci <- ndatac$ci * correctionFactor
   
   # Combine the un-normed means with the normed results
   merge(datac, ndatac)
}