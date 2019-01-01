# unify.R

library(readxl)
library(tidyverse)


excelFiles <- list.files(pattern = '\\.(xls|xlsx)$')

allData <- lapply(excelFiles, function(file) {
  
  # Determine the number of sheets the file
  sheetNameList <- excel_sheets(file)
  dfs <- lapply(sheetNameList, function(sheet) {
    
    d <- read_excel(file, sheet = sheet, col_types = 'text')
    if (!(nrow(d) == 0)) {
      
      # Search for default R column names meaning data
      # were incorrectly read from Excel
      ## We're doing this only with data frames that have
      ## this format 
      if ('X__1' %in% colnames(d)) {
        # Apply a search for NA patterns by rows and then by columns
        allIsNaCols <- apply(d, 2, function(x) all(is.na(x)))
        
        # Fish out the columns and rows that have all NAs
        # and have the first proper header canditate, respecively
        
        d <- d[, !(allIsNaCols)]
        
        anyIsNaRows <- apply(d, 1, function(x) !anyNA(x))
        isNARowsIndex <- which(anyIsNaRows)[1]
        newColnames <- unname(unlist(unclass(d[isNARowsIndex, ])))
        d <- d[-(1:isNARowsIndex), ]
        colnames(d) <- newColnames
      }
      d
    }
  })
  
  # Remove zero-value data frames
  ind <- sapply(dfs, function(df) nrow(df) == 0)
  dfs[ind] <- NULL

  dfs
})



# For each file:


