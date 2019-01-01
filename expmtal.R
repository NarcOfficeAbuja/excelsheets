data <- allData[[28]][[1]]

allIsNaCols <- apply(data, 2, function(x)
  all(is.na(x)))

# Fish out the columns and rows that have all NAs
# and have the first proper header canditate, respecively

data <- data[,!(allIsNaCols)]

anyIsNaRows <- apply(data, 1, function(x) ! anyNA(x))
isNARowsIndex <- which(anyIsNaRows)[1]
newColnames <- unname(unlist(unclass(data[isNARowsIndex,])))
newColnames
