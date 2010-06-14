WDIsearch <- function(keyword){
  doc <- xmlTreeParse("http://open.worldbank.org/indicators?per_page=2500", useInternal=TRUE)
  DF <- xmlToDataFrame(doc)
  indicators <- unlist(lapply(getNodeSet(doc, "//wb:indicator"), xmlAttrs))
  DF <- data.frame(indicators, DF$name)[grep(keyword,DF$name, ignore.case=TRUE),]
  row.names(DF) <- NULL
  for (q in 1:ncol(DF)){DF[,q] <- as.character(DF[,q])}
  names(DF) <- c("indicator", "description")
  return(DF)
}