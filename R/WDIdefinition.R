WDIdefinition <- function(indicator){
  url <- paste("http://open.worldbank.org/indicators/",indicator,sep="")
  doc <- xmlTreeParse(url, useInternal = TRUE)
  print(indicator)
  print(xmlValue(getNodeSet(doc, "//wb:name")[[1]]))
  print(xmlValue(getNodeSet(doc, "//wb:sourceNote")[[1]]))
}