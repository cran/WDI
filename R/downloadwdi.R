downloadwdi <- function(country,indicator,start,end){
    url <- paste('http://open.worldbank.org/countries/',country,'/indicators/',indicator,'?date=',start,':',end,'&per_page=2500',sep='')
    
  ### Find number of pages to request (with per_page=2500)
    ifelse(country=="all", ncalls <- ceiling(230*(end+1-start)/2500), ncalls <- ceiling((length(country)+2)*(end+1-start)/2500))

  ### Loop over the necessary number of pages
  DFLIST <- list()
  for (i in 1:ncalls){
    tempurl <- paste(url,'&page=',i,sep='')
    doc <- xmlTreeParse(tempurl, useInternal = TRUE)

    ### Extract
    DF <- data.frame(xmlToDataFrame(doc)[,2:4],
                     unlist(lapply(getNodeSet(doc, "//wb:country"), xmlAttrs)))
    names(DF) <- c("country","year",indicator , "iso2c")
    
    ### Clean
    for (y in 1:ncol(DF)){DF[,y] <- as.character(DF[,y])}
    DF[,indicator] <- as.numeric(DF[,indicator])
    DF[,"year"] <- as.numeric(DF[,"year"])
    DFLIST[[i]] <- unique(DF)
  }
 return(unique(do.call("rbind", DFLIST)))
}

