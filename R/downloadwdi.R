downloadwdi <- function(country,indicator,start,end){
    url <- paste('http://open.worldbank.org/countries/',country,'/indicators/',indicator,'?date=',start,':',end,'&per_page=2500',sep='')
    
  ### Number of pages to request (with per_page=2500)
    ifelse(country=="all", ncalls <- ceiling(230*(end+1-start)/2500), ncalls <- ceiling((length(country)+2)*(end+1-start)/2500))

  ### Loop over the necessary number of pages
  for (i in 1:ncalls){
    tempurl <- paste(url,'&page=',i,sep='')
    doc <- xmlTreeParse(tempurl, useInternal = TRUE)

    ### Extract
    DF <- xmlToDataFrame(doc)[,2:4]

    ### iso2 codes
    iso2c <- unlist(lapply(getNodeSet(doc, "//wb:country"), xmlAttrs))

    ### Clean
    DF <- data.frame(DF, iso2c)
    for (y in 1:ncol(DF)){
      DF[,y] <- as.character(DF[,y])
    }
    DF[,indicator] <- as.numeric(DF[,"value"])
    DF[,"value"] <- NULL
    DF[,"year"] <- as.numeric(DF[,"date"])
    DF[,"date"] <- NULL
    DFLIST <- list()
    DFLIST[[i]] <- unique(DF)
  }
 return(unique(do.call("rbind", DFLIST)))
}