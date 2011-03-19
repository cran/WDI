WDI <- function (country = "all", indicator = "NY.GNS.ICTR.GN.ZS", start = 2002, end = 2005, extra = FALSE){

  ## Clean user error
  indicator <- sub("^[ \t]+|[ fire\t]+$", "", indicator)
  country <- sub("^[ \t]+|[ \t]+$", "", country)

  ## Download data in XML format for each country and each indicator
  dat.full <- NULL
  dat.indicator <- NULL
  for (a in indicator){
    for (b in country){
      daturl <- paste("http://api.worldbank.org/countries/", b, 
                      "/indicators/", a,
                      "?date=",start,":",end, 
                      "&per_page=25000",
                      "&format=XML",
                      sep = "")
      dat <- xmlTreeParse(daturl, useInternal = TRUE)
      iso2c <- unlist(lapply(getNodeSet(dat, "//wb:country"), xmlAttrs))
      dat <- xmlToDataFrame(dat)[,2:4]
      dat$iso2c <- iso2c

      ## Clean to ensure smooth merging
      colnames(dat) <- c("country","year", a, "iso2c")
      dat$year <- as.numeric(as.character(dat$year))
      dat$iso2c <- gsub(" ", "", as.character(dat$iso2c))
      dat$country <- gsub(" ", "", as.character(dat$country))
      dat.indicator <- rbind(dat.indicator, dat)
      daturl <- NULL
    }
    
    ## Merge
    if(is.null(dat.full)){
      dat.full <- dat.indicator
    }else{
      dat.full <- merge(dat.full, dat.indicator, all=TRUE)
    }
    dat.indicator <- NULL
  }

  ## Clean data
  dat.full <- dat.full[order(dat.full$iso2c, dat.full$year), c("country","iso2c", "year", indicator)]
  for (i in 3:ncol(dat.full)){dat.full[,i] <- as.numeric(as.character(dat.full[,i]))}

  ## Extras
  if(extra==TRUE){
	dat.full <- merge(dat.full, extra2010, all.x=TRUE)
}
  return(dat.full)
}

WDIsearch <- function(string="gdp", field="N", indicator=NULL){
  if(is.null(indicator)==FALSE){
    if(indicator %in% series2010$Series.Code){
      return(series2010[series2010$Series.Code==indicator, c("Series.Code", "Series.Name", "Short.definition", "Long.definition")])
      stop()
    }
  }
  if(field=="SD"){
    dat <- series2010[grep(string, series2010$Short.definition, ignore.case=TRUE), c("Series.Code","Short.definition")]
  }else if(field=="LD"){
    dat <- series2010[grep(string, series2010$Long.definition, ignore.case=TRUE), c("Series.Code","Long.definition")]
  }else{
    dat <- series2010[grep(string, series2010$Series.Name, ignore.case=TRUE), c("Series.Code","Series.Name")]
  }
  return(dat)
}
