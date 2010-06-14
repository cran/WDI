WDI <- function(country="US", indicator="NY.GNS.ICTR.GN.ZS", start=2002, end=2005, extra=FALSE){
INDICATORS <- list()
missingind <- NULL
missing <- 0
for (z in 1:length(indicator)){
  COUNTRIES <- list()
  for (w in 1:length(country)){
    countrydata <- try(downloadwdi(country[w], indicator[z], start, end))
    if (class(countrydata)=="data.frame"){
      COUNTRIES[[w]] <- countrydata
    }
    else{next}
  }
  if (length(COUNTRIES)>0){
    INDICATORS[[(z-missing)]] <- do.call("rbind", COUNTRIES)
  }
  else{
    missing <- missing + 1
    missingind <- c(missingind, indicator[z])
    next}
}
if(length(missingind)>0){print(paste("Unable to download the following indicator(s):", paste(missingind, collapse=" , ")))}
if(length(INDICATORS)==1){DF <- INDICATORS[[1]]}
else{DF <- merge_recurse(INDICATORS)}
if(extra==TRUE){DF <- merge(DF, extrainfo())}
return(DF)
}
