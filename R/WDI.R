WDI <- function(country="all", indicator="NY.GNS.ICTR.GN.ZS", start=1960, end=2009, extra=FALSE){
  DFindicator <- list()
  DFcountries <- list()
  for (o in 1:length(indicator)){
    for (z in 1:length(country)){
      DFcountries[[z]] <- downloadwdi(country[z], indicator[o], start, end)
    }
    DFindicator[[o]] <- do.call("rbind", DFcountries)
  }
  ifelse(length(indicator)==1, DF <- DFindicator, DF <- merge_recurse(DFindicator))
  ifelse(extra==TRUE, DF<-merge(DF, extrainfo()),DF<-DF)
  return(do.call("rbind", DF))
}
