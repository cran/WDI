extrainfo <- function(){
      urlextra <- "http://open.worldbank.org/countries?per_page=2500"
      docextra <- xmlTreeParse(urlextra, useInternal = TRUE)
      IDs <- lapply(getNodeSet(docextra, "//wb:country"), xmlAttrs)
      iso3c <- NULL
      for (z in 1:length(IDs)){
        iso3c <- c(iso3c, IDs[[z]][1])
      }
      DFextra <- data.frame(iso3c, xmlToDataFrame(docextra)[,c("incomeLevel", "iso2Code", "region")])
      DFextra[,"iso2c"] <- DFextra[,"iso2Code"]
      DFextra[,"iso2Code"] <- NULL
      for (m in 1:ncol(DFextra)){
        DFextra[,m] <- as.character(DFextra[,m])
      }
      return(DFextra)
    }