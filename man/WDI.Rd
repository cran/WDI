\name{WDI}
\alias{WDI}
\title{World Development Indicators}
\description{Downloads data from the World Bank's World Development Indicators}
\usage{
WDI(country="US", indicator="NY.GNS.ICTR.GN.ZS", start=2002, end=2005, extra=FALSE)
}
\arguments{
  \item{country}{Vector of countries (ISO-2 character codes, e.g. "BR", "US", "CA") for which the data is needed. Using the string "all" instead of iso codes pulls the data for every available country.}
  \item{indicator}{Vector of indicators (World Bank Codes, see the WDIsearch() function) for which the data is needed}
  \item{start}{First year of observation}
  \item{end}{Last year of observation}
  \item{extra}{TRUE returns the variables region, iso3c, and incomeLevel}
}
\details{
Downloads the requested data by using the World Bank's API, parses the resulting XML file, and formats it in a convenient country-year format. 
}
\value{
	Data frame with country-year observations. 
}
\note{
The function itself does not take a long time to execute, but the network connection may induce some delays.

Occasionally, WDI will produce an error in "matrix(vals, length(nfields))". This error is produced by the XML package, and is relatively harmless. It indicates that R was unable to connect to the World Bank's API to retrieve a given indicator. A list of missing indicators is printed after each use of WDI. 

Thanks to C and Adam Hyland for submitting code and bug reports.
}
\author{Vincent Arel-Bundock <varel@umich.edu>}
\examples{
\dontrun{
WDI(country=c("US","BR"), indicator=c("NY.GNS.ICTR.GN.ZS","NY.GNS.ICTR.GN.ZS"), start=1994, end=2000, extra=TRUE)
WDI(country="all", indicator="NY.GNS.ICTR.GN.ZS", start=1999, end=2000)
}
}

\keyword{ wdi}

