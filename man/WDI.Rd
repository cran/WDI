\name{WDI}
\alias{WDI}
\title{World Development Indicators}
\description{Extract and format data from the World Bank's World Development Indicators}
\usage{
WDI(country="all", indicator="NY.GNS.ICTR.GN.ZS", start=2002, end=2005, extra=FALSE)
}
\arguments{
  \item{country}{Vector of countries (ISO-2 character codes, e.g. "BR", "US", "CA") for which the data is needed. Using the string "all" instead of individual iso codes pulls data for every available country.}
  \item{indicator}{Character vector of indicators codes. See the WDIsearch() function.}
  \item{start}{First year of data}
  \item{end}{Last year of data}
  \item{extra}{TRUE returns extra variables such as region, iso3c code, and incomeLevel}
}
\details{
Downloads the requested data by using the World Bank's API, parses the resulting XML file, and formats it in long country-year format. 
}
\value{
	Data frame with country-year observations. 
}
\note{
The function itself does not take a long time to execute, but the network connection may induce some delays.

Thanks to C and Adam Hyland for submitting code and bug reports.
}
\author{Vincent Arel-Bundock <varel@umich.edu>}
\examples{
\dontrun{
WDI(country="all, indicator=("AG.AGR.TRAC.NO","TM.TAX.TCOM.BC.ZS"), years=1990:2000)
WDI(country=c("US","BR"), indicator="NY.GNS.ICTR.GN.ZS", years=1999, extra=TRUE)
}
}

\keyword{ wdi}

