\name{WDIsearch}
\alias{WDIsearch}
\title{World Development Indicators Search}
\description{Keyword search in the names of World Bank's World Development Indicators}
\usage{
WDIsearch(string='gdp', field='N', indicator=NULL)
}
\arguments{
  \item{string}{A text string to search for}
  \item{field}{The information field to search. 'N' is the name of the series. 'LD' is the long description of the series. 'SD' is the short description of the series.}
  \item{indicator}{World Bank indicator string (e.g. 'NY.GNS'ICTR.GN.ZS')}
}
\details{
Conducts a simple keyword search in the text of all the names of WDI variables (case in-sensitive grep) 
}
\value{
	Data frame with World Bank indicator codes and text field where the string was found.
}

\author{Vincent Arel-Bundock <varel@umich.edu>}
\examples{
\dontrun{
WDIsearch("gdp")
}
}

\keyword{wdisearch}

