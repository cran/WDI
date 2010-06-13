\name{WDIsearch}
\alias{WDIsearch}
\title{World Development Indicators Search}
\description{Keyword search in the names of World Banks' World Development Indicators}
\usage{
WDIsearch(keyword)
}
\arguments{
  \item{keyword}{A text string}
}
\details{
Conducts a simple keyword search in the text of all the names of WDI variables (case in-sensitive grep) 
}
\value{
	Data frame with World Bank indicator codes and variable names.
}

\author{Vincent Arel-Bundock <varel@umich.edu>}
\examples{
\dontrun{
WDIsearch("gdp")
}
}

\keyword{ wdisearch}

