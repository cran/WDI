\name{WDIdefinition}
\alias{WDIdefinition}
\title{World Development Indicators Definition}
\description{Definitions for World Bank's World Development Indicators}
\usage{
WDIdefinition(indicator)
}
\arguments{
  \item{indicator}{A text string that corresponds to a WDI indicator (e.g. GC.FIN.FRGN.GD.ZS}
}
\details{
Prints the definition of a WDI variable 
}
\value{
	Data frame with World Bank indicator code and definition.
}
\author{Vincent Arel-Bundock <varel@umich.edu>}
\examples{
\dontrun{
WDIdefinition("GC.FIN.FRGN.GD.ZS")
}
}

\keyword{ wdisearch}

