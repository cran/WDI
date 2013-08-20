\name{WDIcache}
\alias{WDIcache}
\title{Update the list of available WDI indicators}
\usage{
  WDIcache()
}
\value{
  Series of indicators, sources and descriptions in data
  frame format
}
\description{
  Download an updated list of available WDI indicators from
  the World Bank website. Returns a data frame for use in
  the \code{WDIsearch} function.
}
\note{
  Downloading all series information from the World Bank
  website can take time.

  The \code{WDI} package ships with a local data object
  with information on all the series available on
  2012-06-18. You can update this database by retrieving a
  new list using \code{WDIcache}, and then feeding the
  resulting object to \code{WDIsearch} via the \code{cache}
  argument.
}

