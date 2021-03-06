% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ancestors.R
\name{getNode}
\alias{getNode}
\title{Scrape data for an academic record}
\usage{
getNode(id)
}
\arguments{
\item{id}{integer number, the identifier of a mathematician in the Mathematics Genealogy Project}
}
\description{
Scrape data for an academic record
}
\examples{
\dontrun{
mathGen:::getNode(id = 15860) # John Wilder Tukey
dh <- mathGen:::getNode(id = 7298) # David Hilbert
}
}

