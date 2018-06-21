\name{loadPackages}
\alias{loadPackages}
\title{
Load Packages
}
\description{
  Loads missing packages from a vector of package names
}
\usage{
loadPackages(names)
}
\arguments{
  \item{names}{
  List of names of packages to check for and, if necessary, install
}
}

\examples{
loadPackages(c("data.table", "zoo", "matrixStats", "ggplot2", "tidyverse"))

