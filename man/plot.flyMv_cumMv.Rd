\name{plot.flyMv_cumMv}
\alias{plot.flyMv_cumMv}
\title{
Plot Cumulative Movement
}
\description{
Plots the cumulative movement with sex, treatments, and treatment levels over a given time span
}
\usage{
plot.flyMv_cumMv(speed, sex = NA, treatments = NA, hz = 5, time = 'min', treatmentLevels = NA, title = NA)
}
\arguments{
  \item{speed}{
  Data frame of speed information: \code{\link{readBinary}}
}
\item{sex}{
Vector of sex values for given columns
}
\item{treatments}{
Vector of treatments for given columns 
}
\item{hz}{
Hertz at which the original data was recorded
}
\item{time}{
Time scale to plot on('min' or 'h)
}
\item{treatmentLevels}{
Vector of treatment levels for given data set
}
\item{sampling}{
Gives sample size to to plot
}
