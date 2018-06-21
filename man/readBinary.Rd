\name{readBinary}
\alias{readBinary}
\title{
readBinary
}
\description{
readBinary reads a binary file into a data frame. Specifically singles as exported by autotracker.
}
\usage{
readBinary(fileName, colCount, start = 1, end = colCount)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{fileName}{
The location of binary file holding movement data
}
\item{colCount}{
The count of columns in the given binary data set
}
\item{start}{
Index of first column you want to take
}
\item{end}{
Index of the last column you want to take
}
\details{

}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~
\section{Examples}{
\examples{

readBinary("05-19-2018-19-13-19_Speed.bin", 144, 1,10)

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
}
}
