% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/yogitools.R
\name{global.extract.groups}
\alias{global.extract.groups}
\title{Extract regex groups (global)}
\usage{
global.extract.groups(x, re)
}
\arguments{
\item{x}{A vector of strings from which to extract the groups.}

\item{re}{The regular expression defining the groups}
}
\value{
A \code{list} of \code{matrix}'s containing the group contents, 
     with one list item for every element of x, and with each matrix 
     containing one column for each group and one row for each occurrence
     of the pattern.
}
\description{
Globally excise regular expression groups from string vectors.
I.e. only extract the all occurrences of each group within each string.
}
\keyword{expression}
\keyword{groups}
\keyword{regular}
