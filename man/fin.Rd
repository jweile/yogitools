% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/yogitools.R
\name{fin}
\alias{fin}
\title{Remove infinite and NA values}
\usage{
fin(x)
}
\arguments{
\item{x}{the object to which the function is applied}
}
\value{
the same object, with NAs and infinite values removed
}
\description{
Removes infinite and NA values from vectors, lists, matrices and data.frames.
}
\details{
Warning: If the given object is matrix or data.frame, any row containing infinite or NA
values is removed entirely. All columns must be numeric.
}
\examples{
fin(c(1,2,NA,3))
fin(data.frame(a=c(1,2,NA,3),b=c(4,5,6,7)))
}
