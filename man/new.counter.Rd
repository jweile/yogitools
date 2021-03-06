% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/yogitools.R
\name{new.counter}
\alias{new.counter}
\title{Create new Counter}
\usage{
new.counter()
}
\value{
An object of type \code{yogicounter}.
}
\description{
This constructor method creates an object that can count 
occurrences of different items. It allows importing and exporting
of the counter status in string form.
}
\details{
The object has the following methods:
\itemize{
 \item \code{inc(id)}: Increase the counter for item with \code{id} by 1.
 \item \code{add(id,x)}: Add \code{x} occurrences for the item with \code{id}.
 \item \code{get(id)}: Get the number of occurrences seen for item \code{id}.
 \item \code{ls(id)}: List all counts for all items by id.
 \item \code{export(id)}: Exports the counter state to a string that can be saved or logged.
 \item \code{import.add(str)} Imports a previous counter state from the string \code{str} 
     and adds it to the current counts.
}
}
\examples{
cn <- new.counter()
cn$inc("foo")
cn$inc("bar")
cn$add("foo",6)
cn$get("foo")
# 7
cn$ls()
# foo 7
# bar 1
cn$export()
# foo=7,bar=1
}
