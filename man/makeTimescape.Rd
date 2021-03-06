% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/makeTimescape.R
\name{makeTimescape}
\alias{makeTimescape}
\title{A Cell Tree Generating Function using timescape}
\usage{
makeTimescape(dataSet, outputDir = ".")
}
\arguments{
\item{dataSet}{a ctgPHYLset object}

\item{outputDir}{the directory where output should be saved, defaults to
the current working directory.}
}
\value{
an updated ctgPHYLset object
}
\description{
This function, called by \code{\link{generate_tree}}, creates visualizations
using the data and \pkg{timescape}.
}
\keyword{internal}
