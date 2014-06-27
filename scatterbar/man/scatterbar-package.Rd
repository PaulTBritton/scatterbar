\name{scatterbar-package}
\alias{scatterbar-package}
\docType{package}
\title{
plot scatterbars from a list of number vectors
}
\description{
plot scatterbars from a list of number vectors
}
\details{
\tabular{ll}{
Package: \tab scatterbar\cr
Type: \tab Package\cr
Version: \tab 1.0\cr
Date: \tab 2014-06-27\cr
License: \tab GPL-2\cr
}
~~ An overview of how to use the package, including the most important ~~
~~ functions ~~
}
\author{
Paul Thomas Britton

Maintainer: Paul Thomas Britton <paul.t.britton@nasa.gov>
}
\references{
~~ Literature or other references for background information ~~
}
~~ Optionally other standard keywords, one per line, from file KEYWORDS in ~~
~~ the R documentation directory ~~
\keyword{ package }
\seealso{
~~ Optional links to other man pages, e.g. ~~
~~ \code{\link[<pkg>:<pkg>-package]{<pkg>}} ~~
}
\examples{
x <- c(.1,.2,.3)
y <- c(.2,.02,.03)
scatterbar(file="xyplot.tiff",lst=plotlist(x,y))
}
